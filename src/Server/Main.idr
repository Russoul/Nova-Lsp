||| Implementation of a Idris2 LSP Server.
|||
||| (C) The Idris Community, 2021
module Server.Main

import Data.List1
import Data.String
import Data.Ref

import Language.JSON
import Language.LSP.Message
import Language.LSP.Utils

import Server.Configuration
import Server.Log
import Server.ProcessMessage
import Server.Response
import Server.Utils

import System
import System.Directory
import System.File

data Header = ContentLength Int | ContentType String | StartContent

Show Header where
  show (ContentLength l) = "Content-Length: " ++ show l
  show (ContentType s) = "Content-Type: " ++ s
  show StartContent = "stop"

headerPart : List Header -> String
headerPart [] = ""
headerPart (ContentLength l :: xs) = "Content-Length: " ++ show l ++ "\r\n" ++ headerPart xs
headerPart (ContentType s :: xs) = "Content-Type: " ++ s ++ "\r\n" ++ headerPart xs
headerPart (StartContent :: _) = "\r\n"

parseHeader : String -> Maybe Header
parseHeader "\r\n" = Just StartContent
parseHeader str =
  if "Content-Length:" `isPrefixOf` str
     then let (_ ::: xs) = split (== ':') str in
              ContentLength <$> parseInteger (fastConcat xs)
     else if "Content-Type:" `isPrefixOf` str
             then let (_ ::: xs) = split (== ':') str in
                      Just $ ContentType (fastConcat xs)
             else Nothing

parseHeaderPart : (h : File) -> IO (Either FileError (Maybe Int))
parseHeaderPart h = do
  Right line <- fGetHeader h
    | Left err => pure $ Left err
  case parseHeader line of
    Just (ContentLength l) => parseHeaderPart h *> pure (Right (Just l))
    Just (ContentType s) => parseHeaderPart h
    Just StartContent => pure $ Right Nothing
    Nothing => pure $ Right Nothing

handleMessage : Ref LSPConf LSPConfiguration
            => IO ()
handleMessage = Prelude.do
  inputHandle <- gets LSPConf inputHandle
  Right (Just l) <- parseHeaderPart inputHandle
    | _ => do logD Channel "Cannot parse message header"
              sendUnknownResponseMessage parseError
  Right msg <- fGetChars inputHandle l
    | Left err => Prelude.do
        logE Server "Cannot retrieve body of message: \{show err}"
        sendUnknownResponseMessage $ internalError "Error while recovering the content part of a message"
  logD Channel "Received message: \{msg}"
  let Just msg = parse msg
    | _ => do logE Channel "Cannot parse message"
              sendUnknownResponseMessage parseError
  let JObject fields = msg
    | _ => do logE Channel "Message is not a JSON object"
              sendUnknownResponseMessage $ invalidRequest "Message is not object"
  let Just (JString "2.0") = lookup "jsonrpc" fields
    | _ => do logE Channel "Message has no jsonrpc field"
              sendUnknownResponseMessage (invalidRequest "jsonrpc is not \"2.0\"")
  case lookup "method" fields of
    Just methodJSON => do -- request or notification
      case lookup "id" fields of
        Just idJSON => do -- request
          let Just id = fromJSON {a=OneOf [Int, String]} idJSON
            | _ => do logE Channel "Message id is not of the correct type"
                      sendUnknownResponseMessage (invalidRequest "id is not int or string")
          let Just method = fromJSON {a=Method Client Request} methodJSON
            | _ => do logE Channel "Method not found"
                      sendResponseMessage Initialize $ Failure (extend id) methodNotFound
          logI Channel "Received request for method \{show (toJSON method)}"
          let Just params = fromMaybeJSONParameters method (lookup "params" fields)
            | _ => do logE Channel "Message with method \{show (toJSON method)} has invalid parameters"
                      sendResponseMessage method $ Failure (extend id) (invalidParams "Invalid params for send \{show methodJSON}")
          -- handleRequest can be modified to use a callback if needed
          result <- handleRequest method params
          sendResponseMessage method $ case result of
            Left error => Failure (extend id) error
            Right result => Success (extend id) result

        Nothing => do -- notification
          let Just method = fromJSON {a=Method Client Notification} methodJSON
            | _ => do logE Channel "Method not found"
                      sendUnknownResponseMessage methodNotFound
          logI Channel "Received notification for method \{show (toJSON method)}"
          let Just params = fromMaybeJSONParameters method (lookup "params" fields)
            | _ => do logE Channel "Message with method \{show (toJSON method)} has invalid parameters"
                      sendUnknownResponseMessage $ invalidParams "Invalid params for send \{show methodJSON}"
          handleNotification method params

    Nothing => do -- response
      let Just idJSON = lookup "id" fields
        | _ => do logE Channel "Received message with neither method nor id"
                  sendUnknownResponseMessage (invalidRequest "Message does not have method or id")
      logW Server "Ignoring response with id \{show idJSON}"

runServer : Ref LSPConf LSPConfiguration
         => IO ()
runServer = handleMessage >> runServer

LSP_SEMANTIC_TOKEN_OFFSET_ENCODING : String
LSP_SEMANTIC_TOKEN_OFFSET_ENCODING = "LSP_SEMANTIC_TOKEN_OFFSET_ENCODING"

LSP_WORKSPACE : String
LSP_WORKSPACE = "LSP_WORKSPACE"

parseSemTokenOffset : String -> IO SemanticTokenOffsetEncoding
parseSemTokenOffset "UTF-16" = pure UTF16
parseSemTokenOffset "codepoint" = pure Codepoint
parseSemTokenOffset other = do
  putStrLn "Bad value of \{LSP_SEMANTIC_TOKEN_OFFSET_ENCODING}: \{other}; valid options: {'UTF-16', 'codepoint'}"
  exitFailure

main : IO ()
main = do
  enc <- fromMaybe "UTF-16" <$> getEnv LSP_SEMANTIC_TOKEN_OFFSET_ENCODING
  enc <- parseSemTokenOffset enc
  workspace <- getEnv LSP_WORKSPACE
  Just cur <- currentDir
   | _ => do
     putStrLn "Can't figure out the current working directory"
     exitFailure
  let workspace = fromMaybe cur workspace
  l <- newRef LSPConf ({semanticTokenOffsetEncoding := enc} (defaultConfig workspace))
  runServer

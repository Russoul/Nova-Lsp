||| Functions for the processing of received request and
||| notifications.
|||
||| (C) The Idris Community, 2021
module Server.ProcessMessage

import Data.Ref
import Data.List
import Data.SnocList
import Data.AVL
import Data.OneOf
import Data.SortedSet
import Data.Maybe
import Data.Util

import Language.JSON
import Language.LSP.Message
import Language.LSP.Utils

import Server.Capabilities
import Server.Configuration
import Server.Log
import Server.SemanticTokens
import Server.Response
import Server.Utils
import System
import System.Clock
import System.Directory
import System.File
import System.Path
import System.File.ReadWrite
import Data.List1

import Nova.Core.Context
import Nova.Core.Language
import Nova.Core.Evaluation
import Nova.Core.Monad
import Nova.Core.Substitution
import Nova.Core.Unification
import Nova.Core.Pretty

import Nova.Surface.Language
import Nova.Surface.ModuleSystem
import Nova.Surface.Operator
import Nova.Surface.Parser
import Nova.Surface.SemanticToken

import Data.Location

import Text.PrettyPrint.Prettyprinter

isDirty : Ref LSPConf LSPConfiguration => DocumentURI -> IO Bool
isDirty uri = gets LSPConf (contains uri . dirtyFiles)

isError : Ref LSPConf LSPConfiguration => DocumentURI -> IO Bool
isError uri = gets LSPConf (contains uri . errorFiles)

IOError : Type
IOError = String

takeUpTo : (a -> Bool) -> List a -> List a
takeUpTo f [] = []
takeUpTo f (x :: xs) =
  ifThenElse (f x)
    (x :: takeUpTo f xs)
    [x]

loadURI : Ref LSPConf LSPConfiguration
       => InitializeParams
       -> URI
       -> Maybe Int
       -> IO (Either String ())
loadURI conf uri version = Prelude.do
  logI Server "Loading file \{show uri}"
  update LSPConf ({openFile := Just (uri, fromMaybe 0 version)})
  let fpath = uri.path
  let caps = (publishDiagnostics <=< textDocument) . capabilities $ conf
  workspace <- gets LSPConf workspaceFolder
  let modulesFile = workspace </> "modules.npkg"
  Just modules <- readModuleDescription modulesFile
    | _ => do let msg = "Cannot read the modules file at \{modulesFile}"
              logE Server msg
              pure $ Left msg
  let Just openModule = fileStem fpath
    | _ => do
       let msg = "can't compute the file stem"
       logE Server msg
       pure $ Left msg

  let modules = takeUpTo (/= openModule) modules
  Right (ops, sig, omega, nextOmegaIdx, namedHoles, toks) <- checkModules [<] [<] empty 0 empty empty workspace modules
    | Left (filename, r, msg) => do
       logE Server (renderDocNoAnn msg)
       case filename == Just fpath of
         True => sendDiagnostics caps uri version [(r, msg)]
         False => sendDiagnostics caps uri version [(Nothing, pretty "In file \{show filename}" <+> hardline <+> msg)]
       pure $ Left (renderDocNoAnn msg)
  logI Channel "File type checked successfully"
  update LSPConf {quickfixes := [],
                  semanticTokens := toks,
                  hasError := False,
                  nextOmegaIdx := nextOmegaIdx,
                  namedHoles := namedHoles,
                  sigma := sig,
                  omega := omega}
  sendDiagnostics caps uri version []
  pure $ Right ()

loadIfNeeded : Ref LSPConf LSPConfiguration
            => InitializeParams -> URI -> Maybe Int -> IO (Either String ())
loadIfNeeded conf uri version = do
  Just (oldUri, oldVersion) <- gets LSPConf openFile
    | Nothing => loadURI conf uri version
  if (oldUri == uri && (isNothing version || (Just oldVersion) == version))
     then pure $ Right ()
     else loadURI conf uri version

withURI : Ref LSPConf LSPConfiguration
       => InitializeParams
       -> URI -> Maybe Int -> IO (Either ResponseError a) -> IO (Either ResponseError a) -> IO (Either ResponseError a)
withURI conf uri version d k = do
  False <- isError uri
    | _ => logW Server "Trying to load \{show uri} which has errors" >> d
  case !(loadIfNeeded conf uri version) of
       Right () => k
       Left err => do
         logE Server "Error while loading \{show uri}: \{show err}"
         pure $ Left (MkResponseError (Custom 3) err JNull)

||| Guard for requests that requires a successful initialization before being allowed.
whenInitializedRequest : Ref LSPConf LSPConfiguration => (InitializeParams -> IO (Either ResponseError a)) -> IO (Either ResponseError a)
whenInitializedRequest k =
  case !(gets LSPConf initialized) of
       Just conf => k conf
       Nothing => do logE Server "Cannot process requests before initalization"
                     pure $ Left $ serverNotInitialized

||| Guard for requests that cannot be sent after the shutdown protocol.
whenNotShutdownRequest : Ref LSPConf LSPConfiguration => IO (Either ResponseError a) -> IO (Either ResponseError a)
whenNotShutdownRequest k =
  if !(gets LSPConf isShutdown)
     then do logE Server "Cannot process requests after shutdown"
             pure $ Left $ invalidRequest "Server has been shutdown"
     else k

||| whenInitializedRequest + whenNotShutdownRequest
whenActiveRequest : Ref LSPConf LSPConfiguration => (InitializeParams -> IO (Either ResponseError a)) -> IO (Either ResponseError a)
whenActiveRequest = whenNotShutdownRequest . whenInitializedRequest

||| Guard for notifications that requires a successful initialization before being allowed.
whenInitializedNotification : Ref LSPConf LSPConfiguration => (InitializeParams -> IO ()) -> IO ()
whenInitializedNotification k =
  case !(gets LSPConf initialized) of
       Just conf => k conf
       Nothing => do logE Server "Cannot process notification before initialization"
                     sendUnknownResponseMessage $ serverNotInitialized

||| Guard for notifications that cannot be sent after the shutdown protocol.
whenNotShutdownNotification : Ref LSPConf LSPConfiguration => IO () -> IO ()
whenNotShutdownNotification k =
  if !(gets LSPConf isShutdown)
     then do logE Server "Cannot process notifications after shutdown"
             sendUnknownResponseMessage $ invalidRequest "Server has been shutdown"
     else k

||| whenInitializedNotification + whenNotShutdownNotification
whenActiveNotification : Ref LSPConf LSPConfiguration => (InitializeParams -> IO ()) -> IO ()
whenActiveNotification = whenNotShutdownNotification . whenInitializedNotification

findUserMeta : OrdTree (String, List (Data.Location.Range, OmegaName)) ByFst
            -> String
            -> Data.Location.Range
            -> Maybe OmegaName
findUserMeta tree file r = do
  list <- findFile (List.inorder tree) file
  map snd (find (\(x, _) => isWithin r x) list)
 where
  findFile : List (String, List (Data.Location.Range, OmegaName)) -> String -> Maybe (List (Data.Location.Range, OmegaName))
  findFile list n = map snd (find (\(x, _) => x == n) list)

export
handleRequest :
       Ref LSPConf LSPConfiguration
    => (method : Method Client Request)
    -> (params : MessageParams method)
    -> IO (Either ResponseError (ResponseResult method))

handleRequest Initialize params = Prelude.do
  -- TODO: Here we should analyze the client capabilities.
  logI Channel "Received initialization request"

  case params.initializationOptions of
       Just (JObject xs) => do
         case lookup "logFile" xs of
              Just (JString fname) => changeLogFile fname
              Just _ => logE Configuration "Incorrect type for log file location, expected string"
              Nothing => pure ()
         case lookup "longActionTimeout" xs of
              Just _ => logE Configuration "Incorrect type for long action timeout, expected number"
              Nothing => pure ()
         case lookup "maxCodeActionResults" xs of
              Just (JNumber v) => update LSPConf ({ searchLimit := integerToNat $ cast v })
              Just _ => logE Configuration "Incorrect type for max code action results, expected number"
              Nothing => pure ()
       Just _ => logE Configuration "Incorrect type for initialization options"
       Nothing => pure ()

  update LSPConf ({initialized := Just params})
  logI Server "Server initialized and configured"
  pure $ pure $ MkInitializeResult serverCapabilities (Just serverInfo)

handleRequest Shutdown params = do
  logI Channel "Received shutdown request"
  -- In a future multithreaded model, we must guarantee that all pending request are still executed.
  update LSPConf ({isShutdown := True})
  logI Server "Server ready to be shutdown"
  pure $ pure $ (the (Maybe Null) Nothing)

handleRequest TextDocumentHover params = whenActiveRequest $ \conf => do
  logI Channel "Received hover request for \{show params.textDocument.uri}"
  withURI conf params.textDocument.uri Nothing (pure $ pure $ make $ MkNull) $ do
    let p = cast {to = Point} params.position
    namedHoles <- gets LSPConf namedHoles
    sig <- gets LSPConf sigma
    omega <- gets LSPConf omega
    let file = params.textDocument.uri.path
    let r = MkRange p p
    let Just n = findUserMeta namedHoles file r
      | _ => do
        let markupContent = MkMarkupContent PlainText "Can't find hole at that location"
        let hover = MkHover (make markupContent) Nothing
        pure $ pure (make hover)
    let Just entry = lookup n omega
      | _ => do
        let markupContent = MkMarkupContent PlainText "[Critical] Couldn't find the meta in Ω"
        let hover = MkHover (make markupContent) Nothing
        pure $ pure (make hover)
    Right doc <- eval (prettyOmegaEntry sig omega n entry) ()
      | Left err => do
        let markupContent = MkMarkupContent PlainText "Encountered problem while printing the meta: \{err}"
        let hover = MkHover (make markupContent) Nothing
        pure $ pure (make hover)
    let line = renderDocNoAnn doc
    let markupContent = MkMarkupContent PlainText line
    let hover = MkHover (make markupContent) Nothing
    pure $ pure (make hover)

handleRequest TextDocumentDefinition params = whenActiveRequest $ \conf => do
  logW Channel $ "Received an unsupported definition request"
  pure $ pure $ make MkNull

handleRequest TextDocumentCodeAction params = whenActiveRequest $ \conf => do
  logW Channel $ "Received an unsupported code-action request"
  pure $ pure $ make MkNull

handleRequest TextDocumentSignatureHelp params = whenActiveRequest $ \conf => do
  logW Channel $ "Received an unsupported signature-help request"
  pure $ pure $ make MkNull

handleRequest TextDocumentDocumentSymbol params = whenActiveRequest $ \conf => do
  logW Channel $ "Received an unsupported document-symbol request"
  pure $ pure $ make MkNull

handleRequest TextDocumentCodeLens params = whenActiveRequest $ \conf => do
  -- TODO: still unimplemented, but send empty message to avoid errors
  -- in clients since it's a common request
  logW Channel $ "Received an unsupported codeLens request"
  pure $ pure $ make MkNull

handleRequest TextDocumentCompletion params = whenActiveRequest $ \conf => do
  -- TODO: still unimplemented, but send empty message to avoid errors
  -- in clients since it's a common request
  logW Channel $ "Received an unsupported completion request"
  pure $ pure $ make MkNull

handleRequest TextDocumentDocumentLink params = whenActiveRequest $ \conf => do
  -- TODO: still unimplemented, but send empty message to avoid errors
  -- in clients since it's a common request
  logW Channel $ "Received an unsupported documentLink request"
  pure $ pure $ make MkNull

handleRequest TextDocumentSemanticTokensFull params = whenActiveRequest $ \conf => do
    -- False <- gets LSPConf (contains params.textDocument.uri . semanticTokensSentFiles)
    --   | True => pure $ Left (MkResponseError RequestCancelled "Semantic tokens already sent" JNull)
    withURI conf params.textDocument.uri Nothing (pure $ Left (MkResponseError RequestCancelled "Document Errors" JNull)) $ do
      let fname = params.textDocument.uri.path
      tokss <- gets LSPConf semanticTokens
      let Just (source, toks) = lookup fname tokss
        | _ => pure $ pure $ make MkNull
      hasError <- gets LSPConf hasError
      case hasError of
        True =>
          -- We don't want to resend the tokens because they've been
          -- invalidated by an error in the elaboration process.
          pure $ pure $ make MkNull
        False => Prelude.do
          encodedToks <- getSemanticTokens source toks
          pure $ pure $ (make $ encodedToks)
{- handleRequest WorkspaceExecuteCommand
  (MkExecuteCommandParams partialResultToken "normaliseSelection" (Just [json])) = whenActiveRequest $ \conf => do
    let Just loc@(MkLocation uri range) = fromJSON {a = Location} json
      | _ => do pure $ Left (invalidParams "Expected a {uri, range}")
    Just gamma <- gets LSPConf context
      | Nothing => pure $ Left (invalidParams "Type check the modules first!")
    ng <- gets LSPConf namegen
    Just phi <- gets LSPConf phiContext
      | Nothing => pure $ Left (invalidParams "Type check the modules first!")
    let (Just stem, Just "hott") = (fileStem uri.path, extension uri.path)
      | _ => pure $ Left (invalidParams "Not a .hott file")
    let loc : Definition.Loc = cast loc
    let Just (Just x, idx, (entry ** withinEntry)) = extractTermFromGlobalContext' gamma loc
      | _ => pure $ Right (JString "Couldn't extract a term at that range")
    let (tm ** withinTerm) = toDTerm entry withinEntry
    let (tau, that) = getSubterm tm withinTerm
    let (_, Right (ng, tm')) = run (normaliseTerm gamma phi.metas (cast tau) [<] doUnfold that) ng
      | _ => pure $ Right (JString "Unexpected error during evaluation")
    let entry' = setSubterm entry withinEntry tm'
    let gamma = OrdTree.insert (x, idx, entry') gamma
    update LSPConf {context := Just gamma}
    let textEdit = MkTextEdit (cast (snd loc))
                    (renderDocNoAnn $ prettyTermLvl ([<] <>< tau.vars) tm' InArg)
    let workspaceEdit = MkWorkspaceEdit
         {changes = Just (singleton uri [textEdit]),
          documentChanges = Nothing,
          changeAnnotations = Nothing}
    update LSPConf {namegen := ng}
    sendRequestMessage_ WorkspaceApplyEdit (MkApplyWorkspaceEditParams Nothing workspaceEdit)
    pure $ Right (JString "OK")
handleRequest WorkspaceExecuteCommand
  (MkExecuteCommandParams partialResultToken "inferInContext" (Just [json])) = whenActiveRequest $ \conf => do
    let Just loc@(MkExpressionWithPosition strTerm uri position) = fromJSON {a = ExpressionWithPosition} json
      | _ => do pure $ Left (invalidParams "Expected a {expression, uri, position}")
    namegen <- gets LSPConf namegen
    Just gamma <- gets LSPConf context
      | Nothing => pure $ Left (invalidParams "Type check the modules first!")
    Just phi <- gets LSPConf phiContext
      | Nothing => pure $ Left (invalidParams "Type check the modules first!")
    let (Just stem, Just "hott") = (fileStem uri.path, extension uri.path)
      | _ => pure $ Left (invalidParams "Not a .hott file")
    let p : Definition.Point = cast position
    let Just tau = extractCtxFromGlobalContext gamma (Just uri.path, p)
      | _ => pure $ Right (JString "Couldn't extract context at that position")
    let Right (_, preTerm) = parseFull' term strTerm
      | Left (Error errMsg _) => pure $ Right (JString errMsg)
    let (_, Right (st, term)) = run (desugar gamma phi.metas (cast tau) preTerm Nothing) (MkDesugarSt (vargen 0) [<] [<] False)
      | (_, Left err) => pure $ Right (JString (prettyErrorStringNoAnsi err))
    let (_, Right (namegen, ty)) = run (infer gamma phi.metas (cast tau) term (Action Given term)) namegen
      | (_, Left err) => pure $ Right (JString (prettyErrorStringNoAnsi err))
    update LSPConf {namegen := namegen}
    pure $ Right (JString (renderDocNoAnn (prettyTerm (cast tau.vars) ty)))
handleRequest WorkspaceExecuteCommand
  (MkExecuteCommandParams partialResultToken "contextInfo" (Just [json])) = whenActiveRequest $ \conf => do
    let Just loc@(MkExpressionWithPosition _ uri position) = fromJSON {a = ExpressionWithPosition} json
      | _ => do pure $ Left (invalidParams "Expected a {expression, uri, position}")
    namegen <- gets LSPConf namegen
    Just gamma <- gets LSPConf context
      | Nothing => pure $ Left (invalidParams "Type check the modules first!")
    Just phi <- gets LSPConf phiContext
      | Nothing => pure $ Left (invalidParams "Type check the modules first!")
    let (Just stem, Just "hott") = (fileStem uri.path, extension uri.path)
      | _ => pure $ Left (invalidParams "Not a .hott file")
    let p : Definition.Point = cast position
    let Just tau = extractCtxFromGlobalContext gamma (Just uri.path, p)
      | _ => pure $ Right (JString "Couldn't extract context at that position")
    let (_, Right (namegen, tau)) = run (normaliseTau gamma phi.metas [<] [<] doUnfold tau) namegen
      | _ => pure $ Right (JString "Unexpected error while normalising")

    update LSPConf {namegen := namegen}
    pure $ Right (JString (renderDocNoAnn (prettyContext ([<] <>< tau))))
handleRequest WorkspaceExecuteCommand
  (MkExecuteCommandParams partialResultToken "inferSelection" (Just [json])) = whenActiveRequest $ \conf => do
    let Just loc@(MkLocation uri range) = fromJSON {a = Location} json
      | _ => do pure $ Left (invalidParams "Expected a {uri, range}")
    namegen <- gets LSPConf namegen
    Just gamma <- gets LSPConf context
      | Nothing => pure $ Left (invalidParams "Type check the modules first!")
    Just phi <- gets LSPConf phiContext
      | Nothing => pure $ Left (invalidParams "Type check the modules first!")
    let (Just stem, Just "hott") = (fileStem uri.path, extension uri.path)
      | _ => pure $ Left (invalidParams "Not a .hott file")
    let loc : Definition.Loc = cast loc
    let Just (tau, term) = extractTermFromGlobalContext gamma loc
      | _ => pure $ Right (JString "Couldn't extract a term at that range")
    let (_, Right (namegen, ty)) = run (infer gamma phi.metas (cast tau) term (Action Given term)) namegen
      | (_, Left err) => pure $ Right (JString (prettyErrorStringNoAnsi err))
    update LSPConf {namegen := namegen}
    pure $ Right (JString (renderDocNoAnn (prettyTerm (cast tau.vars) ty))) -}
handleRequest method params = whenActiveRequest $ \conf => do
    logW Channel $ "Received a not supported \{show (toJSON method)} request"
    pure $ Left methodNotFound

export
handleNotification :
       Ref LSPConf LSPConfiguration
    => (method : Method Client Notification)
    -> (params : MessageParams method)
    -> IO ()

handleNotification Exit params = do
  logI Channel "Received exit notification"
  status <- if !(gets LSPConf isShutdown)
               then logI Server "Quitting the server..." >> pure ExitSuccess
               else logC Server "Quitting the server without a proper shutdown" >> pure (ExitFailure 1)
  exitWith status

handleNotification TextDocumentDidOpen params = whenActiveNotification $ \conf => do
  logI Channel "Received didOpen notification for \{show params.textDocument.uri}"
  ignore $ loadURI conf params.textDocument.uri (Just params.textDocument.version)

handleNotification TextDocumentDidSave params = whenActiveNotification $ \conf => do
  logI Channel "Received didSave notification for \{show params.textDocument.uri}"
  update LSPConf
    { dirtyFiles $= delete params.textDocument.uri
    , errorFiles $= delete params.textDocument.uri
    }
  ignore $ loadURI conf params.textDocument.uri Nothing
  when (fromMaybe False $ conf.capabilities.workspace >>= semanticTokens >>= refreshSupport) $ do
    logI SemanticTokens "Sending semantic tokens for \{show params.textDocument.uri}"
    sendRequestMessage_ WorkspaceSemanticTokensRefresh Nothing

handleNotification TextDocumentDidChange params = whenActiveNotification $ \conf => do
  logI Channel "Received didChange notification for \{show params.textDocument.uri}"
  update LSPConf { dirtyFiles $= insert params.textDocument.uri }
  logI Server "File \{show params.textDocument.uri} marked as dirty"

handleNotification TextDocumentDidClose params = whenActiveNotification $ \conf => do
  logI Channel "Received didClose notification for \{show params.textDocument.uri}"
  update LSPConf {         openFile := Nothing
                         , quickfixes := []
                         , dirtyFiles $= delete params.textDocument.uri
                         , errorFiles $= delete params.textDocument.uri
                         }
  logI Server $ "File \{show params.textDocument.uri} closed"

handleNotification method params = whenActiveNotification $ \conf =>
  logW Channel "Received unhandled notification for method \{stringify $ toJSON method}"

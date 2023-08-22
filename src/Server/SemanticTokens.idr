module Server.SemanticTokens

import Data.List
import Data.Location
import Data.Ref
import Data.SnocList
import Data.String

import Language.LSP.Message

import Server.Configuration
import Server.Capabilities
import Server.Log
import Server.Utils

import Nova.Surface.SemanticToken


||| encode using relative tokens according the to LSP spec
encode : Point -> List SemanticToken -> List Int
encode _ [] = []
encode (relLine, relStartChar) (((MkRange (sl, sc) (el, ec)), decor) :: xs) =
  encoding ++ encode (sl, sc) xs
 where
  ||| Line, StartChar, Length, TokenType, TokenModifiers
  encoding : List Int
  encoding = [ sl - relLine
             , if sl == relLine then sc - relStartChar else sc
             , ec - sc
             , encodeDecorAsNum decor
             , 0]

||| Converts a Range given in codepoint coordinates to UTF-16 surrogate pair coordinates.
convert : String -> Int -> Int -> Int -> Int
convert line i wantedI acc =
  case i == wantedI of
    True => acc
    False => convert line (1 + i) wantedI
      (acc + (ifThenElse (ord (assert_total $ strIndex line (cast i)) <= 0xFFFF) 1 2))

||| Assume that tokens can only span one line.
||| Convert codepoint offsets to UTF16 offsets.
convertTokens : (lastLineNum : Int) -> List String -> List SemanticToken -> List SemanticToken
convertTokens _ _ [] = []
convertTokens lastLineNum lines ((MkRange (sl, sc) (el, ec), ann) :: rest) =
  case drop (cast $ sl - lastLineNum) lines of
    line :: lines =>
       (MkRange (sl, convert line 0 sc 0) (el, convert line 0 ec 0), ann)
         :: convertTokens sl (line :: lines) rest
    [] => [] -- Must not happen


||| Get the semantic tokens from the Metadata
export
getSemanticTokens : Ref LSPConf LSPConfiguration => String -> SnocList SemanticToken -> IO SemanticTokens.SemanticTokens
getSemanticTokens source toks = do
  whichKindOfOffsetToDo <- gets LSPConf semanticTokenOffsetEncoding
  let ls = lines source
  logD SemanticTokens "Encoding semantic highlightning metadata"
  let encodedTokens =
       case whichKindOfOffsetToDo of
         UTF16 => encode (0, 0) (convertTokens 0 ls (cast $ sortSemanticTokens toks))
         Codepoint => encode (0, 0) (cast $ sortSemanticTokens toks)
  pure $ MkSemanticTokens Nothing encodedTokens

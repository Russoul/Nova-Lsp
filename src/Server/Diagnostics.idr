module Server.Diagnostics

import Data.OneOf
import Data.String
import Data.Location

import Language.JSON
import Language.LSP.Message

import Server.Configuration
import Server.Log
import Server.Utils

import System.File
import System.Path

import Nova.Surface.SemanticToken

import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter

buildDiagnostic : Loc -> String -> Maybe (List DiagnosticRelatedInformation) -> Diagnostic
buildDiagnostic loc error related =
  MkDiagnostic
    { range = fromMaybe dummyRange (map cast (snd loc))
    , severity = Just Error
    , code = Nothing
    , codeDescription = Nothing
    , source = Just "hott"
    , message = error
    , tags = Nothing
    , relatedInformation = related
    , data_ = Nothing
    }

||| Computes a LSP `Diagnostic` from a compiler error.
|||
||| @caps The client capabilities related to diagnostics
||| @uri The URI of the source file.
||| @err The compiler error.
export
toDiagnostic : (caps : Maybe PublishDiagnosticsClientCapabilities)
            -> (uri : URI)
            -> (error : String)
            -> IO Diagnostic
toDiagnostic caps uri s = do
  pure $ buildDiagnostic EmptyLoc
    s Nothing

export
toDiagnostics : (caps : Maybe PublishDiagnosticsClientCapabilities)
             -> (uri : URI)
             -> (error : String)
             -> IO (List1 Diagnostic)
toDiagnostics caps uri s = pure $ !(toDiagnostic caps uri s) ::: []

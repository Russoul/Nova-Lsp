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

import Nova.Core.Pretty
import Nova.Surface.SemanticToken

import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter

buildDiagnostic : Maybe Data.Location.Range -> Doc Ann -> Maybe (List DiagnosticRelatedInformation) -> Diagnostic
buildDiagnostic r error related =
  MkDiagnostic
    { range = fromMaybe dummyRange (map cast r)
    , severity = Just Error
    , code = Nothing
    , codeDescription = Nothing
    , source = Just "hott"
    , message = renderDocNoAnn error
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
            -> (error : (Maybe Data.Location.Range, Doc Ann))
            -> IO Diagnostic
toDiagnostic caps uri (r, doc) = do
  pure $ buildDiagnostic r doc Nothing

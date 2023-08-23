||| Definition and helper functions related to the LSP server implementation
||| configuration.
|||
||| (C) The Idris Community, 2021
module Server.Configuration

import Data.AVL
import Data.SortedSet
import Language.LSP.Message.CodeAction
import Language.LSP.Message.Initialize
import Language.LSP.Message.Hover
import Language.LSP.Message.Location
import Language.LSP.Message.URI
import System.File

import Data.Location

import Nova.Core.Language
import Nova.Core.Monad
import Nova.Core.Unification
import Nova.Surface.SemanticToken

||| Label for the configuration reference.
public export
data LSPConf : Type where

public export
data SemanticTokenOffsetEncoding : Type where
  Codepoint : SemanticTokenOffsetEncoding
  UTF16 : SemanticTokenOffsetEncoding

||| Type for the LSP server configuration.
public export
record LSPConfiguration where
  constructor MkLSPConfiguration
  ||| File handle where to read LSP messages.
  inputHandle : File
  ||| File handle where to output LSP messages.
  outputHandle : File
  ||| File handle where to put log messages.
  logHandle : File
  ||| If the initialization protocol has succeded, it contains the client configuration.
  ||| @see https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#initialize
  initialized : Maybe InitializeParams
  ||| True if the client has completed the shutdown protocol.
  ||| @see https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#shutdown
  isShutdown : Bool
  ||| The currently loaded file, if any, and its version.
  openFile : Maybe (DocumentURI, Int)
  ||| Files with modification not saved. Command will fail on these files.
  dirtyFiles : SortedSet DocumentURI
  ||| Files with errors
  errorFiles : SortedSet DocumentURI
  ||| File sources and semantic tokens of all checked files.
  semanticTokens : List (Location.FileName, String, SnocList SemanticToken)
  ||| Limit for multiple search results
  searchLimit : Nat
  ||| List of quickfixes to be send in addition to other code actions
  quickfixes : List CodeAction
  ||| next id for requests to the server
  nextRequestId : Nat
  ||| Last check ended up in an error.
  hasError : Bool
  ||| Σ
  sigma : Signature
  ||| Ω
  omega : Omega
  ||| Unification state
  nextOmegaIdx : Nat
  ||| Named holes
  namedHoles : OrdTree (String, List (Data.Location.Range, OmegaName)) ByFst
  semanticTokenOffsetEncoding : SemanticTokenOffsetEncoding
  workspaceFolder : String

||| Server default configuration. Uses standard input and standard output for input/output.
export
defaultConfig : (workspaceFolder : String) -> LSPConfiguration
defaultConfig workspaceFolder =
  MkLSPConfiguration
    { inputHandle       = stdin
    , outputHandle      = stdout
    , logHandle         = stderr
    , initialized       = Nothing
    , isShutdown        = False
    , openFile          = Nothing
    , dirtyFiles        = empty
    , errorFiles        = empty
    , semanticTokens    = []
    , searchLimit       = 5
    , quickfixes        = []
    , nextRequestId     = 0
    , hasError          = False
    , sigma             = [<]
    , omega             = empty
    , nextOmegaIdx      = 0
    , namedHoles        = empty
    , semanticTokenOffsetEncoding = UTF16
    , workspaceFolder = workspaceFolder
    }

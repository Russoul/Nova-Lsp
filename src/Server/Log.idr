||| Logging utilities for the LSP server implementation.
|||
||| (C) The Idris Community, 2021
module Server.Log

import Data.Ref

import System.Directory
import System.File
import System.Path
import System

import Language.LSP.Severity

import Server.Configuration
import Server.Utils

%default total

public export
data Topic
  = AddClause
  | CaseSplit
  | Channel
  | CodeAction
  | Configuration
  | Diagnostic
  | DocumentSymbol
  | ExprSearch
  | GenerateDef
  | GotoDefinition
  | Hover
  | MakeCase
  | MakeLemma
  | MakeWith
  | QuickFix
  | RefineHole
  | SemanticTokens
  | Server
  | SignatureHelp

export
Show Topic where
  show AddClause = "Request.CodeAction.AddClause"
  show CaseSplit = "Request.CodeAction.CaseSplit"
  show Channel = "Communication.Channel"
  show CodeAction = "CodeAction"
  show Configuration = "Request.Configuration"
  show Diagnostic = "Notification.Diagnostic"
  show DocumentSymbol = "Request.DocumentSymbol"
  show ExprSearch = "Request.CodeAction.ExprSearch"
  show GenerateDef = "Request.CodeAction.GenerateDef"
  show GotoDefinition = "Request.GotoDefinition"
  show Hover = "Request.Hover"
  show MakeCase = "Request.CodeAction.MakeCase"
  show MakeLemma = "Request.CodeAction.MakeLemma"
  show MakeWith = "Request.CodeAction.MakeWith"
  show QuickFix = "Request.CodeAction.QuickFix"
  show RefineHole = "Request.CodeAction.RefineHole"
  show SemanticTokens = "Notification.SemanticTokens"
  show Server = "Server"
  show SignatureHelp = "Request.SignatureHelp"

||| Logs a string with the provided severity level.
export
log : Ref LSPConf LSPConfiguration => Severity -> Topic -> String -> IO ()
log severity topic msg = do
  logHandle <- gets LSPConf logHandle
  Right () <- fPutStrLn logHandle "LOG \{show severity}:\{show topic}: \{msg}"
    | Left err =>
        die "Error in fPutStrLn while writing to the log file: \{show err}"
  fflush logHandle

export
logD : Ref LSPConf LSPConfiguration => Topic -> String -> IO ()
logD = log Debug

export
logI : Ref LSPConf LSPConfiguration => Topic -> String -> IO ()
logI = log Info

export
logW : Ref LSPConf LSPConfiguration => Topic -> String -> IO ()
logW = log Warning

export
logE : Ref LSPConf LSPConfiguration => Topic -> String -> IO ()
logE = log Error

export
logC : Ref LSPConf LSPConfiguration => Topic -> String -> IO ()
logC = log Critical

||| Changes the log file location, if possible.
export covering
changeLogFile : Ref LSPConf LSPConfiguration => String -> IO ()
changeLogFile fname = do
  let True = isAbsolute fname
    | False => logE Configuration "Unable to change log file location: \{fname} is not an absolute path"
  whenJust (parent fname) $ \dir => do
    Right _ <- mkdirAll dir
      | Left err => logE Configuration "Unable to create directory \{dir}: \{show err}"
    logD Configuration "Created new directory \{dir} for log file \{fname}"
  Right handle <- openFile fname Append
    | Left err => logE Configuration "Unable to updated log file location \{fname}: \{show err}"
  update LSPConf {logHandle := handle}
  logI Configuration "Log file location updated to \{fname}"

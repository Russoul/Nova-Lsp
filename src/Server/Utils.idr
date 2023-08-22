module Server.Utils

import Data.Ref
import Data.Location

import Language.LSP.Message

import System.File
import System.Path
import System.Directory
import System

||| Gets a specific component of a reference, using the supplied projection.
export
gets : (l : label) -> Ref l a => (a -> b) -> IO b
gets l f = f <$> get l

export
dirExists : String -> IO Bool
dirExists dir = do Right d <- openDir dir
                       | Left _ => pure False
                   closeDir d
                   pure True

-- Create subdirectories, if they don't exist
export
covering
mkdirAll : String -> IO (Either FileError ())
mkdirAll dir = if parse dir == emptyPath
                  then pure (Right ())
                  else do exist <- dirExists dir
                          if exist
                             then pure (Right ())
                             else do Right () <- case parent dir of
                                          Just parent => mkdirAll parent
                                          Nothing => pure (Right ())
                                        | err => pure err
                                     createDir dir

public export
dummyPosition : Position
dummyPosition = MkPosition 0 0

public export
dummyRange : Language.LSP.Message.Location.Range
dummyRange = MkRange dummyPosition dummyPosition

public export
Cast Data.Location.Point Language.LSP.Message.Location.Position where
  cast (x, y) = MkPosition x y

public export
Cast Data.Location.Range Language.LSP.Message.Location.Range where
  cast (MkRange start end) = MkRange (cast start) (cast end)

module Data.Ref

import Data.IORef

public export
data Ref : (l : label) -> Type -> Type where
     [search l]
     MkRef : IORef a -> Ref x a

export
newRef : (x : label) -> t -> IO (Ref x t)
newRef x val
    = do ref <- newIORef val
         pure (MkRef ref)

export %inline
get : (x : label) -> {auto ref : Ref x a} -> IO a
get x {ref = MkRef io} = readIORef io

export %inline
put : (x : label) -> {auto ref : Ref x a} -> a -> IO ()
put x {ref = MkRef io} val = writeIORef io val

export %inline
update : (x : label) -> {auto ref : Ref x a} -> (a -> a) -> IO ()
update x f
  = do v <- get x
       put x (f v)

module Network.HTTP.Pool.IOStuff

import Utils.Handle
import Utils.Bytes
import Utils.Misc
import Utils.Handle.C
import Control.Linear.LIO
import Network.Socket
import Data.String
import Data.String.Extra
import Network.HTTP.Message
import Network.HTTP.Header
import Data.Nat
import Data.Vect
import System.Future
import System
import Data.Fin

%hide Network.Socket.close

public export
OkOrError : Type -> Type -> Type
OkOrError t_ok t_closed = Res Bool $ \ok => if ok then Res String (const $ Handle' t_ok t_closed) else Res String (const t_closed)

public export
read_line' : LinearIO m =>
             List Char ->
             (1 _ : Handle' t_ok t_closed) ->
             L1 m $ OkOrError t_ok t_closed
read_line' acc handle = do
  (True # ([char] # handle)) <- read handle 1
  | (True # (char # handle)) => do
    handle' <- close handle
    pure1 (False # ("read line failed, somehow read returns \{show (length char)} bytes instead of 1" # handle'))
  | (False # (error # handle)) => pure1 (False # ("read line failed: \{error}" # handle))
  case cast {to=Char} char of
    '\n' => pure1 (True # ((pack $ reverse acc) # handle))
    '\r' => do
      (True # ([10] # handle)) <- read handle 1
      | (True # ([char] # handle)) => do
        handle' <- close handle
        pure1 (False # ("read line failed, \\n expected after \\r, got \{show char} instead" # handle'))
      | (True # (char # handle)) => do
        handle' <- close handle
        pure1 (False # ("read line failed, somehow read returns \{show (length char)} bytes instead of 1" # handle'))
      | (False # (error # handle)) => pure1 (False # ("read line failed: \{error}" # handle))
      pure1 (True # ((pack $ reverse acc) # handle))
    x => read_line' (x :: acc) handle

public export
read_line : LinearIO m => (1 _ : Handle' t_ok t_closed) -> L1 m $ OkOrError t_ok t_closed
read_line = read_line' []

public export
read_until_empty_line' : String -> LinearIO m => (1 _ : Handle' t_ok t_closed) -> L1 m $ OkOrError t_ok t_closed
read_until_empty_line' acc handle = do
  (True # (line # handle)) <- read_line handle
  | (False # (error # handle)) => pure1 (False # ("read line failed: \{error}" # handle))
  if null line then pure1 (True # (acc # handle)) else read_until_empty_line' (acc <+> "\n" <+> line) handle

public export
read_until_empty_line : LinearIO m => (1 _ : Handle' t_ok t_closed) -> L1 m $ OkOrError t_ok t_closed
read_until_empty_line = read_until_empty_line' ""

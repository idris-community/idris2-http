module Network.HTTP.Pool.Common

import System.Concurrency.BufferedChannel
import Network.HTTP.Scheduler

public export
data Event : Type -> Type where
  Request : ScheduleRequest e IO -> Event e
  Kill : Event e

public export
Fetcher : Type -> Type
Fetcher e = (dc : BufferedChannel (Event e) ** (BlockingReceiver (Event e)))

public export
Sender : Type -> Type
Sender e = (dc : BufferedChannel (Event e) ** (SendEffect -> SenderFunc (Event e)))

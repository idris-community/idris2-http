module Network.HTTP.Pool.Common

import System.Concurrency.BufferedChannel
import Network.HTTP.Scheduler

public export
Fetcher : Type -> (Type -> Type) -> Type
Fetcher e m = (dc : BufferedChannel (ScheduleRequest e m) ** (BlockingReceiver (ScheduleRequest e m)))

public export
Sender : Type -> (Type -> Type) -> Type
Sender e m = (dc : BufferedChannel (ScheduleRequest e m) ** (SendEffect -> SenderFunc (ScheduleRequest e m)))

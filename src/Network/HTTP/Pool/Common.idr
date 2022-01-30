module Network.HTTP.Pool.Common

import System.Concurrency
import Network.HTTP.Scheduler

public export
data Event : Type -> Type where
  Request : ScheduleRequest e IO -> Event e
  Kill : Maybe Condition -> Event e

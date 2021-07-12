module IntervalLib

import Data.Vect
import Data.So

%default total

public export
data Interval : (lower : Double)->(upper  : Double) -> Type where
      MkInterval : (lower : Double)->(upper  : Double) ->
      So (lower <= upper ) -> Interval lower upper

public export
default_interval : Interval 0 1
default_interval = MkInterval 0 1 Oh

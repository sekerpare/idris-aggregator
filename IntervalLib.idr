module IntervalLib

import Data.Vect
import Data.So

%default total

public export
data Interval : (lower : Double)->(upper  : Double) -> Type where
      MkInterval : (lower : Double)->(upper  : Double) ->
      So (lower <= upper ) -> Interval lower upper

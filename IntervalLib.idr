module IntervalLib

import Data.Vect
import Data.So

%default total

--Source of the Interval is :
--[1]Syntax Extensions. Syntax Extensions - Idris 1.3.3 documentation. (n.d.). http://docs.idris-lang.org/en/latest/tutorial/syntax.html.

public export
data Interval : (lower : Double)->(upper  : Double) -> Type where
      MkInterval : (lower : Double)->(upper  : Double) ->
      So (lower <= upper ) -> Interval lower upper

public export
default_interval : Interval 0 1
default_interval = MkInterval 0 1 Oh

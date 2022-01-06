module Network.HTTP.Mime

import Data.Mime.Apache.Model
import Data.Mime.Apache.Raw
import Data.List
import Data.List1
import Data.Nat
import Data.String

import Network.HTTP.Utils

public export
record QualityValue where
  constructor MkQualityValue
  digits : Nat
  prf : LTE digits 1000

export
one : QualityValue
one = MkQualityValue 1000 (lteAddRight _)

export
zero : QualityValue
zero = MkQualityValue 0 LTEZero

export
quality_to_double : QualityValue -> Double
quality_to_double = (/ 1000) . cast . digits

export
double_to_quality : Double -> Maybe QualityValue
double_to_quality value =
  let v' = cast (value * 1000)
  in case isLTE v' 1000 of
       Yes prf => Just (MkQualityValue v' prf)
       No _ => Nothing

export
Eq QualityValue where
  a == b = (quality_to_double a) == (quality_to_double b)

export
Ord QualityValue where
  compare a b = compare (quality_to_double a) (quality_to_double b)

public export
record MimePredicate where
  constructor MkMimePredicate
  main_type : Maybe String -- Nothing denotes wildcard
  sub_type : Maybe String -- Nothing denotes wildcard
  quality : QualityValue

export
parse_mime_predicate : QualityValue -> String -> Maybe MimePredicate
parse_mime_predicate q str =
  case splitBy '/' (toLower $ trim str) of
    (_, "") => Nothing
    ("", _) => Nothing
    (main, sub) => Just $ MkMimePredicate (guard (main /= "*") $> main) (guard (sub /= "*") $> sub) q

cmp_predicate : Maybe String -> String -> Bool
cmp_predicate Nothing _ = True
cmp_predicate (Just x) y = x == y

export
mime_predicate : MimePredicate -> (Mime -> Bool)
mime_predicate predicate x = (cmp_predicate predicate.main_type $ show x.mainType)
                          && (cmp_predicate predicate.sub_type x.subType)

export
parse_mime_list : String -> Either String (List (QualityValue, Mime))
parse_mime_list string =
  traverse parse_mime $ map (forget . split (';' ==)) $ forget $ split (',' ==) string
  where
    parse_mime : List String -> Either String (QualityValue, Mime)
    parse_mime [x] = Right (one, ?xy x)
    parse_mime [x,q] = Left "empty mime entry"
    parse_mime _ = Left "malformed mime entry"

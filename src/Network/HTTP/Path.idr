module Network.HTTP.Path

import Data.String
import Data.String.Extra
import Data.List1
import Data.List
import Derive.Prelude

%language ElabReflection

data PathComponent : Type where
  Root : PathComponent
  Parent : PathComponent
  Current : PathComponent
  End : PathComponent
  Named : String -> PathComponent

%runElab derive "PathComponent" [Eq]

export
record Path where
  constructor MkPath
  components : List1 PathComponent

zip_with_last : List a -> List (a, Maybe a)
zip_with_last list =
  let (_ :: xs) = map Just list
  | [] => []
  in zip list (snoc xs Nothing)

export
normalize : Path -> Path
normalize path = decompose $ trim_root path.components where
  is_current : PathComponent -> Bool
  is_current Current = True
  is_current (Named "") = True
  is_current _ = False

  rid_current : List PathComponent -> List PathComponent
  rid_current components = filter (not . is_current) components

  rid_parent : List PathComponent -> List PathComponent -> List PathComponent
  rid_parent acc [] = reverse acc
  rid_parent acc@(Parent :: as) (Parent :: xs) = rid_parent (Parent :: acc) xs
  rid_parent acc@(Root :: as) (Parent :: xs) = rid_parent acc xs
  rid_parent (a :: as) (Parent :: xs) = rid_parent as xs
  rid_parent acc (x :: xs) = rid_parent (x :: acc) xs

  append_current : List PathComponent -> List1 PathComponent
  append_current (Named x :: xs) = Current ::: (Named x :: xs)
  append_current [] = Current ::: []
  append_current (x :: xs) = (x ::: xs)

  decompose : List1 PathComponent -> Path
  decompose (x ::: xs) = MkPath $ append_current $ rid_parent [] (x :: rid_current xs)

  trim_root : List1 PathComponent -> List1 PathComponent
  trim_root list = case dropWhile (/= Root) $ forget list of [] => list; (x :: xs) => x ::: xs

concat : List a -> List1 a -> List1 a
concat [] y = y
concat (x :: xs) (y ::: ys) = x ::: (xs <+> (y :: ys))

export
Semigroup Path where
  a <+> b =
    let as = (normalize a).components
        ai = init as
    in normalize $ MkPath $ concat ai (normalize b).components

export
Show Path where
  show path = trim_leading_roots $ join "/" $ map component_to_string path.components
    where
      trim_leading_roots : String -> String
      trim_leading_roots str = assert_total $ if isPrefixOf "//" str then strTail str else str

      component_to_string : PathComponent -> String
      component_to_string Root = "/"
      component_to_string End = "/"
      component_to_string Parent = ".."
      component_to_string Current = "."
      component_to_string (Named x) = x

export
FromString Path where
  fromString "" = MkPath $ singleton Current
  fromString string =
    MkPath $ case split ('/' ==) string of
      ("" ::: xs) => Root ::: (map string_to_component xs)
      (x ::: xs) => map string_to_component $ (x ::: xs)
  where
    string_to_component : String -> PathComponent
    string_to_component "" = End
    string_to_component "." = Current
    string_to_component ".." = Parent
    string_to_component x = Named x

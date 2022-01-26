module Data.Compress.Utils.Parser

import Data.List
import Data.List.Elem
import Data.List1
import Data.Vect
import Data.Void
import Data.Bits
import Data.SnocList
import Decidable.Equality
import Syntax.WithProof
import Data.Compress.Utils.Bytes

public export
interface Cons a s | s where
  singleton : a -> s
  uncons : s -> Maybe (a, s)

public export
Cons Char String where
  singleton = cast
  uncons = strUncons

public export
Cons a (List a) where
  singleton = pure
  uncons [] = Nothing
  uncons (x :: xs) = Just (x, xs)

public export
record Bitstream where
  constructor MkBitstream 
  content : List (Fin 8, Bits8)

export
fromBits8 : List Bits8 -> Bitstream
fromBits8 = MkBitstream . map (FZ,)

export
toBits8 : Bitstream -> List Bits8
toBits8 bs = mapMaybe (\(i,x) => (guard (i == 0)) $> x) bs.content

export
Semigroup Bitstream where
  a <+> b = MkBitstream (a.content <+> b.content)

export
Monoid Bitstream where
  neutral = MkBitstream []

export
is_byte_aligned : Bitstream -> Bool
is_byte_aligned bs = all (\(i,_) => 0 == i) bs.content

public export
Cons Bool Bitstream where
  singleton x = MkBitstream [(0, if x then 1 else 0)]
  uncons bitstream =
    case bitstream.content of
      [] => Nothing
      (i, x) :: xs =>
        case strengthen (FS i) of
          Just i' => Just (testBit x i, MkBitstream ((i', x) :: xs))
          Nothing => Just (testBit x i, MkBitstream xs)

public export
data Parser : (input : Type) -> (error : Type) -> (a : Type) -> Type where
  Fail : e -> Parser i e a
  Pure : (leftover : i) -> a -> Parser i e a
  More : (on_feed : (i -> Parser i e a)) -> Parser i e a
  Alt : Parser i e a -> Lazy (Parser i e a) -> Parser i e a

public export
Functor (Parser i e) where
  map f (Fail e) = Fail e
  map f (Pure leftover x) = Pure leftover (f x)
  map f (More on_feed) = More (map f . on_feed)
  map f (Alt p1 p2) = Alt (map f p1) (map f p2)

public export
transform : (i -> Either e i') -> (i' -> Either e i) -> Parser i e a -> Parser i' e a
transform f g (Fail e) = Fail e
transform f g (Alt p1 p2) = Alt (transform f g p1) (transform f g p2)
transform f g (Pure leftover x) =
  case f leftover of
    Right leftover' => Pure leftover' x
    Left err => Fail err
transform f g (More on_feed) = More $ \x =>
  let Right x' = g x
      | Left err => Fail err
  in transform f g $ on_feed x'

||| maps over the errors of the parser
public export
map_error : (e -> e') -> Parser i e a -> Parser i e' a
map_error f (Fail e) = Fail (f e)
map_error f (Pure leftover x) = Pure leftover x
map_error f (More on_feed) = More (map_error f . on_feed)
map_error f (Alt p1 p2) = Alt (map_error f p1) (map_error f p2)

public export
(<|>) : Semigroup e => Parser i e a -> Lazy (Parser i e a) -> Parser i e a
Fail e <|> p = map_error (e <+>) p
Pure leftover x <|> p = Pure leftover x
p <|> q = Alt p q

||| fail with an error
public export
fail : e -> Parser i e a
fail = Fail

||| feed input into the parser incrementally
public export
feed : (Semigroup e, Semigroup i) => i -> Parser i e a -> Parser i e a
feed input (Fail e) = Fail e
feed input (Pure leftover x) = Pure (leftover <+> input) x
feed input (More on_feed) = on_feed input
feed input (Alt p1 p2) = feed input p1 <|> feed input p2

apply : (Semigroup e, Semigroup i) => (Parser i e a -> Parser i e b) -> Parser i e a -> Parser i e b
apply f (Fail msg) = Fail msg
apply f (Alt p1 p2) = Alt (f p1) (f p2)
apply f (More on_feed) = More (f . on_feed)
apply f parser = More (\input => f $ feed input parser)

public export
pure : Monoid i => a -> Parser i e a
pure x = Pure neutral x

public export
(<*>) : (Semigroup e, Monoid i) => Parser i e (a -> b) -> Lazy (Parser i e a) -> Parser i e b
Pure leftover f <*> p = map f $ feed leftover p
p1 <*> p2 = apply (<*> p2) p1

public export
(<*) : (Semigroup e, Monoid i) => Parser i e a -> Parser i e b -> Parser i e a
x <* y = map const x <*> y

public export
(*>) : (Semigroup e, Monoid i) => Parser i e a -> Parser i e b -> Parser i e b
x *> y = map (const id) x <*> y

public export
(>>=) : (Semigroup e, Monoid i) => Parser i e a -> (a -> Parser i e b) -> Parser i e b
(>>=) (Pure leftover x) f = feed leftover $ f x
(>>=) p f = apply (>>= f) p

public export
more : (i -> Parser i e a) -> Parser i e a
more = More

||| peek into the next token without consuming it
public export
peek : Cons c i => Parser i e c
peek = more $ \input =>
  case uncons input of
    Just (x, _) => Pure input x
    Nothing => peek

||| reads the next token
public export
token : Cons c i => Parser i e c
token = more $ \input =>
  case uncons input of
    Just (x, xs) => Pure xs x
    Nothing => token

||| run `p` `k` times and collect the results
public export
count : (Semigroup e, Monoid i) => (k : Nat) -> (p : Parser i e a) -> Parser i e (Vect k a)
count Z parser = pure []
count (S k) parser = pure $ !parser :: !(count k parser)

||| return the result of `p` if it succeeds, otherwise return `x`
public export
option : (Semigroup e, Monoid i) => (x : a) -> (p : Parser i e a) -> Parser i e a
option x p = p <|> pure x

mutual
  public export
  some : (Semigroup e, Monoid i) => Parser i e a -> Parser i e (List1 a)
  some p = pure $ !p ::: !(many p)

  public export
  many : (Semigroup e, Monoid i) => Parser i e a -> Parser i e (List a)
  many p = option [] (forget <$> some p)

namespace Error
  ||| example: `SimpleError String`
  public export
  data SimpleError : Type -> Type where
    Msg : a -> SimpleError a
    Alt : SimpleError a -> SimpleError a -> SimpleError a
    Under : a -> SimpleError a -> SimpleError a

  public export
  Semigroup (SimpleError a) where
    (<+>) = Alt

  public export
  Show a => Show (SimpleError a) where
    show (Msg x) = show x
    show (Alt a b) = "(" <+> show a <+> " <|> " <+> show b <+> ")"
    show (Under x a) = "(" <+> show x <+> ": " <+> show a <+> ")"

  public export
  msg : e -> SimpleError e
  msg = Msg

  public export
  under : e -> Parser i (SimpleError e) a -> Parser i (SimpleError e) a
  under = map_error . Under

export
bit_getbyte : Parser Bitstream e Bits8
bit_getbyte = more $ \(MkBitstream input) => f input
  where
  f : List (Fin 8, Bits8) -> Parser Bitstream e Bits8
  f ((FZ, x) :: xs) = Pure (MkBitstream xs) x
  f ((_ , x) :: xs) = f xs
  f [] = bit_getbyte

||| parse the next `n` bytes as a natural number in big endian style
export
p_nat : Semigroup e => (n : Nat) -> Parser (List Bits8) e Nat
p_nat n = cast {to = Nat} . le_to_integer <$> count n token

export
le_nat : Semigroup e => (n : Nat) -> Parser Bitstream e Nat
le_nat n = cast {to = Nat} . le_to_integer <$> count n bit_getbyte

||| make sure that `p` MUST consume at least `n` tokens, fails otherwise
public export
p_exact : (Cons c i, Monoid i) => (n : Nat) -> (p : Parser i (SimpleError String) a) -> Parser i (SimpleError String) a
p_exact Z (Pure leftover x) = pure x
p_exact (S i) (Pure leftover x) = fail $ msg $ "over fed, " <+> show (S i) <+> " bytes more to go"
p_exact i (Fail msg) = fail msg
p_exact Z parser = fail $ msg $ "under fed, wants more"
p_exact (S i) parser = do
  b <- token
  p_exact i (feed (singleton b) parser)

record LazyParser i e a where
  constructor Lazify
  parser : Lazy (Parser i e a)

Functor (LazyParser i e) where
  map f p = Lazify $ delay $ map {f=Parser i e} f p.parser

(Semigroup e, Monoid i) => Applicative (LazyParser i e) where
  pure a = Lazify $ delay $ pure a
  a <*> b = Lazify $ delay (Parser.(<*>) a.parser b.parser)

export
ifA : Monoid i => Bool -> Lazy (Parser i e a) -> Parser i e (Maybe a)
ifA cond action = if cond then map Just (Force action) else pure Nothing

export
for : {i,e: _} -> (Semigroup e, Monoid i, Traversable f) => f a -> (a -> Lazy (Parser i e b)) -> Parser i e (f b)
for f p = Force (for f (Lazify . p)).parser

export
take_until : (Semigroup e, Monoid i) => (a -> Bool) -> Parser i e a -> Parser i e (List a)
take_until f parser = loop Lin where
  loop : SnocList a -> Parser i e (List a)
  loop acc = do
    t <- parser
    if not $ f t then loop (acc :< t) else pure $ toList acc

export
get_bit : Num n => Parser Bitstream (SimpleError String) n
get_bit = map (\b => if b then 1 else 0) token

fin_range : (n : Nat) -> List (Fin n)
fin_range _ = toList Fin.range

read_bits : (List Bool -> List Bool) -> Fin 32 -> Parser Bitstream (SimpleError String) Bits32
read_bits f n = do
  bits <- toList <$> count (finToNat n) token
  pure $ foldl (\a,(i,b) => if b then setBit a i else a) 0 $ zip (fin_range 32) (f bits)

export
get_bits : ?
get_bits = read_bits id

export
get_huff : ?
get_huff = read_bits reverse

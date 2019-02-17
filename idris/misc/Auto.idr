-- -*- coding: utf-8 -*-
-- Copyright: © 2019 Simon Nielsen Knights <tauoverpi@yandex.com>
-- License  : GNU AGPL, version 3 or later; http://www.gnu.org/licenses/agpl.html

module Auto

import Data.Morphisms
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Fin
import Text.PrettyPrint.WL

%default total
%access public export

namespace Pairs
  first : (a -> b) -> (a, x) -> (b, x)
  first f (a, x) = (f a, x)

  second : (a -> b) -> (x, a) -> (x, b)
  second f (x, a) = (x, f a)

  dup : x -> (x, x)
  dup x = (x, x)

  lassoc : (a, (b, c)) -> ((a, b), c)
  lassoc (a, (b, c)) = ((a, b), c)

  rassoc : ((a, b), c) -> (a, (b, c))
  rassoc ((a, b), c) = (a, (b, c))

  distl : (a, Either b c) -> Either (a, b) (a, c)
  distl (a, Left b)  = Left (a, b)
  distl (a, Right c) = Right (a, c)

  distr : Either (a, b) (a, c) -> (a, Either b c)
  distr (Left (a, b))  = (a, Left b)
  distr (Right (a, c)) = (a, Right c)

data Auto : (m : Type -> Type) -> (a, b : Type) -> Type where
  MkAuto : c -> (Pair a c -> m (Pair b c)) -> Auto m a b

record Dual (k : Type -> Type -> Type) a b where
  constructor MkDual
  dual : k b a

data Compose : (f, g : Type -> Type) -> (x : Type) -> Type where
  Comp : f (g x) -> Compose f g x

(Functor f, Functor g) => Functor (Compose f g) where
  map f (Comp x) = Comp (map (map f) x)

(Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Comp . pure . pure
  (Comp f) <*> (Comp x) = Comp ((<*>) <$> f <*> x)

auto : c -> (Pair a c -> m (Pair b c)) -> Auto m a b
auto = MkAuto

step : Functor m => Auto m a b -> a -> m (Auto m a b, b)
step (MkAuto c f) a = swap . second (flip auto f) <$> f (a, c)

id : Applicative m => Auto m a a
id = auto () pure

(.) : Monad m => Auto m b c -> Auto m a b -> Auto m a c
(MkAuto c f) . (MkAuto c' g) =
  auto (c, c') $ \(x, c, c') =>
   do (y, c') <- g (x, c')
      (z, c ) <- f (y, c )
      pure (z, c, c')

infixr 0 >>>
%inline
(>>>) : Monad m => Auto m a b -> Auto m b c -> Auto m a c
(>>>) = flip (.)

settable : Functor m => c -> ((a, c) -> m (b, c)) -> Auto m (a, Maybe c) b
settable c fn = auto c $ \((x, e), c) =>
  case e of
       Just c  => fn (x, c)
       Nothing => fn (x, c)

loop : Functor m => c -> Auto m (a, c) (b, c) -> Auto m a b
loop c (MkAuto c' f) = auto (c, c') (map rassoc . f . lassoc)

fun : Applicative m => (a -> b) -> Auto m a b
fun f = auto () (pure . (flip MkPair ()) . f . fst)

clip : (Applicative m, Ord a) => a -> a -> Auto m a a
clip l u = fun (max l . min u)

apply : Applicative m => Auto m (a -> b, a) b
apply = fun (uncurry apply)

const : Applicative m => b -> Auto m a b
const b = fun (const b)

it : Applicative m => Auto m a ()
it = const ()

swap : Applicative m => Auto m (a, b) (b, a)
swap = fun swap

delayed : Applicative m => a -> Auto m a a
delayed b = loop b swap

delay : Monad m => b -> Auto m a b -> Auto m a b
delay b m = m >>> loop b swap

accum : Applicative m => b -> (a -> b -> b) -> Auto m a b
accum b plus = auto b (pure . dup . uncurry plus)

dup : Applicative m => Auto m a (a, a)
dup = fun dup

pair : Applicative m => Auto m a b -> Auto m x y -> Auto m (a, x) (b, y)
pair (MkAuto c f) (MkAuto c' g) = auto (c, c') $ \(x, c, c') =>
  (\(b, c), (y, c') => ((b, y), c, c')) <$> f (fst x, c) <*> g (snd x, c')

infixr 3 ***
(***) : Applicative m => Auto m a b -> Auto m x y -> Auto m (a, x) (b, y)
(***) = pair

exl : Applicative m => Auto m (a, b) a
exl = fun fst

exr : Applicative m => Auto m (a, b) b
exr = fun snd

first : Applicative m => Auto m a b -> Auto m (a, x) (b, x)
first = flip pair id

second : Applicative m => Auto m a b -> Auto m (x, a) (x, b)
second = pair id

distl : Applicative m => Auto m (a, Either b c) (Either (a, b) (a, c))
distl = fun distl

distr : Applicative m => Auto m (Either (a, b) (a, c)) (a, Either b c)
distr = fun distr

both : Functor m
    => Auto m a b -> Auto m x y -> Auto m (Either a x) (Either b y)
both (MkAuto c f) (MkAuto c' g) = auto (c, c') $ \(e, c, c') =>
  case e of
       Left l  => (\(b, c) => (Left b, c, c')) <$> f (l, c)
       Right r => (\(y, c') => (Right y, c, c')) <$> g (r, c')

infixr 2 +++
(+++) : Functor m
    => Auto m a b -> Auto m x y -> Auto m (Either a x) (Either b y)
(+++) = both

inl : Applicative m => Auto m a (Either a b)
inl = fun Left

inr : Applicative m => Auto m b (Either a b)
inr = fun Right

left : Applicative m => Auto m a b -> Auto m (Either a x) (Either b x)
left = flip both id

right : Applicative m => Auto m a b -> Auto m (Either x a) (Either x b)
right = both id

either : Functor m
    => Auto m a b -> Auto m x b -> Auto m (Either a x) b
either (MkAuto c f) (MkAuto c' g) = auto (c, c') $ \(e, c, c') =>
  case e of
       Left l  => (\(b, c) => (b, c, c')) <$> f (l, c)
       Right r => (\(y, c') => (y, c, c')) <$> g (r, c')

infixr 2 \|/
(\|/) : Functor m
    => Auto m a b -> Auto m x b -> Auto m (Either a x) b
(\|/) = either

split : Applicative m => Auto m a b -> Auto m a y -> Auto m a (b, y)
split (MkAuto c f) (MkAuto c' g) = auto (c, c') $ \(x, c, c') =>
  (\(b, c), (y, c') => ((b, y), c, c')) <$> f (x, c) <*> g (x, c')

infixr 3 &&&
(&&&) : Applicative m => Auto m a b -> Auto m a y -> Auto m a (b, y)
(&&&) = split

test : Applicative m => (a -> Bool) -> Auto m a (Either a a)
test t = fun pred where pred x = if t x then Left x else Right x

decideOn : Monad m => (a -> Bool) -> Auto m a b -> Auto m a b -> Auto m a b
decideOn t c a = test t >>> c \|/ a

effect : Functor m => (a -> m b) -> Auto m a b
effect e = auto () (map (flip MkPair ()) . e . fst)

eff : Functor m => m b -> Auto m a b
eff = effect . const

animate : Functor m => Auto m (m a) a
animate = effect id

-------------------------------------------------------------[ implementations ]
-- Standard interfaces.

Functor m => Functor (Auto m a) where
  map f (MkAuto c g) = auto c (map (first f) . g)

Monad m => Applicative (Auto m a) where
  pure = const
  f <*> x = split f x >>> apply

(Monad m, Num b) => Num (Auto m a b) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  fromInteger = pure . fromInteger

(Monad m, Neg b) => Neg (Auto m a b) where
  (-) = liftA2 (-)
  negate = map negate

(Monad m, Abs b) => Abs (Auto m a b) where
  abs = map abs

(Monad m, Fractional b) => Fractional (Auto m a b) where
  (/) = liftA2 (/)
  recip = map recip

(Monad m, Integral b) => Integral (Auto m a b) where
  div = liftA2 div
  mod = liftA2 mod

(Monad m, Semigroup b) => Semigroup (Auto m a b) where
  (<+>) = liftA2 (<+>)

(Monad m, Monoid b) => Monoid (Auto m a b) where
  neutral = pure neutral

-----------------------------------------------------------------------[ trans ]
-- Swapping out the underlying monad and generalizing the Identity monad to the
-- contained context.

hoist : (Monad m', Monad m)
     => (input : a -> a')
     -> (trans : {x:_} -> a -> m' x -> m x)
     -> Auto m' a' b
     -> Auto m a b
hoist f trans m = loop m (effect machine)
  where machine (x, m) = map swap (trans x (step m (f x)))

Mealy : Type -> Type -> Type
Mealy = Auto Identity

generalize : Monad m => Mealy a b -> Auto m a b
generalize = hoist id (const (pure . runIdentity))

reader : Monad m => Auto (ReaderT r m) a b -> Auto m (r, a) b
reader = hoist snd (\x, c => runReaderT c (fst x))

-----------------------------------------------------------------------[ event ]
-- Discrete values which may have a value at some point in time. Try not to mix
-- them with continous portions as they clutter the application.

never : Applicative m => Auto m a (Maybe b)
never = const Nothing

always : Applicative m => Auto m a (Maybe a)
always = fun Just

now : Monad m => b -> Auto m a (Maybe b)
now b = delay (Just b) never

once : Monad m => Auto m a (Maybe a)
once = auto False sum
  where sum (a, False) = pure (Just a , True)
        sum (a, True ) = pure (Nothing, True)

event : Applicative m => Auto m (Maybe a) (Either a ())
event = fun (maybe (Right ()) Left)

onEvent : Monad m => Auto m a b -> Auto m (Maybe a) (Maybe b)
onEvent m = event >>> always . m \|/ never

hold : Monad m => b -> Auto m a (Maybe b) -> Auto m a b
hold b m = m >>> loop b (fun sum)
  where sum (Nothing, b) = (b, b)
        sum (Just b , _) = (b, b)

is : (Monad m, Eq a) => a -> Auto m a (Maybe a)
is a = test (==a) >>> always \|/ never

match : (Monad m, Eq a) => a -> Auto m a b -> Auto m a (Maybe b)
match a m = is a >>> onEvent m

isNot : (Monad m, Eq a) => a -> Auto m a (Maybe a)
isNot a = test (/=a) >>> always \|/ never

----------------------------------------------------------------------[ switch ]
-- This section makes mealy machines equivalent to monads in that we lose static
-- guarantees given by the interface defined prior to this section.

-- TBD: refactor this section into a single general function...
until : Monad m => Auto m a b -> Auto m a (Maybe b) -> Auto m a (Maybe b)
until (MkAuto c f) (MkAuto c' {c=k} g) = MkAuto (Left (c, c') {b=k}) $ \(x, e) =>
  case e of
       Left  l => do (Nothing, c') <- g (x, snd l) | (b, c') => pure (b, Right c')
                     (b, c) <- f (x, c)
                     pure (Just b, Left (c, c'))
       Right r => do (b, r) <- g (x, r); pure (b, Right r)

fby : Monad m => Auto m a (Maybe b) -> Auto m a (Maybe b) -> Auto m a (Maybe b)
fby (MkAuto c f) (MkAuto c' {c=k} g) = MkAuto (Left c {b=k}) $ \(x, e) =>
  case e of
       Left  l => do (Nothing, c ) <- f (x, l) | (b, c) => pure (b, Left  c )
                     (b, c') <- g (x, c')
                     pure (b, Right c')
       Right r => do (b, c') <- g (x, r); pure (b, Right c')

finally : Monad m => Auto m a (Maybe b) -> Auto m a b -> Auto m a b
finally (MkAuto c f) (MkAuto c' {c=k} g) = MkAuto (Left c {b=k}) $ \(x, e) =>
  case e of
       Left  l => do (Nothing, c ) <- f (x, l) | (Just b, c) => pure (b, Left  c )
                     (b, c') <- g (x, c')
                     pure (b, Right c')
       Right r => do (b, c') <- g (x, r); pure (b, Right c')

alt : Monad m => Auto m a (Maybe b) -> Auto m a (Maybe b) -> Auto m a (Maybe b)
alt f g = split id (event . f) >>> distl >>> always . exr \|/ g . exl

(<|>) : Monad m => Auto m a (Maybe b) -> Auto m a (Maybe b) -> Auto m a (Maybe b)
(<|>) = alt

default : Monad m => Auto m a (Maybe b) -> Auto m a b -> Auto m a b
default f g = split id (event . f) >>> distl >>> exr \|/ g . exl

infixr 1 </>
(</>) : Monad m => Auto m a (Maybe b) -> Auto m a b -> Auto m a b
(</>) = default

---------------------------------------------------------------------[ varying ]
-- Varying continous values which signify when they've changed allowing for more
-- efficient computation by avoiding recomputaiton.

record Varying a where
  constructor Vary
  changed : Bool
  value   : a

Functor Varying where
  map f (Vary c a) = Vary c (f a)

Applicative Varying where
  pure = Vary False
  (Vary c f) <*> (Vary c' x) = Vary (c || c') (f x)

Monad Varying where
  join (Vary c (Vary c' a)) = Vary (c || c') a

Num a => Num (Varying a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  fromInteger = pure . fromInteger

Neg a => Neg (Varying a) where
  (-) = liftA2 (-)
  negate = map negate

Abs a => Abs (Varying a) where
  abs = map abs

Fractional a => Fractional (Varying a) where
  (/) = liftA2 (/)
  recip = map recip

Integral a => Integral (Varying a) where
  div = liftA2 div
  mod = liftA2 mod

Semigroup a => Semigroup (Varying a) where
  (<+>) = liftA2 (<+>)

Monoid a => Monoid (Varying a) where
  neutral = pure neutral

holdV : Applicative m => a -> Auto m (Maybe a) (Varying a)
holdV a = loop a (fun sum)
  where sum (Nothing, c) = (Vary False c, c)
        sum (Just c , _) = (Vary True c , c)

changedV : (Applicative m, Eq a) => Auto m a (Varying a)
changedV = loop Nothing (fun sum)
  where sum : (a, Maybe a) -> (Varying a, Maybe a)
        sum (a, Nothing) = (Vary True a  , Just a)
        sum (a, Just b ) = (Vary (a/=b) a, Just a)

value : Applicative m => Auto m (Varying a) a
value = fun value

different : Applicative m => Auto m a (Varying a)
different = fun (Vary True)

same : Applicative m => Auto m a (Varying a)
same = fun (Vary False)

onChange : Monad m => Auto m a b -> Auto m (Varying a) (Varying b)
onChange m {b} = auto (m, Nothing {a=b}) $ \(v, m, a) =>
  case (changed v, a) of
       (False, Just b) => pure (Vary False b, m, Just b)
       _ => do (m, b) <- step m (value v)
               pure (Vary True b, m, Just b)

animateV : Monad m => Auto m (Varying (m a)) (Varying a)
animateV = onChange animate

------------------------------------------------------------------------[ task ]
-----------------------------------------------------------------[ collections ]

zipAuto : Monad m => %static a -> List (Auto m a b) -> Auto m (List a) (List b)
zipAuto default as = loop as (effect (uncurry (go id id)))
  where go : (List (Auto m a b) -> List (Auto m a b))
          -> (List b -> List b)
          -> List a
          -> List (Auto m a b)
          -> m (List b, List (Auto m a b))
        go as bs _ [] = pure (bs [], as [])
        go as bs [] (f::fs) =
          do (a', b') <- step f default
             go (as . (a'::)) (bs . (b'::)) [] fs
        go as bs (x::xs) (f::fs) =
          do (a', b') <- step f x
             go (as . (a'::)) (bs . (b'::)) xs fs

dZipAuto : Monad m => a -> List (Auto m a b) -> Auto m (List a) (List b)
dZipAuto def ws = delay (replicate (length ws) def) id >>> (zipAuto def ws)

dynamic : (Monad m, Eq k) => (k -> Auto m a (Maybe b)) -> Auto m (k, a) (List b)
dynamic gen = loop [] (effect (uncurry (update id id)))
  where update : (List (k, Auto m a (Maybe b), b)
                 -> List (k, Auto m a (Maybe b), b))
              -> (List b -> List b)
              -> (k, a)
              -> List (k, Auto m a (Maybe b), b)
              -> m (List b, List (k, Auto m a (Maybe b), b))
        update as bs (k', x) [] =
          do (a', Just b') <- step (gen k') x | _ => pure (bs [], as [])
             let as = as . ((k', a', b')::)
             let bs = bs . (b'::)
             pure (bs [], as [])
        update as bs (k, x) (kfb@(k', af, bd) :: kfbs) =
          if k == k'
             then do (a', Just b') <- step af x
                     | _ => pure (bs (map (snd . snd) kfbs), as kfbs)
                     let as = as . ((k', a', b')::)
                     let bs = bs . (b'::)
                     pure (bs (map (snd . snd) kfbs), as kfbs)
             else update (as . (kfb::)) (bs . (bd::)) (k, x) kfbs

parallel : (Applicative m, Traversable f)
        => %static ({au:_} -> a -> f au -> f (au, b))
        -> f (Auto m b c)
        -> Auto m a (f c)
parallel tran col = loop col (effect (uncurry sum))
  where sum x co = map (\x => (map snd x, map fst x))
                 $ traverse (uncurry step) (tran x co)

------------------------------------------------------------------------[ misc ]

covering
control : Monad m => Auto m () (Maybe b) -> m b
control m =
  do (m, Just b) <- step m () | (m, _) => control m
     pure b

---------------------------------------------------------------------[ records ]

data Field : label -> Type -> Type where
  Set : (f : label) -> b -> Field f b

data Record : List (label, Type) -> Type where
  Nil  : Record []
  (::) : Field label a
      -> Record xs
      -> Record ((label, a) :: xs)

data Has : label -> Type -> List (label, Type) -> Type where
  Here : Has f t ((f, t) :: xs)
  There : Has f t xs -> Has f t (x :: xs)

get : (f : label) -> Record {label} xs -> {auto prf : Has f t xs} -> t
get _ ((Set f x) :: y) {prf=Here}   = x
get l (_ :: xs)        {prf=There z} = get l xs {prf=z}

delete : (f : label)
     -> {auto prf : Has f t xs}
     -> Record {label} xs
     -> (ys ** Record {label} ys)
delete l {prf=Here} (x :: xs) = (_ ** xs)
delete l {prf=There y} (x :: xs) with (delete l {prf=y} xs)
  | (as ** ys) = (_ :: as ** x :: ys)

(++) : Record xs -> Record ys -> Record (xs ++ ys)
[] ++ ys = ys
(x :: xs) ++ ys = x :: xs ++ ys

set : label
   -> a
   -> Record {label} xs
   -> {auto prf : Has f t xs}
   -> (ys ** Record {label} ys)
set l v (x :: xs) {prf=Here} = (_ ** (Set l v) :: xs)
set l v (x :: xs) {prf=There y} with (set l v xs {prf=y})
  | (as ** ys) = (_ :: as ** x :: ys)

syntax "{" [x] ":" [t] "|" {xs} "}" "->" [k]
  = {auto prf : Has x t xs}
 -> Record xs -> k

syntax "{" [x] ":" [t] "," [x1] ":" [t1] "|" {xs} "}" "->" [k]
  = {auto prf : Has x t xs}
 -> {auto prf1 : Has x1 t1 xs}
 -> Record xs -> k

-------------------------------------------------------------------------[ web ]

data Element = MkElement Ptr

data HtmlTag = Div | Span

Show HtmlTag where
  show Div = "div"
  show Span = "span"

data WebEvent = OnClick | OnMouseMove

Show WebEvent where
  show OnClick = "click"
  show OnMouseMove = "mousemove"

data HtmlEvent a = MkHEvent WebEvent a

Attribute : Type
Attribute = (String, String)
data Html : Type -> Type where
  HtmlElem : HtmlTag
          -> List Attribute
          -> List (HtmlEvent a)
          -> List (Html a)
          -> Html a
  HtmlText : String -> Html a

Web : (sub, a, b : Type) -> Type
Web sub = Auto (ReaderT sub Identity)

web : Monad m => Web s a b -> Auto m (s, a) b
web w = generalize (reader w)

Show a => Show (HtmlEvent a) where
  show (MkHEvent e a) =
    "on" ++ show e ++ "=\"dctp_" ++ show e ++ "(" ++ show a ++ ")\""

Show a => Show (Html a) where
  show = toString . prettyHtml
    where prettyHtml : Show a => Html a -> Doc
          prettyHtml (HtmlText s) = text s
          prettyHtml (HtmlElem tag attr event html) =
            let events = concatMap ((" "++) . show) event
                attrs  = case attr of
                              [] => ""
                              _  => " style=\""
                                    ++ concatMap (\(a,b) => a ++ ": " ++ b ++ ";") attr
                                    ++ "\""
                open = angles $
                    text (show tag)
                    |+| text events
                    |+| text attrs
                content = assert_total (concatMap prettyHtml html)
                close = angles $
                    text "/" |+| text (show tag)
            in open |$| (indent 2 content) |$| close

style : (List Attribute -> List Attribute) -> Html a -> Html a
style f (HtmlElem x xs ys zs) = HtmlElem x (f xs) ys zs
style f x = x

decl syntax htmltag {name} [tag] =
  ||| Html element constructor
  name : (events : List (HtmlEvent a))
      -> (nodes : List (Html a))
      -> Html a
  name = HtmlElem tag []

htmltag div Div
htmltag span Span

text : String -> Html a
text = HtmlText

getElementById : String -> JS_IO (Maybe Element)
getElementById name =
  do p <- foreign FFI_JS "document.getElementById(%0)" (String -> JS_IO Ptr) name
     if (prim__eqPtr p null) /= 0
        then pure Nothing
        else pure (Just (MkElement p))

covering
controlWeb : Auto JS_IO () () -> JS_IO ()
controlWeb wm = go wm ()
  where requestAnimationFrame : (() -> JS_IO ()) -> JS_IO ()
        requestAnimationFrame fn = assert_total $
          foreign FFI_JS "requestAnimationFrame(%0)"
                  (JsFn (() -> JS_IO ()) -> JS_IO ())
                  (MkJsFn fn)

        go w () =
          do (w, _) <- step w ()
             requestAnimationFrame (go w)

data Command a = Com a | None
record App subs com input output where
  constructor MkApp
  subscriptions : Command com -> JS_IO subs
  view          : output -> Html input
  application   : Web subs input (output, Command com)

-- Web application architecture
--  * Events
--    + Computes only when given input from subscriptions & UI.
--    + All handlers call using the same IORef (single thread so it's ok).
--    - Can't compute between events thus splitting work over frames is no
--      longer practical.
--    + Closer to the browser model.
--    - Can't embed an animation system inside.
--  * Animation frames
--    + Computes each render frame regardless of events.
--    + All handlers return pure data through a single IORef as events.
--    - Must poll events.
--    + Works for simulations.
--    + Can embed an event system inside.
render : (root : Element)
      -> %static (handler_gen : Ptr -> JS_IO ())
      -> (old : Html a)
      -> (new : Html a)
      -> JS_IO ()
render root gen old new = ?new
  where isNull : Ptr -> JS_IO Bool
        isNull = pure . (==0) . prim__eqPtr null

        createElement : HtmlTag -> JS_IO Element
        createElement = map MkElement .
          foreign FFI_JS "document.createElement(%0)" _ . show

        createTextNode : String -> JS_IO Element
        createTextNode = map MkElement .
          foreign FFI_JS "document.createTextNode(%0)" _

        removeChild : (parent, child : Element) -> JS_IO ()
        removeChild (MkElement p) (MkElement c) =
          foreign FFI_JS "%0.removeChild(%1)"
            (Ptr -> Ptr -> JS_IO ()) p c

        appendChild : (parent, child : Element) -> JS_IO ()
        appendChild (MkElement p) (MkElement c) =
          foreign FFI_JS "%0.appendChild(%1)" (Ptr -> Ptr -> JS_IO ()) p c

        nthChild : Element -> Int -> JS_IO Element
        nthChild (MkElement ptr) = map MkElement .
          foreign FFI_JS "%0.nthChild[%1]" (Ptr -> Int -> JS_IO Ptr) ptr

        setAttribute : Element -> (attr, value : String) -> JS_IO ()
        setAttribute (MkElement ptr) =
          foreign FFI_JS "%0.setAttribute(%1, %2)"
            (Ptr -> String -> String -> JS_IO ()) ptr

        removeAttribute : Element -> (attr : String) -> JS_IO ()
        removeAttribute (MkElement ptr) =
          foreign FFI_JS "%0.removeAttribute(%1)"
            (Ptr -> String -> JS_IO ()) ptr

        addEventListener : Element
                        -> (name : String)
                        -> (handler : Ptr -> JS_IO ())
                        -> JS_IO ()
        addEventListener (MkElement ptr) name handler = assert_total $
          foreign FFI_JS "%0.addEventListener(%1, %2)"
            (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> JS_IO ())
            ptr name (MkJsFn handler)

        removeEventListener : Element
                           -> (name : String)
                           -> (handler : Ptr -> JS_IO ())
                           -> JS_IO ()
        removeEventListener (MkElement ptr) name handler = assert_total $
          foreign FFI_JS "%0.removeEventListener(%1, %2)"
            (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> JS_IO ())
            ptr name (MkJsFn handler)

        go : Html a -> Html a -> Int -> JS_IO ()
        go (HtmlText t) (HtmlText t') n =
          if t == t' then pure ()
                     else pure ()
        go _ _ _ = ?pojnf

---------------------------------------------------------------------[ example ]

-- hi bye

covering
hibye : IO ()
hibye =
  let prompt = eff (putStr "mealy: ") >>> eff getLine
      put = \x => eff (putStrLn x)
  in control $ prompt
           >>> (match "hi" (put "bye"))
           <|> (match "fi" (put "fo"))
           </> (put "huh?")
           >>> never

-- bytebeat

data Track = Ambient

covering
bytebeat : Track -> IO ()
bytebeat track = control {b=()} $ split (accum 0 (+) . 1) 0
                 >>> reader beat
                 >>> fun (prim__intToChar . prim__sextB32_Int)
                 >>> effect putChar
                 >>> never
  where BB : Type
        BB = Auto (ReaderT Bits32 IO) Bits32 Bits32

        infixr 5 .&.
        infixr 6 .>>., .<<.
        infixr 4 .|.
        (.&.)  : BB -> BB -> BB
        (.|.)  : BB -> BB -> BB
        (.>>.) : BB -> BB -> BB
        (.<<.) : BB -> BB -> BB
        time : BB
        (.&.)  = liftA2 prim__andB32
        (.|.)  = liftA2 prim__orB32
        (.>>.) = liftA2 prim__lshrB32
        (.<<.) = liftA2 prim__shlB32
        time = eff ask

        beat : BB
        beat = case track of
                    Ambient => time .&. time .>>. 8

-- Todo

record Item where
  constructor MkItem
  level : Int
  task : String
  done : Bool

Eq Item where
  ia == ib = level ia == level ib

Ord Item where
  compare ia ib = level ia `compare` level ib

data IAct = Toggle | Level Int | Remove | Add

covering
todoApp : IO ()
todoApp = control $ prompt
      >>> event
      >>> (generalize (todo >>> display) >>> effect putStrLn) \|/ failed
      >>> never
  where failed = eff (putStrLn "not a valid command")

        item : String -> Mealy IAct (Maybe Item)
        item name = accum (Just (MkItem 0 name False)) match
          where match : IAct -> Maybe Item -> Maybe Item
                match Toggle    = map (record {done $= not})
                match (Level n) = map (record {level = n})
                match Remove    = const Nothing
                match Add       = id

        todo : Mealy (String, IAct) (List Item)
        todo = dynamic item >>> fun sort

        display : Mealy (List Item) String
        display = fun (unlines . map ren)
          where ren item = show (level item) ++ ". ["
                        ++ (if (done item) then "X" else " ")
                        ++ "] " ++ task item

        prompt : Auto IO a (Maybe (String, IAct))
        prompt = eff (putStr "todo: ")
             >>> eff getLine
             >>> fun (parse . words)
          where parse ("remove":: entry) = Just (unwords entry, Remove)
                parse ("toggle":: entry) = Just (unwords entry, Toggle)
                parse ("add"   :: entry) = Just (unwords entry, Add)
                parse ("level" :: num :: entry) =
                  if all isDigit (unpack num)
                     then Just (unwords entry, Level (cast {to=Int} num))
                     else Nothing
                parse _ = Nothing

-- Animation

partial
Main.main : IO ()
Main.main = case drop 1 !getArgs of
            ["hibye"] => hibye
            ["bytebeat", "ambient"] => bytebeat Ambient
            ["todo"] => todoApp
            _ => putStrLn "unknown example"

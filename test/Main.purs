module Test.Main where

import Prelude

import Data.Maybe
import Data.Either
import Data.Lens
import Data.Lens.Zoom
import Data.Tuple
import Data.Traversable (Traversable)

import Control.Monad.Eff.Console
import Control.Monad.State

foo :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
foo = lens _.foo (_ { foo = _ })

bar :: forall a b r. Lens { bar :: a | r } { bar :: b | r } a b
bar = lens _.bar (_ { bar = _ })

type Foo a = { foo :: Maybe { bar :: Array a } }

doc :: Foo String
doc = { foo: Just { bar: [ "Hello", " ", "World" ]} }

bars :: forall a b. Traversal (Foo a) (Foo b) a b
bars = foo <<< _Just <<< bar <<< traversed

stateTest :: Tuple Int String
stateTest = evalState go (Tuple 4 ["Foo", "Bar"]) where
  go = Tuple <$> zoom _1 get <*> zoom (_2 <<< traversed) get

data GetterTest = GetterTest Int

getterTest :: GetterP GetterTest Int
getterTest = to \ (GetterTest i) -> i

{--
getterTestZho :: GetterP GetterTest Int
getterTestZho = toZho $ \ (GetterTest i) -> i

Error found:
in module Test.Main
at /Users/bkolera/src/purescript-profunctor-lenses/test/Main.purs line 39, column 17 - line 41, column 1

  Could not match type
    Int
  with type
    GetterTest

while trying to match type _0 _1 _2
  with type Forget Int GetterTest GetterTest
while checking that expression (($) toZho) (\$7 ->
                                              case $7 of
                                                (GetterTest i) -> ...
                                           )
  has type Forget Int Int Int -> Forget Int GetterTest GetterTest
in value declaration getterTestZho

where _0 is an unknown type
      _2 is an unknown type
      _1 is an unknown type
-}

main = do
  print $ view bars doc
  print stateTest
  print $ show ((Tuple (GetterTest 1337) "Foo") ^. _1 <<< getterTest)
  {--
  print $ show ((Just $ GetterTest 1337) ^? _Just <<< getterTest )
Error found:
in module Test.Main
at /Users/bkolera/src/purescript-profunctor-lenses/test/Main.purs line 69, column 17 - line 69, column 66

  Could not match type

    First _0

  with type

    Int


while trying to match type Forget (First _0)
  with type Forget Int
while checking that expression ((<<<) _Just) getterTest
  has type Forget (First _0) _0 _1 -> Forget (First _0) (Maybe GetterTest) _2
in value declaration main

where _1 is an unknown type
      _0 is an unknown type
      _2 is an unknown type
  --}

module Text.HTML.TagSoup.Parser.MTL (
   Parser, Status(Status, source, sourcePos),
   nextChar, force,
   run, gets, tell, mfix, noEmit, ignoreEmit,
   ) where


import qualified Text.HTML.TagSoup.Position as Position
import Text.HTML.TagSoup.Position (Position)

import Control.Monad.RWS (RWST(..), gets, tell)
import Control.Monad.Fix (mfix)


-- cf. Haskore.General.Parser
type Parser w a = RWST () [w] Status Maybe a

data Status =
   Status {
      sourcePos :: Position,
      source    :: String}
   deriving Show


run :: Parser w a -> Status -> Maybe (a, Status, [w])
run p = runRWST p ()


nextChar :: Parser w Char
nextChar =
   RWST $ \ () (Status pos str) ->
      case str of
         []     -> Nothing
         (c:cs) -> Just (c, Status (Position.updateOnChar c pos) cs, [])


{- |
Turn a parser @x@ into one that is evaluated to bottom if @x@ fails.
This is useful if you know that @x@ cannot fail.
Using force let you return data immediately and thus makes parsing lazier.
-}
force :: Parser w a -> Parser w a
force x =
   RWST $ \ () pcs ->
      let Just (y,pcs',ws) = runRWST x () pcs
      in  Just (y,pcs',ws)

{- |
Turn a parser @x@ into one that is evaluated to bottom if @x@ emits something
throught the writer part of the monad.
This is useful if you know that @x@ does not emit something.
In this case you allow subsequent emitters to emit,
also if the current statement needs infinite steps to complete.

It checks, whether the ignored information is actually empty.
However this safety leads to too much strictness.
-}
noEmit :: Parser w a -> Parser w a
noEmit x =
   RWST $ \ () pcs ->
      fmap (\ ~(y,pcs',empty@ ~[]) -> (y,pcs',empty)) $ runRWST x () pcs

ignoreEmit :: Parser w a -> Parser w a
ignoreEmit x =
   RWST $ \ () pcs ->
      fmap (\ ~(y,pcs',_) -> (y,pcs',[])) $ runRWST x () pcs

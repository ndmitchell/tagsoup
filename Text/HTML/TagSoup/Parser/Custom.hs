module Text.HTML.TagSoup.Parser.Custom (
   Parser, Status(Status, source, sourcePos),
   nextChar, force,
   run, gets, tell, mfix, noEmit, ignoreEmit,
   forceSequence, forceSequence_, forceSequence', forceSequence'_,
   ) where


import Text.ParserCombinators.Parsec.Pos
          (SourcePos, updatePosChar)

import Control.Monad (MonadPlus, mzero, mplus, liftM, liftM2)
import Control.Monad.Fix (MonadFix, mfix)


{- |
This type is essentially equivalent to @RWST () [w] Status Maybe a@,
but its monadic combinator @>>=@ is defined more lazily.
See ParserMTL.

cf. Haskore.General.Parser
-}
newtype Parser w a = Cons { run :: Status -> Maybe (a, Status, [w]) }


instance Monad (Parser w) where
   return a = Cons $ \s -> return (a, s, [])
   m >>= k  = Cons $ \s -> do
	   ~(a, s', w)  <- run m s
	   ~(b, s'',w') <- run (k a) s'
	   return (b, s'', w ++ w')
   fail msg = Cons $ \_ -> fail msg

instance MonadPlus (Parser w) where
   mzero       = Cons $ \_ -> mzero
   m `mplus` n = Cons $ \s -> run m s `mplus` run n s


{- |
Cf. 'Control.Monad.State.gets'
-}
gets :: (Status -> a) -> Parser w a
gets f =
   Cons $ \ st -> Just (f st, st, [])

{- |
Cf. 'Control.Monad.Writer.tell'
-}
tell :: [w] -> Parser w ()
tell ws = Cons $ \ st -> return ((),st,ws)


instance MonadFix (Parser w) where
   mfix f = Cons $ \ st -> mfix $ \ ~(a, _, _) -> run (f a) st



data Status =
   Status {
      sourcePos :: SourcePos,
      source    :: String}
   deriving Show



nextChar :: Parser w Char
nextChar =
   Cons $ \ (Status pos str) ->
      case str of
         []     -> Nothing
         (c:cs) -> Just (c, Status (updatePosChar pos c) cs, [])


{- |
Turn a parser @x@ into one that is evaluated to bottom if @x@ fails.
This is useful if you know that @x@ cannot fail.
Using force let you return data immediately and thus makes parsing lazier.
-}
force :: Parser w a -> Parser w a
force x =
   Cons $ \ pcs ->
      let Just (y,pcs',ws) = run x pcs
      in  Just (y,pcs',ws)

      {- it is important to disect Status, otherwise laziness is restricted
      let Just (y,(Status p cs),ws) = run x pcs
      in  Just (y,(Status p cs),ws)
      -}

forceSequence'_ :: [Parser w a] -> Parser w ()
forceSequence'_ = foldr (\p rest -> force $ p >> rest) (return ())

forceSequence' :: [Parser w a] -> Parser w [a]
forceSequence' = foldr (\p rest -> force $ liftM2 (:) p rest) (return [])

forceSequence_ :: [Parser w a] -> Parser w ()
forceSequence_ ps0 =
   force $ liftM (const ()) $
   case ps0 of
      [] -> return undefined
      (p:ps) -> p >> forceSequence_ ps

forceSequence :: [Parser w a] -> Parser w [a]
forceSequence ps0 =
   force $
   case ps0 of
      [] -> return []
      (p:ps) -> liftM2 (:) p (forceSequence ps)


{- |
Turn a parser @x@ into one that is evaluated to bottom if @x@ emits something
through the writer part of the monad.
This is useful if you know that @x@ does not emit something.
In this case you allow subsequent emitters to emit,
also if the current statement needs infinite steps to complete.
-}
noEmit :: Parser w a -> Parser w a
noEmit x =
   Cons $ \ pcs ->
      fmap (\(y,pcs',[]) -> (y,pcs',[])) $ run x pcs

ignoreEmit :: Parser w a -> Parser w a
ignoreEmit x =
   Cons $ \ pcs ->
      fmap (\ ~(y,pcs',_) -> (y,pcs',[])) $ run x pcs

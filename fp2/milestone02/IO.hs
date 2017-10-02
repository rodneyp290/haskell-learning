{-# LANGUAGE TupleSections #-}

-- Code Written by Rhys
-- Taken from https://gitlab.com/rimmington/tictactoe/blob/master/IO.hs

module IO (gameLoop) where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

inputLoop :: (a -> String) -> (a -> Maybe b) -> ((Int, Int) -> a -> Maybe a) -> a -> IO (a, b)
inputLoop shw won insert a0 = iterateUntilJust won' turn a0
  where
    won' a = (a,) <$> won a
    turn a = putStrLn (shw a) *> untilJust (tryInsert a =<< untilJust prompt)
    prompt = putStr "Enter a move like (0,0): " *> hFlush stdout *> (readMaybe <$> getLine)
    tryInsert a coords
      | Just a' <- insert coords a = pure $ Just a'
      | otherwise                  = Nothing <$ putStrLn "That is not a valid move"
    -- Originally from Control.Monad.Loops
    untilJust :: (Monad m) => m (Maybe a) -> m a
    untilJust ma = iterateUntilJust id (const ma) Nothing
    iterateUntilJust :: (Monad m) => (a -> Maybe b) -> (a -> m a) -> a -> m b
    iterateUntilJust p f v
      | Just b <- p v = pure b
      | otherwise     = iterateUntilJust p f =<< f v

gameLoop :: (b -> String)                -- ^ How to show the board
         -> (r -> String)                -- ^ How to show the result
         -> (b -> Maybe r)               -- ^ Is there a game result?
         -> ((Int, Int) -> b -> Maybe b) -- ^ Try to add a cross at coordinates
         -> b                            -- ^ Initial board
         -> IO ()
gameLoop sb sr won add b0 = declare =<< inputLoop sb won add b0
  where
    declare (b, res) = putStrLn (sb b) *> putStrLn (sr res)

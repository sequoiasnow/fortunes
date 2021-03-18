-- | A simple cursor for always storing fortunes

module Cursor where

data Cursor a = Cursor
  { cursorPrevious :: [a]
  , cursorCurrent  :: a
  , cursorNext     :: [a]
  , cursorTmpEntry :: Maybe a
  }

cursorCurrentEntry :: Cursor a -> a
cursorCurrentEntry (Cursor _ _ _ (Just x)) = x
cursorCurrentEntry (Cursor _ x _ Nothing)  = x

mkCursor :: a -> Cursor a
mkCursor x = Cursor [] x [] Nothing

previousEntryOr :: a -> Cursor a -> Cursor a
previousEntryOr b (Cursor [] x xs Nothing)  = Cursor [] x xs (Just b)
previousEntryOr b (Cursor [] x xs (Just _)) = Cursor [] x xs (Just b)
previousEntryOr _ (Cursor ps x ns _)     = Cursor (init ps) (last ps) (x:ns) Nothing

nextEntryOrAdd :: Monad m => m a -> Cursor a -> m (Cursor a)
nextEntryOrAdd _ (Cursor ps x ys (Just _)) = return $ Cursor ps x ys Nothing
nextEntryOrAdd f (Cursor ps x [] _) = (\ n -> Cursor (ps ++ [x]) n [] Nothing) <$> f
nextEntryOrAdd _ (Cursor ps x (n:ns) _) = return $ Cursor (ps ++ [x]) n ns Nothing

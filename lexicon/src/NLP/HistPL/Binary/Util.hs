{-# LANGUAGE ScopedTypeVariables #-} 


module NLP.HistPL.Binary.Util
( getUsefulContents
, emptyDirectory
, maybeErr
, maybeT
, maybeErrT
) where


import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Exception (try, SomeException)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           System.Directory (getDirectoryContents)


-- | Load the directory contents.
getUsefulContents :: FilePath -> IO [FilePath]
getUsefulContents
    = fmap (filter (`notElem` [".", ".."]))
    . getDirectoryContents


-- | Check if the directory is empty.
emptyDirectory :: FilePath -> IO Bool
emptyDirectory path = null <$> getUsefulContents path


maybeErr :: MonadIO m => IO a -> m (Maybe a)
maybeErr io = do
    r <- liftIO (try io)
    case r of
        Left (_e :: SomeException)  -> return Nothing
        Right x                     -> return (Just x)


maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return
{-# INLINE maybeT #-}


maybeErrT :: MonadIO m => IO a -> MaybeT m a
maybeErrT io = do
    r <- liftIO (maybeErr io)
    maybeT r

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |Contains logic for reading configuration files.
module System.REPL.Config (
   readConfigFile,
   readConfigJSON,
   readConfigShow,
   NoParseError(..),
   ) where

import Prelude hiding ((++), FilePath)

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Functor.Monadic
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as T
import Data.Typeable
import qualified System.FilePath as Fp
import System.Directory
import Text.Read (readMaybe)

-- |Indicates that some string was not able to be parsed.
data NoParseError = NoParseError T.Text deriving (Show, Eq, Read, Typeable)

instance Exception NoParseError

-- |Creates a NoParseError out of a 'Fp.FilePath'.
noParseError :: Fp.FilePath -> NoParseError
noParseError = NoParseError . T.pack

-- |Variant of 'readConfigFile' that uses 'Show' and 'Read' for (de)serialization.
--
--  If the file's content's can't be parsed, a 'NoParseError' will be thrown.
readConfigShow :: forall m a.
                  (MonadThrow m, Functor m, MonadIO m, Default a, Show a,
                   Read a)
               => Fp.FilePath
               -> m a
readConfigShow path = readConfigFile path readEither showBL
   where
      showBL = encodeUtf8 . T.pack . show
      readEither = maybe (Left $ noParseError path) Right . readMaybe . T.unpack . decodeUtf8

-- |Variant of 'readConfigFile' that uses JSON for (de)serialization.
--
--  If the file's content's can't be parsed, a 'NoParseError' will be thrown.
readConfigJSON :: forall m a.
                  (MonadThrow m, Functor m, MonadIO m, Default a, ToJSON a,
                   FromJSON a)
               => Fp.FilePath
               -> m a
readConfigJSON path = readConfigFile path decodeEither encode
   where
      decodeEither = maybe (Left $ noParseError path) Right . decode

-- |Tries to read a configuration from file. If the file is missing,
--  a default instance is written to file and returned. The following
--  exceptions may be thrown:
--
--  * @IOException@, if the IO operations associated with reading or creating the
--    configuration file fail, and
--  * An exception of type @e@ if the configuration file is present, but its
--    contents can't be parsed.
readConfigFile :: forall e m a.
                  (MonadThrow m, Functor m, MonadIO m, Default a, Exception e)
               => Fp.FilePath -- ^Path of the configuration file.
               -> (BL.ByteString -> Either e a)
                  -- ^Parser for the file's contents.
               -> (a -> BL.ByteString)
                  -- ^Encoder for the default value. If the given configuration
                  --  file does not exist, a default value will be serialized
                  --  using this function.
               -> m a
readConfigFile path parser writer = do
   liftIO $ createDirectoryIfMissing True $ Fp.takeDirectory path
   exists <- liftIO $ doesFileExist path
   content <- if not exists then do liftIO $ BL.writeFile path (writer (def :: a))
                                    return $ Right def
              else liftIO (BL.readFile path) >$> parser
   either throwM return content

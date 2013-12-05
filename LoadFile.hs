{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}


module Main where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Typeable               (Typeable)
import qualified Database.Persist            as P
import           Database.Persist.Postgresql (PostgresConf)
import           Database.Persist.Quasi
import           Filesystem
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS   as FS
import           Prelude                     hiding (FilePath)
import           Text.XML                    hiding (Document)
import qualified Text.XML                    as X
import           Text.XML.Lens               hiding (Document)
import           Yesod
import qualified Yesod.Default.Config        as C


hole = undefined

data Hole = Hole

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "dialoguer/config/models")

xmlDir :: FilePath
xmlDir = "./gk-texts/"

psqlConfig :: FilePath
psqlConfig = "./dialoguer/config/postgresql.yml"

parseDocument :: X.Document -> T.Text -> FilePath -> Either T.Text Document
parseDocument X.Document{..} content src =
    note (mappend "Unable to parse document: " . either id id $ FS.toText src) $
         Document <$> fmap T.strip (documentRoot ^? el "text" ./ el "body" ./ el "head" . text)
                  <*> Just content
                  <*> hush (FS.toText src)

loadFiles :: FilePath -> PersistConfigBackend PostgresConf (ResourceT (LoggingT IO)) ()
loadFiles srcDir = do
    files <- liftIO $ listDirectory srcDir
    forM_ files $ \xmlFile -> do
        doc' <- liftIO $ parseDocument <$> X.readFile def xmlFile
                                       <*> readTextFile xmlFile
                                       <*> pure xmlFile
        case doc' of
            Right doc -> do
                log_ . mappend "Loading " . either id id $ FS.toText xmlFile
                insert_ doc
            Left msg  -> log_ msg

log_ :: MonadIO m => T.Text -> m ()
log_ = liftIO . putStrLn . T.unpack

main :: IO ()
main = do
    dbconf <- C.withYamlEnvironment (encodeString psqlConfig) C.Development
                P.loadConfig >>= P.applyEnv
    pool   <- P.createPoolConfig (dbconf :: PostgresConf)

    runStdoutLoggingT $ runResourceT $ P.runPool dbconf (loadFiles xmlDir) pool


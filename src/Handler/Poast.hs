
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Poast where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Text.Markdown
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

renderPoast :: Text -> Text -> IO Html
renderPoast d t = fmap (markdown defaultMarkdownSettings) p
   where p = fmap fromStrict $ TIO.readFile $ poastDirectory ++ T.unpack d ++ "_" ++ T.unpack t ++ ".md"

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getPoastR:: Text -> Text -> Handler Html
getPoastR t p = do
    defaultLayout $ do
        poast <- liftIO $ renderPoast t p
        setTitle "blough"
        $(widgetFile "poast")


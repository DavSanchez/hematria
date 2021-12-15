{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text, pack)
import Monomer
import qualified Monomer.Lens as L
import Paths_hematria_gui
import TextShow

newtype AppModel = AppModel
  { _clickCount :: Int
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ label "Hello world",
          spacer,
          hstack
            [ label $ "Click count: " <> showt (model ^. clickCount),
              spacer,
              button "Increase count" AppIncrease
            ]
        ]
        `styleBasic` [padding 10]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]

main :: IO ()
main = do
  font <- getDataFileName "assets/fonts/Roboto-Regular.ttf"
  let config =
        [ appWindowTitle "Hello world",
          appTheme darkTheme,
          appFontDef "Regular" (pack font),
          appInitEvent AppInit
        ]
      model = AppModel 0
   in startApp model handleEvent buildUI config

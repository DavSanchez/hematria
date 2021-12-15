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

import Types
import Data.Default (Default(def))

-- newtype AppModel = AppModel
--   { _clickCount :: Int
--   }
--   deriving (Eq, Show)

data AppEvent
  = HematriaInit
  | HematriaPerform
  deriving (Eq, Show)

-- makeLenses 'AppModel

buildUI ::
  WidgetEnv HematriaModel AppEvent ->
  HematriaModel ->
  WidgetNode HematriaModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ label "Perform Gematria:",
          spacer,
          hstack
            [ label $ "Click count: ", -- <> showt (model ^. clickCount),
              spacer,
              button "Get Dict. Cache" undefined -- AppIncrease
            ]
        ]
        `styleBasic` [padding 10]

handleEvent ::
  WidgetEnv HematriaModel AppEvent ->
  WidgetNode HematriaModel AppEvent ->
  HematriaModel ->
  AppEvent ->
  [AppEventResponse HematriaModel AppEvent]
handleEvent wenv node model evt = case evt of
  HematriaInit -> []
  HematriaPerform -> undefined -- [Model (model & clickCount +~ 1)]

main :: IO ()
main = do
  font <- getDataFileName "assets/fonts/Roboto-Regular.ttf"
  let config =
        [ appWindowTitle "Hematria",
          appTheme darkTheme,
          appFontDef "Regular" (pack font),
          appInitEvent HematriaInit
        ]
      model = HematriaModel def
   in startApp model handleEvent buildUI config

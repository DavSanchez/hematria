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
import qualified Data.Text.Hematria as H
import qualified Cache as HCache

import qualified Tutorial01_Basics
import qualified Tutorial02_Styling
import qualified Tutorial03_LifeCycle
import qualified Tutorial04_Tasks
import qualified Tutorial05_Producers
import qualified Tutorial06_Composite
import qualified Tutorial07_CustomWidget
import qualified Tutorial08_Themes

-- newtype AppModel = AppModel
--   { _clickCount :: Int
--   }
--   deriving (Eq, Show)

data HematriaAppEvent
  = HematriaInit
  | HematriaGetCache
  | HematriaPerform
  deriving (Eq, Show)

-- makeLenses 'AppModel

buildUI ::
  WidgetEnv HematriaModel HematriaAppEvent ->
  HematriaModel ->
  WidgetNode HematriaModel HematriaAppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ label "Perform Gematria:",
          spacer,
          -- hstack
          --   [
          --   label "Word or phrase",
          --   spacer,
          --   textField
          --   ],
          hstack
            [ label $ "Click count: ", -- <> showt (model ^. clickCount),
              spacer,
              button "Get Dict. Cache" HematriaGetCache -- AppIncrease
            ]
        ]
        `styleBasic` [padding 10]

handleEvent ::
  WidgetEnv HematriaModel HematriaAppEvent ->
  WidgetNode HematriaModel HematriaAppEvent ->
  HematriaModel ->
  HematriaAppEvent ->
  [AppEventResponse HematriaModel HematriaAppEvent]
handleEvent wenv node model evt = case evt of
  HematriaInit -> undefined -- [HCache.updateCache]
  HematriaGetCache -> undefined
  HematriaPerform -> undefined -- [Model (model & clickCount +~ 1)]

main' :: IO ()
main' = do
  font <- getDataFileName "assets/fonts/Roboto-Regular.ttf"
  let config =
        [ appWindowTitle "Hematria",
          appTheme darkTheme,
          appFontDef "Regular" (pack font),
          appInitEvent HematriaInit
        ]
      model = HematriaModel def
   in startApp model handleEvent buildUI config

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Francisco Vallarino
-- License     : BSD-3-Clause (see the LICENSE file)
-- Maintainer  : fjvallarino@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Main module for the tutorials.

main :: IO ()
main = do
  --  Tutorial01_Basics.main01
  Tutorial02_Styling.main02
  --  Tutorial03_LifeCycle.main03
  --  Tutorial04_Tasks.main04
  --  Tutorial05_Producers.main05
  --  Tutorial06_Composite.main06
  --  Tutorial07_CustomWidget.main07
  --  Tutorial08_Themes.main08
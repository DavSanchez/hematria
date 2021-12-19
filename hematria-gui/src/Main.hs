{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Cache as HCache
import Control.Lens
import Data.Default (Default (def))
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Hematria as H
import Monomer
import qualified Monomer.Lens as L
import Paths_hematria_gui
import TextShow
import qualified Tutorial01_Basics
import qualified Tutorial02_Styling
import qualified Tutorial03_LifeCycle
import qualified Tutorial04_Tasks
import qualified Tutorial05_Producers
import qualified Tutorial06_Composite
import qualified Tutorial07_CustomWidget
import qualified Tutorial08_Themes
import Types

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
        [ titleText "Perform Gematria",
          spacer,
          hstack
            [ label "Word or phrase:",
              spacer,
              box (textField hematriaWord)
            ]
            `styleBasic` [paddingV 5],
          hgrid
            [ hstack [         
                label "Type:",
                spacer,
                textDropdownS (activeTodo . todoType) todoTypes `nodeKey` "todoType",
                spacer -- Added here to avoid grid expanding it to 1/3 total width
                ], -- Cipher
              hstack [          
                label "Type:",
                spacer,
                textDropdownS (activeTodo . todoType) todoTypes `nodeKey` "todoType",
                spacer -- Added here to avoid grid expanding it to 1/3 total width
                ], -- Dict
              filler,
              button "Dict. Cache" HematriaGetCache
            ]
            `styleBasic` [paddingV 5],
          label "Result:" `styleBasic` [paddingV 5],
          label_ (T.unlines (model ^. hematriaResult)) [multiline, ellipsis]
            `styleBasic` [ border 1 lightGray,
                           radius 5,
                           textCenter,
                           flexHeight 100,
                           paddingV 5
                         ]
        ]
        `styleBasic` [padding 10]
    titleText text =
      label text
        `styleBasic` [textFont "Regular", textSize 20]

handleEvent ::
  WidgetEnv HematriaModel HematriaAppEvent ->
  WidgetNode HematriaModel HematriaAppEvent ->
  HematriaModel ->
  HematriaAppEvent ->
  [AppEventResponse HematriaModel HematriaAppEvent]
handleEvent wenv node model evt = case evt of
  HematriaInit -> [] -- [HCache.updateCache]
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
      model = def
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
  main'

-- Tutorial01_Basics.main01
-- Tutorial02_Styling.main02

-- Tutorial03_LifeCycle.main03
-- Tutorial04_Tasks.main04
-- Tutorial05_Producers.main05
-- Tutorial06_Composite.main06
-- Tutorial07_CustomWidget.main07
-- Tutorial08_Themes.main08
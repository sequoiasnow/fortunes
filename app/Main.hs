{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Brick                  (on)
import           Brick.AttrMap          (AttrName, attrMap)
import qualified Brick.Main             as M
import           Brick.Types            (BrickEvent (..), Widget)
import qualified Brick.Types            as T
import           Brick.Widgets.Center   (hCenter)
import qualified Brick.Widgets.Center   as C
import           Brick.Widgets.Core     (cached, padBottom, padTopBottom, str,
                                         vBox, withDefAttr)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.List              (intercalate)
import qualified Graphics.Vty           as V
import           System.Process         (readProcess)

data Fortunes = Fortunes
  deriving (Eq, Ord, Show)

unTab :: String -> String
unTab = concatMap unTabC
  where
    unTabC '\t' = "    "
    unTabC c    = [c]

fortune :: IO String
fortune = unTab <$> readProcess "fortune" [] ""

drawUi :: String -> [Widget Fortunes]
drawUi fortune = [ui]
    where
        ui = C.vCenter $
             vBox $ hCenter <$>
             [ padBottom (T.Pad 4) $ str fortune
             , hCenter $ str "Press 'n' to read your next fortune,"
             , str "'Esc' to quit."
             ]

appEvent :: String -> BrickEvent Fortunes e -> T.EventM Fortunes (T.Next String)
appEvent _ (VtyEvent (V.EvKey (V.KChar 'n') [])) = liftIO fortune >>= M.continue
appEvent t (VtyEvent (V.EvKey V.KEsc [])) = M.halt t
appEvent t _ = M.continue t

emphAttr :: AttrName
emphAttr = "emphasis"

app :: M.App String e Fortunes
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr [(emphAttr, V.white `on` V.blue)]
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = do
  firstFortune <- fortune
  void $ M.defaultMain app firstFortune

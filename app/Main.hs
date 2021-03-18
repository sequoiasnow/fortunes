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
import           Cursor
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

drawUi :: Cursor String -> [Widget Fortunes]
drawUi c = [ui]
    where
        ui = C.vCenter $
             vBox $ hCenter <$>
             [ padBottom (T.Pad 4) $ str $ cursorCurrentEntry c
             , hCenter $ str "Press 'n' to read your next fortune,"
             , str "'p' for the previous fortune,"
             , str "'Esc' to quit."
             ]

endMessage :: String
endMessage = "The moving hand writes and having writ, moves on. \n It seems you've reached the end of your fortunes history!"

appEvent :: Cursor String -> BrickEvent Fortunes e -> T.EventM Fortunes (T.Next (Cursor String))
appEvent c (VtyEvent (V.EvKey (V.KChar 'n') [])) =
  liftIO (nextEntryOrAdd fortune c) >>= M.continue
appEvent c (VtyEvent (V.EvKey (V.KChar 'p') [])) =
  M.continue $ previousEntryOr endMessage c
appEvent t (VtyEvent (V.EvKey V.KEsc [])) = M.halt t
appEvent t (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt t
appEvent t _ = M.continue t

emphAttr :: AttrName
emphAttr = "emphasis"

app :: M.App (Cursor String) e Fortunes
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
  void $ M.defaultMain app $ mkCursor firstFortune


------
-- conveniance

infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Reflex.Dom
import Reflex.SubtleCrypto

main :: IO ()
main = mainWidgetWithHead headSection $ do
  let input = "some string to be hashed" :: LBS.ByteString
      alg = SHA256

  e <- getPostBuild >>= delay 0.1
  digestValE <- digest alg (input <$ e)
  let digestValEM =
        fmap
          ( \case
              Left err -> text . T.pack . show $ err
              Right result -> text . T.pack . show $ result
          )
          digestValE
  widgetHold_ blank digestValEM
  blank
  where
    headSection :: DomBuilder t m => m ()
    headSection = blank

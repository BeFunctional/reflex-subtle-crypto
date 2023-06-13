{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reflex.SubtleCrypto where

import Control.Concurrent
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHCJS.DOM.Types (JSM, MonadJSM, liftJSM)
import Language.Javascript.JSaddle.Object (fun, js, js1, js2, jsg, new)
import Language.Javascript.JSaddle.Value (JSVal, valToText)
import Reflex.Dom.Core

data DigestAlgorithm
  = SHA1
  | SHA256
  | SHA384
  | SHA512

instance Show DigestAlgorithm where
  show SHA1 = "SHA-1"
  show SHA256 = "SHA-256"
  show SHA384 = "SHA-384"
  show SHA512 = "SHA-512"

newtype SubtleCryptoError
  = CanNotComputeDigest Text
  deriving stock (Show)

newtype DigestResult = DigestResult {unDigestResult :: LBS.ByteString}
  deriving stock (Show)

-- | Reflex interface to digest from SubtleCrypto Web Api
digest ::
  (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m)) =>
  DigestAlgorithm ->
  Event t LBS.ByteString ->
  m (Event t (Either SubtleCryptoError DigestResult))
digest alg inputE = do
  (onResultEvent, onResultCallback) <- newTriggerEvent

  performEvent_ $
    ffor inputE $ \input ->
      liftJSM $
        digestJSM alg input (liftIO . onResultCallback)

  pure onResultEvent

-- | https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest
digestJSM ::
  DigestAlgorithm ->
  LBS.ByteString ->
  (Either SubtleCryptoError DigestResult -> JSM ()) ->
  JSM ()
digestJSM alg input onResult = do
  inputUint8Array <- new (jsg @Text "TextEncoder") () ^. js1 @Text "encode" (T.decodeUtf8 . LBS.toStrict $ input)

  hashBufferMVar <- liftIO newEmptyMVar :: JSM (MVar JSVal)
  _hashBuffer <-
    jsg @Text "crypto"
      ^. js ("subtle" :: Text)
        . js2 @Text "digest" (T.pack . show $ alg) inputUint8Array
        . js1 @Text
          "then"
          ( fun $ \_ _ args -> case args of
              (result : _) -> liftIO $ putMVar hashBufferMVar result
              _ -> void $ jsg @Text "console" ^. js1 @Text "log" ("no arg" :: Text) -- Unreachable
          )

  uint8Array <- liftIO (readMVar hashBufferMVar) >>= new (jsg @Text "Uint8Array")
  hashArray <- jsg @Text "Array" ^. js1 @Text "from" uint8Array

  mapResultMVar <- liftIO (newMVar "") :: JSM (MVar Text)
  _hashHex <-
    hashArray
      ^. js1 @Text
        "map"
        ( fun $ \_ _ args -> case args of
            (b : _) -> do
              result <- b ^. js1 @Text "toString" (16 :: Int) . js2 @Text "padStart" (2 :: Int) ("0" :: Text)
              newText <- valToText result
              liftIO $ modifyMVar_ mapResultMVar (\oldText -> pure $ oldText <> newText)
            _ -> void $ jsg @Text "console" ^. js1 @Text "log" ("no arg" :: Text) -- Unreachable
        )

  liftIO (readMVar mapResultMVar) >>= onResult . Right . DigestResult . LBS.fromStrict . T.encodeUtf8

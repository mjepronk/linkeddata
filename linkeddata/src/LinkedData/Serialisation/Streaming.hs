{-

Use like this:

runResourceT $ S.print $ (parsed parser (decodeByteString decodeUtf8Pure (Q.readFile "utf8-encoded.txt")))

-}

module LinkedData.Serialisation.Streaming
  (
    toGraph
  , toGraphWithMeta
  , fromGraph

  -- * Parsing
  , parsed
  , parsedWith
  , handleParserError

  -- * Encoding and decoding
  , encodeByteString
  , decodeByteString
  , handleDecodeError

  -- * Encoding functions
  , TE.encodeUtf8
  , TE.encodeUtf16LE
  , TE.encodeUtf16BE
  , TE.encodeUtf32LE
  , TE.encodeUtf32BE

  -- * Decoding functions
  , DST.decodeUtf8Pure
  , DST.decodeUtf16LE
  , DST.decodeUtf16BE
  , DST.decodeUtf32LE
  , DST.decodeUtf32BE
  )
where

import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import Data.Maybe (maybe)
import           Control.Monad.Trans.State.Strict (StateT, runStateT)
import           Streaming (Stream, Of(..), lift)
import qualified Streaming.Prelude as S
import           Streaming.Internal (Stream(..))
import qualified Data.ByteString.Streaming as BS
import           Data.ByteString.Streaming.Internal (ByteString(..))
import qualified Data.Streaming.Text as DST
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import LinkedData.Types (Triple, Graph(..), GraphMeta, SerialisationError(..), emptyGraph)


-- | Convert a Stream of Triples to a Graph.
toGraph :: Monad m
        => Stream (Of Triple) m (Either e x)
        -> m (Either e Graph)
toGraph s = do
   (ts :> x) <- S.toList s
   case x of
     Right _ -> pure . Right $ emptyGraph { triples=ts }
     Left err -> pure $ Left err

toGraphWithMeta :: Monad m
                => Stream (Of Triple) m (Either e GraphMeta)
                -> m (Either e Graph)
toGraphWithMeta s = do
   (ts :> x) <- S.toList s
   case x of
     Right meta' -> pure . Right $ emptyGraph { triples=ts, meta=meta' }
     Left err -> pure $ Left err

-- | Convert a Graph to a Stream of Triples.
fromGraph :: Monad m => Graph -> Stream (Of Triple) m ()
fromGraph = S.each . triples


handleDecodeError :: Monad m => Either DST.DecodeResult r -> m (Either SerialisationError ())
handleDecodeError (Left err) = case err of
    DST.DecodeResultSuccess _ _  -> pure $ Right ()
    DST.DecodeResultFailure _ bs -> pure $ Left (SDecodingError bs)
handleDecodeError (Right _) = pure $ Right ()


handleParserError :: Monad m => Either (Errors, Stream (Of T.Text) m r) r -> m (Either SerialisationError ())
handleParserError (Left ((cs, e), s)) = do
    x <- maybe T.empty fst <$> S.uncons s
    pure $ Left (SParsingError x cs e)
handleParserError (Right _) = pure $ Right ()


decodeByteString :: Monad m
                 => (B.ByteString -> DST.DecodeResult)  -- ^ decoding function from 'Data.Streaming.Text'
                 -> ByteString m r                      -- ^ raw input
                 -> Stream (Of T.Text) m (Either DST.DecodeResult r)  -- ^ Stream of 'T.Text' values
decodeByteString = go
  where
    go decoder p0 =
      case p0 of
        Go m       -> lift m >>= go decoder
        Empty r    -> case decoder B.empty of  -- end of stream, feed empty chunk to decoder
                        DST.DecodeResultSuccess _ _       -> Return (Right r)
                        err@(DST.DecodeResultFailure _ _) -> Return (Left err)
        Chunk bs p1 | B.null bs -> go decoder p1  -- ignore emty chunks from input
                    | otherwise -> case decoder bs of  -- decode chunk
                        DST.DecodeResultSuccess t cont    -> Step (t :> go cont p1)
                        err@(DST.DecodeResultFailure _ _) -> Return (Left err)
{-# INLINABLE decodeByteString #-}

encodeByteString :: Monad m
                 => (T.Text -> B.ByteString)
                 -> Stream (Of T.Text) m r
                 -> ByteString m r
encodeByteString f s = BS.fromChunks (S.map f s)
{-# INLINABLE encodeByteString #-}


-- From streaming-attoparsec
-- https://github.com/haskell-streaming/streaming-attoparsec/blob/master/Data/Attoparsec/ByteString/Streaming.hs

type Errors = ([String], String)

parsed :: Monad m
       => A.Parser a              -- ^ Attoparsec Text parser
       -> Stream (Of T.Text) m r  -- ^ Raw input
       -> Stream (Of a) m (Either (Errors, Stream (Of T.Text) m r) r)
parsed parser = begin
  where
    begin p0 = do
        x <- lift $ S.next p0
        case x of
          Left r -> Return (Right r)
          Right (t, p1)
            | T.null t  -> begin p1
            | otherwise -> step (S.yield t >>) (A.parse parser t) p1

    step diffP res p0 = case res of
      A.Fail _ c m -> Return (Left ((c, m), diffP p0))
      A.Done t a | T.null t  -> Step (a :> begin p0)
                 | otherwise -> Step (a :> begin (S.yield t >> p0))
      A.Partial k  -> do
        x <- lift $ S.next p0
        case x of
          Left r -> step diffP (k T.empty) (Return r)
          Right (t, p1) | T.null t  -> step diffP res p1
                        | otherwise -> step (diffP . (S.yield t >>)) (k t) p1
{-# INLINABLE parsed #-}


-- | Repeatedly apply 'A.Parser' with state.
-- Adapted from 'parsed' from 'streaming-attoparsec' to use StateT.
parsedWith :: Monad m
           => StateT s A.Parser a     -- ^ Attoparsec Text parser wrapped in StateT
           -> s                       -- ^ state
           -> Stream (Of T.Text) m r  -- ^ Raw input
           -> Stream (Of a) m (Either (Errors, Stream (Of T.Text) m r) (r, s))
parsedWith parser = begin
  where
    begin s p0 = do
        x <- lift $ S.next p0
        case x of
          Left r -> Return (Right (r, s))
          Right (t, p1)
            | T.null t  -> begin s p1
            | otherwise -> step (S.yield t >>) (A.parse (runStateT parser s) t) p1

    step diffP res p0 = case res of
      A.Fail _ c m -> Return (Left ((c, m), diffP p0))
      A.Done t (a, s) | T.null t  -> Step (a :> begin s p0)
                      | otherwise -> Step (a :> begin s (S.yield t >> p0))
      A.Partial k  -> do
        x <- lift $ S.next p0
        case x of
          Left r -> step diffP (k T.empty) (Return r)
          Right (t, p1) | T.null t  -> step diffP res p1
                        | otherwise -> step (diffP . (S.yield t >>)) (k t) p1
{-# INLINABLE parsedWith #-}

-- | N-Triples serialiser
--
-- See:
-- - https://www.w3.org/TR/n-triples/#n-triples-grammar
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LinkedData.Serialisation.NTriplesSerialiser
  ( serialiseNTriples
  , serialiseNTriplesFile
  )
where

import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Streaming as BS
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Streaming (Stream, Of)
import qualified Streaming.Prelude as S

import           LinkedData.Types
import           LinkedData.IRI (unIRI)
import           LinkedData.Serialisation.Streaming (encodeByteString, encodeUtf8)


-- | Serialise a producer of triples to strict 'T.Text' values in N-Triples
-- format. When a serialisation error occurs a Left SerialisationError is
-- returned.
serialiseNTriples :: Monad m => Stream (Of Triple) m r -> Stream (Of T.Text) m r
serialiseNTriples = S.map (TL.toStrict . B.toLazyText . renderTriple)

-- | Serialise a producer of triples to a N-Triples file. When a serialisation
-- error occurs a Left SerialisationError is returned.
serialiseNTriplesFile
  :: MonadResource m => FilePath -> Stream (Of Triple) m r -> m r
serialiseNTriplesFile fp p = BS.writeFile fp (encodeByteString encodeUtf8 (serialiseNTriples p))


renderTriple :: Triple -> B.Builder
renderTriple (Triple s p o) = renderTerm s <> " "
                           <> renderTerm p <> " "
                           <> renderTerm o <> " .\n"

renderTerm :: Term -> B.Builder
renderTerm (ITerm i)    = "<" <> renderT (unIRI i) <> ">"
renderTerm (LTerm v)    = renderLiteral v
renderTerm (BNode s)    = if "genid" `T.isPrefixOf` s
                            then "_:x" <> renderT s
                            else "_:" <> renderT s
renderTerm (BNodeGen i) = "_:genid" <> B.decimal i

renderLiteral :: LValue -> B.Builder
renderLiteral (PlainL s)    = "\"" <> renderLiteralT s <> "\""
renderLiteral (PlainLL s l) = "\"" <> renderLiteralT s <> "\"@" <> renderT l
renderLiteral (TypedL s t)  =
    "\"" <> renderLiteralT s <> "\"^^<" <> renderT (unIRI t) <> ">"

renderT :: T.Text -> B.Builder
renderT = B.fromText

-- Render literal text (escape special characters).
renderLiteralT :: T.Text -> B.Builder
renderLiteralT = B.fromText . T.concatMap escape
  where
    escape c = if c `elem` ['\t', '\b', '\n', '\r', '\f', '\"', '\'', '\\']
        then "\\" <> T.singleton c
        else T.singleton c

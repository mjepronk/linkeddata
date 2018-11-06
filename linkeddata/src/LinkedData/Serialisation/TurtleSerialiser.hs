-- | Turtle serialiser
--
-- See:
-- - https://www.w3.org/TR/turtle/#sec-grammar-grammar
--
-- TODO:
-- - handle RDF Collections (section 2.8)
-- - optionally sort triples in memory before writing to file (this will allow
--   better compression for unosorted triple streams, but will require that all
--   triples can be stored in RAM, so works for small graphs only)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}

module LinkedData.Serialisation.TurtleSerialiser
  ( serialiseTurtle
  , serialiseTurtleFile
  )
where

import           Control.Applicative ((<|>))
import           Control.Monad.Trans.Reader (Reader, runReader, ask)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Streaming as BS
import           Data.List (groupBy, intersperse)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Streaming (Stream, Of)
import qualified Streaming.Prelude as S

import           LinkedData.Types
import           LinkedData.IRI (IRI, Abs, Prefix(..), relativeFrom, unIRI, findPrefix)
import           LinkedData.Namespaces (rdfType)
import           LinkedData.Serialisation.Streaming (encodeByteString, encodeUtf8)


type TurtleSerialiser = Reader GraphMeta


-- | Serialise a producer of triples to strict 'T.Text' values in Turtle format.
-- When a serialisation error occurs a Left SerialisationError is returned.
serialiseTurtle
  :: Monad m => GraphMeta -> Stream (Of Triple) m r -> Stream (Of T.Text) m r
serialiseTurtle g p = do
    S.yield $ renderBase (base g)
    S.yield $ renderPrefixes (M.toList (namespaces g))
    renderTriples g p

-- | Serialise a producer of triples to a Turtle file. When a serialisation
-- error occurs a Left SerialisationError is returned.
serialiseTurtleFile
  :: (MonadResource m) => FilePath -> GraphMeta -> Stream (Of Triple) m r -> m r
serialiseTurtleFile fp g p = BS.writeFile fp (encodeByteString encodeUtf8 (serialiseTurtle g p))


-- | Render the base statement
renderBase :: Maybe (IRI Abs) -> T.Text
renderBase (Just b) = "@base <" <> unIRI b <> "> .\n"
renderBase _ = T.empty

-- | Render all the prefix statements (namespaces).
renderPrefixes :: [(Prefix, IRI Abs)] -> T.Text
renderPrefixes ps = TL.toStrict . B.toLazyText . mconcat $ fmap (\(Prefix p, i) ->
    "@prefix " <> B.fromText p <> ": <" <> B.fromText (unIRI i) <> "> .\n") ps

-- | Render all the triples yielded by a producer to Text.
renderTriples :: Monad m => GraphMeta -> Stream (Of Triple) m r -> Stream (Of T.Text) m r
renderTriples g s0 = S.mapped renderTriples' (groupsBySubject s0)
  where
    renderTriples' :: Monad m => Stream (Of Triple) m r -> m (Of T.Text r)
    renderTriples' s =
      S.mapOf (\ts -> TL.toStrict . B.toLazyText $
        runReader (renderTriplesCommS ts) g) <$> S.toList s

    groupsBySubject :: Monad m => Stream (Of Triple) m r -> Stream (Stream (Of Triple) m) m r
    groupsBySubject = S.groupBy (\(Triple s _ _) (Triple s' _ _) -> s == s')

-- | Render triples with a common subject.
renderTriplesCommS :: Triples -> TurtleSerialiser B.Builder
renderTriplesCommS [] = pure ""
renderTriplesCommS ts@(Triple s _ _:_) = do
    s' <- renderTerm s
    ts' <- mconcat . intersperse sep <$>
        traverse renderTriplesCommP (groupBy predicate ts)
    pure $ s' <> " " <> ts' <> " .\n"
  where
    predicate (Triple _ p _) (Triple _ p' _) = p == p'
    sep = ";\n    " :: B.Builder

-- | Render triples with a common subject and predicate.
renderTriplesCommP :: Triples -> TurtleSerialiser B.Builder
renderTriplesCommP [] = pure ""
renderTriplesCommP ts@(Triple _ p _:_) = do
    p' <- renderPredicate p
    ts' <- mconcat . intersperse sep <$>
        traverse (\(Triple _ _ o) -> renderTerm o) ts
    pure $ p' <> " " <> ts'
  where
    sep = ",\n        " :: B.Builder

renderTerm :: Term -> TurtleSerialiser B.Builder
renderTerm (ITerm i)    = renderIRI' i
renderTerm (LTerm v)    = renderLiteral v
renderTerm (BNode s)    = pure $ if "genid" `T.isPrefixOf` s
                            then "_:x" <> B.fromText s
                            else "_:" <> B.fromText s
renderTerm (BNodeGen i) = pure $ "_:genid" <> B.decimal i

renderPredicate :: Term -> TurtleSerialiser B.Builder
renderPredicate (ITerm iri)
  | iri == rdfType = pure "a"
  | otherwise      = renderIRI' iri
renderPredicate t = renderTerm t

renderIRI' :: IRI Abs -> TurtleSerialiser B.Builder
renderIRI' iri = do
    env <- ask
    pure $ fromMaybe defaultBuilder (
        tryPrefixes (namespaces env) <|> tryBase (base env))
  where
    defaultBuilder :: B.Builder
    defaultBuilder = "<" <> B.fromText (unIRI iri) <> ">"

    tryBase :: Maybe (IRI Abs) -> Maybe B.Builder
    tryBase Nothing = Nothing
    tryBase (Just base) =
      let renderIRI'' iri' = Just $ "<" <> B.fromText (unIRI iri') <> ">"
      in  maybe Nothing renderIRI'' (iri `relativeFrom` base)

    tryPrefixes :: Namespaces -> Maybe B.Builder
    tryPrefixes ns = do
      (Prefix p, iri') <- findPrefix ns iri
      pure $ B.fromText p <> ":" <> (B.fromText . unIRI $ iri')

-- Render literal.
renderLiteral :: LValue -> TurtleSerialiser B.Builder
renderLiteral (PlainL s)    = pure $ "\"" <> renderLiteralT s <> "\""
renderLiteral (PlainLL s l) = pure $ "\"" <> renderLiteralT s <> "\"@" <> B.fromText l
renderLiteral (TypedL s t)  = do
    t' <- renderIRI' t
    pure $ "\"" <> renderLiteralT s <> "\"^^" <> t'

-- Render literal text (escape special characters).
renderLiteralT :: T.Text -> B.Builder
renderLiteralT = B.fromText . T.concatMap escape
  where
    escape c = if c `elem` ['\t', '\b', '\n', '\r', '\f', '\"', '\'', '\\']
        then "\\" <> T.singleton c
        else T.singleton c

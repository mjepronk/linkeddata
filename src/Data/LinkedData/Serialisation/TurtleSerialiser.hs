-- | Turtle serialiser
--
-- See:
-- - https://www.w3.org/TR/turtle/#sec-grammar-grammar
--
-- TODO:
-- - handle RDF Collections (section 2.8)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.LinkedData.Serialisation.TurtleSerialiser
  ( serialiseTurtle
  , serialiseTurtleFile )
where

import           Safe (headMay)
import           Control.Applicative ((<|>))
import           Control.Monad.Reader (Reader, runReader, ask)
import           Data.List (sort, groupBy, intersperse)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as B
import           Data.LinkedData.Types
import qualified Data.LinkedData.IRI as LD
import qualified Data.LinkedData.Utils as LD
import           Path (Path, File, toFilePath)


data SerialiserEnv = SerialiserEnv {
    stBase         :: Maybe IRI
  , stNamespaces   :: [(Prefix, IRI)]
  }
  deriving Show

initialSerialiserEnv :: SerialiserEnv
initialSerialiserEnv = SerialiserEnv {
    stBase         = Nothing
  , stNamespaces   = []
  }

type TurtleSerialiser = Reader SerialiserEnv

serialiseTurtle :: Graph -> TL.Text
serialiseTurtle g@Graph { base, namespaces } =
    B.toLazyText (runReader (renderDoc g) st)
  where
    st = initialSerialiserEnv { stBase = base, stNamespaces = M.toList namespaces }

serialiseTurtleFile :: Graph -> Path a File -> IO ()
serialiseTurtleFile g fp =
    TL.writeFile (toFilePath fp) (serialiseTurtle g)

-- | Render a Turtle document.
renderDoc :: Graph -> TurtleSerialiser B.Builder
renderDoc Graph { triples, base, namespaces } = do
    let b = case base of
              Just (IRI b') -> "@base <" <> B.fromText b' <> "> .\n"
              Nothing -> ""
        ps = renderPrefixes (M.toList namespaces)
    ts <- renderTriples (sort triples)
    pure $ b <> ps <> ts

-- | Render all the prefix statements (namespaces).
renderPrefixes :: [(Prefix, IRI)] -> B.Builder
renderPrefixes ps = mconcat $ fmap (\(Prefix p, IRI i) ->
    "@prefix " <> B.fromText p <> ": <" <> B.fromText i <> "> .\n") ps

-- | Render a sorted list of triples.
renderTriples :: Triples -> TurtleSerialiser B.Builder
renderTriples ts = mconcat <$>
    traverse renderTriplesCommS (groupBy subject ts)
  where
    subject (Triple s _ _) (Triple s' _ _) = s == s'

-- | Render triples with a common subject.
renderTriplesCommS :: Triples -> TurtleSerialiser B.Builder
renderTriplesCommS [] = pure ""
renderTriplesCommS ts@(t:_) = do
    s' <- renderTerm . LD.subjectOf $ t
    ts' <- mconcat . intersperse sep <$>
        traverse renderTriplesCommP (groupBy predicate ts)
    pure $ s' <> " " <> ts' <> " .\n"
  where
    predicate (Triple s p _) (Triple s' p' _) = s == s' && p == p'
    sep = ";\n    " :: B.Builder

-- | Render triples with a common subject and predicate.
renderTriplesCommP :: Triples -> TurtleSerialiser B.Builder
renderTriplesCommP [] = pure ""
renderTriplesCommP ts@(t:_) = do
    p' <- renderPredicate . LD.predicateOf $ t
    ts' <- mconcat . intersperse sep <$>
        traverse (renderTerm . LD.objectOf) ts
    pure $ p' <> " " <> ts'
  where
    sep = ",\n        " :: B.Builder

renderTerm :: Term -> TurtleSerialiser B.Builder
renderTerm (ITerm i)    = renderIRI i
renderTerm (LTerm v)    = renderLiteral v
renderTerm (BNode s)    = pure $ if "genid" `T.isPrefixOf` s
                            then "_:x" <> B.fromText s
                            else "_:" <> B.fromText s
renderTerm (BNodeGen i) = pure $ "_:genid" <> B.fromString (show i)
renderTerm (Var _)      = pure "" -- fail "Can't serialize query variables!"

renderPredicate :: Term -> TurtleSerialiser B.Builder
renderPredicate (ITerm iri)
  | iri == rdfType = pure "a"
  | otherwise      = renderIRI iri
renderPredicate t = renderTerm t

renderIRI :: IRI -> TurtleSerialiser B.Builder
renderIRI iri = do
    env <- ask
    pure $ fromMaybe defaultBuilder (
        tryBase (stBase env) <|> tryPrefixes (stNamespaces env))
  where
    defaultBuilder :: B.Builder
    defaultBuilder = "<" <> B.fromText (LD.unIRI iri) <> ">"

    tryBase :: Maybe IRI -> Maybe B.Builder
    tryBase Nothing = Nothing
    tryBase (Just base') = do
      (IRI r) <- iri `LD.relativeFrom` base'
      pure $ "<" <> B.fromText r <> ">"

    tryPrefixes :: [(Prefix, IRI)] -> Maybe B.Builder
    tryPrefixes ns = do
      (Prefix p, base') <- headMay $ filter (\(_, b) -> b `LD.isBaseIRIOf` iri) ns
      (IRI r) <- iri `LD.relativeFrom` base'
      pure $ B.fromText p <> ":" <> B.fromText r

-- Render literal.
renderLiteral :: LValue -> TurtleSerialiser B.Builder
renderLiteral (PlainL s)    = pure $ "\"" <> renderLiteralT s <> "\""
renderLiteral (PlainLL s l) = pure $ "\"" <> renderLiteralT s <> "\"@" <> B.fromText l
renderLiteral (TypedL s t)  = do
    t' <- renderIRI t
    pure $ "\"" <> renderLiteralT s <> "\"^^" <> t'

-- Render literal text (escape special characters).
renderLiteralT :: T.Text -> B.Builder
renderLiteralT = B.fromText . T.concatMap escape
  where
    escape c = if c `elem` ['\t', '\b', '\n', '\r', '\f', '\"', '\'', '\\']
        then "\\" <> T.singleton c
        else T.singleton c

rdfType :: IRI
rdfType = (IRI "type") `LD.unsafeRelativeTo` rdfNS

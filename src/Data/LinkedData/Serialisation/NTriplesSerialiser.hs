-- | N-Triples serialiser
--
-- See:
-- - https://www.w3.org/TR/n-triples/#n-triples-grammar
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.LinkedData.Serialisation.NTriplesSerialiser
  ( serialiseNTriples
  , serialiseNTriplesFile )
where

import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as B
import           Data.LinkedData.Types
import           Path (Path, File, toFilePath)


serialiseNTriples :: Graph -> TL.Text
serialiseNTriples Graph { triples } =
    B.toLazyText (renderTriples triples)

serialiseNTriplesFile :: Graph -> Path a File -> IO ()
serialiseNTriplesFile g fp =
    TL.writeFile (toFilePath fp) (serialiseNTriples g)


renderTriples :: Triples -> B.Builder
renderTriples ts = mconcat (renderTriple <$> ts)

renderTriple :: Triple -> B.Builder
renderTriple (Triple s p o) = renderTerm s <> " "
                           <> renderTerm p <> " "
                           <> renderTerm o <> " .\n"

renderTerm :: Term -> B.Builder
renderTerm (ITerm (IRI i)) = "<" <> renderT i <> ">"
renderTerm (LTerm v)       = renderLiteral v
renderTerm (BNode s)       = if "genid" `T.isPrefixOf` s
                               then "_:x" <> renderT s
                               else "_:" <> renderT s
renderTerm (BNodeGen i)    = "_:genid" <> B.fromString (show i)
renderTerm (Var _)         = "" -- fail "Can't serialize query variables!"

renderLiteral :: LValue -> B.Builder
renderLiteral (PlainL s)    = "\"" <> renderT s <> "\""
renderLiteral (PlainLL s l) = "\"" <> renderT s <> "\"@" <> renderT l
renderLiteral (TypedL s (IRI t)) =
    "\"" <> renderT s <> "\"^^<" <> renderT t <> ">"

renderT :: T.Text -> B.Builder
renderT = B.fromText

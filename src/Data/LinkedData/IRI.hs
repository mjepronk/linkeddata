module Data.LinkedData.IRI where

import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.LinkedData.Types (Term(..), IRI(..))
import qualified Data.Text as T
import qualified Text.URI as URI


-- | Construct IRI term from (expanded) prefix and IRI.
iterm :: IRI -> IRI -> Term
iterm a b =
    case appendIRI a b of
      Just (IRI i) -> ITerm i
      Nothing -> iriToTerm b

unIRI :: IRI -> T.Text
unIRI (IRI iri) = iri

iriToTerm :: IRI -> Term
iriToTerm (IRI i) = ITerm i

appendIRI :: IRI -> IRI -> Maybe IRI
appendIRI (IRI a) b'@(IRI b) = do
   b'' <- isAbsoluteIRI b'
   if b''
    then pure b'
    else pure $ IRI (a <> b)

unsafeAppendIRI :: IRI -> IRI -> IRI
unsafeAppendIRI a b = fromJust $ appendIRI a b

isAbsoluteIRI :: IRI -> Maybe Bool
isAbsoluteIRI (IRI i) = do
    u <- URI.mkURI i
    pure (URI.isPathAbsolute u)

uriToIRI :: URI.URI -> IRI
uriToIRI u = IRI (URI.render u)

-- | QuasiQuoters for using hardcoded IRI's in your code.
--

{-# LANGUAGE TemplateHaskell #-}

module LinkedData.QQ
  ( absiri
  , reliri
  )
where

import qualified Data.Text as T
import Data.Typeable (cast)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift, dataToExpQ)
import LinkedData.IRI (mkAbsIRI, mkRelIRI)

absiri :: QuasiQuoter
absiri = qq (\str ->
  case mkAbsIRI (T.pack str) of
    Just x -> dataToExpQ (fmap liftText . cast) x
    Nothing -> error ("Invalid absolute IRI: " <> str))

reliri :: QuasiQuoter
reliri = qq (\str ->
  case mkRelIRI (T.pack str) of
    Just x -> dataToExpQ (fmap liftText . cast) x
    Nothing -> error ("Invalid relative IRI: " <> str))

qq :: (String -> Q Exp) -> QuasiQuoter
qq quoteExp' = QuasiQuoter {
    quoteExp  = quoteExp'
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)


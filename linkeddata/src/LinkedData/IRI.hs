-- | Internationalized Resource Identifier.
--
-- From `RDF 1.1 Concepts and Abstract Syntax` [1]:
--
--   URIs and IRIs: IRIs are a generalization of URIs [RFC3986] that permits a
--   wider range of Unicode characters. Every absolute URI and URL is an IRI,
--   but not every IRI is an URI. When IRIs are used in operations that are only
--   defined for URIs, they must first be converted according to the mapping
--   defined in section 3.1 of [RFC3987]. A notable example is retrieval over
--   the HTTP protocol. The mapping involves UTF-8 encoding of non-ASCII
--   characters, %-encoding of octets not allowed in URIs, and Punycode-encoding
--   of domain names.
--
-- See:
-- - [1] https://www.w3.org/TR/rdf11-concepts/#section-IRIs
-- - [2] https://tools.ietf.org/html/rfc3987
--

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}

module LinkedData.IRI
  (
  -- * Basics
    Abs, Rel, IRI
  , mkAbsIRI
  , unIRI

  -- * Base IRI's
  , mkRelIRI
  , mkIRIWithBase
  , relativeTo
  , relativeFrom

  -- * Namespaces and prefixes
  , Namespaces
  , Prefix(..)
  , (.:.)
  , findPrefix
  )
where

import           Prelude hiding (takeWhile)

import           Data.Attoparsec.Text (Parser, parseOnly, char, endOfInput,
                     string, option, takeWhile, takeWhile1)
import           Data.Binary (Binary)
import           Data.Char (isDigit, isAsciiUpper, isAsciiLower)
import           Data.Data (Data)
import           Data.Hashable (Hashable(..))
import           Data.List (intersperse, stripPrefix, dropWhileEnd)
import qualified Data.Map as M
import           Data.Maybe (isJust, catMaybes)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import           GHC.Generics (Generic)
import           Safe (headMay, lastMay)
import           Test.QuickCheck (Arbitrary, Gen, oneof, arbitrary, frequency)


-- | An absolute IRI.
data Abs deriving Data

-- | A relative IRI.
data Rel deriving Data

-- | The type to represent an absolute or relative Internationalized Resource
-- Identifier. The type parameter 't' can be 'Abs' (absolute for IRI's) or 'Rel'
-- (for relative IRI's).
newtype IRI t = IRI
  { unIRI :: T.Text }
  deriving (Eq, Ord, Generic, Data)

instance Show (IRI t) where
  show (IRI i) = "<" <> T.unpack i <> ">"

instance Binary (IRI Abs)
instance Binary (IRI Rel)

instance Hashable (IRI Abs)
instance Hashable (IRI Rel)

instance Arbitrary (IRI Rel) where
  arbitrary = renderIRI <$> arbitrary

instance Arbitrary (IRI Abs) where
  arbitrary = renderIRI . unAbsIRIParts <$> arbitrary


-- | Smart constructor for an absolute IRI, will test if an IRI follows RFC 3987
-- and is absolute.
mkAbsIRI :: T.Text -> Maybe (IRI Abs)
mkAbsIRI iri = do
  iri' <- parseIRI iri
  case iriScheme iri' of
    Just _ -> pure (IRI iri)
    Nothing -> Nothing


-- | Smart constructor for a relative IRI, will test if an IRI follows RFC 3987.
mkRelIRI :: T.Text -> Maybe (IRI Rel)
mkRelIRI iri = do
  _ <- parseIRI iri
  pure (IRI iri)


-- | Smart constructor for an absolute 'IRI', that takes an optional base 'IRI'
-- to which the given IRI is resolved if relative. Base 'IRI's are strictly not
-- part of RDF, but they are used in serialisation formats (notably the Turtle
-- format).
mkIRIWithBase :: Maybe (IRI Abs) -> T.Text -> Either String (IRI Abs)
mkIRIWithBase base iri =
  case parseIRI iri of
    Nothing -> Left $ "Invalid IRI <" <> T.unpack iri <> ">."
    Just iri' ->
      case iriScheme iri' of
        Just _ -> pure (IRI iri)
        Nothing ->
          case base of
            Nothing -> Left $ "IRI " <> show iri' <> " is relative, but no base defined."
            Just base' ->
              case parseIRI (unIRI base') of
                Nothing -> Left $ "Invalid base IRI <" <> T.unpack (unIRI base') <> ">."
                Just base'' -> pure . renderIRI $ iri' `relativeTo'` base''


-- | Resolves a relative 'IRI' to a base 'IRI'.
relativeTo :: IRI a    -- ^ relative 'IRI'
           -> IRI Abs  -- ^ base 'IRI'
           -> Maybe (IRI Abs)
relativeTo (IRI r) (IRI base) = do
    r' <- parseIRI r
    base' <- parseIRI base
    pure (renderIRI (r' `relativeTo'` base'))


-- | Make an IRI relative from an absolute base IRI.
relativeFrom :: IRI Abs  -- ^ relative 'IRI'
             -> IRI Abs  -- ^ base 'IRI'
             -> Maybe (IRI Rel)
relativeFrom (IRI r) (IRI base) = do
    r' <- parseIRI r
    base' <- parseIRI base
    pure (renderIRI (r' `relativeFrom'` base'))


type Namespaces = M.Map Prefix (IRI Abs)

-- | Namespace prefix.
--
-- Again from `RDF 1.1 Concepts and Abstract Syntax`:
--
--   Some namespace IRIs are associated by convention with a short name known as
--   a namespace prefix.
--
newtype Prefix = Prefix T.Text
  deriving (Eq, Ord, Show)


-- | Create a new IRI from a prefix IRI and a suffix IRI. This is used to add
-- the relative part to an IRI from a 'Namespace'. It just concatenates the Text
-- values.
(.:.) :: IRI Abs -> IRI Rel -> IRI Abs
(.:.) (IRI prefix) (IRI suffix) = IRI (prefix <> suffix)


-- TODO
-- mkPrefix :: T.Text -> Maybe Prefix
-- mkPrefix t = parse PN_PREFIX

-- Find the namespace prefix and relative 'IRI' for a given 'IRI' from the
-- provided 'Namespaces'.
findPrefix :: Namespaces -> IRI Abs -> Maybe (Prefix, IRI Rel)
findPrefix ns i = do
    (p, iri) <- headMay . catMaybes $ findPrefix' (unIRI i) <$> M.toList ns
    iri' <- mkRelIRI iri
    pure (p, iri')
  where
    findPrefix' i' (p, b) =
      case T.stripPrefix (unIRI b) i' of
        Just iri' -> pure (p, iri')
        Nothing   -> Nothing


-- | IRI parsing, validation and resolving.
--
-- Strictly in RDF there is no such thing as relative IRI's, and so there is no
-- need for parsing and joining IRI's. However, in serialisation formats like
-- Turtle we may need to resolve a relative IRI to the documents (absolute) base
-- IRI. This module provides the functions to do that.
--
-- Note: no normalization as specified in section 6.2.2 of RFC3986 is performed,
-- this is by design (the Turtle docs specify in section 6.3 that it shouldn't
-- be done.
--

data IRIParts = IRIParts
  { iriScheme   :: Maybe T.Text
  , iriAuth     :: Maybe T.Text
  , iriPath     :: T.Text
  , iriQuery    :: Maybe T.Text
  , iriFragment :: Maybe T.Text
  } deriving (Eq, Ord, Generic)

instance Show IRIParts where
  show x = "<" <> (T.unpack . unIRI . renderIRI $ x) <> ">"

newtype AbsIRIParts = AbsIRIParts { unAbsIRIParts :: IRIParts }
  deriving (Show)

instance Arbitrary IRIParts where
  arbitrary = do
    iriScheme <- arbitraryMaybe 20 (oneof $ pure <$> sampleSchemes)
    iriAuth <- arbitraryMaybe 2 arbitraryIRIAuth
    iriPath <- oneof $ pure <$> sampleAbsPaths <> sampleRelPaths
    iriQuery <- arbitraryMaybe 8 (oneof $ pure <$> sampleQueries)
    iriFragment <- arbitraryMaybe 6 (oneof $ pure <$> sampleFragments)
    pure IRIParts { .. }

instance Arbitrary AbsIRIParts where
  arbitrary = do
    iriScheme <- oneof $ pure . Just <$> sampleSchemes
    iriAuth <- Just <$> arbitraryIRIAuth
    iriPath <- oneof $ pure <$> sampleAbsPaths
    iriQuery <- arbitraryMaybe 8 (oneof $ pure <$> sampleQueries)
    iriFragment <- arbitraryMaybe 6 (oneof $ pure <$> sampleFragments)
    pure (AbsIRIParts (IRIParts { .. }))


-- | Parse 'IRI' to the 'IRIParts' data structure.
--
-- See:
-- - https://tools.ietf.org/html/rfc3987#section-2.2
--
parseIRI :: T.Text -> Maybe IRIParts
parseIRI iri = either (const Nothing) Just (parseOnly (parseIRI' <* endOfInput) iri)

parseIRI' :: Parser IRIParts
parseIRI' = IRIParts <$> parseScheme
                     <*> parseAuth
                     <*> parsePath
                     <*> parseQuery
                     <*> parseFragment

parseScheme :: Parser (Maybe T.Text)
parseScheme = option Nothing (pure <$> parseScheme')
  where
    parseScheme' = takeWhile1 isScheme <* char ':'
    isScheme c = isAsciiUpper c || isAsciiLower c ||
                 isDigit c || c `elem` ['+', '-', '.']

parseAuth :: Parser (Maybe T.Text)
parseAuth = option Nothing (Just <$> parseAuth')
  where
    parseAuth' = string "//" *> takeWhile isAuth
    isAuth c = c `notElem` ['/', '?', '#'] && isIRIChar c

parsePath :: Parser T.Text
parsePath = (<>) <$> option "" (T.singleton <$> char '/')
                 <*> takeWhile isPath
  where isPath c = c `notElem` ['?', '#'] && isIRIChar c

parseQuery :: Parser (Maybe T.Text)
parseQuery = option Nothing (Just <$> parseQuery')
  where
    parseQuery' = char '?' *> takeWhile isQuery
    isQuery c = c /= '#' && isIRIChar c

parseFragment :: Parser (Maybe T.Text)
parseFragment = option Nothing (Just <$> parseFrag)
  where
    parseFrag = char '#' *> takeWhile isIRIChar

isIRIChar :: Char -> Bool
isIRIChar c = c > '\x0020' &&
              c `notElem` ['<', '>', '"', '{', '}', '|', '^', '`', '\\']


-- | Render a parsed IRI to a 'IRI'.
renderIRI :: IRIParts -> IRI a
renderIRI = IRI . TL.toStrict . B.toLazyText . renderIRI'

renderIRI' :: IRIParts -> B.Builder
renderIRI' IRIParts { .. } =
       maybe mempty ((<> ":")  . B.fromText) iriScheme
    <> maybe mempty (("//" <>) . B.fromText) iriAuth
    <> B.fromText iriPath
    <> maybe mempty (("?" <>)  . B.fromText) iriQuery
    <> maybe mempty (("#" <>)  . B.fromText) iriFragment


-- TODO
--
-- See: https://tools.ietf.org/html/rfc3987#section-3.1
-- encodeURI :: IRI -> Maybe T.Text
-- encodeURI = undefined


-- | Resolves a relative 'IRI' to a base 'IRI'.
--
-- See: https://tools.ietf.org/html/rfc3986#section-5.2.2
relativeTo' :: IRIParts -> IRIParts -> IRIParts
relativeTo' r base
    | isJust (iriScheme r) =
        r { iriPath   = removeDotSegments (iriPath r)
          }
    | isJust (iriAuth r) =
        r { iriScheme = iriScheme base
          , iriPath   = removeDotSegments (iriPath r)
          }
    | T.null (iriPath r) =
        r { iriScheme = iriScheme base
          , iriAuth   = iriAuth base
          , iriPath   = iriPath base
          , iriQuery  = if not (null (iriQuery r))
                          then iriQuery r
                          else iriQuery base
          }
    | isPathAbsolute (iriPath r) =
        r { iriScheme = iriScheme base
          , iriAuth   = iriAuth base
          , iriPath   = removeDotSegments (iriPath r)
          }
    | otherwise =
        r { iriScheme = iriScheme base
          , iriAuth   = iriAuth base
          , iriPath   = removeDotSegments (mergePaths base r)
          }


-- | Non-strict version of 'relativeTo'.
-- nonStrictRelativeTo :: IRIParts -> IRIParts -> IRIParts
-- nonStrictRelativeTo r base
--    | iriScheme r == iriScheme base = r' `relativeTo'` base
--    | otherwise                     = r `relativeTo'` base
--   where r' = r { iriScheme=Nothing }


-- | Make an IRI relative from an absolute base IRI.
relativeFrom' :: IRIParts -> IRIParts -> IRIParts
relativeFrom' r base
    | iriScheme r   /=? iriScheme base = r
    | iriAuth r     /=? iriAuth base = r
    | iriPath r      /= iriPath base =
        r { iriAuth   = Nothing
          , iriScheme = Nothing
          , iriPath   = relativePath (iriPath base) (iriPath r) }
    | iriQuery r    /=? iriQuery base =
        r { iriAuth   = Nothing
          , iriScheme = Nothing
          , iriPath   = "" }
    | iriFragment r /=? iriFragment base =
        r { iriAuth   = Nothing
          , iriScheme = Nothing
          , iriPath   = ""
          , iriQuery  = Nothing }
    | otherwise = r
  where
    (/=?) :: Eq a => Maybe a -> Maybe a -> Bool
    (/=?) (Just a) (Just b) = a /= b
    (/=?) Nothing  Nothing  = False
    (/=?) _        _        = True

    relativePath :: T.Text -> T.Text -> T.Text
    relativePath basep rp
      | T.length basep >= T.length rp = rp
      | otherwise =
        let basep' = if "/" `T.isSuffixOf` basep
                       then dropWhileEnd (== "") . T.split (== '/') $ basep
                       else init . T.split (== '/') $ basep
            rp'    = T.split (== '/') rp
        in  case stripPrefix basep' rp' of
              Just p  -> mconcat (intersperse "/" p)
              Nothing -> rp


-- | Merge the path components of two IRI's.
--
-- See: https://tools.ietf.org/html/rfc3986#section-5.2.3
mergePaths :: IRIParts -> IRIParts -> T.Text
mergePaths base r
    | isJust (iriAuth base) &&
      T.null (iriPath base) = "/" <> T.dropWhile (== '/') (iriPath r)
    | otherwise = initPath (iriPath base) <> iriPath r
  where
    initPath p = fst (T.breakOnEnd "/" p)


-- | Remove dot segments ("." and "..") from the path.
--
-- See: https://tools.ietf.org/html/rfc3986#section-5.2.4
removeDotSegments :: T.Text -> T.Text
removeDotSegments p =
    let xs = T.split (== '/') p
        (xs', ts) =  -- ts = trailing slash
            case lastMay xs of
              Just "" -> (init xs, True)
              _       -> (xs, False)
    in mconcat (intersperse "/" (go xs' [] ts))
  where
    go :: [T.Text] -> [T.Text] -> Bool -> [T.Text]
    go []     []  _  = []
    go []     out ts = reverse (if ts then "" : out else out)
    go (x:xs) out ts
      | x == "."  = go xs out (null xs || ts)
      | x == ".." && (not . null) out = go xs (initPath out) (null xs || ts)
      | x == ".." = go xs out ts
      | otherwise = go xs (x : out) ts

    initPath :: [T.Text] -> [T.Text]
    initPath [] = []
    initPath [""] = [""]
    initPath (_:xs) = xs


-- | Test if a path component is absolute (starts with a slash).
isPathAbsolute :: T.Text -> Bool
isPathAbsolute p = "/" `T.isPrefixOf` p


-- * QuickCheck helpers

arbitraryIRIAuth :: Gen T.Text
arbitraryIRIAuth = do
  iriUser <- arbitraryMaybe 2 (oneof $ pure <$> sampleUsers)
  iriHost <- oneof $ pure <$> sampleHosts
  iriPort <- arbitraryMaybe 3 (oneof $ pure <$> samplePorts)
  pure $ mconcat
    [ maybe mempty (<> "@") iriUser
    , iriHost
    , maybe mempty ((":" <>) . T.pack . show) iriPort
    ]

arbitraryMaybe :: Int -> Gen a -> Gen (Maybe a)
arbitraryMaybe freq gen = frequency [
    (10, pure Nothing),
    (freq, Just <$> gen)]


sampleSchemes :: [T.Text]
sampleSchemes = ["http", "https"]

sampleRelPaths :: [T.Text]
sampleRelPaths =
  [ ""
  , "a"
  , "a/"
  , "../b"
  , "/b/.."
  , "a/b/c"
  , "a/b/c/"
  ]

sampleAbsPaths :: [T.Text]
sampleAbsPaths =
  [ "/"
  , "/a"
  , "/a/"
  , "/a/b"
  , "/a/b/"
  , "/a/b/c"
  , "/a/b/c/"
  ]

sampleQueries :: [T.Text]
sampleQueries =
  [ "q=ultimate+question"
  , "answer=42&lang=en&message=You're+not+going+to+like+it!"
  ]

sampleFragments :: [T.Text]
sampleFragments = [ "top" , "string" , "type" , "" ]

sampleUsers :: [T.Text]
sampleUsers = [ "arthurdent" , "arthurdent:trillian" , "anonymous:anonymous" ]

sampleHosts :: [T.Text]
sampleHosts =
  [ "example.com"
  , "example.org"
  , "3g2upl4pq6kufc4m.onion"
  , "forum.i2p"
  , "192.0.2.42"
  , "203.0.113.42"
  , "2001:db8:ffff:f01d:ab1e:ffff:f01d:ab1e"
  ]

samplePorts :: [Int]
samplePorts = [ 80, 443, 3000, 8000, 8080 ]

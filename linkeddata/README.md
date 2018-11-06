# Linked Data for Haskell

## Examples

### Query a graph

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import LinkedData
import LinkedData.QQ
import LinkedData.Serialisation

-- | Who does Arthur Dent know?
main = do
    let arthur  = Var "arthur"
        who     = Var "who"
        name    = Var "name"
        clauses = [
            TriplePattern (Left arthur) (Right . ITerm $ rdfNS .:. [reliri|type|]) (Right . ITerm $ foafNS .:. [reliri|Person|])
          , TriplePattern (Left arthur) (Right . ITerm $ foafNS .:. [reliri|name|]) (Right (plainL "Arthur Dent"))
          , TriplePattern (Left arthur) (Right . ITerm $ foafNS .:. [reliri|knows|]) (Left who)
          , TriplePattern (Left who) (Right . ITerm $ foafNS .:. [reliri|name|]) (Left name)
          ]
    g <- runResourceT $ toGraphWithMeta (parseTurtleFile Nothing ("test/hhgttg.ttl"))
    case g of
      Right g' -> print $ select g' [name] clauses
      Left err -> print err
```

### Converting between RDF serialisation formats

Right now we support (de-)serialisation of the N-Triples and Turtle formats.

```haskell
import LinkedData.Serialisation

main =
    runResourceT $ serialiseNTriplesFile "hhgttg.nt" (parseTurtleFile Nothing "test/hhgttg.ttl")
```

# Linked Data for Haskell


```haskell
import Data.LinkedData.Serialisation

runResourceT $ serialiseNTriplesFile "rma-edm-collection.nt" (parseTurtleFile "/home/mp/Downloads/201604-rma-edm-collection.ttl")

runResourceT $ serialiseTurtleFile "rma-edm-collection.ttl" emptyGraphMeta (parseNTriplesFile "rma-edm-collection.nt")
```

---

Do not use this library, this code is experimental!!! At this time we are
avoiding success at all costs :-)

You probably want to use:
- rdf4h - https://github.com/robstewart57/rdf4h
- hsparql - https://github.com/robstewart57/hsparql

---

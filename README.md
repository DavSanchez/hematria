# Hematria

Perform gematria with the command line

## Usage

```bash
$ hematria --update # Download dictionaries and ciphers cache
Successfully updated cache.
$ hematria --cipher SpanishSimple --dict Spanish --show 5 "Ritual" # If cache is not present, the program errors.
Numeric value for word "Ritual": 84
Words with the same value:
  ""
  ""
  ""
  ""
  ""
```

## Notes on IO

How `gematria` obtains the list:

```diagram
gematria -> getCipheredWords -> getCipheredDictionary -> getDictionary
```

`getDictionary` should be on the `IO` monad as it pulls from a cached file. So... Where do we put the boundary?

### Type shuffling

Perhaps some type shuffling, like turning `gematria` to `gematria'`, where:

```haskell
gematria :: Cipher -> Dictionary -> T.Text -> (Int, Maybe [T.Text])

gematria' :: Cipher -> DictionaryData -> T.Text -> (Int, Maybe [T.Text])
```

And following the chain on the auxiliary functions would allow us to get the `DictionaryData` first in `performGematria` and then pass it to the `gematria` function.

### Threading `IO` on the whole chain

Would work, but looks like bad design.

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

## Cache

Cache dictionary files are structured like:

- macOS:
  - `~/Library/Caches/hematria/dict/es.txt`
  - `~/Library/Caches/hematria/dict/en.txt`

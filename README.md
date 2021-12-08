# Hematria

[![GitHub CI](https://github.com/DavSanchez/hematria/workflows/CI/badge.svg)](https://github.com/DavSanchez/hematria/actions)

Perform gematria with the command line

## Usage

```bash
$ hematria update # Download dictionaries cache
Successfully updated cache.

$ hematria list dicts
Available dictionaries:

        - sample (sample words, mainly for testing)
        - english
        - spanish

$ hematria list ciphers
Available ciphers:

        - simple-es (simple, ascending value cipher)
        - simple-en (simple, ascending value cipher)

$ hematria value [--cipher simple-es] "Ritual"
The numerical value of the word "Ritual" is 84.

$ hematria [--cipher simple-es] [--dict spanish] [--show 5] "Ritual" # If cache is not present, the program errors.
The numerical value of the word "Ritual" is 84.
Words in the dictionary with the same numerical value are:

        - fogonera
        - cojuelo
        - inalienable
        - hermética
        - silbante
        - tostada
```

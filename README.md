# Hematria

[![GitHub CI](https://github.com/DavSanchez/hematria/workflows/CI/badge.svg)](https://github.com/DavSanchez/hematria/actions)

Perform gematria with the command line

## Usage

```console
$ hematria update # Download dictionaries cache.
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

$ hematria value [--cipher simple-es] "Ritual" # Cache is not needed for this.
The numerical value of the word "Ritual" is 84.

$ hematria [--cipher simple-es] [--dict spanish] [--show 6] "Ritual"
The numerical value of the word "Ritual" is 84.
Words in the dictionary with the same numerical value are:

        - Fogonera
        - Cojuelo
        - Inalienable
        - Hermética
        - Silbante
        - Tostada
```

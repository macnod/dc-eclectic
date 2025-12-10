# dc-eclectic
A collection of eclectic, battle-tested Common Lisp utilities for everyday programming tasks.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Quicklisp](https://raw.githubusercontent.com/quicklisp/quicklisp-bootstrap/master/badge.png)](https://quicklisp.org)

## Overview
`dc-eclectic` provides a grab-bag of high-quality utility functions for file operations, string manipulation, hashing, random generation, list processing, path handling, and more. These functions are designed to be practical, performant, and easy to use.

## Table of Contents
- [Installation](#installation)
- [Quick Start](#quick-start)
- [API Documentation](#api-documentation)
- [Dependencies](#dependencies)
- [License](#license)

## Installation

### Quicklisp
```lisp
(ql:quickload :dc-eclectic)
```

### Manual
```lisp
;; Clone and load
(asdf:load-system :dc-eclectic)
```

## Quick Start

```lisp
(use-package :dc-eclectic)

;; File operations
(file-exists-p "/tmp/test.txt")
(path-type "~/src")
(filename-only "/path/to/file.txt")  ; => "file.txt"

;; String processing
(split-n-trim "Hello  world  ")  ; => ("Hello" "world")
(to-ascii "café ☕" :printable-only t)  ; => "cafe ?"

;; Hashing
(hash-string "password" :salt "mysalt" :size 64)
(hash-hmac-256 "secret" "message")

;; Random
(uuid)
(random-string 16 ascii-alpha-num)
(shuffle '(1 2 3 4 5))

;; Data structures
(hashify-list '(7 8 7 7 8 9) :method :count)
;; => #{7=3, 8=2, 9=1}
```

## API Documentation

### File & Directory Utilities

**`join-paths` &rest path-parts**
> Joins parameters into a unix-like file path, inserting slashes where necessary.

**`path-only` filename**
> Retrieves the path (path only, without the filename) of FILENAME.

**`filename-only` filename**
> Retrieves the filename (filename only, without the path) of FILENAME.

**`leaf-directory-only` path**
> Returns just the leaf directory name from PATH.

**`root-path` files**
> Given FILES, a list of paths, returns the starting path that all paths have in common.

**`file-exists-p` path**
> Returns true if the file specified by PATH exists.

**`directory-exists-p` path**
> Returns true if the directory specified by PATH exists.

**`path-type` path**
> Returns `:FILE`, `:DIRECTORY`, or `:NOT-FOUND` depending on what PATH points to.

**`file-extension` path**
> Returns the file extension for the file name given in PATH.

**`replace-extension` filename new-extension**
> Replaces the file extension in FILENAME with NEW-EXTENSION.

**`copy-file` source destination &key (if-exists :supersede) (buffer-size (* 64 1024))**
> Copies SOURCE file to DESTINATION. Creates directories if needed. Preserves modification time.

**`slurp` filename**
> Reads entire file into a string.

**`spew` string filename**
> Writes STRING to FILENAME, overwriting if exists.

### String Utilities

**`to-ascii` string &key (replacement-char #\?) (printable-only t)**
> Replaces non-ASCII characters with REPLACEMENT-CHAR. PRINTABLE-ONLY keeps only printable ASCII.

**`verify-string` string regex &key ignore-case**
> Returns t if STRING matches REGEX exactly (case-sensitive unless IGNORE-CASE is t).

**`split-n-trim` string &key (on-regex "\\s+") (fat "^\\s+|\\s+$")**
> Splits STRING on ON-REGEX and trims FAT from each substring. Default splits on whitespace.

**`trim` s &optional (fat "^\\s+|\\s+$")**
> Trims FAT regex from the beginning/end of S. Default trims whitespace.

**`trim-whitespace` s**
> Trims all standard whitespace characters from S.

**`all-permutations-of-string` s**
> Returns all permutations of characters in S as strings. `"abc"` → `("abc" "acb" "bac" "bca" "cab" "cba")`.

**`existing-permutations-of-string` s hash**
> Like `all-permutations-of-string` but only returns permutations that exist as keys in HASH.

**`n-gram-strings` chars count**
> Returns all possible n-grams of length COUNT from CHARS. `"abc" 2` → `("aa" "ab" "ac" "ba" "bb" ...)`.

**`existing-n-gram-strings` chars count hash**
> Like `n-gram-strings` but only returns n-grams that exist as keys in HASH.

### Hashing & Data Structures

**`hash-string` string &key (salt "") (size 128)**
> SHA512 hash of STRING+SALT as hex string, first SIZE characters.

**`hash-hmac-256` secret text**
> HMAC-SHA256 of TEXT using SECRET, returned as hex string.

**`hashify-list` list &key (method :count) f-key hash-key plist-key alist-key f-value (initial-value 0)**
> Creates hash table from LIST. Methods: `:COUNT`, `:PLIST`, `:ALIST`, `:INDEX`, `:CUSTOM`.

**`distinct-elements` sequence &key (key #'identity)**
> Returns sequence with duplicate elements removed, using KEY function for comparison.

**`distinct-values` list**
> Alias for `distinct-elements`.

**`comparable-hash-dump` hash &key (f-sort #'string<) (f-make-sortable (lambda (k) (format nil "~a" k))) flat**
> Returns sorted list of hash key-value pairs (or flat plist if FLAT is t).

**`plist-keys` plist**
> Returns list of keys from PLIST.

**`plistp` list**
> Returns t if LIST is a proper plist (even length, keyword keys).

### List & Sequence Utilities

**`flatten` l**
> Flattens nested list L. Treats sequences as single elements.

**`all-permutations` list**
> Returns all permutations of LIST. Duplicates removed.

**`shuffle` seq &optional rstate**
> Returns shuffled copy of SEQ using optional random state RSTATE.

**`choose-one` seq &optional rstate**
> Returns random element from SEQ.

**`choose-some` seq n &optional rstate**
> Returns N random elements from SEQ without replacement.

**`range` start end &key (step 1) (filter #'identity) shuffle**
> Generates numbers from START to END by STEP, optionally filtered and shuffled.

**`index-of-max` list-or-vector**
> Returns index of maximum value in sequence.

**`n-grams` list count**
> Returns all n-grams of length COUNT from LIST.

**`normalize-list` list &key max min**
> Normalizes LIST values to 0.0-1.0 range. Auto-computes MAX/MIN if not provided.

**`denormalize-list` list min max &key integer**
> Denormalizes 0.0-1.0 LIST values to MIN-MAX range. INTEGER returns integers.

### Random Generation

**`rand` value &optional rstate**
> Like `random` but accepts optional random state.

**`random-number` &optional (digits 4) rstate**
> Generates random DIGITS-digit number.

**`random-hex-number` &optional (digits 7) (non-zero-start) rstate**
> Generates random DIGITS-digit hex number. NON-ZERO-START skips leading zero.

**`random-string` string-length alphabet &optional rstate**
> Generates random string using ALPHABET characters.

**`uuid` &optional rstate**
> Generates RFC 4122 UUIDv4 string.

**Character Alphabets:**
- `ascii-alpha`, `ascii-alpha-lower`, `ascii-alpha-upper`
- `ascii-numeric`
- `ascii-alpha-num(-lower|-upper)`

### Encoding & Environment

**`base64-encode/decode` string**
> Standard base64 encode/decode.

**`getenv` name &key default required (type :string)**
> Gets environment variable with type conversion (:string/:integer/:boolean).

**`setenv` name value**
> Sets environment variable NAME to VALUE (converted to string).

**`shell-command-to-string` command**
> Runs shell COMMAND, returns (values output error-output exit-code).

### Logging (not exported but available)
- `open-log`, `close-log`, `log-it`, `log-it-lazy`, `log-it-pairs`
- `set-log-severity-threshold`

## Full API Reference
Exported symbols (from `dc-eclectic` package):
```
all-permutations  ascii-alpha  ascii-alpha-lower  ascii-alpha-num
ascii-alpha-num-lower  ascii-alpha-num-upper  ascii-alpha-upper
ascii-char-range  ascii-numeric  base64-decode  base64-encode
choose-one  choose-some  close-log  comparable-hash-dump
copy-file  define-base-encoder  denormalize-list  directory-exists-p
distinct-elements  distinct-values  existing-n-gram-strings
existing-permutations-of-string  file-exists-p  file-extension
filename-only  flatten  freeze  getenv  hash-hmac-256
hash-keys  hash-string  hash-values  hashify-list  index-of-max
join-paths  leaf-directory-only  log-it  log-it-lazy  log-it-pairs
make-decoder  make-encoder  n-gram-strings  n-grams  normalize-list
open-log  path-only  path-type  plist-keys  plistp  rand
random-hex-number  random-number  random-string  range
reference-random-state  replace-extension  root-path  safe-decode
safe-encode  set-log-severity-threshold  setenv  shell-command-to-string
shuffle  slurp  spew  split-n-trim  thaw  to-ascii  trim
trim-whitespace  uuid  verify-string
```

## API Documentation
Full API documentation available via [CLHS-style quickdocs](https://introspectable.github.io/quickdocs/dc-eclectic/) or locally with:

```lisp
(ql:quickload :quickdocs)
(quickdocs:make-local-quickdocs :dc-eclectic)
```

## Dependencies
- `trivial-utf-8`
- `sb-thread`, `sb-ext` (SBCL)
- `ppcre` (cl-ppcre)
- `ironclad`
- `cl-base64`
- `uiop`

## License
MIT License. See [LICENSE](LICENSE) for details.

## Contributing
PRs welcome! Please add tests for new features.

## Author
[Your Name](https://github.com/yourusername)

---

⭐ **Star this repo if it saves you time!**

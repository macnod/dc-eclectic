<a id="x-28DC-ECLECTIC-3A-40MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="DC-ECLECTIC:@MANUAL%20MGL-PAX:SECTION"></a>

# `DC-ECLECTIC` Reference

## Table of Contents

- [1 Overview][1219]
- [2 Installation][a260]
- [3 Functions and Macros][1c3b]
- [4 Special Variables][ee87]

###### \[in package DC-ECLECTIC\]
A collection of eclectic, battle-tested Common Lisp utilities for everyday programming tasks.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Quicklisp](https://raw.githubusercontent.com/quicklisp/quicklisp-bootstrap/master/badge.png)](https://quicklisp.org)

<a id="x-28DC-ECLECTIC-3A-40OVERVIEW-20MGL-PAX-3ASECTION-29"></a>
<a id="DC-ECLECTIC:@OVERVIEW%20MGL-PAX:SECTION"></a>

## 1 Overview

`dc-eclectic` provides a grab-bag of high-quality utility functions for file operations, string manipulation, hashing, random generation, list processing, path handling, and more. These functions are designed to be practical, performant, and easy to use.

<a id="x-28DC-ECLECTIC-3A-40INSTALLATION-20MGL-PAX-3ASECTION-29"></a>
<a id="DC-ECLECTIC:@INSTALLATION%20MGL-PAX:SECTION"></a>

## 2 Installation

### With Rowswell

```sh
ros install macnod/dc-eclectic/{release}
```

where `{release}` looks like vX.XX. You can find the latest release in the [repo](https://github.com/macnod/dc-eclectic).

### With QuickLisp

Clone the repo to a directory that is visible to ASDF, such as `~/common-lisp`, then, in a REPL, do `(ql:quickload :dc-eclectic)`.

<a id="x-28DC-ECLECTIC-3A-40FUNCTIONS-20MGL-PAX-3ASECTION-29"></a>
<a id="DC-ECLECTIC:@FUNCTIONS%20MGL-PAX:SECTION"></a>

## 3 Functions and Macros

Alphabetical list of functions and macros that DC-UTILITIES defines.

<a id="x-28DC-ECLECTIC-3AALL-PERMUTATIONS-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ALL-PERMUTATIONS%20FUNCTION"></a>

- [function] **ALL-PERMUTATIONS** *LIST*

    Returns a list of every permutation of elements in `LIST`. For
    example:
       '(1 2 3) -> '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

<a id="x-28DC-ECLECTIC-3AALL-PERMUTATIONS-OF-STRING-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ALL-PERMUTATIONS-OF-STRING%20FUNCTION"></a>

- [function] **ALL-PERMUTATIONS-OF-STRING** *S*

    Returns a list of strings representing every permutation of the
    orginal string `S`. For example:
        "abc" -> '("abc" "acb" "bac" "bca" "cab" "cba")

<a id="x-28DC-ECLECTIC-3AASCII-ALPHA-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ASCII-ALPHA%20FUNCTION"></a>

- [function] **ASCII-ALPHA**

    Return a string with all the alphabetic ASCII characters.

<a id="x-28DC-ECLECTIC-3AASCII-ALPHA-LOWER-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ASCII-ALPHA-LOWER%20FUNCTION"></a>

- [function] **ASCII-ALPHA-LOWER**

    Returns a string with all the lower-case alphabetic ASCII
    characters (a-z).

<a id="x-28DC-ECLECTIC-3AASCII-ALPHA-NUM-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ASCII-ALPHA-NUM%20FUNCTION"></a>

- [function] **ASCII-ALPHA-NUM**

    Returns a string with all the alpha-numeric ASCII characters.

<a id="x-28DC-ECLECTIC-3AASCII-ALPHA-NUM-LOWER-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ASCII-ALPHA-NUM-LOWER%20FUNCTION"></a>

- [function] **ASCII-ALPHA-NUM-LOWER**

    Returns a string with all the lower-case alpha-numeric ASCII
    characters.

<a id="x-28DC-ECLECTIC-3AASCII-ALPHA-NUM-UPPER-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ASCII-ALPHA-NUM-UPPER%20FUNCTION"></a>

- [function] **ASCII-ALPHA-NUM-UPPER**

    Returns a string with all the upper-case alpha-numeric ASCII
    characters.

<a id="x-28DC-ECLECTIC-3AASCII-ALPHA-UPPER-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ASCII-ALPHA-UPPER%20FUNCTION"></a>

- [function] **ASCII-ALPHA-UPPER**

    Returns a string with all the upper-case alphabetic ASCII
    characters (A-Z).

<a id="x-28DC-ECLECTIC-3AASCII-CHAR-RANGE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ASCII-CHAR-RANGE%20FUNCTION"></a>

- [function] **ASCII-CHAR-RANGE** *BEGIN END*

    Returns a string that includes ASCII characters in the ASCII code
    range given by `BEGIN` and `END`.

<a id="x-28DC-ECLECTIC-3AASCII-NUMERIC-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ASCII-NUMERIC%20FUNCTION"></a>

- [function] **ASCII-NUMERIC**

    Returns a string with all the ASCII characters that represent
    digits.

<a id="x-28DC-ECLECTIC-3ABASE64-DECODE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:BASE64-DECODE%20FUNCTION"></a>

- [function] **BASE64-DECODE** *BASE64-ENCODED-STRING*

    Decodes `BASE64-ENCODED-STRING` and returns the result. This function
    is compatible with base64-encode and with popular implementation of base64
    encoding and decoding functions. However, this function doesn't properly handle
    multi-byte characters. Unless you require compatibility with external functions,
    use the other decoding functions in `DC-ECLECTIC`. See [`SAFE-ENCODE`][3763], [`SAFE-DECODE`][4316],
    and [`DEFINE-BASE-ENCODER`][e556].

<a id="x-28DC-ECLECTIC-3ABASE64-ENCODE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:BASE64-ENCODE%20FUNCTION"></a>

- [function] **BASE64-ENCODE** *STRING*

    Returns a base64-encoded version of `STRING` that that works list like
    the standard base64-encode implementations in the Linux command line and Emacs
    Lisp. These do not properly handle multibyte characters, so use this function
    only if you need compatibility with popular implementations. `DC-ECLECTIC`
    contains better encoding and decoding functions. See [`SAFE-ENCODE`][3763], [`SAFE-DECODE`][4316],
    and [`DEFINE-BASE-ENCODER`][e556].

<a id="x-28DC-ECLECTIC-3ACHOOSE-ONE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:CHOOSE-ONE%20FUNCTION"></a>

- [function] **CHOOSE-ONE** *SEQ &OPTIONAL RSTATE*

    Returns a random element from `SEQ`.

<a id="x-28DC-ECLECTIC-3ACHOOSE-SOME-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:CHOOSE-SOME%20FUNCTION"></a>

- [function] **CHOOSE-SOME** *SEQ N &OPTIONAL RSTATE*

    Choose `N` elements from `SEQ` and return a new sequence with those
    elements, with the new sequence having the same type as `SEQ`.
    
    `N` must be an integer that is greater than or equal to 1.
    
    If `N` is greater than the length of `SEQ`, this function returns a copy
    of `SEQ`.
    
    If `N` is less than 1, then this function returns nil, even if the
    sequence is not a list.
    
    This function uses the [`RANDOM`][1f1d] function, which accepts an optional
    `RSTATE` parameter that you can optionally pass in here.

<a id="x-28DC-ECLECTIC-3ACOMPARABLE-HASH-DUMP-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:COMPARABLE-HASH-DUMP%20FUNCTION"></a>

- [function] **COMPARABLE-HASH-DUMP** *HASH &KEY (F-SORT \#'STRING\<) (F-MAKE-SORTABLE (LAMBDA (K) (FORMAT NIL "~a" K))) FLAT*

    Turns the given hash table into a list of pairs or, if `FLAT` is `T`,
    into a plist that represents the hash. In the resulting list, the keys are
    sorted, so that the list can be more easily compared with other lists. `HASH` is
    the hash you want to dump. `F-SORT` is the sort predicate, which defaults to
    sorting strings in ascending order.  `F-MAKE-SORTABLE` is a function that accepts
    a key and returns a sortable version of the key. This defaults to turning the
    key into a string. If the keys to the hash are integers, for example, you can
    provide an `F-SORT` of #'\<, which sorts integers numerically in ascending order,
    and you can provide an `F-MAKE-SORTABLE` of #'identity, which will leave the keys
    as integers for sorting purposes.

<a id="x-28DC-ECLECTIC-3ACOPY-FILE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:COPY-FILE%20FUNCTION"></a>

- [function] **COPY-FILE** *SOURCE DESTINATION &KEY (IF-EXISTS :SUPERSEDE) (BUFFER-SIZE (\* 64 1024))*

    Copies `SOURCE` file to `DESTINATION` file. If `DESTINATION`'s directories
    do not exist, they are created. `IF-EXISTS` controls the behavior if `DESTINATION`
    already exists, and may be :error, :new-version, :rename, :rename-and-delete,
    :overwrite, :append, or :supersede. `IF-EXISTS` defaults to :supersede.
    `BUFFER-SIZE` controls the size of the buffer used during the copy operation, and
    defaults to 64 KB. Returns the `DESTINATION` path.

<a id="x-28DC-ECLECTIC-3ADEEP-COPY-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:DEEP-COPY%20FUNCTION"></a>

- [function] **DEEP-COPY** *THING*

    Deep copy `THING`, recursively copying lists, vectors, and strings.

<a id="x-28DC-ECLECTIC-3ADEFINE-BASE-ENCODER-20MGL-PAX-3AMACRO-29"></a>
<a id="DC-ECLECTIC:DEFINE-BASE-ENCODER%20MGL-PAX:MACRO"></a>

- [macro] **DEFINE-BASE-ENCODER** *NAME ALPHABET*

    Creates 2 functions using `NAME` (a symbol), and `ALPHABET` (a string):
    {NAME}-ENCODE and {NAME-DECODE}. {NAME}-ENCODE takes 1 parameter, `DATA` (a
    string), and returns the string encoded with `ALPHABET`. {NAME}-DECODE accepts a
    single parameter (an encoded string) and returns the original strinng. For
    example, if you need to encode strings using hexadecimal characters, you can
    call this function as follows:
    
        (define-base-encoder hex "0123456789abcdef")
    
    That will produce the functions HEX-ENCODE and HEX-DECODE. Those function will
    work as follows:
    
        (hex-encode "Hello World!")             ;; => "48656c6c6f20576f726c6421"
        (hex-decode "48656c6c6f20576f726c6421") ;; => "Hello World!"


<a id="x-28DC-ECLECTIC-3ADENORMALIZE-LIST-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:DENORMALIZE-LIST%20FUNCTION"></a>

- [function] **DENORMALIZE-LIST** *LIST MIN MAX &KEY INTEGER*

    Returns a new list with the numbers in `LIST`, which are
    floating-point numbers between 0 and 1, expanded to the range `MAX` - `MIN`, such
    that the number 1.0 is converted to `MAX`, the number 0.0 is converted to `MIN`, and
    all the other numbers fall in the range `MIN` to `MAX`. `INTEGER` is `T`, the new list
    contains integers. Otherwise, the new list contains floating-point numbers.

<a id="x-28DC-ECLECTIC-3ADIRECTORY-EXISTS-P-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:DIRECTORY-EXISTS-P%20FUNCTION"></a>

- [function] **DIRECTORY-EXISTS-P** *PATH*

    Returns a boolean value indicating if the directory specified by
    `PATH` exists.

<a id="x-28DC-ECLECTIC-3ADISTINCT-ELEMENTS-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:DISTINCT-ELEMENTS%20FUNCTION"></a>

- [function] **DISTINCT-ELEMENTS** *SEQUENCE &KEY (KEY \#'IDENTITY)*

    Accepts a sequence of elements (list or vector) and returns a new
    sequence of the same type with distinct elements from the original. If the
    elements in the sequence are hash tables, plists, or objects with methods, then
    you can provide a value or function for the :key parameter.  If you provide a
    value, the function will use the value as the key of the element, and the value
    of the key will represent the unique signature of the element.  If you provide a
    function, then the function will be applied to the element to compute the
    elements unique signature.

<a id="x-28DC-ECLECTIC-3ADISTINCT-STRINGS-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:DISTINCT-STRINGS%20FUNCTION"></a>

- [function] **DISTINCT-STRINGS** *LIST*

    Alternative for returning a list of distinct strings.

<a id="x-28DC-ECLECTIC-3ADISTINCT-VALUES-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:DISTINCT-VALUES%20FUNCTION"></a>

- [function] **DISTINCT-VALUES** *LIST*

    Synonym for [`DISTINCT-ELEMENTS`][4836].

<a id="x-28DC-ECLECTIC-3AENDS-WITH-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ENDS-WITH%20FUNCTION"></a>

- [function] **ENDS-WITH** *S SUFFIX*

    Returns `T` if string `S` ends with `SUFFIX`.

<a id="x-28DC-ECLECTIC-3AEXCLUDE-20GENERIC-FUNCTION-29"></a>
<a id="DC-ECLECTIC:EXCLUDE%20GENERIC-FUNCTION"></a>

- [generic-function] **EXCLUDE** *REFERENCE-LIST EXCLUDE*

    Returns a list containing the elements of
    `REFERENCE-LIST` that are not in `EXCLUDE`. If `EXCLUDE` is a list, this function
    excludes all elements in `EXCLUDE` from `REFERENCE-LIST`. If `EXCLUDE` is a string,
    this function excludes the string from `REFERENCE-LIST`

<a id="x-28DC-ECLECTIC-3AEXCLUDE-REGEX-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:EXCLUDE-REGEX%20FUNCTION"></a>

- [function] **EXCLUDE-REGEX** *REFERENCE-LIST EXCLUDE &OPTIONAL EXCEPTIONS*

    Returns a list of the elements of `REFERENCE-LIST` that that don't
    match the `EXCLUDE` regular expression. However, elements that are not in
    `EXCEPTIONS` are not excluded, even if they match `EXCLUDE`.

<a id="x-28DC-ECLECTIC-3AEXISTING-N-GRAM-STRINGS-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:EXISTING-N-GRAM-STRINGS%20FUNCTION"></a>

- [function] **EXISTING-N-GRAM-STRINGS** *CHARS COUNT HASH*

    Returns all the `COUNT` n-grams for string `CHARS`, but only of n-grams
    that exist as keys in `HASH`.

<a id="x-28DC-ECLECTIC-3AEXISTING-PERMUTATIONS-OF-STRING-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:EXISTING-PERMUTATIONS-OF-STRING%20FUNCTION"></a>

- [function] **EXISTING-PERMUTATIONS-OF-STRING** *S HASH*

    Works just like all-permutations-of-string, but excludes any
    permutations of `S` that are not among the keys in `HASH`.

<a id="x-28DC-ECLECTIC-3AFILE-EXISTS-P-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:FILE-EXISTS-P%20FUNCTION"></a>

- [function] **FILE-EXISTS-P** *PATH*

    Returns a boolean value indicating if the file specified by `PATH`
    exists.

<a id="x-28DC-ECLECTIC-3AFILE-EXTENSION-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:FILE-EXTENSION%20FUNCTION"></a>

- [function] **FILE-EXTENSION** *PATH*

    Returns a string consisting of the file extension for the file name
    given in `PATH`.

<a id="x-28DC-ECLECTIC-3AFILENAME-ONLY-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:FILENAME-ONLY%20FUNCTION"></a>

- [function] **FILENAME-ONLY** *FILENAME*

    Retrieves the filename (filename only, without the path) of
    `FILENAME`.

<a id="x-28DC-ECLECTIC-3AFLATTEN-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:FLATTEN%20FUNCTION"></a>

- [function] **FLATTEN** *L*

    Given a nested list `L`, return a flat list. If an array or other
    sequence is among the elements of `L`, the sequence is not flattened,
    but treated as a single element.

<a id="x-28DC-ECLECTIC-3AFREEZE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:FREEZE%20FUNCTION"></a>

- [function] **FREEZE** *OBJECT*

    Stringifies `OBJECT`.

<a id="x-28DC-ECLECTIC-3AGETENV-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:GETENV%20FUNCTION"></a>

- [function] **GETENV** *NAME &KEY DEFAULT REQUIRED (TYPE :STRING)*

    Get the value of the environment variable `NAME`, returning a string
    or an integer, depending on `TYPE`. If the environment variable is not set, then
    return `DEFAULT`, which must be of type `TYPE`. If the environment variable is not
    set and `DEFAULT` is `NIL`, then this function returns `NIL`.
    
    `TYPE` can be :integer, :string, or :boolean.

<a id="x-28DC-ECLECTIC-3AHAS-20GENERIC-FUNCTION-29"></a>
<a id="DC-ECLECTIC:HAS%20GENERIC-FUNCTION"></a>

- [generic-function] **HAS** *REFERENCE-LIST THING*

    Returns `T` if `REFERENCE-LIST` contains `THING`. If `THING`
    is a string, number, or symbol (including keywords), this function checks for
    that item in `REFERENCE-LIST`. If `THING` is a list, this function checks that all
    elements of `THING` are in `REFERENCE-LIST`.

<a id="x-28DC-ECLECTIC-3AHAS-SOME-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:HAS-SOME%20FUNCTION"></a>

- [function] **HAS-SOME** *REFERENCE-LIST QUERY-LIST*

    Returns `T` if `REFERENCE-LIST` contains any of the elements in
    `QUERY-LIST`.

<a id="x-28DC-ECLECTIC-3AHASH-HMAC-256-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:HASH-HMAC-256%20FUNCTION"></a>

- [function] **HASH-HMAC-256** *SECRET TEXT*

    Hash `TEXT` using `SECRET` and hmac-sha-256 and return a hex
    representation of the hash

<a id="x-28DC-ECLECTIC-3AHASH-KEYS-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:HASH-KEYS%20FUNCTION"></a>

- [function] **HASH-KEYS** *HASH*

    Returns a list of the keys in hash table `HASH`.

<a id="x-28DC-ECLECTIC-3AHASH-STRING-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:HASH-STRING%20FUNCTION"></a>

- [function] **HASH-STRING** *STRING &KEY (SALT "") (SIZE 128)*

    Hash `STRING` and return a hex representation of the hash

<a id="x-28DC-ECLECTIC-3AHASH-VALUES-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:HASH-VALUES%20FUNCTION"></a>

- [function] **HASH-VALUES** *HASH*

    Returns a list of the values in hash table `HASH`.

<a id="x-28DC-ECLECTIC-3AHASHIFY-LIST-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:HASHIFY-LIST%20FUNCTION"></a>

- [function] **HASHIFY-LIST** *LIST &KEY (METHOD :COUNT) F-KEY HASH-KEY PLIST-KEY ALIST-KEY (F-VALUE (LAMBDA (KEY-RAW KEY-CLEAN VALUE) (DECLARE (IGNORE KEY-RAW KEY-CLEAN)) VALUE)) (INITIAL-VALUE 0)*

    Creates a hash table from `LIST` and returns the hash table, according
    to `METHOD`. Supported methods are `:COUNT`, `:PLIST`, `:ALIST`, `:INDEX`, `AND`([`0`][425d] [`1`][dd55]) `:CUSTOM`.
    
    `:COUNT`
    
        With the :COUNT method, which the function uses by default, the function
        creates a hash table in which each key is an item of the list and the
        associated value for each key is the incidence of the item in the list. For
        example:
        
        (hashify-list '(7 8 7 7 8 9))
        
        gives you a hash table that looks like this:
        
        {7: 3, 8: 2, 9: 1}
    
    `:ALIST` and `:PLIST`
    
        The :ALIST and :PLIST methods convert the list into a hash that conceptually
        represent the same map as the list. Alists and plists both consist of
        collections of key/value pairs. Alists look like this:
        
        '((key1 . value1) (key2 . value2) (key3 . value3)...)
        
        Plists look like this:
        
        '(:key1 value1 :key2 value2 :key3 value3 ...)
        
        If a key repeats in one of these lists, its value simply overwrites the
        value of the repeated key. However, you can change that behavior. See the
        description of the :CUSTOM method for information on how to do that.
    
    `:INDEX`
    
        The :index method causes the values in the list to become the keys in the
        hash table. The value associated with each key should be an increasing
        integer, starting with 0. Thus, the list '(a b c) becomes the hash {a: 1, b:
        2, c: 3}.
        
        If the objects in the list that you're indexing are hash tables, then you
        can specify the object key for the value that the function should use as a
        key in the resulting hash. That object key should be present in every object
        in the list. This allows you to index a list of hash tables by some specific
        value in the hash table. Consider the following example:
        
        [
          {id: "a-001", first: "john", last: "doe"},
          {id: "a-002", first: "jane", last: "doe"}
        ]
        
        If you specify :method :index :hash-key "id", this function will
        create a hash table that looks like this:
        
        {
          "a-001": {id: "a-001", first: "john", last: "doe"},
          "a-002": {id: "a-002", first: "jane", last: "doe"}
        }
        
        And, voilá, you no longer need to iterate through a list to find your
        object.
        
        If the objects are plists, and you specify the index with plist-key, you'll
        see the same behavior with the plist as we demonstrated above for hash
        tables.
        
        HASH-KEY and PLIST-KEY are just shortcuts to save you from having to write
        some code for F-KEY. You can specify only one of HASH-KEY, PLIST-KEY, and
        F-KEY.
    
    `:CUSTOM`
    
        The :CUSTOM method requires that you provide functions for computing the
        keys and values that the function inserts into the resulting hash.
        
        Use F-KEY to provide a function that accepts an element from LIST and
        returns a computed hash key. Here are some examples F-KEY of acceptable
        definitions:
        
            - #'identity
            - #'string-upcase
            - (lambda (x) (zerop (mod x 10)))
        
        Use F-VALUE to provide a function that accepts an element from LIST, the
        computed key (which might be different from the element), and the value
        that's currently associated with the computed key in the resulting hash
        table. Here are some examples:
        
            - (lambda (element computed-key value)
                (declare (ignore element computed-key))
                value)
            - (lambda (element computed-key value)
                (declare (ignore element value))
                (incf value))
        
        If there's no hash value associated with the computed key, then the value
        specified by :INITIAL-VALUE is used.


<a id="x-28DC-ECLECTIC-3AINDEX-OF-MAX-20GENERIC-FUNCTION-29"></a>
<a id="DC-ECLECTIC:INDEX-OF-MAX%20GENERIC-FUNCTION"></a>

- [generic-function] **INDEX-OF-MAX** *LIST-OR-VECTOR*

    Returns the index of the largest number in
    `LIST-OR-VECTOR`.

<a id="x-28DC-ECLECTIC-3AJOIN-PATHS-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:JOIN-PATHS%20FUNCTION"></a>

- [function] **JOIN-PATHS** *&REST PATH-PARTS*

    Joins parameters (collected in `PATH-PARTS`) into a unix-like file
    path, inserting slashes where necessary. `PATH-PARTS` can be strings or
    pathnames. If the first element in `PATH-PARTS` starts with a slash, the resulting
    path will be absolute; otherwise, it will be relative. If the last element of
    `PATH-PARTS` ends in a slash, the resulting path will also end in a
    slash. `PATH-PARTS` elements should be strings or pathnames, but this function
    will try to convert other types to strings, if possible. `NIL` or empty strings
    are ignored.

<a id="x-28DC-ECLECTIC-3ALEAF-DIRECTORY-ONLY-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:LEAF-DIRECTORY-ONLY%20FUNCTION"></a>

- [function] **LEAF-DIRECTORY-ONLY** *PATH*

    Returns the last part of the directory `PATH`. For example,
    /home/one/two => two. If `PATH` is /, this function returns /.

<a id="x-28DC-ECLECTIC-3AN-GRAM-STRINGS-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:N-GRAM-STRINGS%20FUNCTION"></a>

- [function] **N-GRAM-STRINGS** *CHARS COUNT*

    Accepts `CHARS`, a string, and `COUNT`, an integer, and returns all the
    possible combinations of length `COUNT` of the characters in `CHARS`. For example,
    (n-gram-strings "abc" 2) => '("aa" "ab" "ac" "ba" "bb"
    "bc" "ca" "cb" "cc")

<a id="x-28DC-ECLECTIC-3AN-GRAMS-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:N-GRAMS%20FUNCTION"></a>

- [function] **N-GRAMS** *LIST COUNT*

    Returns n-gram lists of length `COUNT` using elements of `LIST`.

<a id="x-28DC-ECLECTIC-3ANORMALIZE-LIST-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:NORMALIZE-LIST%20FUNCTION"></a>

- [function] **NORMALIZE-LIST** *LIST &KEY MAX MIN*

    Returns a new list with new values between 0.0 and 1.0. `MAX` is the
    largest value that `LIST` can hold, and `MIN` is the smallest.  Each new value N is
    computed from the corresponding old value O in `LIST`, as follows:
    
    N = (O - `MIN`) / (`MAX` - `MIN`)
    
    If you don't provide `MAX` and `MIN`, this function does an initial pass through
    list where it sets `MAX` and `MIN` to the largest number and the smallest number in
    `LIST`, respectively.  Therefore, you can improve the performance of this function
    if you already know those values. Furthermore, in some cases the list may not
    even contain the values for `MAX` and `MIN` that you need.

<a id="x-28DC-ECLECTIC-3APATH-ONLY-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:PATH-ONLY%20FUNCTION"></a>

- [function] **PATH-ONLY** *FILENAME*

    Retrieves the path (path only, without the filename) of `FILENAME`.
    `FILENAME` should be a string, a pathname, or `NIL`. If `FILENAME` is `NIL` or the empty
    string, this function returns the empty string. If `FILENAME` has no path
    component, this function returns "/".

<a id="x-28DC-ECLECTIC-3APATH-PARENT-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:PATH-PARENT%20FUNCTION"></a>

- [function] **PATH-PARENT** *PATH*

    Returns the absolute path to the directory that is the parent of
    `PATH`. `PATH` must be an absolute path. If `PATH` points to a file, then this returns
    the path to the file, minus the file name. If `PATH` points to a directory, this
    returns the parent of the directory in `PATH`. If `PATH` points to a directory, it
    must end in /. If `PATH` is / or not an absolute path, this function returns `NIL`.

<a id="x-28DC-ECLECTIC-3APATH-TYPE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:PATH-TYPE%20FUNCTION"></a>

- [function] **PATH-TYPE** *PATH*

    Returns `:FILE`, `:DIRECTORY`, or `:NOT-FOUND`, depending on what `PATH`
    points to.

<a id="x-28DC-ECLECTIC-3APLIST-KEYS-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:PLIST-KEYS%20FUNCTION"></a>

- [function] **PLIST-KEYS** *PLIST*

    Returns list of the keys in `PLIST`.

<a id="x-28DC-ECLECTIC-3APLISTP-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:PLISTP%20FUNCTION"></a>

- [function] **PLISTP** *LIST*

    Returns `T` if `LIST` is a plist.

<a id="x-28DC-ECLECTIC-3APLURAL-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:PLURAL%20FUNCTION"></a>

- [function] **PLURAL** *WORD*

    Convert singular to plural form (works most of the time).

<a id="x-28DC-ECLECTIC-3ARAND-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:RAND%20FUNCTION"></a>

- [function] **RAND** *VALUE &OPTIONAL RSTATE*

    When called without `RSTATE`, this is the same as calling [`RANDOM`][1f1d] with
    only the `VALUE` parameter. Otherwise, this calls `RANDOM` with `VALUE` and `RSTATE`.

<a id="x-28DC-ECLECTIC-3ARANDOM-HEX-NUMBER-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:RANDOM-HEX-NUMBER%20FUNCTION"></a>

- [function] **RANDOM-HEX-NUMBER** *&OPTIONAL (DIGITS 7) (NON-ZERO-START) RSTATE*

    Returns a random hexadecimal number with `DIGITS` digits. If
    `NON-ZERO-START` is specified, then the resulting hexadecimal number starts with
    a character other than 0.

<a id="x-28DC-ECLECTIC-3ARANDOM-NUMBER-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:RANDOM-NUMBER%20FUNCTION"></a>

- [function] **RANDOM-NUMBER** *&OPTIONAL (DIGITS 4) RSTATE*

    Returns a random integer of `DIGITS` digits.

<a id="x-28DC-ECLECTIC-3ARANDOM-STRING-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:RANDOM-STRING%20FUNCTION"></a>

- [function] **RANDOM-STRING** *STRING-LENGTH ALPHABET &OPTIONAL RSTATE*

    Returns a random string of length `STRING-LENGTH`. The string is
    constructed from characters in `ALPHABET`. `ALPHABET` is a string. There are
    several functions available for building `ALPHABET`, all starting with the
    prefix 'ASCII-'. For example:
    
      (random-string 10 (ascii-alpha-num-lower))

<a id="x-28DC-ECLECTIC-3ARANGE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:RANGE%20FUNCTION"></a>

- [function] **RANGE** *START END &KEY (STEP 1) (FILTER \#'IDENTITY) SHUFFLE*

    Returns a list of values between `START` and `END` (inclusive), skipping
    values by `STEP`, filtering remaining values with the function in `FILTER`, and
    shuffling the remaining values if `SHUFFLE` is true.  `STEP` defaults to 1, `FILTER`
    defaults to allowing all values through, and `SHUFFLE` default to nil.

<a id="x-28DC-ECLECTIC-3AREPLACE-EXTENSION-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:REPLACE-EXTENSION%20FUNCTION"></a>

- [function] **REPLACE-EXTENSION** *FILENAME NEW-EXTENSION*

    Replaces the file extension in `FILENAME` with the file extension
    provided in `NEW-EXTENSION`.

<a id="x-28DC-ECLECTIC-3AROOT-PATH-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:ROOT-PATH%20FUNCTION"></a>

- [function] **ROOT-PATH** *FILES*

    Given `FILES`, a list of paths in the form of strings, returns the
    starting path that all the paths have in common.

<a id="x-28DC-ECLECTIC-3ASAFE-DECODE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SAFE-DECODE%20FUNCTION"></a>

- [function] **SAFE-DECODE** *DATA*

    Decodes `DATA` using the `SAFE` alphabet.

<a id="x-28DC-ECLECTIC-3ASAFE-ENCODE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SAFE-ENCODE%20FUNCTION"></a>

- [function] **SAFE-ENCODE** *DATA*

    Encodes data using the `SAFE` alphabet.

<a id="x-28DC-ECLECTIC-3ASAFE-SORT-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SAFE-SORT%20FUNCTION"></a>

- [function] **SAFE-SORT** *LIST &KEY PREDICATE*

    Returns a sorted copy of `LIST`, without modifying `LIST`. If `PREDICATE`
    is not provided, the function determines the type of the first element of the
    list to pick a suitable sort predicate. The function assumes that all the
    elements in `LIST` are of the same type and that they are either strings,
    keywords, or numbers. The function sorts in ascending order by default.  If
    `PREDICATE` is provided, then the function uses that predicate.

<a id="x-28DC-ECLECTIC-3ASETENV-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SETENV%20FUNCTION"></a>

- [function] **SETENV** *NAME VALUE*

    Set environment variable `NAME` to `VALUE`. `VALUE` is always converted into
    a string. Returns a string with `VALUE`.

<a id="x-28DC-ECLECTIC-3ASHELL-COMMAND-BACKGROUND-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SHELL-COMMAND-BACKGROUND%20FUNCTION"></a>

- [function] **SHELL-COMMAND-BACKGROUND** *COMMAND &KEY (WAIT-INTERVAL 0.1)*

    Run `COMMAND` in the background. Returns a process-info plist with
    keys: :pid, :exit-code (nil if running), :output, :error-output, :running-p,
    :wait-interval (for polling), :status.
    
    Returns an `INFO` object that you can use later to check on the backgrounded
    process. Use ([`SHELL-COMMAND-RUNNING-P`][beee] `INFO`), ([`SHELL-COMMAND-WAIT`][0f15] `INFO`),
    (SHELL-COMMAND-EXIT-CODE `INFO`) to check status.

<a id="x-28DC-ECLECTIC-3ASHELL-COMMAND-RUNNING-P-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SHELL-COMMAND-RUNNING-P%20FUNCTION"></a>

- [function] **SHELL-COMMAND-RUNNING-P** *INFO*

    Returns `T` if the background process is still running. `INFO` is the
    object that [`SHELL-COMMAND-BACKGROUND`][0c8e] returns when it starts the process.

<a id="x-28DC-ECLECTIC-3ASHELL-COMMAND-TO-STRING-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SHELL-COMMAND-TO-STRING%20FUNCTION"></a>

- [function] **SHELL-COMMAND-TO-STRING** *COMMAND*

    Executes `COMMAND` in the shell and returns the output as a string.

<a id="x-28DC-ECLECTIC-3ASHELL-COMMAND-WAIT-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SHELL-COMMAND-WAIT%20FUNCTION"></a>

- [function] **SHELL-COMMAND-WAIT** *INFO &OPTIONAL TIMEOUT*

    Wait for process to finish (with optional `TIMEOUT` seconds). `INFO` is
    the object that [`SHELL-COMMAND-BACKGROUND`][0c8e] returns when Updates `INFO` with output
    and exit code. Returns updated `INFO`. If `TIMEOUT` is ommitted or `NIL`, waits until
    the process finishes, which might be forever if the process is a server process,
    for example.  If `TIMEOUT` is provided and the process does not finish within
    `TIMEOUT` seconds, the process is terminated. When the process terminates
    normally, `INFO` is updated with :status :completed, the exit code, and the output
    and error-output strings.  When the process is terminated due a timeout, `INFO` is
    updated with :status :terminated, and the exit code, but output and error-output
    are left `NIL`.

<a id="x-28DC-ECLECTIC-3ASHUFFLE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SHUFFLE%20FUNCTION"></a>

- [function] **SHUFFLE** *SEQ &OPTIONAL RSTATE*

    Return a sequence with the same elements as the given sequence S,
    but in random order (shuffled).

<a id="x-28DC-ECLECTIC-3ASINGULAR-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SINGULAR%20FUNCTION"></a>

- [function] **SINGULAR** *WORD*

    Convert plural to singular form (works most of the time).

<a id="x-28DC-ECLECTIC-3ASLURP-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SLURP%20FUNCTION"></a>

- [function] **SLURP** *FILENAME*

    Returns a string with the content of the file at `FILENAME`.

<a id="x-28DC-ECLECTIC-3ASPEW-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SPEW%20FUNCTION"></a>

- [function] **SPEW** *STRING FILENAME*

    Write `STRING` to filename, creating the file if necessary, and
    replacing the contents of the file if the file already exists.

<a id="x-28DC-ECLECTIC-3ASPLIT-N-TRIM-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:SPLIT-N-TRIM%20FUNCTION"></a>

- [function] **SPLIT-N-TRIM** *STRING &KEY (ON-REGEX "\\\\s+") (FAT "^\\\\s+|\\\\s+$")*

    Splits `STRING` into substrings on `ON-REGEX`, then trims `FAT` from each
    substring.  The `ON-REGEX` parameter value, which is optional, defaults to
    "\s+", which is to say that the string is split into a list of words at the
    whitespace boundaries.  The default value for `FAT`, which is also optional,
    "\s+|\s+$", causes this function to trim whitespace from the beggining and
    end of each substring.  Here's an example:
    
        (split-n-trim "Hello  beautiful      world!")
        
        => '("Hello" "beautiful" "world!")


<a id="x-28DC-ECLECTIC-3ASTARTS-WITH-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:STARTS-WITH%20FUNCTION"></a>

- [function] **STARTS-WITH** *S PREFIX*

    Returns `T` if string `S` starts with `PREFIX`.

<a id="x-28DC-ECLECTIC-3ATHAW-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:THAW%20FUNCTION"></a>

- [function] **THAW** *STRING*

    Returns an object resulting from the evalation of `STRING`. This
    is the opposite of [`FREEZE`][20cd].

<a id="x-28DC-ECLECTIC-3ATO-ASCII-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:TO-ASCII%20FUNCTION"></a>

- [function] **TO-ASCII** *STRING &KEY (REPLACEMENT-CHAR \#\\?) (PRINTABLE-ONLY T)*

    In `STRING`, replaces non-ASCII characters with REPLACEMENT\_CHAR,
    which defaults to the question mark. If `PRINTABLE-ONLY` is true, only printable
    ASCII characters are kept, with the rest being replaced by `REPLACEMENT-CHAR`.

<a id="x-28DC-ECLECTIC-3ATREE-GET-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:TREE-GET%20FUNCTION"></a>

- [function] **TREE-GET** *TREE &REST PATH*

    Get value from the `TREE` structure, at `PATH`. `TREE` is a nested data
    structure where each value can be a plist, list, object, t, or nil.

<a id="x-28DC-ECLECTIC-3ATREE-PUT-20MGL-PAX-3AMACRO-29"></a>
<a id="DC-ECLECTIC:TREE-PUT%20MGL-PAX:MACRO"></a>

- [macro] **TREE-PUT** *VALUE TREE &REST PATH*

    Set `VALUE` at the location specified by `PATH` in the `TREE` structure.
    Expands into a series of [`getf`][104a] and [`nth`][1aa3] calls for efficient access.

<a id="x-28DC-ECLECTIC-3ATRIM-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:TRIM%20FUNCTION"></a>

- [function] **TRIM** *S &OPTIONAL (FAT "^\\\\s+|\\\\s+$")*

    Trim `FAT` from the string in `S`.  The `FAT` parameter is optional and
    defaults to "^\s+|\s+$", which means "Whitespace at the beginning
    or end of the string".

<a id="x-28DC-ECLECTIC-3ATRIM-WHITESPACE-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:TRIM-WHITESPACE%20FUNCTION"></a>

- [function] **TRIM-WHITESPACE** *S*

    Trim Whitespace from the beginning and end of `S`.

<a id="x-28DC-ECLECTIC-3AUUID-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:UUID%20FUNCTION"></a>

- [function] **UUID** *&OPTIONAL RSTATE*

    Returns a random `UUID` string.

<a id="x-28DC-ECLECTIC-3AVERIFY-STRING-20FUNCTION-29"></a>
<a id="DC-ECLECTIC:VERIFY-STRING%20FUNCTION"></a>

- [function] **VERIFY-STRING** *STRING REGEX &KEY IGNORE-CASE*

    Return t if `STRING` matches the `REGEX` exactly.  Use the `IGNORE-CASE`
    parameter if you want case-insensitve matches.

<a id="x-28DC-ECLECTIC-3A-40VARIABLES-20MGL-PAX-3ASECTION-29"></a>
<a id="DC-ECLECTIC:@VARIABLES%20MGL-PAX:SECTION"></a>

## 4 Special Variables

Exported special variables.

<a id="x-28DC-ECLECTIC-3A-2AALPHABET-ALPHANUM-2A-20VARIABLE-29"></a>
<a id="DC-ECLECTIC:*ALPHABET-ALPHANUM*%20VARIABLE"></a>

- [variable] **\*ALPHABET-ALPHANUM\*** *"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"*

    Alphabet with all ASCII letters and numbers.

<a id="x-28DC-ECLECTIC-3A-2AALPHABET-BITCOIN-2A-20VARIABLE-29"></a>
<a id="DC-ECLECTIC:*ALPHABET-BITCOIN*%20VARIABLE"></a>

- [variable] **\*ALPHABET-BITCOIN\*** *"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"*

    Bitcoin alphabet. No zero, no capital 'o' or 'i', and now lowercase 'l'.

<a id="x-28DC-ECLECTIC-3A-2AALPHABET-ALPHANUM-UPPER-2A-20VARIABLE-29"></a>
<a id="DC-ECLECTIC:*ALPHABET-ALPHANUM-UPPER*%20VARIABLE"></a>

- [variable] **\*ALPHABET-ALPHANUM-UPPER\*** *"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"*

    Alphabet with uppercase letters and numbers.

  [0c8e]: #DC-ECLECTIC:SHELL-COMMAND-BACKGROUND%20FUNCTION "DC-ECLECTIC:SHELL-COMMAND-BACKGROUND FUNCTION"
  [0f15]: #DC-ECLECTIC:SHELL-COMMAND-WAIT%20FUNCTION "DC-ECLECTIC:SHELL-COMMAND-WAIT FUNCTION"
  [104a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_getf.htm "GETF (MGL-PAX:CLHS FUNCTION)"
  [1219]: #DC-ECLECTIC:@OVERVIEW%20MGL-PAX:SECTION "Overview"
  [1aa3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_nth.htm "NTH (MGL-PAX:CLHS FUNCTION)"
  [1c3b]: #DC-ECLECTIC:@FUNCTIONS%20MGL-PAX:SECTION "Functions and Macros"
  [1f1d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_random.htm "RANDOM (MGL-PAX:CLHS FUNCTION)"
  [20cd]: #DC-ECLECTIC:FREEZE%20FUNCTION "DC-ECLECTIC:FREEZE FUNCTION"
  [3763]: #DC-ECLECTIC:SAFE-ENCODE%20FUNCTION "DC-ECLECTIC:SAFE-ENCODE FUNCTION"
  [425d]: http://www.lispworks.com/documentation/HyperSpec/Body/m_and.htm "AND (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [4316]: #DC-ECLECTIC:SAFE-DECODE%20FUNCTION "DC-ECLECTIC:SAFE-DECODE FUNCTION"
  [4836]: #DC-ECLECTIC:DISTINCT-ELEMENTS%20FUNCTION "DC-ECLECTIC:DISTINCT-ELEMENTS FUNCTION"
  [a260]: #DC-ECLECTIC:@INSTALLATION%20MGL-PAX:SECTION "Installation"
  [beee]: #DC-ECLECTIC:SHELL-COMMAND-RUNNING-P%20FUNCTION "DC-ECLECTIC:SHELL-COMMAND-RUNNING-P FUNCTION"
  [dd55]: http://www.lispworks.com/documentation/HyperSpec/Body/t_and.htm "AND (MGL-PAX:CLHS TYPE)"
  [e556]: #DC-ECLECTIC:DEFINE-BASE-ENCODER%20MGL-PAX:MACRO "DC-ECLECTIC:DEFINE-BASE-ENCODER MGL-PAX:MACRO"
  [ee87]: #DC-ECLECTIC:@VARIABLES%20MGL-PAX:SECTION "Special Variables"

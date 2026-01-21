(in-package :dc-eclectic)

(defun to-ascii (string &key
                          (replacement-char #\?)
                          (printable-only t))
  "[public] In STRING, replaces non-ASCII characters with REPLACEMENT_CHAR,
which defaults to the question mark. If PRINTABLE-ONLY is true, only printable
ASCII characters are kept, with the rest being replaced by REPLACEMENT-CHAR."
  (let* ((mincode (if printable-only (code-char 32) (code-char 0)))
         (maxcode (if printable-only (code-char 126) (code-char 255)))
         (replacement-function (lambda (c)
                                 (if (or (char< c mincode) (char> c maxcode))
                                     replacement-char
                                     c))))
    (map 'string replacement-function string)))

(defun flatten (l)
  "[public] Given a nested list L, return a flat list. If an array or other
sequence is among the elements of L, the sequence is not flattened,
but treated as a single element."
  (cond
    ((null l) nil)
    ((atom l) (list l))
    (t (loop for i in l append (flatten i)))))

(defun verify-string (string regex &key ignore-case)
  "[public] Return t if STRING matches the REGEX exactly.  Use the IGNORE-CASE
parameter if you want case-insensitve matches."
  (let ((s (format nil "~a" string)))
    (multiple-value-bind (a b)
        (re:scan (if ignore-case (concatenate 'string "(?is)" regex) regex) s)
      (and a b (zerop a) (= b (length s))))))

;;
;; BEGIN File and directory utilities
;;

(defun join-paths (&rest path-parts)
  "[public] Joins parameters (collected in PATH-PARTS) into a unix-like file
path, inserting slashes where necessary. PATH-PARTS can be strings or
pathnames. If the first element in PATH-PARTS starts with a slash, the resulting
path will be absolute; otherwise, it will be relative.  PATH-PARTS elements
should be strings or pathnames, but this function will try to convert other
types to strings, if possible. NIL or empty strings are ignored."
  (when (null path-parts) (return-from join-paths ""))
  (let* ((parts (loop for part in path-parts
                      for part-string = (when part (format nil "~a" part))
                      unless (or (null part-string) (zerop (length part-string)))
                        collect part-string))
         (absolute (verify-string (car parts) "^/.*$"))
         (clean-parts (remove-if
                       (lambda (p) (zerop (length p)))
                       (mapcar
                        (lambda (p) (re:regex-replace-all "^/|/$" p ""))
                        parts)))
         (path (format nil "~{~a~^/~}" clean-parts)))
    (format nil "~a~a" (if absolute "/" "") path)))

(defun path-only (filename)
  "[public] Retrieves the path (path only, without the filename) of FILENAME.
FILENAME should be a string, a pathname, or NIL. If FILENAME is NIL or the empty
string, this function returns the empty string. If FILENAME has no path
component, this function returns \"/\"."
  (declare (type (or pathname string null) filename))
  (if (or (not filename) (zerop (length (format nil "~a" filename))))
    ""
    (loop
      with s = (format nil "~a" filename)
      with parts = (re:split "/" s)
      with directory = (unless (zerop (length parts)) (re:scan "/$" s))
      and absolute = (re:scan "^/" s)
      for part in (butlast parts)
      when (and part (not (zerop (length part))))
      collect part into new-parts
      finally
      (return
        (format nil "~a~{~a/~}~a"
          (if absolute "/" "")
          new-parts
          (if directory
            (format nil "~a/" (car (last parts)))
            ""))))))

(defun filename-only (filename)
  "[public] Retrieves the filename (filename only, without the path) of
FILENAME."
  (if (null filename)
      ""
    (let ((file (cond
                  ((stringp filename) filename)
                  ((pathnamep filename) (namestring filename))
                  (t (error "FILENAME must be a string or pathname.")))))
      (multiple-value-bind (match parts)
        (re:scan-to-strings "((.*)/)?([^\/]*)$" file)
        (declare (ignore match))
        (if (zerop (length parts))
          ""
          (elt parts (1- (length parts))))))))

(defun leaf-directory-only (path)
  "[public] Returns the last part of the directory PATH. For example,
/home/one/two => two"
  (car (last (re:split "/" (string-trim "/" path)))))

;; Needs tests
(defun root-path (files)
  "[public] Given FILES, a list of paths in the form of strings, returns the
starting path that all the paths have in common."
  (when files
    (loop
      with paths = (mapcar (lambda (d) (re:split "/" (string-trim "/" d))) files)
      for part-index from 0 below (length (car paths))
      for current-part = (elt (car paths) part-index)
      for common = (loop
                     for current-path in (cdr paths) always
                     (and
                       (< part-index (length current-path) )
                       (equal current-part (elt current-path part-index))))
      when common collect current-part into current-parts
      finally (return (format nil "/~{~a/~}" current-parts)))))

;; Needs tests
(defun file-exists-p (path)
  "[public] Returns a boolean value indicating if the file specified by PATH
exists."
  (let ((path (probe-file path)))
    (and path
         (not (equal (file-namestring path) "")))))

;; Needs tests
(defun directory-exists-p (path)
  "[public] Returns a boolean value indicating if the directory specified by
PATH exists."
  (let ((path (probe-file path)))
    (and path
         (not (equal (directory-namestring path) ""))
         (equal (file-namestring path) ""))))

;; Needs tests
(defun path-type (path)
  "[public] Returns :FILE, :DIRECTORY, or :NOT-FOUND, depending on what PATH
 points to."
  (cond ((file-exists-p path) :file)
        ((directory-exists-p path) :directory)
        (t :not-found)))

;; Needs tests
(defun file-extension (path)
  "[public] Returns a string consisting of the file extension for the file name
given in PATH."
  (multiple-value-bind (a b)
      (re:scan-to-strings "\\.([a-z0-9]+)$" path)
    (when a (aref b 0))))

;; Needs tests
(defun replace-extension (filename new-extension)
  "[public] Replaces the file extension in FILENAME with the file extension
provided in NEW-EXTENSION."
  (let* ((new-extension (if (re:scan "^\\." new-extension)
                            (subseq new-extension 1)
                            new-extension))
         (new-filename (multiple-value-bind (a b)
                           (re:scan-to-strings "^(.*)\\.[^.]+$" filename)
                         (declare (ignore a))
                         (if b (elt b 0) filename))))
    (when (and new-filename (not (zerop (length new-extension))))
      (setf new-filename (format nil "~a.~a" new-filename new-extension)))
    new-filename))

;;
;; END File and directory utilities
;;

;; Needs tests
(defgeneric index-of-max (list-or-vector)
  (:method ((vector vector))
    (if (zerop (length vector))
        nil
        (loop
          with index-of-max = 0 and max-value = (aref vector 0)
          for value across vector
          for index = 0 then (1+ index)
          when (> value max-value)
            do (setf index-of-max index
                     max-value value)
          finally (return index-of-max))))
  (:method ((list list))
    (when list
      (loop
        with index-of-max = 0 and max-value = (car list)
        for value in (cdr list)
        for index = 1 then (1+ index)
        when (> value max-value)
          do (setf index-of-max index
                   max-value value)
        finally (return index-of-max))))
  (:documentation "[public] Returns the index of the largest number in
LIST-OR-VECTOR."))

(defun hash-string (string &key (salt "") (size 128))
  "[public] Hash STRING and return a hex representation of the hash"
  (subseq
    (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence
        'ironclad:sha512
        (string-to-utf-8-bytes (concatenate 'string salt string))))
    0
    size))

(defun hash-hmac-256 (secret text)
  "[public] Hash TEXT using SECRET and hmac-sha-256 and return a hex
representation of the hash"
  (let ((hmac (ironclad:make-hmac
               (ironclad:ascii-string-to-byte-array secret) :sha256)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array text))
    (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))

(defun distinct-elements (sequence &key (key #'identity))
  "[public] Accepts a sequence of elements (list or vector) and returns a new
sequence of the same type with distinct elements from the original. If the
elements in the sequence are hash tables, plists, or objects with methods, then
you can provide a value or function for the :key parameter.  If you provide a
value, the function will use the value as the key of the element, and the value
of the key will represent the unique signature of the element.  If you provide a
function, then the function will be applied to the element to compute the
elements unique signature."
  (let* ((list (if (vectorp sequence)
                   (map 'list 'identity sequence)
                   sequence))
         (first (car list))
         (f-key (if (functionp key)
                    key
                    (cond ((hash-table-p first)
                           (lambda (x) (gethash key x)))
                          ((plistp first)
                           (lambda (x) (getf x key)))
                          (t (lambda (x) (funcall key x))))))
         (f-value (lambda (e k v)
                    (declare (ignore k v)) e))
         (distinct (hash-values
                    (hashify-list list :method :custom
                                       :f-key f-key
                                       :f-value f-value))))
    (cond ((stringp sequence) (map 'string 'identity distinct))
          ((vectorp sequence) (map 'vector 'identity distinct))
          (t distinct))))

(defun distinct-values (list)
  "[public] Synonym for DISTINCT-ELEMENTS."
  (distinct-elements list))

;; Needs tests
(defun distinct-strings (list)
  "[public] Alternative for returning a list of distinct strings."
  (when list
    (loop with sorted = (sort list #'string<)
          with last-item = nil
          with result = nil
          for item in sorted
          when (not (string= item last-item))
            do (push item result)
               (setf last-item item)
          finally
             (return (reverse result)))))

(defun hash-values (hash)
  "[public] Returns a list of the values in hash table HASH."
  (loop for v being the hash-values in hash collect v))

(defun hash-keys (hash)
  "[public] Returns a list of the keys in hash table HASH."
  (loop for k being the hash-keys in hash collect k))

(defun plist-keys (plist)
  "[public] Returns list of the keys in PLIST."
  (loop for key in plist by #'cddr collect key))

(defun range (start end &key (step 1) (filter #'identity) shuffle)
  "[public] Returns a list of values between START and END (inclusive), skipping
values by STEP, filtering remaining values with the function in FILTER, and
shuffling the remaining values if SHUFFLE is true.  STEP defaults to 1, FILTER
defaults to allowing all values through, and SHUFFLE default to nil."
  (let ((range (loop for a from start to end by step
                     when (funcall filter a) collect a)))
    (if shuffle (shuffle range) range)))

(defun rand (value &optional rstate)
  "[public] When called without RSTATE, this is the same as calling RANDOM with
only the VALUE parameter. Otherwise, this calls RANDOM with VALUE and RSTATE."
  (if rstate
    (random value rstate)
    (random value)))

(defun shuffle (seq &optional rstate)
  "[public] Return a sequence with the same elements as the given sequence S,
but in random order (shuffled)."
  (loop
    with l = (length seq)
    with w = (make-array l :initial-contents seq)
    for i from 0 below l
    for r = (rand l rstate)
    for h = (aref w i)
    do
       (setf (aref w i) (aref w r))
       (setf (aref w r) h)
    finally (return (if (listp seq) (map 'list 'identity w) w))))

;; Needs test
(defun choose-one (seq &optional rstate)
  "[public] Returns a random element from SEQ."
  (when seq
    (elt seq (rand (length seq) rstate))))

(defun choose-some (seq n &optional rstate)
  "[public] Choose N elements from SEQ and return a new sequence with those
elements, with the new sequence having the same type as SEQ.

N must be an integer that is greater than or equal to 1.

If N is greater than the length of SEQ, this function returns a copy
of SEQ.

If N is less than 1, then this function returns nil, even if the
sequence is not a list.

This function uses the RANDOM function, which accepts an optional
RSTATE parameter that you can optionally pass in here."
  (declare (type integer n))
  (cond ((< n 1) nil)
        ((>= n (length seq)) (copy-seq seq))
        (t (loop
             with h = (make-hash-table :test 'equal)
             and l = (length seq)
             for a from 1 to n
             for b = (loop for c = (rand l rstate)
                           while (gethash c h)
                           finally (setf (gethash c h) t)
                                   (return c))
             collect (elt seq b) into result
             finally (return (if (vectorp seq)
                                 (map 'vector 'identity result)
                                 result))))))

(defun hashify-list (list
                     &key (method :count)
                       f-key
                       hash-key
                       plist-key
                       alist-key
                       (f-value (lambda (key-raw key-clean value)
                                  (declare (ignore key-raw key-clean))
                                  value))
                       (initial-value 0))

  "[public] Creates a hash table from LIST and returns the hash table, according
to METHOD. Supported methods are :COUNT, :PLIST, :ALIST, :INDEX, AND :CUSTOM.

:COUNT

    With the :COUNT method, which the function uses by default, the function
    creates a hash table in which each key is an item of the list and the
    associated value for each key is the incidence of the item in the list. For
    example:

    (hashify-list '(7 8 7 7 8 9))

    gives you a hash table that looks like this:

    {7: 3, 8: 2, 9: 1}

:ALIST and :PLIST

    The :ALIST and :PLIST methods convert the list into a hash that conceptually
    represent the same map as the list. Alists and plists both consist of
    collections of key/value pairs. Alists look like this:

    '((key1 . value1) (key2 . value2) (key3 . value3)...)

    Plists look like this:

    '(:key1 value1 :key2 value2 :key3 value3 ...)

    If a key repeats in one of these lists, its value simply overwrites the
    value of the repeated key. However, you can change that behavior. See the
    description of the :CUSTOM method for information on how to do that.

:INDEX

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
      {id: \"a-001\", first: \"john\", last: \"doe\"},
      {id: \"a-002\", first: \"jane\", last: \"doe\"}
    ]

    If you specify :method :index :hash-key \"id\", this function will
    create a hash table that looks like this:

    {
      \"a-001\": {id: \"a-001\", first: \"john\", last: \"doe\"},
      \"a-002\": {id: \"a-002\", first: \"jane\", last: \"doe\"}
    }

    And, voilá, you no longer need to iterate through a list to find your
    object.

    If the objects are plists, and you specify the index with plist-key, you'll
    see the same behavior with the plist as we demonstrated above for hash
    tables.

    HASH-KEY and PLIST-KEY are just shortcuts to save you from having to write
    some code for F-KEY. You can specify only one of HASH-KEY, PLIST-KEY, and
    F-KEY.

:CUSTOM

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
    specified by :INITIAL-VALUE is used."
  (let ((h (make-hash-table :test 'equal))
        (counter 0))
    (case method
      (:count (loop
                with h-index
                  = (cond
                      (hash-key (hashify-list
                                 list
                                 :method :index
                                 :hash-key hash-key))
                      (plist-key (hashify-list
                                  list
                                  :method :index
                                  :plist-key plist-key))
                      (alist-key (hashify-list
                                  list
                                  :method :index
                                  :alist-key alist-key))
                      (t nil))
                and h1 = (make-hash-table :test 'equal)
                and key-function
                      = (cond
                          (hash-key (lambda (x) (gethash hash-key x)))
                          (plist-key (lambda (x) (getf x plist-key)))
                          (alist-key (lambda (x) (cdr (assoc alist-key x))))
                          (t (if f-key f-key #'identity)))
                for k-raw in list
                for k-clean = (funcall key-function k-raw)
                do (incf (gethash k-clean h1 0))
                finally (loop for k-old being the hash-keys in h1
                                using (hash-value v)
                              for k-new = (if h-index
                                              (gethash k-old h-index)
                                              k-old)
                              do (setf (gethash k-new h) v))))
      (:custom (loop
                 with key-function = (or f-key #'identity)
                 for k-raw in list
                 for k-clean = (funcall key-function k-raw)
                 for value-old = (gethash k-clean h initial-value)
                 for value-new = (funcall f-value k-raw k-clean value-old)
                 do (setf (gethash k-clean h) value-new)))
      (:plist (loop
                with key-function = (or f-key #'identity)
                for (k-raw value) on list by #'cddr
                for k-clean = (funcall key-function k-raw)
                for value-new = (funcall f-value k-raw k-clean value)
                do (setf (gethash k-clean h) value-new)))
      (:alist (loop
                with key-function = (or f-key #'identity)
                for (k-raw value) in list
                for k-clean = (funcall key-function k-raw)
                for value-new = (funcall f-value k-raw k-clean value)
                do (setf (gethash k-clean h) value-new)))
      (:lists (loop
                with key-function = (or f-key #'identity)
                for value in list
                for key = (funcall f-key value)
                do (setf (gethash key h) value)))
      (:index (loop
                with key-function
                  = (cond (hash-key (lambda (x) (gethash hash-key x)))
                          (plist-key (lambda (x) (getf x plist-key)))
                          (alist-key (lambda (x) (cdr (assoc alist-key x))))
                          (f-key f-key)
                          (t #'identity))
                with value-function
                  = (if (equal key-function #'identity)
                        (lambda (k-raw k-clean value)
                          (declare (ignore k-raw k-clean value))
                          (1- (incf counter)))
                        (lambda (k-raw k-clean value)
                          (declare (ignore k-raw k-clean))
                          value))
                for value in list
                for k-raw = (funcall key-function value)
                for k-clean = k-raw
                for value-new = (funcall value-function k-raw k-clean value)
                do (setf (gethash k-clean h) value-new))))
    h))

(defun comparable-hash-dump (hash &key
                                    (f-sort #'string<)
                                    (f-make-sortable
                                     (lambda (k) (format nil "~a" k)))
                                    flat)
  "[public] Turns the given hash table into a list of pairs or, if FLAT is T,
 into a plist that represents the hash. In the resulting list, the keys are
 sorted, so that the list can be more easily compared with other lists. HASH is
 the hash you want to dump. F-SORT is the sort predicate, which defaults to
 sorting strings in ascending order.  F-MAKE-SORTABLE is a function that accepts
 a key and returns a sortable version of the key. This defaults to turning the
 key into a string. If the keys to the hash are integers, for example, you can
 provide an F-SORT of #'<, which sorts integers numerically in ascending order,
 and you can provide an F-MAKE-SORTABLE of #'identity, which will leave the keys
 as integers for sorting purposes."
  (loop for k being the hash-keys in hash
          using (hash-value v)
        for k-sortable = (funcall f-make-sortable k)
        collect (list k-sortable k v) into pairs
        finally (return
                  (let ((result (mapcar #'cdr (sort pairs f-sort :key #'car))))
                    (if flat (flatten result) result)))))

(defun all-permutations-base (list)
  "[private] Internal helper function for ALL-PERMUTATIONS."
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop
             for item in list
             appending (mapcar (lambda (sublist) (cons item sublist))
                               (all-permutations-base (remove item list)))))))

(defun all-permutations (list)
  "[public] Returns a list of every permutation of elements in LIST. For
 example:
    '(1 2 3) -> '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))"
  (let ((v (apply #'vector list)))
    (distinct-elements
     (mapcar (lambda (l) (mapcar (lambda (i) (elt v i)) l))
             (dc-eclectic::all-permutations-base
              (dc-eclectic::range 0 (1- (length v))))))))

(defun all-permutations-of-string (s)
  "[public] Returns a list of strings representing every permutation of the
orginal string S. For example:
    \"abc\" -> '(\"abc\" \"acb\" \"bac\" \"bca\" \"cab\" \"cba\")"
  (mapcar (lambda (list) (map 'string 'identity list))
          (all-permutations (map 'list 'identity s))))

(defun existing-permutations-of-string (s hash)
  "[public] Works just like all-permutations-of-string, but excludes any
 permutations of S that are not among the keys in HASH."
  (loop for word in (all-permutations-of-string s)
        when (gethash word hash)
          collect word))

(defun n-grams-of-list (list count &optional prefix)
  "[private] Returns n-grams of LIST each of length COUNT, optionally prefixed
with PREFIX. This is helper function for N-GRAMS and N-GRAM-STRINGS."
  (if (zerop count)
      (apply #'vector (reverse prefix))
      (loop for item in list
            collect (n-grams-of-list
                     list
                     (1- count)
                     (cons item prefix))
              into result
            finally (return (flatten result)))))

(defun n-grams (list count)
  "[public] Returns n-gram lists of length COUNT using elements of LIST."
  (mapcar (lambda (v) (map 'list 'identity v))
          (n-grams-of-list list count)))

(defun n-gram-strings (chars count)
  "[public] Accepts CHARS, a string, and COUNT, an integer, and returns all the
  possible combinations of length COUNT of the characters in CHARS. For example,
  (n-gram-strings \"abc\" 2) => '(\"aa\" \"ab\" \"ac\" \"ba\" \"bb\"
  \"bc\" \"ca\" \"cb\" \"cc\")"
  (mapcar (lambda (v) (map 'string 'identity v))
          (n-grams-of-list (map 'list 'identity chars)
                           count)))

(defun existing-n-gram-strings (chars count hash)
  "[public] Returns all the COUNT n-grams for string CHARS, but only of n-grams
that exist as keys in HASH."
  (remove-if-not (lambda (word)
                   (gethash word hash))
                 (n-gram-strings chars count)))

(defun split-n-trim (string &key (on-regex "\\s+") (fat "^\\s+|\\s+$"))
  "[public] Splits STRING into substrings on ON-REGEX, then trims FAT from each
substring.  The ON-REGEX parameter value, which is optional, defaults to
\"\\s+\", which is to say that the string is split into a list of words at the
whitespace boundaries.  The default value for FAT, which is also optional,
\"\\s+|\\s+$\", causes this function to trim whitespace from the beggining and
end of each substring.  Here's an example:

    (split-n-trim \"Hello  beautiful      world!\")

    => '(\"Hello\" \"beautiful\" \"world!\")"
  (remove-if (lambda (s) (zerop (length s)))
             (mapcar (lambda (x) (trim x fat))
                     (re:split on-regex string))))

(defun trim (s &optional (fat "^\\s+|\\s+$"))
  "[public] Trim FAT from the string in S.  The FAT parameter is optional and
defaults to \"^\\s+|\\s+$\", which means \"Whitespace at the beginning
or end of the string\"."
  (re:regex-replace-all fat s ""))

(defun trim-whitespace (s)
  "[public] Trim Whitespace from the beginning and end of S."
	(string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return
								 #\Rubout)
							 s))

(defun plistp (list)
  "[public] Returns T if LIST is a plist."
  (and (evenp (length list))
       (loop for key in list by #'cddr always (keywordp key))))

;; Needs tests!
(defun normalize-list (list &key max min)
  "[public] Returns a new list with new values between 0.0 and 1.0. MAX is the
largest value that LIST can hold, and MIN is the smallest.  Each new value N is
computed from the corresponding old value O in LIST, as follows:

  N = (O - MIN) / (MAX - MIN)

If you don't provide MAX and MIN, this function does an initial pass through
list where it sets MAX and MIN to the largest number and the smallest number in
LIST, respectively.  Therefore, you can improve the performance of this function
if you already know those values. Furthermore, in some cases the list may not
even contain the values for MAX and MIN that you need."
  (loop
    with min-max = (if (and min max)
                       (cons min max)
                       (loop for n in list
                             maximize n into mini
                             minimize n into maxi
                             finally (return (cons mini maxi))))
    with range = (float (- max min))
    for n in list collect (/ (float (- n min)) range)))

;; Needs tests!
(defun denormalize-list (list min max &key integer)
  "[public] Returns a new list with the numbers in LIST, which are
floating-point numbers between 0 and 1, expanded to the range MAX - MIN, such
that the number 1.0 is converted to MAX, the number 0.0 is converted to MIN, and
all the other numbers fall in the range MIN to MAX. INTEGER is T, the new list
contains integers. Otherwise, the new list contains floating-point numbers."
  (if integer
      (loop with range = (- max min)
            for o in list
            collect (truncate (+ (* o range) min)))
      (loop with range = (- max min)
            for o in list
            collect (+ (* o range) min))))

(defun slurp (filename)
  "[public] Returns a string with the content of the file at FILENAME."
  (with-open-file (in filename :direction :input)
    (let ((contents (make-string (file-length in))))
      (read-sequence contents in)
      contents)))

(defun spew (string filename)
  "[public] Write STRING to filename, creating the file if necessary, and
replacing the contents of the file if the file already exists."
  (with-open-file (out filename
                    :direction :output
                    :if-exists :supersede)
    (write-string string out)))

(defun freeze (object)
  "[public] Stringifies OBJECT."
  (prin1-to-string object))

(defun thaw (string)
  "[public] Returns an object resulting from the evalation of STRING. This
is the opposite of FREEZE."
  (let ((*read-eval* nil))
    (read-from-string string)))

(defun getenv (name &key default required (type :string))
  "[public] Get the value of the environment variable NAME, returning a string
or an integer, depending on TYPE. If the environment variable is not set, then
return DEFAULT, which must be of type TYPE. If the environment variable is not
set and DEFAULT is NIL, then this function returns NIL.

TYPE can be :integer, :string, or :boolean."
  (unless (member type '(:string :integer :boolean))
    (error "Invalid type ~a. Use :integer, :string, or :boolean." type))
  (let ((value (trim (sb-ext:posix-getenv name))))
    (if value
      (cond
        ((eql type :string)
          value)
        ((eql type :integer)
          (handler-case (parse-integer value)
            (error (condition)
              (perror :status "failed to parse environment var as integer"
                :value value :condition condition)
              nil)))
        ((and (eql type :boolean))
          (cond
            ((equal (string-downcase value) "true") t)
            ((equal (string-downcase value) "false") nil)
            ((null value) nil)
            (t (perror :status "invalid value for type :boolean"
                 :value value)))))
      (cond
        ((and required (null default))
          (error "A value for environment variable ~a is required." name))
        ((and (member default '(t nil)) (eql type :boolean))
          default)
        ((null default)
          default)
        ((and (stringp default) (eql type :string))
          default)
        ((and (stringp default) (eql type :integer))
          (handler-case (parse-integer default)
            (error (condition)
              (perror :status "failed to parse default value as integer"
                :value value :condition condition)
              nil)))
        ((integerp default)
          default)))))

(defun setenv (name value)
  "[public] Set environment variable NAME to VALUE. VALUE is always converted into
a string. Returns a string with VALUE."
  (let ((string-value (cond
                        ((eq value t) "true")
                        ((eq value nil) "false")
                        (t (format nil "~a" value)))))
    (sb-posix:setenv name string-value 1)
    string-value))

;; Everything that follows neeeds tests

(defun random-number (&optional (digits 4) rstate)
  "[public] Returns a random integer of DIGITS digits."
  (loop for a from 1 to digits
    for digit = (1+ (rand 9 rstate)) then (rand 10 rstate)
    for power downfrom (1- digits) to 0
    summing (* digit (expt 10 power))))

(defun random-hex-number (&optional (digits 7) (non-zero-start) rstate)
  "[public] Returns a random hexadecimal number with DIGITS digits. If
NON-ZERO-START is specified, then the resulting hexadecimal number starts with
a character other than 0."
  (loop with hex-digits = "0123456789abcdef"
    for a from 1 to digits
    for digit = (elt hex-digits (if non-zero-start
                                  (1+ (rand 15 rstate))
                                  (rand 16 rstate)))
    then (elt hex-digits (rand 16 rstate))
    collect digit into number
    finally (return (map 'string 'identity number))))

(defun random-string (string-length alphabet &optional rstate)
  "[public] Returns a random string of length STRING-LENGTH. The string is
constructed from characters in ALPHABET. ALPHABET is a string. There are
several functions available for building ALPHABET, all starting with the
prefix 'ASCII-'. For example:

  (random-string 10 (ascii-alpha-num-lower))"
  (loop with alphabet-length = (length alphabet)
    for a from 1 to string-length
    for letter = (elt alphabet (rand alphabet-length rstate))
    collect letter into string
    finally (return (map 'string 'identity string))))

(defun ascii-char-range (begin end)
  "[public] Returns a string that includes ASCII characters in the ASCII code
range given by BEGIN and END."
  (loop for code from (char-code begin) to (char-code end)
    collect (code-char code) into string
    finally (return (map 'string 'identity string))))

(defun ascii-alpha-lower ()
  "[public] Returns a string with all the lower-case alphabetic ASCII
characters (a-z)."
  (ascii-char-range #\a #\z))

(defun ascii-alpha-upper ()
  "[public] Returns a string with all the upper-case alphabetic ASCII
characters (A-Z)."
  (ascii-char-range #\A #\Z))

(defun ascii-alpha ()
  "[public] Return a string with all the alphabetic ASCII characters."
  (concatenate 'string (ascii-alpha-lower) (ascii-alpha-upper)))

(defun ascii-numeric ()
  "[public] Returns a string with all the ASCII characters that represent
digits."
  (ascii-char-range #\0 #\9))

(defun ascii-alpha-num-lower ()
  "[public] Returns a string with all the lower-case alpha-numeric ASCII
characters."
  (concatenate 'string (ascii-alpha-lower) (ascii-numeric)))

(defun ascii-alpha-num-upper ()
  "[public] Returns a string with all the upper-case alpha-numeric ASCII
characters."
  (concatenate 'string (ascii-alpha-upper) (ascii-numeric)))

(defun ascii-alpha-num ()
  "[public] Returns a string with all the alpha-numeric ASCII characters."
  (concatenate 'string (ascii-alpha) (ascii-numeric)))

(defun uuid (&optional rstate)
  "[public] Returns a random UUID string."
  (format nil "~{~a~^-~}"
    (list
      (random-hex-number 8 t rstate)
      (random-hex-number 4 nil rstate)
      (random-hex-number 4 nil rstate)
      (random-hex-number 4 nil rstate)
      (random-hex-number 12 nil rstate))))

;; There's much better encoding code in encoder.lisp, where you can create
;; encoders with any alphabet, and where multibyte characters are supported.
;; However, that encoding code is not compatible with the industry-standard
;; base64-encode. Therefore, these functions remain here.
;;
;; BEGIN
(defun base64-encode (string)
  "[public] Returns a base64-encoded version of STRING that that works list like
the standard base64-encode implementations in the Linux command line and Emacs
Lisp. These do not properly handle multibyte characters, so use this function
only if you need compatibility with popular implementations. DC-ECLECTIC
contains better encoding and decoding functions. See SAFE-ENCODE, SAFE-DECODE,
and DEFINE-BASE-ENCODER."
  (if string
    (cl-base64:string-to-base64-string string)
    ""))

(defun base64-decode (base64-encoded-string)
  "[public] Decodes BASE64-ENCODED-STRING and returns the result. This function
is compatible with base64-encode and with popular implementation of base64
encoding and decoding functions. However, this function doesn't properly handle
multi-byte characters. Unless you require compatibility with external functions,
use the other decoding functions in DC-ECLECTIC. See SAFE-ENCODE, SAFE-DECODE,
and DEFINE-BASE-ENCODER."
  (if base64-encoded-string
    (cl-base64:base64-string-to-string base64-encoded-string)
    ""))
;; END

(defun copy-file (source destination &key
                   (if-exists :supersede)
                   (buffer-size (* 64 1024)))
  "[public] Copies SOURCE file to DESTINATION file. If DESTINATION's directories
do not exist, they are created. IF-EXISTS controls the behavior if DESTINATION
already exists, and may be :error, :new-version, :rename, :rename-and-delete,
:overwrite, :append, or :supersede. IF-EXISTS defaults to :supersede.
BUFFER-SIZE controls the size of the buffer used during the copy operation, and
defaults to 64 KB. Returns the DESTINATION path."
  (when (stringp source)
    (setf source (pathname source)))
  (when (stringp destination)
    (setf destination (pathname destination)))
  (ensure-directories-exist destination)
  (with-open-file (in source
                      :direction :input
                      :element-type '(unsigned-byte 8)
                      :if-does-not-exist :error)
    (with-open-file (out destination
                        :direction :output
                        :element-type '(unsigned-byte 8)
                        :if-exists if-exists)
      (let ((buf (make-array buffer-size :element-type '(unsigned-byte 8))))
        (loop
          (let ((pos (read-sequence buf in)))
            (when (zerop pos)
              (return))
            (write-sequence buf out :end pos))))))
  (let ((mtime (file-write-date source)))
    (when mtime
      (ignore-errors
        (sb-posix:utimes (namestring destination) mtime mtime))))
  destination)

(defun shell-command-to-string (command)
  "[public] Executes COMMAND in the shell and returns the output as a string."
  (multiple-value-bind (output error-output exit-code)
    (uiop:run-program command
      :output :string
      :error-output :string)
    (values (trim output) error-output exit-code)))

;; Background a shell command and support functions
;; BEGIN

(defun shell-command-background (command &key (wait-interval 0.1))
  "[public] Run COMMAND in the background. Returns a process-info plist with
keys: :pid, :exit-code (nil if running), :output, :error-output, :running-p,
:wait-interval (for polling), :status.

Returns an INFO object that you can use later to check on the backgrounded
process. Use (SHELL-COMMAND-RUNNING-P INFO), (SHELL-COMMAND-WAIT INFO),
(SHELL-COMMAND-EXIT-CODE INFO) to check status.
"
  (let* ((start-time (get-universal-time))
         (process (uiop:launch-program command
                                       :output :stream
                                       :error-output :stream)))
    (list :pid (uiop:process-info-pid process)
          :process process
          :output nil
          :error-output nil
          :exit-code nil
          :start-time start-time
          :wait-interval wait-interval
          :status :running)))

(defun shell-command-running-p (info)
  "[public] Returns T if the background process is still running. INFO is the
object that SHELL-COMMAND-BACKGROUND returns when it starts the process."
  (uiop:process-alive-p (getf info :process)))

(defun shell-command-wait (info &optional timeout)
  "[public] Wait for process to finish (with optional TIMEOUT seconds). INFO is
the object that SHELL-COMMAND-BACKGROUND returns when Updates INFO with output
and exit code. Returns updated INFO. If TIMEOUT is ommitted or NIL, waits until
the process finishes, which might be forever if the process is a server process,
for example.  If TIMEOUT is provided and the process does not finish within
TIMEOUT seconds, the process is terminated. When the process terminates
normally, INFO is updated with :status :completed, the exit code, and the output
and error-output strings.  When the process is terminated due a timeout, INFO is
updated with :status :terminated, and the exit code, but output and error-output
are left NIL."
  (let ((process (getf info :process))
        (wait-interval (or (getf info :wait-interval) 0.1))
        (end-time (when timeout (+ (get-universal-time) timeout))))

    (loop while (and (uiop:process-alive-p process)
                  (or (not timeout) (< (get-universal-time) end-time)))
          do (sleep wait-interval))

    (when (uiop:process-alive-p process)
      (uiop:terminate-process process))

    (let* ((code (uiop:wait-process process))
            (status (if (>= code 128) :terminated :completed))
            (exit-code (if (equal status :terminated) (- code 128) code))
            (output-stream (uiop:process-info-output process))
            (error-output-stream (uiop:process-info-error-output process))
            (output (when (equal status :completed)
                      (with-open-stream (s output-stream)
                        (loop for line = (read-line s nil nil)
                          while line collect line into lines
                          finally (return (format nil "~{~a~%~}" lines))))))
            (error-output (when (equal status :completed)
                            (with-open-stream (s error-output-stream)
                              (loop for line = (read-line s nil nil)
                                while line collect line into lines
                                finally (return (format nil "~{~a~^~%~}" lines)))))))
      (uiop:close-streams process)
      (setf
        (getf info :output) output
        (getf info :error-output) error-output
        (getf info :exit-code) exit-code
        (getf info :status) status))
      info))

(defun safe-sort (list &key predicate)
  "[public] Returns a sorted copy of LIST, without modifying LIST. If PREDICATE
is not provided, the function determines the type of the first element of the
list to pick a suitable sort predicate. The function assumes that all the
elements in LIST are of the same type and that they are either strings,
keywords, or numbers. The function sorts in ascending order by default.  If
PREDICATE is provided, then the function uses that predicate."
  (let ((p (or predicate
             (cond
               ((stringp (car list)) #'string<)
               ((keywordp (car list)) #'string<)
               ((numberp (car list)) #'<)
               (t (error "Cannot determine sort predicate for LIST"))))))
    (stable-sort (copy-seq list) p)))

;; END
;; Background a shell command and support functions

(defgeneric has (reference-list thing)
  (:documentation "[public] Returns T if REFERENCE-LIST contains THING. If THING
is a string, this function checks for that string in REFERENCE-LIST. If THING is
a list, this function checks that all elements of THING are in REFERENCE-LIST.")
  (:method ((reference-list list) (thing string))
    (when (member thing reference-list :test 'equal) t))
  (:method ((reference-list list) (thing number))
    (when (member thing reference-list :test 'eql) t))
  (:method ((reference-list list) (thing list))
    (when
      (every (lambda (s) (member s reference-list :test 'equal)) thing)
      t)))

(defun has-some (reference-list query-list)
  "[public] Returns T if REFERENCE-LIST contains any of the elements in
QUERY-LIST."
  (if (null query-list)
    t
    (when
      (some (lambda (s) (member s reference-list :test 'equal)) query-list)
      t)))

(defgeneric exclude (reference-list exclude)
  (:documentation "[public] Returns a list containing the elements of
REFERENCE-LIST that are not in EXCLUDE. If EXCLUDE is a list, this function
excludes all elements in EXCLUDE from REFERENCE-LIST. If EXCLUDE is a string,
this function excludes the string from REFERENCE-LIST")
  (:method ((reference-list list) (exclude string))
    "[public] Returns a list containing the elements of REFERENCE-LIST
excluding the one that is equal to EXCLUDE."
    (remove-if (lambda (s) (equal s exclude)) reference-list))
  (:method ((reference-list list) (exclude number))
    "[publlic] Returns a list containing the elements of REFERENCE-LIST
excluding the one that is equal to EXCLUDE."
    (remove-if (lambda (n) (= n exclude)) reference-list))
  (:method ((reference-list list) (exclude list))
    "[public] Returns a list containing the elements of REFERENCE-LIST excluding
all elements in EXCLUDE."
    (remove-if
      (lambda (s) (member s exclude :test 'equal))
      reference-list)))

(defun exclude-regex (reference-list exclude &optional exceptions)
  "[public] Returns a list of the elements of REFERENCE-LIST that that don't
match the EXCLUDE regular expression. However, elements that are not in
EXCEPTIONS are not excluded, even if they match EXCLUDE."
  (remove-if (lambda (s)
               (and
                 exclude
                 (not (member s exceptions :test 'equal))
                 (re:scan exclude s)))
    reference-list))

(defun deep-copy (thing)
  "[public] Deep copy THING, recursively copying lists, vectors, and strings."
  (cond
    ((null thing) nil)
    ((consp thing)
      (cons (deep-copy (car thing)) (deep-copy (cdr thing))))
    ((stringp thing) (copy-seq thing))
    ((vectorp thing) (map 'vector #'deep-copy thing))
    (t thing)))

(defun singular (word)
  "[public] Convert plural to singular form (works most of the time)."
  (loop with pairs = '(("ies" . "y")
                        ("ves" . "f")
                        ("is" . "es")
                        ("s" . "")
                        ("men" . "man")
                        ("children" . "child")
                        ("feet" . "foot")
                        ("teeth" . "tooth")
                        ("mice" . "mouse")
                        ("people" . "person")
                        ("data" . "datum"))
    for pair in pairs
    for regex = (format nil "~a$" (car pair))
    for replacement = (cdr pair)
    when (re:scan regex word)
    do (return-from singular (re:regex-replace regex word replacement))
    finally (return word)))

(defun tree-get (tree &rest path)
  "[public] Get value from the TREE structure, at PATH. TREE is a nested data
structure where each value can be a plist, list, object, t, or nil."
  (loop for key in path
        do (etypecase key
             (integer (progn
                        (assert (listp tree) (tree) "Expected a list.")
                        (setf tree (nth key tree))))
             (symbol (progn
                       (assert (plistp tree) (tree) "Expected a plist.")
                       (setf tree (getf tree key)))))
        finally (return tree)))

(defmacro tree-put (value tree &rest path)
  "[public] Set VALUE at the location specified by PATH in the TREE structure.
Expands into a series of `getf` and `nth` calls for efficient access."
  (let ((place
          (reduce (lambda (current key)
                    (etypecase key
                      (integer `(nth ,key ,current))
                      (symbol `(getf ,current ,key))))
                  path
                  :initial-value tree)))
    `(setf ,place ,value)))

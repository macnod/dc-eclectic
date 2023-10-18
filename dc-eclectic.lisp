(in-package :dc-eclectic)


(defvar *unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0))
(defparameter *debug* nil)

(defun universal-time-to-unix-time (&optional universal-time)
  "Converts universal time to unix time. If you don't provide a universal time,
this function returns the current unix time.

* Unix time is the number of seconds elapsed since the epoch, January
  1, 1970 at 00:00:00 UTC.

* Universal time is the number of seconds elapsed since January 1, 1900 at 00:00:00 UTC
"
  (let ((universal-time (or universal-time (get-universal-time))))
    (- universal-time *unix-epoch-difference*)))


(defun unix-time-to-universal-time (&optional unix-time)
  "Converts unix time to universal time. If you don't provide a unix time,
this function returns the current universal time, as an integer.

* Unix time is the number of seconds elapsed since the epoch, January
1, 1970 at 00:00:00 UTC.

* Universal time is the number of seconds elapsed since January 1,
1900 at 00:00:00 UTC.
"
  (let ((unix-time (or unix-time (universal-time-to-unix-time))))
    (+ unix-time *unix-epoch-difference*)))


(defun get-unix-time ()
  "Returns the current unix time, as an integer. Unix time is the number of
seconds elapsed since the epoch, January 1, 1970 at 00:00:00 UTC."
  (universal-time-to-unix-time (get-universal-time)))


(defun to-ascii (string &key
                          (replacement-char #\?)
                          (printable-only t))
  "In STRING, replaces non-ASCII characters with REPLACEMENT_CHAR, which
defaults to the question mark. If PRINTABLE-ONLY is true, only
printable ASCII characters are kept, with the rest being replaced by
REPLACEMENT-CHAR."
  (let* ((mincode (if printable-only (code-char 32) (code-char 0)))
         (maxcode (if printable-only (code-char 126) (code-char 255)))
         (replacement-function (lambda (c)
                                 (if (or (char< c mincode) (char> c maxcode))
                                     replacement-char
                                     c))))
    (map 'string replacement-function string)))


(defun timestamp-string (&key
                           (universal-time (get-universal-time))
                           (timezone 0)
                           (format "%Y-%M-%DT%h:%m:%s"))
  "Returns the given time (or the current time, in universal time
format) formatted according to the FORMAT parameter, followed by an
optional value for STRING.  If STRING is provided, the function adds a
space to the result and then appends the string to that.  The FORMAT
string can contain any characters.  This function will replace the
format characters Y, M, D, h, m, and s, with numbers representing the
year,month, day, hour, minute, and second, respectively.  All the
numbers are 2 digits long, except for the year, which is 4 digits
long."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time timezone)
    (let* ((parts (ds (list :map
                            "%Y" (format nil "~d"     year)
                            "%M" (format nil "~2,'0d" month)
                            "%D" (format nil "~2,'0d" day)
                            "%h" (format nil "~2,'0d" hour)
                            "%m" (format nil "~2,'0d" minute)
                            "%s" (format nil "~2,'0d" second))))
           (elements (loop for i from 0 below (length format)
                           for v = (subseq format i (+ i 2))
                           for fs = (ds-get parts v)
                           collect (if fs (progn (incf i) fs) (subseq v 0 1)))))
      (format nil "~{~a~}" elements))))

(defun log-entry (&rest messages)
  "Concatenates one or more strings (collected in MESSAGES), precedes
the result with a timestamp, and returns a string that looks like a
log entry."
  (format nil "~a ~{~a~}~%" (timestamp-string) messages))


(defun log-entries (&rest messages)
  "Flattens MESSAGES, then calls LOG-ENTRY with each element of the
resulting list."
  (loop for m in (flatten messages)
        collect (log-entry m)
          into entries
        finally (return (apply #'concatenate 'string entries))))


(defun flatten (l)
  "Given a nested list L, return a flat list. If an array or other
sequence is among the elements of L, the sequence is not flattened,
but treated as a single element."
  (cond
    ((null l) nil)
    ((atom l) (list l))
    (t (loop for i in l append (flatten i)))))


(defun write-log-entry (stream &rest messages)
  "Concatenates one or more strings (collected in MESSAGES), precedes
the result with a timestamp, writes to STREAM a string that looks like
a log entry. Returns the same string that was written to STREAM."
  (let ((entry (apply #'log-entry messages)))
    (format stream "~a" entry)
    entry))


(defun verify-string (string regex &key ignore-case)
  "Return t if STRING matches the REGEX exactly.  Use the IGNORE-CASE
parameter if you want case-insensitve matches."
  (multiple-value-bind (a b)
      (scan
       (if ignore-case (concatenate 'string "(?is)" regex) regex)
       string)
    (and a b (zerop a) (= b (length string)))))


(defun join-paths (&rest path-parts)
  "Joins parameters (collected in PATH-PARTS) into a unix-like file
path, inserting slashes where necessary."
  (cond ((null path-parts) "")
        ((loop for s in path-parts thereis (not (or (null s) (stringp s))))
         (error "All path parts must be strings or nil."))
        (t (let* ((clean-parts (remove-if
                                (lambda (p) (or (null p) (string= p "")))
                                (mapcar
                                 (lambda (s)
                                   (regex-replace-all "^/+|/+$" s ""))
                                 path-parts)))
                  (path (format nil "~{~a~^/~}" clean-parts)))
             (format nil "~a~a"
                     (if (and (stringp (car path-parts))
                              (verify-string (car path-parts) "^/.*$"))
                         "/" "")
                     path)))))


(defun path-only (filename)
  "Retrieves the path (path only, without the filename) of FILENAME."
  (declare (type string filename))
  (multiple-value-bind (match strings)
      (scan-to-strings "(.+)\/[^\/]*$" filename)
    (declare (ignore match))
    (let ((string-list (map 'list 'identity strings)))
      (if (or (null string-list)
              (null (car string-list)))
          "./"
          (elt string-list 0)))))


(defun filename-only (filename)
  "Retrieves the filename (filename only, without the path) of FILENAME."
  (if (null filename)
      ""
      (if (stringp filename)
          (multiple-value-bind (match parts)
              (scan-to-strings "((.*)/)?([^\/]*)$" filename)
            (declare (ignore match))
            (if (not (zerop (length parts)))
                (elt parts (1- (length parts)))
                ""))
          (error "FILENAME must be a string."))))

(defun ds-collection-p (x)
  "Return T if X is of type ds-collection. See the documentation for
 ds-collection."
  (cond ((atom x) t)
        ((and (listp x)
              (not (zerop (length x)))
              (member (car x) '(:map :list :array)))
         (if (eq (car x) :map) (evenp (length (cdr x))) t))
        (t nil)))

(deftype ds-collection ()
  "A specifier for the type ds-collection. An item of type ds-collection
is either an atom or a list. If it is a list, then the first element
must be the type of the collection, and the type must be one of:
  :list
  :array
  :map

The rest of the list must represent the ds-collection item's
contents. If the ds-collection item is a map, then the list must have
an even number of elements (key/value pairs). Here are some examles of
collections:

  - (:list 1 2 3)
  - (:array 1 2 3)
  - (:map \"a\" 1 \"b\" 2 \"c\" 3)
  - (:list (:map \"a\" 1 \"b\" 2)
           (:map \"c\" 3 \"d\" 4)
           (:map \"e\" 5 \"f\" 6))

A ds-collection item is just an easy way for humans to write a
representation of a nested data structure. The ds command can
turn that representation into an actual data structure that
uses standard Common Lisp types. The ds-list command accepts
a nested data structure and writes it out as a ds-collection
item."
  `(satisfies ds-collection-p))


(declaim (ftype (function (ds-collection) t) ds))
(defun ds (collection)
  "Create a nested data structure.  Each node in LIST-OR-ATOM can be an
atom or a collection. A collection is a map (hash table), an array, or
a list.  Valid collection types are :map, :array, and :list. These
types respresent the corresponding types in Common Lisp. An atom is
represented by any Common Lisp atom. A collection consists of a Common
Lisp list that starts with collection type. In lists and arrays, the
elements that follow the collection type are the elements of the list
or array. In maps, the elements that follow the collection type are
taken in pairs, to represent the key and value pairs of the map.

For example, here's how you would call DS to create the
given data structures:

  A list of integers:

    (ds '(:list 1 2 3))

  A list of strings:

    (ds '(:list \"one\" \"two\" \"three\"))

  An array of integers:

    (ds '(:array 1 2 3))

  A map of strings to integers:

    (ds '(:map \"one\" 1 \"two\" 2 \"three\" 3))

If you want to create a datastructure that consists of an atom, you
can do this:

    (ds 1)

However, if you pass a list instead of an atom, and you don't provide
a type for the list, you get an error. Don't try this:

    (ds '(1 2 3))  ==>  ERROR: Unknown collection type 1

The data structures you pass can be nested to any depth. Here's an
example:

    (ds '(:list 1 2 3 (:list \"one\" \"two\" \"three\")))

Note how you have to specify the type for each collection.

Here's an example of a list of maps:

    (ds '(:array (:map :name \"Donnie\" :age 50 :height \"6'4\" :weight 225)
                 (:map :name \"Tracy\" :age 45 :height \"5'0'\" :weight 120)))

When you create a data structure like the one above, you can use other
ds functions to access and manipulate the data."
  (if (atom collection)
      collection
      (let* ((list (copy-list collection))
             (type (pop list)))
        (case type
          (:map (loop with h = (make-hash-table :test #'equal)
                      while list
                      for key = (pop list)
                      for val = (ds (pop list))
                      do (setf (gethash key h) val)
                      finally (return h)))
          (:array (apply #'vector (mapcar 'ds list)))
          (:list (mapcar #'ds list))))))


(defun ds-valid-keys (keys)
  "Check that the keys are strings, integers, or keywords."
  (loop for key in keys
        for key-type = (type-of key)
        when (not (or (stringp key)
                      (integerp key)
                      (eq key-type 'keyword)))
          do (error "Invalid key type. Must be a string, integer, or keyword.")))


(defun ds-get (ds &rest keys)
  "Get a node (a leaf or a subtree) of DS, a dc-utilities data
structure.  The parameters that follow ds, collected in KEYS, describe
the path to the node.  For example, given the following data structure
in bogus-ds:

    (ds '(:array (:map :name \"Donnie\" :age 50 :height \"6'4\" :weight 225)
                 (:map :name \"Tracy\" :age 45 :height \"5'0'\" :weight 120)))

You can get Tracy's weight like this:

    (ds-get bogus-ds 1 :weight)

or like this:

    (ds-get (elt (remove-if-not (lambda (x) (string= (ds-get x :name) \"Tracy\"))
                                bogus-ds)
                 0)
            :weight)"
  (ds-valid-keys keys)
  (if keys
      (case (ds-type ds)
        (hash-table
         (multiple-value-bind (value exists)
             (gethash (car keys) ds)
           (if exists
               (if (= (length keys) 1)
                   (values value t)
                   (values (apply #'ds-get (cons value (cdr keys))) t))
               (values nil nil))))
        (sequence
         (if (< (car keys) (length ds))
             (if (= (length keys) 1)
                 (values (elt ds (car keys)) t)
                 (values (apply #'ds-get (cons (elt ds (car keys))
                                               (cdr keys)))
                         t))
             (values nil nil)))
        (t (values nil nil)))
      (values ds t)))

(defun ds-paths (ds &optional parent-keys)
  "Given a nested data structure DS, this function returns the path
to every leaf.  If you provide a key or list of keys in PARENT-KEYS,
those keys are prepended to the path to every leaf."
  (when (and parent-keys (atom parent-keys))
    (setf parent-keys (list parent-keys)))
  (case (ds-type ds)
    (hash-table
     (loop for k being the hash-keys in ds
           for new-parent-keys = (append parent-keys (list k))
           for child-ds = (gethash k ds)
           for child-keys = (ds-paths child-ds new-parent-keys)
           append child-keys))
    (sequence
     (loop for i from 0 below (length ds)
           for new-parent-keys = (append parent-keys (list i))
           for child-ds = (elt ds i)
           append (ds-paths child-ds new-parent-keys)))
    (t (list parent-keys))))

(defun ds-type (ds)
  "Given a dc-eclectic data structure DS, this function returns the type
of the data structure.  Valid return values include 'string,
'sequence, 'hash-table, and some Common Lisp types. This function is
used internally, by the ds-clone, ds-get, ds-paths, ds-list, ds-set,
and ds-to-json functions."
  (let* ((a (type-of ds))
         (b (string-downcase (format nil "~a" a))))
    (cond ((ppcre:scan
            "simple-array character|vector character"
            b)
           'string)
          ((or (string= b "cons")
               (ppcre:scan "vector|array" b))
           'sequence)
          ((atom a) a)
          (t (car a)))))

(defun set-list-element (list index value)
  "Set the element of LIST at INDEX to VALUE. If INDEX is greater than
the last index of LIST, the list is extended with nils before setting
the element."
  (if (< index (length list))
      (setf (elt list index) value)
      (loop while (< (length list) index)
            do (push nil (cdr (last list)))
            finally (push value (cdr (last list)))))
  list)

(defun set-collection-element (sequence-or-hashtable index value)
  "Set the element of SEQUENCE-OR-HASHTABLE at INDEX to VALUE. If
SEQUENCE-OR-HASHTABLE is a list and the index is larger than the last
index of the sequence, then this function extends the list with nil
values before setting the element. If SEQUENCE-OR-HASHTABLE is a
non-list sequence and the index is larger than the last index of the
sequence, then this function signals an error. SEQUENCE-OR-HASHTABLE
can also be a hash table, in which case INDEX becomes the value's key."
  (cond ((listp sequence-or-hashtable)
         (set-list-element sequence-or-hashtable index value))
        ((arrayp sequence-or-hashtable)
         (if (< index (length sequence-or-hashtable))
             (setf (elt sequence-or-hashtable index) value)
             (error "Index out of bounds.")))
        ((hash-table-p sequence-or-hashtable)
         (setf (gethash index sequence-or-hashtable) value))
        (t (error "Invalid data structure."))))

(defun create-sub-ds (keys)
  (if (integerp (second keys))
      (make-list (1+ (second keys)))
      (make-hash-table :test #'equal)))

(defun ds-set (ds location value)
  "In the given dc-utilities data structure DS, this function sets the
value of the node at LOCATION-KEY-PATH, which is a key or an index, or
a list of keys or indexes, to VALUE."
  (let* ((keys (if (atom location)
                   (list location)
                   location))
         (key (car keys)))
    (when *debug* (format t "ds=~a; keys=~a~%" (ds-list ds) keys))
    (ds-valid-keys keys)
    (if (= (length keys) 1)
        (progn
          (when *debug* (format t "one key left"))
          (set-collection-element ds key value))
        (multiple-value-bind (target-ds exists)
            (ds-get ds key)
          (if exists
              (progn
                (when *debug*
                  (format t "key ~a exists~%" key)
                  (format t "target-ds=~a~%" (ds-list target-ds)))
                (if (or (stringp target-ds) (numberp target-ds) (null target-ds))
                    (let ((sub-ds (if (integerp (second keys))
                                      (make-list (1+ (second keys)))
                                      (make-hash-table :test #'equal))))
                      (set-collection-element ds key sub-ds)
                      (ds-set sub-ds (cdr keys) value))
                    (ds-set target-ds (cdr keys) value)))
              (progn
                (when *debug* (format t "key ~a doesn't exist~%" key))
                (case (ds-type ds)
                  (hash-table
                   (setf (gethash key ds) (create-sub-ds keys))
                   (ds-set ds keys value))
                  (sequence
                   (setf (elt ds key) (create-sub-ds keys))
                   (ds-set ds keys value))
                  (otherwise
                   (setf ds (create-sub-ds keys))
                   (ds-set ds keys value)))))))))

(defun ds-merge (ds-base &rest ds-rest)
  "Merges dc-utilities data structures, starting with DS-BASE and then
progressing through the rest of the data structures, collected in
ds-set, in order.  Values in later data structures override values in
earlier data structures when the paths of the values coincide."
  (loop with ds-main = (ds-clone ds-base)
        for ds in ds-rest do
          (loop for key-path in (ds-paths ds)
                do (ds-set ds-main key-path (apply #'ds-get (cons ds key-path))))
        finally (return ds-main)))

(defun ds-clone (ds)
  "Clone the dc-utilities data structure DS."
  (case (ds-type ds)
    (hash-table
     (loop with ds-new = (make-hash-table :test 'equal)
           for key being the hash-keys in ds
           do (setf (gethash key ds-new) (ds-clone (gethash key ds)))
           finally (return ds-new)))
    (string
     (copy-seq ds))
    (sequence
     (if (equal (type-of ds) 'cons)
         (loop
           with ds-new = nil
           for i from 0 below (length ds)
           do (push (ds-clone (elt ds i)) ds-new)
           finally (return (reverse ds-new)))
         (loop
           with l = (length ds)
           with ds-new = (make-array l)
           for i from 0 below l
           do (setf (elt ds-new i) (ds-clone (elt ds i)))
           finally (return ds-new))))
    (t ds)))

(defun ds-list (ds)
  "Render the dc-utilities data structure DS in a human-readable way"
  (case (ds-type ds)
    (hash-table
     (loop with list = (list :map)
           for k being the hash-keys in ds
           for v = (gethash k ds)
           do (push k list)
              (push (ds-list v) list)
           finally (return (nreverse list))))
    (string
     (map 'string 'identity (copy-seq ds)))
    (sequence
     (if (equal (type-of ds) 'cons)
         (loop
           with list = (list :list)
           for a in ds
           do (push (ds-list a) list)
           finally (return (nreverse list)))
         (loop
           with list = (list :array)
           for a across ds
           do (push (ds-list a) list)
           finally (return (nreverse list)))))
    (otherwise ds)))

(defun ds-from-json (json)
  "Creates a dc-utilities data structure from JSON.  This is useful if
you want to easily traverse the JSON data structure."
  (let* ((data (yason:parse json)))
    (ds (if (hash-table-p data)
            (ds data)
            (ds (cons :array data))))))

(defun ds-to-json (ds)
  "Converts the dc-utilities data structure DS into JSON."
  (case (ds-type ds)
    (hash-table
     (format nil "{~{~a~^,~}}"
             (loop for k being the hash-keys in ds using (hash-value v)
                   for v-json = (ds-to-json v)
                   for k-json = (if (symbolp k) (string-downcase (format nil "~a" k)) k)
                   collect (format nil "\"~a\":~a" k-json v-json))))
    (sequence
     (format nil "[~{~a~^,~}]"
             (if (consp ds)
                 (loop with list = nil
                       for a in ds do (push (ds-to-json a) list)
                       finally (return (nreverse list)))
                 (loop with list = nil
                       for a across ds do (push (ds-to-json a) list)
                       finally (return (nreverse list))))))
    (otherwise
     (let ((v (if (and ds (symbolp ds)) (string-downcase (format nil "~a" ds)) ds)))
       (format nil
               (cond
                 ((floatp v) "~,9f")
                 ((numberp v) "~a")
                 ((null v) "null")
                 (t "~s")) v)))))

(defun hash-string (string)
  "Hash STRING using sha-512 and return a hex representation of the hash"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    'ironclad:sha512 (string-to-utf-8-bytes string))))

(defun hash-hmac-256 (secret text)
  "Hash TEXT using SECRET and hmac-sha-256 and return a hex
representation of the hash"
  (let ((hmac (ironclad:make-hmac
               (ironclad:ascii-string-to-byte-array secret) :sha256)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array text))
    (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))

(defun distinct-elements (sequence &key (key #'identity))
  "Accepts a sequence of elements (list or vector) and returns a new
sequence of the same type with distinct elements from the original
sequence.  If the elements in the sequence are hash tables, plists, or
objects with methods, then you can provide a value or function for the
:key parameter.  If you provide a value, the function will use the
value as the key of the element, and the value of the key will
represent the unique signature of the element.  If you provide a
function, then the function will be applied to the element to compute
the elements unique signature."
  (let* ((list (if (vectorp sequence)
                   (map 'list 'identity sequence)
                   sequence))
         (f-key (if (functionp key)
                    key
                    (cond ((hash-table-p (car list))
                           (lambda (x) (gethash key x)))
                          ((listp (car list))
                           (lambda (x) (getf x key)))
                          (t (lambda (x) (funcall key x))))))
         (distinct (hash-values
                    (hashify-list list :method :index :f-key f-key))))
    (cond ((stringp sequence) (map 'string 'identity distinct))
          ((vectorp sequence) (map 'vector 'identity distinct))
          (t distinct))))

(defun distinct-values (list)
  (distinct-elements list))

(defun hash-values (hash)
  (loop for v being the hash-values in hash collect v))

(defun hash-keys (hash)
  (loop for k being the hash-keys in hash collect k))

(defun range (start end &key (step 1) (filter #'identity) shuffle)
  "Returns a list of values between START and END (inclusive), skipping
values by STEP, filtering remaining values with the function in
FILTER, and shuffling the remaining values if SHUFFLE is true.  STEP
defaults to 1, FILTER defaults to allowing all values through, and
SHUFFLE default to nil."
  (let ((range (loop for a from start to end by step
                     when (funcall filter a) collect a)))
    (if shuffle (shuffle range) range)))

(defun shuffle (seq)
  "Return a sequence with the same elements as the given sequence S, but
in random order (shuffled)."
  (loop
    with l = (length seq)
    with w = (make-array l :initial-contents seq)
    for i from 0 below l
    for r = (random l)
    for h = (aref w i)
    do
       (setf (aref w i) (aref w r))
       (setf (aref w r) h)
    finally (return (if (listp seq) (map 'list 'identity w) w))))

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

  "Takes a list and returns a hash table, using the specified
method. Supported methods, specified via the :method key, are :count,
:plist, :alist, :index, and :custom.

With the :count method, which the function uses by default, the
function creates a hash table in which the keys are the distinct items
of the list and the value for each key is the count of that distinct
element in the list.

The :alist and :plist methods convert the list into a hash that
conceptually represent the same map as the list. Alists and plists
both consist of collections of key/value pairs. Alists look like this:

'((key1 . value1) (key2 . value2) (key3 . value3)...)

Plists look like this:

'(key1 value1 key2 value2 key3 value3 ...)

If a key repeats in one of these lists, its value simply overwrites
the value of the repeated key.

The :index method tells this function that you to specify the key with
one of the f-key, hash-key, and plist-key parameters, and that the
value should be the list value. By default, the :index method uses a
1-based counter as the key and the elements of the given list to
hashify are made into values. Thus, the list '(a b c) becomes the
hash {1: a, 2: b, 3: c}.

If the objects in the list that you're indexing are hash tables, then
you can specify the index key with hash-key. That key should be present
in every object in the list. The key's value becomes the index to the
object. For example, for a given hash:

[
  {id: 1, name: \"abc\"},
  {id: 2, name: \"def\"}
]

If you specify :method :index :hash-key \"id\", this function will
create a hash table that looks like this:

{
  1: {id: 1, name: \"abc\"},
  2: {id: 2, name: \"def\"}
}

If the objects are plists, then you can specify the index with
plist-key. hash-key and plist-key are just shortcuts to save you from
having to write some code for f-key. You can specify only one of
hash-key, plist-key, and f-key.

The :index method allows you to later look up an element in the list,
by the given key, in O(1) time.

The :custom method requires that you provide functions for computing
the key from the element in the list and for computing the value given
the element, the computed key, and the existing hash value currently
associated with the computed key.  If there's no hash value associated
with the computed key, then the value specified via :initial-value is
used. The :count, :pairs, and :merged-pairs methods allow you to
specify functions for computing the key (given the element) and the
value (given the element, the computed key, and the existing value)."

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
      (:index (loop with key-function
                      = (cond (hash-key (lambda (x) (gethash hash-key x)))
                              (plist-key (lambda (x) (getf x plist-key)))
                              (alist-key (lambda (x) (cdr (assoc alist-key x))))
                              (f-key f-key)
                              (t (lambda (x)
                                   (declare (ignore x))
                                   (incf counter))))
                    for value in list
                    for k-raw = (funcall key-function value)
                    for k-clean = k-raw
                    for value-new = (funcall f-value k-raw k-clean value)
                    do (setf (gethash k-clean h) value-new))))
    h))

(defun comparable-hash-dump (hash &key
                                    (f-sort #'string<)
                                    (f-make-sortable
                                     (lambda (k) (format nil "~a" k)))
                                    flat)
  "Turns the given hash table into a list of pairs or, if FLAT is T,
 into a plist that represents the hash. In the resulting list, the
 keys are sorted, so that the list can be more easily compared with
 other lists. HASH is the hash you want to dump. F-SORT is the sort
 predicate, which defaults to sorting strings in ascending order.
 F-MAKE-SORTABLE is a function that accepts a key and returns a
 sortable version of the key. This defaults to turning the key into a
 string. If the keys to the hash are integers, for example, you can
 provide an F-SORT of #'<, which sorts integers numerically in
 ascending order, and you can provide an F-MAKE-SORTABLE of
 #'identity, which will leave the keys as integers for sorting
 purposes."
  (loop for k being the hash-keys in hash
          using (hash-value v)
        for k-sortable = (funcall f-make-sortable k)
        collect (list k-sortable k v) into pairs
        finally (return
                  (let ((result (mapcar #'cdr (sort pairs f-sort :key #'car))))
                    (if flat (flatten result) result)))))

(defun all-permutations-base (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for item in list appending
                                  (mapcar (lambda (sublist) (cons item sublist))
                                          (all-permutations-base (remove item list)))))))

(defun all-permutations (list)
  "Returns a list of every permutation of elements in LIST. For
 example:
    '(1 2 3) -> '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))"
  (let ((h (hashify-list list :method :index)))
    (distinct-values
     (mapcar (lambda (list) (mapcar (lambda (x) (gethash x h)) list))
             (all-permutations-base (hash-keys h))))))

(defun all-permutations-of-string (s)
  "Returns a list of strings representing every permutation of the
orginal string S. For example:
    \"abc\" -> '(\"abc\" \"acb\" \"bac\" \"bca\" \"cab\" \"cba\")"
  (mapcar (lambda (list) (map 'string 'identity list))
          (all-permutations (map 'list 'identity s))))

(defun existing-permutations-of-string (s hash)
  "Works just like all-permutations-of-string, but excludes any
 permutations of S that are not among the keys in HASH."
  (loop for word in (all-permutations-of-string s)
        when (gethash word hash)
          collect word))

(defun n-grams-of-list (list count &optional prefix)
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
  (mapcar (lambda (v) (map 'list 'identity v))
          (n-grams-of-list list count)))

(defun n-gram-strings (chars count)
  "Accepts CHARS, a string, and COUNT, an integer, and returns all the
  possible combinations of length COUNT of the characters in CHARS. For example,
  (n-gram-strings \"abc\" 3) => '(\"aa\" \"ab\" \"ac\" \"ba\" \"bb\"
  \"bc\" \"ca\" \"cb\" \"cc\")"
  (mapcar (lambda (v) (map 'string 'identity v))
          (n-grams-of-list (map 'list 'identity chars)
                           count)))

(defun existing-n-gram-strings (chars count hash)
  (remove-if-not (lambda (word)
                   (gethash word hash))
                 (n-gram-strings chars count)))

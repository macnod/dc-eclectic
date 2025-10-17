(in-package :dc-eclectic)

(defvar *unix-epoch* (encode-universal-time 0 0 0 1 1 1970 0))
(defparameter *debug* nil)
(defparameter *log-mutex* (make-mutex :name "dc-eclectic-log"))

(defparameter *log* nil)
(defparameter *log-is-file* nil)
(defparameter *log-format* nil)
(defparameter *log-severity-map* (list :error 3 :warn 2 :info 1 :debug 0))
(defparameter *log-severity-threshold* :debug)

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
    (let* ((parts (ds:ds (list :map
                               "%Y" (format nil "~d"     year)
                               "%M" (format nil "~2,'0d" month)
                               "%D" (format nil "~2,'0d" day)
                               "%h" (format nil "~2,'0d" hour)
                               "%m" (format nil "~2,'0d" minute)
                               "%s" (format nil "~2,'0d" second))))
           (elements (loop for i from 0 below (length format)
                           for v = (subseq format i (+ i 2))
                           for fs = (ds:pick parts v)
                           collect (if fs (progn (incf i) fs) (subseq v 0 1)))))
      (format nil "~{~a~}" elements))))

(defun set-log-severity-threshold (log-severity)
  "Sets the system's log severity threshold to LOG-SEVERITY, which must be
:error, :warn, :info, or :debug.

The LOG-IT function will log any messages with a severity that is equal to or
greater LOG-SEVERITY. For example, if you call this function with a
LOG-SEVERITY value of :warn, then subsequent calls to the LOG-IT function
will result in a log entry when the LOG-IT function's parameter SEVERITY
is :warn or :error. However, subsequent calls to LOG-IT with a SEVERITY
value of :info or :debug will not result in a log entry."
  (unless (getf *log-severity-map* log-severity)
    (error "~a ':~(~a~)'. Use ~a."
      "Invalid value for log-severity"
      log-severity
      ":error, :warn, :info, or :debug"))
  (setf *log-severity-threshold* log-severity))

(defun open-log (file-or-stream &key
                  (log-format :jsonl)
                  (append t)
                  (severity-threshold :debug))
  "Opens a log file or sets up logging to a specified stream, allowing the LOG-IT
function to cease to be a no-op.

FILE-OR-STREAM must be a string, a pathname, a stream, or NIL. If FILE-OR-STREAM
is a string or a pathname, it is treated as a file path, and this function opens
the file and uses that as the stream. If FILE-OR-STREAM is a stream, such as
*STANDARD-OUTPUT*, then this function uses it directly. Finally, if
FILE-OR-STREAM is NIL, then it defaults to *STANDARD-OUTPUT*.

LOG-FORMAT can be :jsonl (where each line is a JSON object) or
:plain (traditional, plain text log line). LOG-FORMAT defaults to :jsonl.

APPEND indicates that if a file exists at FILE-OR-STREAM, calls to LOG-IT should
append log entries to the end of the existing file. If APPEND is NIL, the file
at FILE-OR-STREAM is cleared. Regardless of the value of APPEND, if the file at
FILE-OR-STREAM doesn't exist, this function creates it. If FILE-OR-STREAM is
a stream, APPEND has not effect.

SEVERITY-THRESHOLD is :debug, :info, :warn, or :error. See the function
SET-LOG-SEVERITY-THRESHOLD for more information.

If *LOG* is set (if this function was called and CLOSE-LOG was never called),
then this function does nothing and returns NIL. If *LOG* is NIL (if this
function has not been called or it was called after a call to CLOSE-LOG), then
this function opens the log as described above, sets *LOG* to the stream, and
returns the value of *LOG*."
  (unless *log*
    (if (member log-format '(:jsonl :plain))
      (setf *log-format* log-format)
      (error "Invalid value '~a' for LOG-FORMAT" log-format))
    (cond
      ((stringp file-or-stream)
        (setf
          *log* (open (parse-namestring file-or-stream)
                  :direction :output
                  :if-exists (if append :append :supersede)
                  :if-does-not-exist :create)
          *log-is-file* t))
      ((streamp file-or-stream)
        (setf
          *log* file-or-stream
          *log-is-file* nil))
      ((null file-or-stream)
        (setf
          *log* *standard-output*
          *log-is-file* nil))
      (t
        (error "Bad file-or-stream value ~a. It must be a string, a stream, or NIL"
          file-or-stream)))
    (set-log-severity-threshold severity-threshold)
    *log*))

(defun close-log ()
  "If the log stream is open (see OPEN-LOG), then this function closes the stream
and returns T. If the stream is a file stream, the file is closed too.
Otherwise, if the log stream is not open, this function does nothing."
  (when *log*
    (when *log-is-file*
      (close *log*))
    (setf
      *log* nil
      *log-is-file* nil)
    t))

(defun log-it (severity message &rest params)
  "Computes a log entry string using MESSAGE. Logs the entry when there's an open
log (see OPEN-LOG) and SEVERITY is greather than or equal to the system's
log-severity threshold (see OPEN-LOG and SET-LOG-SEVERITY-THRESHOLD). If
SEVERITY is below the threshold, the entry is not written to the log.

If the entry is logged, this function returns the log entry string. If the entry
is not logged, the function returns NIL.

SEVERITY indicates the severity of the message. It must be one of the following
values: :error, :warn, :info, or :debug.

MESSAGE is a string that works just like the FORMAT function's CONTROL-STRING
parameter.

PARAMS holds the values that MESSAGE references.

The log entry includes the MESSAGE, the current time stamp and SEVERITY."
  (when *log*
    (let* ((message-severity (or (getf *log-severity-map* severity)
                               (error "Invalid message severity: ~a" severity)))
            (system-severity (getf *log-severity-map* *log-severity-threshold*)))
      (when (>= message-severity system-severity)
        (let* ((message-with-params (apply #'format (append `(nil ,message) params)))
                (log-entry (case *log-format*
                             (:jsonl
                               (format nil "~a~%"
                                 (ds:to-json
                                   (ds:ds
                                     `(:map
                                        :timestamp ,(timestamp-string)
                                        :severity ,severity
                                        :message ,message-with-params)))))
                             (:plain
                               (format nil "~a [~a] ~a~%"
                                 (timestamp-string)
                                 severity
                                 message-with-params))
                             (otherwise (error "Invalid log format")))))
          (format *log* log-entry)
          (force-output *log*)
          log-entry)))))

(defun log-it-lazy (severity message-function)
  "Works just like LOG-IT, but accepts MESSAGE-FUNCTION instead of a control string
and parameters. This function is convenient when there's work that the caller
wants to avoid unless SEVERITY is greater than or equal to the sytem's log
severity threshold. If SEVERITY is greater than or equal to the system's log
severity threshold, then this function calls MESSAGE-FUNCTION with no
parameters, expecting to receive a string that this function then logs and sends
back as the return value.

If the entry is logged, this function returns the log entry string. Otherwise,
if the entry is not logged, this function returns NIL.

SEVERITY indicates the severity of the message. It must be one of the following
values: :error, :warn, :inof, or :debug.

MESSAGE-FUNCTION is a function that takes no parameters and returns the string
to be logged.

This function prepends the return value of MESSAGE-FUNCTION with the current
time stamp and SEVERITY.

For more information, see OPEN-LOG, LOG-IT, SET-LOG-SEVERITY-THRESHOLD."
  (when *log*
    (let ((message-severity (or (getf *log-severity-map* severity)
                              (error "Invalid message severity: ~a" severity)))
           (system-severity (getf *log-severity-map*  *log-severity-threshold*)))
      (when (>= message-severity system-severity)
        (let ((log-entry (case *log-format*
                           (:jsonl
                               (format nil "~a~%"
                                 (ds:to-json
                                   (ds:ds
                                     `(:map
                                        :timestamp ,(timestamp-string)
                                        :severity ,severity
                                        :message ,(funcall message-function))))))
                           (:plain
                             (format nil "~a [~a] ~a~%"
                               (timestamp-string)
                               severity
                               (funcall message-function)))
                           (other (error "Invalid log format")))))
          (format *log* log-entry)
          log-entry)))))

(defun mark-time ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun elapsed-time (start-time)
  (- (/ (get-internal-real-time) internal-time-units-per-second)
     start-time))

(defun universal-time-to-unix-time (&optional universal-time)
  "Converts UNIVERSAL-TIME to unix time. If you don't provide a universal time,
this function returns the current unix time.

Unix time is the number of seconds elapsed since the epoch, January 1, 1970 at
00:00:00 UTC.

Universal is the number of seconds elapsed since January 1, 1900 at 00:00:00 UTC
"
  (let ((universal-time (or universal-time (get-universal-time))))
    (- universal-time *unix-epoch*)))


(defun unix-time-to-universal-time (&optional unix-time)
  "Converts UNIX-TIME to universal time. If you don't provide UNIX-TIME,
this function returns the current universal time, as an integer.

Unix time is the number of seconds elapsed since the epoch, January 1, 1970 at
00:00:00 UTC.

Universal time is the number of seconds elapsed since January 1, 1900 at
00:00:00 UTC.
"
  (let ((unix-time (or unix-time (universal-time-to-unix-time))))
    (+ unix-time *unix-epoch*)))


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

(defun flatten (l)
  "Given a nested list L, return a flat list. If an array or other
sequence is among the elements of L, the sequence is not flattened,
but treated as a single element."
  (cond
    ((null l) nil)
    ((atom l) (list l))
    (t (loop for i in l append (flatten i)))))

(defun verify-string (string regex &key ignore-case)
  "Return t if STRING matches the REGEX exactly.  Use the IGNORE-CASE
parameter if you want case-insensitve matches."
  (let ((s (format nil "~a" string)))
    (multiple-value-bind (a b)
        (re:scan (if ignore-case (concatenate 'string "(?is)" regex) regex) s)
      (and a b (zerop a) (= b (length s))))))


;;
;; BEGIN File and directory utilities
;;

(defun join-paths (&rest path-parts)
  "Joins parameters (collected in PATH-PARTS) into a unix-like file
path, inserting slashes where necessary."
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
  "Retrieves the path (path only, without the filename) of FILENAME."
  (declare (type (or string null) filename))
  (multiple-value-bind (match strings)
    (re:scan-to-strings "(.+)\/[^\/]*$" filename)
    (declare (ignore match))
    (let ((string-list (map 'list 'identity strings)))
      (if (or (null string-list)
            (null (car string-list)))
        "/"
        (elt string-list 0)))))

(defun filename-only (filename)
  "Retrieves the filename (filename only, without the path) of FILENAME."
  (if (null filename)
      ""
      (if (stringp filename)
          (multiple-value-bind (match parts)
              (re:scan-to-strings "((.*)/)?([^\/]*)$" filename)
            (declare (ignore match))
            (if (not (zerop (length parts)))
                (elt parts (1- (length parts)))
                ""))
          (error "FILENAME must be a string."))))

(defun leaf-directory-only (path)
  (car (last (re:split "/" (string-trim "/" path)))))

;; Needs tests
(defun root-path (files)
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
  "Returns a boolean value indicating if the file specified by PATH exists."
  (let ((path (probe-file path)))
    (and path
         (not (equal (file-namestring path) "")))))

;; Needs tests
(defun directory-exists-p (path)
  "Returns a boolean value indicating if the directory specified by PATH
exists."
  (let ((path (probe-file path)))
    (and path
         (not (equal (directory-namestring path) ""))
         (equal (file-namestring path) ""))))

;; Needs tests
(defun path-type (path)
  "Returns :FILE, :DIRECTORY, or :NOT-FOUND, depending on what PATH
 points to."
  (cond ((file-exists-p path) :file)
        ((directory-exists-p path) :directory)
        (t :not-found)))

;; Needs tests
(defun file-extension (path)
  "Returns a string consisting of the file extension for the file name
given in PATH."
  (multiple-value-bind (a b)
      (re:scan-to-strings "\\.([a-z0-9]+)$" path)
    (when a (aref b 0))))

;; Needs tests
(defun replace-extension (filename new-extension)
  "This function replaces the file extension in FILENAME with the file
extension provided in NEW-EXTENSION."
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
        finally (return index-of-max)))))

(defun hash-string (string &key (salt "") (size 128))
  "Hash STRING and return a hex representation of the hash"
  (subseq
    (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence
        'ironclad:sha512
        (string-to-utf-8-bytes (concatenate 'string salt string))))
    0
    size))

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
  (distinct-elements list))

;; Needs tests
(defun distinct-strings (list)
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
  (loop for v being the hash-values in hash collect v))

(defun hash-keys (hash)
  (loop for k being the hash-keys in hash collect k))

(defun plist-keys (plist)
  (loop for key in plist by #'cddr collect key))

(defun range (start end &key (step 1) (filter #'identity) shuffle)
  "Returns a list of values between START and END (inclusive), skipping
values by STEP, filtering remaining values with the function in
FILTER, and shuffling the remaining values if SHUFFLE is true.  STEP
defaults to 1, FILTER defaults to allowing all values through, and
SHUFFLE default to nil."
  (let ((range (loop for a from start to end by step
                     when (funcall filter a) collect a)))
    (if shuffle (shuffle range) range)))

(defun rand (value &optional rstate)
  "When called without RSTATE, this is the same as calling RANDOM with only the
VALUE parameter. Otherwise, this calls RANDOM with VALUE and RSTATE."
  (if rstate
    (random value rstate)
    (random value)))

(defun shuffle (seq &optional rstate)
  "Return a sequence with the same elements as the given sequence S, but
in random order (shuffled)."
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
  (when seq
    (elt seq (rand (length seq) rstate))))

(defun choose-some (seq n &optional rstate)
  "Choose N elements from SEQ and return a new sequence with those
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

  "Creates a hash table from LIST and returns the hash table, according to
METHOD. Supported methods are :COUNT, :PLIST, :ALIST, :INDEX, AND :CUSTOM.

:COUNT

    With the :COUNT method, which the function uses by default, the
    function creates a hash table in which each key is an item of the
    list and the associated value for each key is the incidence of
    the item in the list. For example:

    (hashify-list '(7 8 7 7 8 9))

    gives you a hash table that looks like this:

    {7: 3, 8: 2, 9: 1}

:ALIST and :PLIST

    The :ALIST and :PLIST methods convert the list into a hash that
    conceptually represent the same map as the list. Alists and plists
    both consist of collections of key/value pairs. Alists look like
    this:

    '((key1 . value1) (key2 . value2) (key3 . value3)...)

    Plists look like this:

    '(:key1 value1 :key2 value2 :key3 value3 ...)

    If a key repeats in one of these lists, its value simply
    overwrites the value of the repeated key. However, you can change
    that behavior. See the description of the :CUSTOM method for
    information on how to do that.

:INDEX

    The :index method causes the values in the list to become the keys
    in the hash table. The value associated with each key should be an
    increasing integer, starting with 0. Thus, the list '(a b c)
    becomes the hash {a: 1, b: 2, c: 3}.

    If the objects in the list that you're indexing are hash tables,
    then you can specify the object key for the value that the
    function should use as a key in the resulting hash. That object
    key should be present in every object in the list. This allows
    you to index a list of hash tables by some specific value in
    the hash table. Consider the following example:

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

    And, voilá, you no longer need to iterate through a list to find
    your object.

    If the objects are plists, and you specify the index with
    plist-key, you'll see the same behavior with the plist as we
    demonstrated above for hash tables.

    HASH-KEY and PLIST-KEY are just shortcuts to save you from having
    to write some code for F-KEY. You can specify only one of
    HASH-KEY, PLIST-KEY, and F-KEY.

:CUSTOM

    The :CUSTOM method requires that you provide functions for
    computing the keys and values that the function inserts into the
    resulting hash.

    Use F-KEY to provide a function that accepts an element from LIST
    and returns a computed hash key. Here are some examples F-KEY of
    acceptable definitions:

        - #'identity
        - #'string-upcase
        - (lambda (x) (zerop (mod x 10)))

    Use F-VALUE to provide a function that accepts an element from LIST,
    the computed key (which might be different from the element), and
    the value that's currently associated with the computed key in the
    resulting hash table. Here are some examples:

        - (lambda (element computed-key value)
            (declare (ignore element computed-key))
            value)
        - (lambda (element computed-key value)
            (declare (ignore element value))
            (incf value))

    If there's no hash value associated with the computed key, then
    the value specified by :INITIAL-VALUE is used."
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
        (t (loop
             for item in list
             appending (mapcar (lambda (sublist) (cons item sublist))
                               (all-permutations-base (remove item list)))))))

(defun all-permutations (list)
  "Returns a list of every permutation of elements in LIST. For
 example:
    '(1 2 3) -> '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))"
  (let ((v (apply #'vector list)))
    (distinct-elements
     (mapcar (lambda (l) (mapcar (lambda (i) (elt v i)) l))
             (dc-eclectic::all-permutations-base
              (dc-eclectic::range 0 (1- (length v))))))))

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

(defun split-n-trim (string &key (on-regex "\\s+") (fat "^\\s+|\\s+$"))
  "Splits STRING into substrings on ON-REGEX, then trims FAT from each
substring.  The ON-REGEX parameter value, which is optional, defaults
to \"\\s+\", which is to say that the string is split into a list of
words at the whitespace boundaries.  The default value for FAT, which
is also optional, \"\\s+|\\s+$\", causes this function to trim
whitespace from the beggining and end of each substring.  Here's an
example:

    (split-n-trim \"Hello  beautiful      world!\")

    => '(\"Hello\" \"beautiful\" \"world!\")"
  (remove-if (lambda (s) (zerop (length s)))
             (mapcar (lambda (x) (trim x fat))
                     (re:split on-regex string))))

(defun trim (s &optional (fat "^\\s+|\\s+$"))
  "Trim FAT from the string in S.  The FAT parameter is optional and
defaults to \"^\\s+|\\s+$\", which means \"Whitespace at the beginning
or end of the string\"."
  (re:regex-replace-all fat s ""))

(defun trim-whitespace (s)
	(string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return
								 #\Rubout)
							 s))

(defun plistp (list)
  (and (evenp (length list))
       (loop for key in list by #'cddr always (keywordp key))))

;; Needs tests!
(defun normalize-list (list &key max min)
  "Return a new list with new values between 0.0 and 1.0. MAX is the
largest value that LIST can hold, and MIN is the smallest.  Each new
value N is computed from the corresponding old value O in LIST, as
follows: N = (O - MIN) / (MAX - MIN). If you don't provide MAX and
MIN, this function does an initial pass through list where it sets MAX
and MIN to the largest number and the smallest number in LIST,
respectively.  Therefore, you can improve the performance of this
function if you already know those values. Furthermore, in some cases
the list may not even contain the values for MAX and MIN that you
need."
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
  "Returns a new list with the numbers in LIST, which are
 floating-point numbers between 0 and 1, expanded to the range MAX -
 MIN, such that the number 1.0 is converted to MAX, the number 0.0 is
 converted to MIN, and all the other numbers fall in the range MIN to
 MAX. INTEGER is T, the new list contains integers. Otherwise, the new
 list contains floating-point numbers."
  (if integer
      (loop with range = (- max min)
            for o in list
            collect (truncate (+ (* o range) min)))
      (loop with range = (- max min)
            for o in list
            collect (+ (* o range) min))))

(defun slurp (filename)
  (with-open-file (in filename :direction :input)
    (let ((contents (make-string (file-length in))))
      (read-sequence contents in)
      contents)))

(defun spew (string filename)
  (with-open-file (out filename
                    :direction :output
                    :if-exists :supersede)
    (write-string string out)))

(defun freeze (object)
  (prin1-to-string object))

(defun thaw (string)
  (let ((*read-eval* nil))
    (read-from-string string)))

(defun getenv (name &key default required (type :string))
  "Get the value of the environment variable NAME, returning a string or an
integer, depending on TYPE. If the environment variable is not set, then return
DEFAULT, which must be of type TYPE. If the environment variable is not set and
DEFAULT is NIL, then this function returns NIL.

TYPE can be :integer, :string, or :boolean."
  (unless (member type '(:string :integer :boolean))
    (error "Invalid type ~a. Use :integer or :string." type))
  (let ((value (trim (sb-ext:posix-getenv name))))
    (if value
      (cond
        ((eql type :string)
          value)
        ((eql type :integer)
          (handler-case (parse-integer value)
            (error (condition)
              (log-it :error "Failed to parse ~s as integer: ~a"
                value condition)
              nil)))
        ((and (eql type :boolean) (equal (string-downcase value) "true"))
          t)
        ((and (eql type :boolean) (equal (string-downcase value) "false"))
          nil))
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
              (log-it :error "Failed to parse default value ~s as integer: ~a"
                value condition)
              nil)))
        ((integerp default)
          default)))))

(defun setenv (name value)
  "Set environment variable NAME to VALUE. VALUE is always converted into a
string. Returns a string with VALUE."
  (let ((string-value (cond
                        ((eq value t) "true")
                        ((eq value nil) "false")
                        (t (format nil "~a" value)))))
    (sb-posix:setenv name string-value 1)
    string-value))

;; Everything that follows neeeds tests

(defun random-number (&optional (digits 4) rstate)
  (loop for a from 1 to digits
    for digit = (1+ (rand 9 rstate)) then (rand 10 rstate)
    for power downfrom (1- digits) to 0
    summing (* digit (expt 10 power))))

(defun random-hex-number (&optional (digits 7) (non-zero-start) rstate)
  (loop with hex-digits = "0123456789abcdef"
    for a from 1 to digits
    for digit = (elt hex-digits (if non-zero-start
                                  (1+ (rand 15 rstate))
                                  (rand 16 rstate)))
    then (elt hex-digits (rand 16 rstate))
    collect digit into number
    finally (return (map 'string 'identity number))))

(defun random-string (string-length alphabet &optional rstate)
  (loop with alphabet-length = (length alphabet)
    for a from 1 to string-length
    for letter = (elt alphabet (rand alphabet-length rstate))
    collect letter into string
    finally (return (map 'string 'identity string))))

(defun ascii-char-range (begin end)
  (loop for code from (char-code begin) to (char-code end)
    collect (code-char code) into string
    finally (return (map 'string 'identity string))))

(defun ascii-alpha-lower ()
  (ascii-char-range #\a #\z))

(defun ascii-alpha-upper ()
  (ascii-char-range #\A #\Z))

(defun ascii-alpha ()
  (concatenate 'string (ascii-alpha-lower) (ascii-alpha-upper)))

(defun ascii-numeric ()
  (ascii-char-range #\0 #\9))

(defun ascii-alpha-num-lower ()
  (concatenate 'string (ascii-alpha-lower) (ascii-numeric)))

(defun ascii-alpha-num-upper ()
  (concatenate 'string (ascii-alpha-upper) (ascii-numeric)))

(defun ascii-alpha-num ()
  (concatenate 'string (ascii-alpha) (ascii-numeric)))

(defun uuid (&optional rstate)
  (format nil "~{~a~^-~}"
    (list
      (random-hex-number 8 t rstate)
      (random-hex-number 4 nil rstate)
      (random-hex-number 4 nil rstate)
      (random-hex-number 4 nil rstate)
      (random-hex-number 12 nil rstate))))

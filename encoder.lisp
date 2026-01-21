(in-package :dc-eclectic)

;; IMPORTANT NOTE
;;
;;   This code is better than standard code for base64 encoding, because it allows
;;   you to create encoders with any alphabet, strictly limits characters in the
;;   encoded string to the characters in the alphabet, and supports strings with
;;   multibyte characters.
;;
;;   However, this code is not compatible with the industry standards. If you
;;   encode something with this code, you must decode it with this code.


;; Predefined common alphabets
(defparameter *alphabet-alphanum*
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  ":public: Alphabet with all ASCII letters and numbers.")

;; Bitcoin style: no zero, no capital O or I, no lowercase l
(defparameter *alphabet-bitcoin*
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  ":public: Bitcoin alphabet. No zero, no capital 'o' or 'i', and now lowercase 'l'.")

;; Numeric digits and upper-case letters
(defparameter *alphabet-alphanum-upper*
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  ":public: Alphabet with uppercase letters and numbers.")

(defun string-to-bigint (str)
  ":private: Helper function for make-encoder."
  (let ((bytes (babel:string-to-octets str :encoding :utf-8)))
    (reduce (lambda (acc byte) (+ (* acc 256) byte)) bytes :initial-value 0)))

(defun bigint-to-string (bigint)
  ":private: Helper function for make-decoder."
  (if (zerop bigint)
    (babel:octets-to-string #() :encoding :utf-8)
    (let ((bytes '()))
      (loop while (> bigint 0) do
        (setf bytes (cons (mod bigint 256) bytes))
        (setf bigint (floor bigint 256)))
      (babel:octets-to-string
        (make-array (length bytes)
          :element-type '(unsigned-byte 8)
          :initial-contents bytes)
        :encoding :utf-8))))

(defun make-encoder (alphabet)
  ":private: Returns a lambda that implements an encoder that uses ALPHABET."
  (lambda (data)
    (if (zerop (length data))
      ""
      (let ((base (length alphabet))
             (value (string-to-bigint data)))
        (if (zerop value)
          (subseq alphabet 0 1)
          (let ((digits '()))
            (loop while (> value 0) do
              (let ((digit (mod value base)))
                (push (char alphabet digit) digits)
                (setf value (floor value base))))
            (coerce digits 'string)))))))

(defun make-decoder (alphabet)
  ":private: Returns a lambda that implements a decoder that uses ALPHABET."
  (let ((base (length alphabet))
         (char-map (make-hash-table :test #'eql)))
    (loop for i from 0 below base
      for ch across alphabet
      do (setf (gethash ch char-map) i))
    (lambda (encoded)
      (if (zerop (length encoded))
        ""
        (let ((value 0)
               (power 1))
          (dolist (ch (reverse (coerce encoded 'list)))
            (let ((digit (gethash ch char-map)))
              (unless digit
                (error "Invalid character in encoded string: ~C (not in alphabet ~S)"
                  ch alphabet))
              (incf value (* digit power))
              (setf power (* power base))))
          (bigint-to-string value))))))

(defmacro define-base-encoder (name alphabet)
  ":public: Creates 2 functions using NAME (a symbol), and ALPHABET (a string):
{NAME}-ENCODE and {NAME-DECODE}. {NAME}-ENCODE takes 1 parameter, DATA (a
string), and returns the string encoded with ALPHABET. {NAME}-DECODE accepts a
single parameter (an encoded string) and returns the original strinng. For
example, if you need to encode strings using hexadecimal characters, you can
call this function as follows:

    (define-base-encoder hex \"0123456789abcdef\")

That will produce the functions HEX-ENCODE and HEX-DECODE. Those function will
work as follows:

    (hex-encode \"Hello World!\")             ;; => \"48656c6c6f20576f726c6421\"
    (hex-decode \"48656c6c6f20576f726c6421\") ;; => \"Hello World!\"
"
  `(progn
     (defvar ,(intern (format nil "*ENCODE-~A*" name)) (make-encoder ,alphabet))
     (defvar ,(intern (format nil "*DECODE-~A*" name)) (make-decoder ,alphabet))
     (defun ,(intern (format nil "~A-ENCODE" name)) (data)
       ,(format nil ":public: Encodes data using the ~a alphabet." name)
       (funcall ,(intern (format nil "*ENCODE-~A*" name)) data))
     (defun ,(intern (format nil "~A-DECODE" name)) (data)
       ,(format nil ":public: Decodes DATA using the ~a alphabet." name)
       (funcall ,(intern (format nil "*DECODE-~A*" name)) data))))

;; Creates safe-encode and safe-decode methods that accept a single string
;; parameter. You can use it to create encoders with any alphabet.
(define-base-encoder safe *alphabet-alphanum*)

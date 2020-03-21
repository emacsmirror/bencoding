;;; bencode.el --- Bencode decoding and encoding     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/bencode.el
;; Package-Requires: ((emacs "25.1"))
;; Version: 0
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs Lisp library for reading and writing Bencode
;; <https://en.wikipedia.org/wiki/Bencode>

;;; Code:

(require 'cl-lib)
(require 'map)                          ; `map-into'
(require 'json)                         ; `json-alist-p'

(defvar bencode-list-type 'list
  "Type to convert Bencode lists to.
Must be one of `vector' and `list'.  Consider let-binding this around
your call to `bencode-read' instead of `setq'ing it.")

(defvar bencode-dictionary-type 'alist
  "Type to convert Bencode dictionaries to.
Must be one of `alist' or `hash-table'.  Consider let-binding
this around your call to `bencode-read' instead of `setq'ing it.")

(define-error 'bencode-error "Bencode error" 'error)
(define-error 'bencode-end-of-file "End of file while parsing Bencode"
  '(end-of-file bencode-error))

(defun bencode-read-byte ()
  "Read one byte."
  (if (eobp)
      (signal 'bencode-end-of-file nil)
    (prog1 (following-char)
      (forward-char 1))))

(defun bencode-peek-byte ()
  "Peek one byte."
  (if (eobp)
      (signal 'bencode-end-of-file nil)
    (following-char)))

(defun bencode-read-bytes (amt)
  "Read AMT bytes."
  (cl-assert (>= amt 0))
  (if (> amt (- (point-max) (point)))
      (signal 'bencode-end-of-file nil)
    (let ((op (point)))
      (forward-char amt)
      (buffer-substring-no-properties op (point)))))

(defun bencode-read-integer ()
  "Read the Bencode integer following point, return an integer."
  (cond
   ((looking-at
     (rx (or "0" (and (opt "-") (in "1-9") (* (in "0-9"))))))
    (goto-char (match-end 0))
    (string-to-number (match-string 0)))
   (t (signal 'bencode-error (list "Not a Bencode integer at point")))))

(defun bencode-read-end ()
  "Read the end mark ('e')."
  (pcase (bencode-read-byte)
    (?e)
    (not-e (signal 'bencode-error
                   (list "Not a Bencode end mark 'e'" not-e)))))

(defun bencode-read-byte-string ()
  "Read the Bencode byte string following point, return a unibyte string."
  (let ((len (bencode-read-integer)))
    (pcase (bencode-read-byte)
      (?:)
      (not-: (signal 'bencode-error (list "Not string separator ':'" not-:))))
    (bencode-read-bytes len)))

(defun bencode-read-list ()
  "Read the Bencode list following point."
  (cl-loop until (= (bencode-peek-byte) ?e)
           collect (bencode-read) into l
           finally return (pcase-exhaustive bencode-list-type
                            ('list l)
                            ('vector (vconcat l)))))

(defun bencode-read-dictionary ()
  "Read the Bencode dictionary following point."
  (cl-loop until (= (bencode-peek-byte) ?e)
           collect (cons (bencode-read) (bencode-read)) into alist
           ;; All keys must be byte strings and must appear in lexicographical order.
           finally do
           (let ((keys (mapcar #'car alist)))
             (dolist (k keys)
               (unless (stringp k)
                 (signal 'bencode-error
                         (list "wrong type of dictionary key" #'stringp k))))
             (unless (equal keys (sort (copy-sequence keys) #'string<))
               (signal 'bencode-error
                       (list "Dictionary keys are not sorted in lexicographical order"
                             keys))))
           finally return
           (pcase-exhaustive bencode-dictionary-type
             ('alist alist)
             ('hash-table (map-into alist 'hash-table)))))

(defun bencode-read ()
  "Read and return the Bencode object at point.
Advances point just past Bencode object.

Bencode integer is mappped to Emacs Lisp integer.  Bencode byte
string is mapped to Emacs Lisp unibyte string.  Bencode list is
mapped to Emacs Lisp list (default) or vector according to
`bencode-list-type'.  Bencode dictionary is mapped to Emacs Lisp
alist (default) or plist or hash-table according to
`bencode-dictionary-type'."
  (cl-assert (not enable-multibyte-characters))
  (let ((b (bencode-read-byte)))
    (pcase b
      (?i
       (prog1 (bencode-read-integer)
         (bencode-read-end)))
      ((guard (<= ?0 b ?9))
       (forward-char -1)
       (bencode-read-byte-string))
      (?l
       (prog1 (bencode-read-list)
         (bencode-read-end)))
      (?d
       (prog1 (bencode-read-dictionary)
         (bencode-read-end)))
      (unknown
       (signal 'bencode-error (list "Unknown data type" unknown))))))

(defun bencode-read-from-string (string)
  "Read the Bencode object contained in STRING and return it."
  (cl-assert (not (multibyte-string-p string)))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert string)
    (goto-char (point-min))
    (bencode-read)))

(defun bencode-read-file (file)
  "Read the Bencode object contained in FILE and return it."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (bencode-read)))

(defun bencode-encode-integer (integer)
  "Encode INTEGER as a Bencode integer, return a unibyte string."
  (cl-assert (integerp integer))
  (format "i%de" integer))

(defun bencode-encode-byte-string (string)
  "Encode STRING as a Bencode byte string, return a unibyte string."
  (cl-assert (not (multibyte-string-p string)))
  (format "%d:%s" (length string) string))

(defun bencode-encode-list (list-or-vector)
  "Encode LIST-OR-VECTOR as a Bencode list, return a unibyte string."
  (format "l%se"
          (mapconcat #'bencode-encode list-or-vector "")))

(defun bencode-encode-dictionary (alist-or-hash-table)
  "Encode ALIST-OR-HASH-TABLE as a Bencode dictionary.
Return a unibyte string."
  (let ((alist (pcase alist-or-hash-table
                 ((and (pred hash-table-p) ht) (map-into ht 'list))
                 (alist
                  ;; Make sure we don't alter the list structure
                  (copy-sequence alist)))))
    (setq alist
          (sort alist (pcase-lambda (`(,k1 . ,_)
                                     `(,k2 . ,_))
                        (string< k1 k2))))
    (cl-loop for (k . v) in alist
             concat (concat (bencode-encode k) (bencode-encode v)) into s
             finally return (concat "d" s "e"))))

(defun bencode-encode (object)
  "Return a JSON representation of OBJECT as a unibyte string."
  (pcase object
    ((pred integerp) (bencode-encode-integer object))
    ((pred stringp) (bencode-encode-byte-string object))
    ((pred vectorp) (bencode-encode-list object))
    ((pred hash-table-p) (bencode-encode-dictionary object))
    ((pred listp) (if (json-alist-p object)
                      ;; Note `json-alist-p' treats nil as alist, so to get a
                      ;; empty bencode list, use []
                      (bencode-encode-dictionary object)
                    (bencode-encode-list object)))))

(provide 'bencode)
;;; bencode.el ends here

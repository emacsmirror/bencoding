;;; bencoding-tests.el --- Tests                       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang

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

;; Tests for bencoding.el

;;; Code:

(require 'bencoding)
(require 'ert)

(ert-deftest bencoding-read-integer ()
  "Test reading Bencoding integers."
  (should (zerop (bencoding-read-from-string "i0e")))
  (should (= (bencoding-read-from-string "i123e") 123))
  (should (= (bencoding-read-from-string "i-123e") -123))
  (should-error (bencoding-read-from-string "i01e") :type 'bencoding-error)
  (should-error (bencoding-read-from-string "i123") :type 'bencoding-end-of-file))

(ert-deftest bencoding-read-byte-string ()
  "Test reading Bencoding byte strings."
  (should (equal "spam" (bencoding-read-from-string "4:spam")))
  (should (equal "" (bencoding-read-from-string "0:")))
  (should-error (bencoding-read-from-string "4:abc") :type 'bencoding-end-of-file))

(ert-deftest bencoding-read-byte-string ()
  "Test reading Bencoding lists."
  (should (equal '("spam" 123) (bencoding-read-from-string "l4:spami123ee")))
  (should (equal '()           (bencoding-read-from-string "le")))
  (should (equal ["spam" 123]

                 (let ((bencoding-list-type 'vector))
                   (bencoding-read-from-string "l4:spami123ee"))))
  (should (equal '(((()))) (bencoding-read-from-string "lllleeee")))
  (should (equal '(1(2(3(4)))) (bencoding-read-from-string "li1eli2eli3eli4eeeee"))))

(ert-deftest bencoding-read-dictionary ()
  "Test reading Bencoding dictionaries."
  (should (equal '() (bencoding-read-from-string "de")))
  (should (equal '(("bar" . "spam") ("foo" . 42)) (bencoding-read-from-string "d3:bar4:spam3:fooi42ee")))
  ;; wrong key type
  (should-error (bencoding-read-from-string "di1ei2ee") :type 'bencoding-error)
  ;; wrong key order
  (should-error (bencoding-read-from-string "d3:fooi1e3:bari2ee") :type 'bencoding-error))

(ert-deftest bencoding-encode-integer ()
  "Test encoding integers."
  (should (equal "i42e" (bencoding-encode-integer 42)))
  (should (equal "i0e" (bencoding-encode-integer 0)))
  (should (equal "i-42e" (bencoding-encode-integer -42)))
  ;; `should-error' doesn't catch error of `cl-assert', not sure why, and now
  ;; I'm sure I don't really understand `cl-assert' such as its correct use
  ;; (should-error (bencoding-encode-integer 1.0) :type 'cl-assertion-failed)
  )

(ert-deftest bencoding-encode-byte-string ()
  "Test writing Bencoding byte strings."
  (should (equal (bencoding-encode-byte-string "spam") "4:spam"))
  (should (equal (bencoding-encode-byte-string "") "0:")))

(ert-deftest bencoding-encode-list ()
  "Test writing Bencoding lists."
  (should (equal (bencoding-encode-list []) "le"))
  (should (equal (bencoding-encode-list ["spam" 42]) "l4:spami42ee"))
  (should (equal (bencoding-encode-list '("spam" 42)) "l4:spami42ee")))

(ert-deftest bencoding-encode-dictionary ()
  "Test writing Bencoding dictionaries."
  (should (equal (bencoding-encode-dictionary '()) "de"))
  (should (equal (bencoding-encode-dictionary '(("foo" . 1) ("bar" . 2)))
                 "d3:bari2e3:fooi1ee"))
  (let* ((alist (list (cons "foo" 1)    
                      (cons "bar" 2)))
         (copy (copy-sequence alist)))
    (bencoding-encode-dictionary alist)
    (should (equal alist copy)))        ; free of side-effects check
  (should (equal (bencoding-encode-dictionary
                  #s(hash-table test equal data ("foo" 1 "bar" 2)))
                 "d3:bari2e3:fooi1ee")))

(ert-deftest bencoding-encode ()
  "Test encoding."
  (should (equal (bencoding-encode 42) "i42e"))
  (should (equal (bencoding-encode "hi") "2:hi"))
  (should (equal (bencoding-encode [42 "hi"]) "li42e2:hie"))
  (should (equal (bencoding-encode ()) "de"))
  (should (equal (bencoding-encode []) "le"))
  (should (equal (bencoding-encode #s(hash-table)) "de"))
  (let ((obj '(42 "hi" ("nested" "list" (("a" . (("nested dict" . "xxx"))) ("b" . 2))))))
    (should (equal obj (bencoding-read-from-string (bencoding-encode obj))))))

(ert-deftest bencoding-read-file ()
  "Test reading torrent."
  (cl-loop for file in (file-expand-wildcards "*.torrent")
           do (should (bencoding-read-file file))))

(provide 'bencoding-tests)
;;; bencoding-tests.el ends here

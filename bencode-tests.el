;;; bencode-tests.el --- Tests                       -*- lexical-binding: t; -*-

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

;; Tests for bencode.el

;;; Code:

(require 'bencode)
(require 'ert)

(ert-deftest bencode-read-integer ()
  "Test reading Bencode integers."
  (should (zerop (bencode-read-from-string "i0e")))
  (should (= (bencode-read-from-string "i123e") 123))
  (should (= (bencode-read-from-string "i-123e") -123))
  (should-error (bencode-read-from-string "i01e") :type 'bencode-error)
  (should-error (bencode-read-from-string "i123") :type 'bencode-end-of-file))

(ert-deftest bencode-read-byte-string ()
  "Test reading Bencode byte strings."
  (should (equal "spam" (bencode-read-from-string "4:spam")))
  (should (equal "" (bencode-read-from-string "0:")))
  (should-error (bencode-read-from-string "4:abc") :type 'bencode-end-of-file))

(ert-deftest bencode-read-byte-string ()
  "Test reading Bencode lists."
  (should (equal '("spam" 123) (bencode-read-from-string "l4:spami123ee")))
  (should (equal '()           (bencode-read-from-string "le")))
  (should (equal ["spam" 123]

                 (let ((bencode-list-type 'vector))
                   (bencode-read-from-string "l4:spami123ee"))))
  (should (equal '(((()))) (bencode-read-from-string "lllleeee")))
  (should (equal '(1(2(3(4)))) (bencode-read-from-string "li1eli2eli3eli4eeeee"))))

(ert-deftest bencode-read-dictionary ()
  "Test reading Bencode dictionaries."
  (should (equal '() (bencode-read-from-string "de")))
  (should (equal '(("bar" . "spam") ("foo" . 42)) (bencode-read-from-string "d3:bar4:spam3:fooi42ee")))
  ;; wrong key type
  (should-error (bencode-read-from-string "di1ei2ee") :type 'bencode-error)
  ;; wrong key order
  (should-error (bencode-read-from-string "d3:fooi1e3:bari2ee") :type 'bencode-error))

(ert-deftest bencode-encode-integer ()
  "Test encoding integers."
  (should (equal "i42e" (bencode-encode-integer 42)))
  (should (equal "i0e" (bencode-encode-integer 0)))
  (should (equal "i-42e" (bencode-encode-integer -42)))
  ;; `should-error' doesn't catch error of `cl-assert', not sure why, and now
  ;; I'm sure I don't really understand `cl-assert' such as its correct use
  ;; (should-error (bencode-encode-integer 1.0) :type 'cl-assertion-failed)
  )

(ert-deftest bencode-encode-byte-string ()
  "Test writing Bencode byte strings."
  (should (equal (bencode-encode-byte-string "spam") "4:spam"))
  (should (equal (bencode-encode-byte-string "") "0:")))

(ert-deftest bencode-encode-list ()
  "Test writing Bencode lists."
  (should (equal (bencode-encode-list []) "le"))
  (should (equal (bencode-encode-list ["spam" 42]) "l4:spami42ee"))
  (should (equal (bencode-encode-list '("spam" 42)) "l4:spami42ee")))

(ert-deftest bencode-encode-dictionary ()
  "Test writing Bencode dictionaries."
  (should (equal (bencode-encode-dictionary '()) "de"))
  (should (equal (bencode-encode-dictionary '(("foo" . 1) ("bar" . 2)))
                 "d3:bari2e3:fooi1ee"))
  (let* ((alist (list (cons "foo" 1)    
                      (cons "bar" 2)))
         (copy (copy-sequence alist)))
    (bencode-encode-dictionary alist)
    (should (equal alist copy)))        ; free of side-effects check
  (should (equal (bencode-encode-dictionary
                  #s(hash-table test equal data ("foo" 1 "bar" 2)))
                 "d3:bari2e3:fooi1ee")))

(ert-deftest bencode-encode ()
  "Test encoding."
  (should (equal (bencode-encode 42) "i42e"))
  (should (equal (bencode-encode "hi") "2:hi"))
  (should (equal (bencode-encode [42 "hi"]) "li42e2:hie"))
  (should (equal (bencode-encode ()) "de"))
  (should (equal (bencode-encode []) "le"))
  (should (equal (bencode-encode #s(hash-table)) "de"))
  (let ((obj '(42 "hi" ("nested" "list" (("a" . (("nested dict" . "xxx"))) ("b" . 2))))))
    (should (equal obj (bencode-read-from-string (bencode-encode obj))))))

(ert-deftest bencode-read-file ()
  "Test reading torrent."
  (cl-loop for file in (file-expand-wildcards "*.torrent")
           do (should (bencode-read-file file))))

(provide 'bencode-tests)
;;; bencode-tests.el ends here

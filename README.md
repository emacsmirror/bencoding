# BitTorrent Bencode library for Emacs Lisp

`bencode.el` is a [Bencode](https://en.wikipedia.org/wiki/Bencode) library for
Emacs Lisp. It let you read and write Bencode.

## Usage

To decode Bencode, use `bencode-read` or `bencode-read-from-string`, e.g.,

``` emacs-lisp
(bencode-read-from-string "d5:applei1e6:bananai2ee")
;; => (("apple" . 1) ("banana" . 2))
```

To encode Bencode, use `bencode-encode`, e.g.,

``` emacs-lisp
(bencode-encode '(("apple" . 1) ("banana" . 2)))
;; => "d5:applei1e6:bananai2ee"
```

## API

### `(bencode-read)`

Read and return the Bencode object at point.

| Bencode type | Emacs Lisp type                                                                      |
|--------------|--------------------------------------------------------------------------------------|
| integer      | integer                                                                              |
| byte string  | unibyte string                                                                       |
| list         | list, if you need vector, let-binding `bencode-list-type` to `vector`                |
| dictionary   | alist, if you need hash table, let-binding `bencode-dictionary-type` to `hash-table` |

### `(bencode-read-from-string STRING)`

Read and return the Bencode object in STRING.

### `(bencode-encode OBJECT)`

Encode OBJECT.

| Emacs Lisp type | Bencode type                                                                        |
|-----------------|-------------------------------------------------------------------------------------|
| integer         | integer                                                                             |
| unibyte string  | byte string                                                                         |
| vector          | list                                                                                |
| hash table      | dictionary                                                                          |
| list            | dictionary if the list looks like a alist judging by `json-alist-p`, otherwise list |


## Requires

- Emacs 25.1

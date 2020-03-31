# BitTorrent Bencoding library for Emacs Lisp

`bencoding.el` is a [Bencode](https://en.wikipedia.org/wiki/Bencode) library for
Emacs Lisp. It let you read and write Bencode.

## Usage

To decode Bencode, use `bencoding-read` or `bencoding-read-from-string`, e.g.,

``` emacs-lisp
(bencoding-read-from-string "d5:applei1e6:bananai2ee")
;; => (("apple" . 1) ("banana" . 2))
```

To encode Bencode, use `bencoding-encode`, e.g.,

``` emacs-lisp
(bencoding-encode '(("apple" . 1) ("banana" . 2)))
;; => "d5:applei1e6:bananai2ee"
```

## API

### `(bencoding-read)`

Read and return the Bencode object at point.

| Bencode type | Emacs Lisp type                                                                      |
|--------------|--------------------------------------------------------------------------------------|
| integer      | integer                                                                              |
| byte string  | unibyte string                                                                       |
| list         | list, if you need vector, let-binding `bencoding-list-type` to `vector`                |
| dictionary   | alist, if you need hash table, let-binding `bencoding-dictionary-type` to `hash-table` |

### `(bencoding-read-from-string STRING)`

Read and return the Bencode object in STRING.

### `(bencoding-read-from-file FILE)`

Read and return the Bencode object in FILE.


### `(bencoding-encode OBJECT)`

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

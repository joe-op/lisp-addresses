# lisp-addresses

Create, save, and view addresses.

## Usage

### Create an address

```lisp
(make-address
 :name "Q Quigley"
 :addr1 "12 Elm St"
 :addr2 "B"
 :city "Minneapolis"
 :state "MN"
 :zip "55403"
 :notes "Notes")
```

### View an address
```lisp
(print-address *quigley-address*)
```

### Save addresses
```lisp
(save-addresses
  (list *quigley-address* *thompson-address*)
  "addresses.db")
```

### Load addresses
```lisp
(setf *db* (load-addresses "addresses.db"))
```

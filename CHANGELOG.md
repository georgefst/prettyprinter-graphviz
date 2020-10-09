# Revision history for prettyprinter-graphviz

## 1.1.0.0 -- 2020-10-09

* Use shallower module hierarchy, reflecting changes in `prettyprinter-1.7`.
* Take a list of attributes as annotation, so that zero or many can be specified.
* Handle newlines correctly. Previously, labels would always take up just one line.

## 1.0.0.0 -- 2020-06-12

* Simplify considerably by using helper functions from `prettyprinter`.

## 0.1.1.1 -- 2020-02-22

* Fix bug causing graphviz to fall over if an annotation were applied to an empty string. This could easily occur in the case of a non-indented newline.

## 0.1.1.0 -- 2020-02-10

* Include safe versions of functions.

## 0.1.0.0 -- 2020-01-16

* First version. Released on an unsuspecting world.

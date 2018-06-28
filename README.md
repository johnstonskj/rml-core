# Racket Machine Learning - Core

[![GitHub release](https://img.shields.io/github/release/johnstonskj/rml-core.svg?style=flat-square)](https://github.com/johnstonskj/rml-core/releases)
![Travis Status](https://travis-ci.org/johnstonskj/rml-core.svg)
[![Coverage Status](https://coveralls.io/repos/github/johnstonskj/rml-core/badge.svg?branch=master)](https://coveralls.io/github/johnstonskj/rml-core?branch=master)
[![raco pkg install rml-core](https://img.shields.io/badge/raco%20pkg%20install-rml--core-blue.svg)](http://pkgs.racket-lang.org/package/rml-core)
[![Documentation](https://img.shields.io/badge/raco%20docs-rml--core-blue.svg)](http://docs.racket-lang.org/rml-core/index.html)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/rml-core.svg)](https://github.com/johnstonskj/rml-core/stargazers)
![MIT License](https://img.shields.io/badge/license-MIT-118811.svg)

This Package is part of an expected set of packages implementing machine learning capabilities
for Racket. The core of this package is the management of *datasets*, these data sets are assumed
to be for training and testing of machine learning capabilities. This package does not assume
anything about such capabilities, and uses an expansive notion of machine learning that should
cover statistical inferencing, tree and decision matrix models, as well as deep leaning approaches.

This module deals with two opaque structure types, `data-set` and `data-set-field`. These are not
available to clients directly although certain accessors are exported by this module.
Conceptually a `data-set` is a table of data, columns represent fields that are either *features*
that represent properties of an instance, and *classifiers* or *labels* that are used to train
and match instances.

See the `rml-knn` (not quite there yet) repository for an example capability built upon this package.

## Modules

* `data` - manage data sets, load from CSV and JSON files, save and load snapshots as well as
manage partitions and statistics.

## Example

The following example loads a sample data set and displays some useful information before_script
writing a snapshot to the current output port.

```scheme
(require rml/data.rkt)

(define iris-data-set
  (load-data-set "iris_training_data2.csv"
                 'csv
                 (list
                   (make-feature "sepal-length" #:index 0)
                   (make-feature "sepal-width" #:index 1)
                   (make-feature "petal-length" #:index 2)
                   (make-feature "petal-width" #:index 3)
                   (make-classifier "classification" #:index 4))))

(displayln (classifier-product dataset))
(newline)

(displayln (feature-statistics dataset "sepal-width"))
(newline)

(write-snapshot dataset (current-output-port))
(newline)
```

#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          (for-label "../data.rkt"
                     racket/contract
                     math/statistics))

@title[#:tag "ml" #:version "1.0"]{Racket Machine Learning --- Core}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

@defmodule[rml/data  #:use-sources ("rml/data.rkt")]

This Package is part of an expected set of packages implementing machine learning capabilities
for Racket. The core of this package is the management of `datasets', these data sets are assumed
to be for training and testing of machine learning capabilities. This package does not assume
anything about such capabilities, and uses an expansive notion of machine learning that should
cover statistical inferencing, tree and decision matrix models, as well as deep leaning approaches.

This module deals with two opaque structure types, @racket[data-set] and @racket[data-set-field].
These are not available to clients directly although certain accessors are exported by this
module. Conceptually a @racket[data-set] is a table of data, columns represent fields that are
either @italic{features} that represent properties of an instance, and @italic{classifiers} or
@italic{labels} that are used to train and match instances.

You can view the source on @hyperlink[
  "https://github.com/johnstonskj/rml-core"
  "GitHub"].

@;{============================================================================}
@section[#:tag "rml:types"]{Types and Predicates}

@defproc[#:kind "predicate"
         (data-set?
           [a any])
         boolean?]{
Determines whether the value @racket[a] is a @racket[data-set] structure, primarily
used as a contract predicate.
}

@defproc[#:kind "predicate"
         (data-set-field?
           [a any])
         boolean?]{
Determines whether the value @racket[a] is a @racket[data-set-field] structure,
primarily used as a contract predicate.
}

@;{============================================================================}
@section[#:tag "rml:construct"]{Construction}

@defproc[#:kind "io"
         (load-data-set
           [file-name string?]
           [format symbol?]
           [fields (listof data-set-field?)])
         data-set?]{
Returns a new @racket[data-set], with the specified @italic{features} and
@italic{classifiers}, from the specified file.
}

@defthing[#:kind "io" supported-formats (listof symbol?)]{
Returns a list of file formats supported by the @racket[load-data-set] function.
}

@;{============================================================================}
@section[#:tag "rml:access"]{Accessors}

@defproc[#:kind "accessor"
         (classifiers?
           [dataset data-set?])
         (listof string?)]{
The name of all classifier features in the data set.
}

@defproc[#:kind "accessor"
         (classifier-product
           [dataset data-set?])
         (listof string?)]{
A list of the cartesian product, of the set of values for each classifier.
}

@defproc[#:kind "accessor"
         (features?
           [dataset data-set?])
         (listof string?)]{
The name of all `feature' features in the data set.
}

@defproc[#:kind "accessor"
         (data-count
           [dataset data-set?])
         exact-nonnegative-integer?]{
The number of data rows in the data set.
}

@defproc[#:kind "accessor"
         (feature-statistics
           [dataset data-set?]
           [feature-name string?])
         statistics?]{
A @racket[statistics] structure (from @racket[math/statistics]) for the feature @racket[feature-name].
}

@defproc[#:kind "accessor"
         (feature-vector
           [dataset data-set?]
           [partition-id (or/c exact-nonnegative-integer? symbol?)]
           [feature-name string?])
         (vectorof number?)]{
The vector of underlying data for the feature @racket[feature-name].
}

@defproc[#:kind "accessor"
         (partition-count
           [dataset data-set?])
         exact-nonnegative-integer?]{
The number of partitions in the data set.
}

@defproc[#:kind "accessor"
         (partition
           [dataset data-set?]
           [partition-id (or/c exact-nonnegative-integer? symbol?)])
         (vectorof vector?)]{
The partition (vector of feature vectors) data itself.
}

@;{============================================================================}
@section[#:tag "rml:transforms"]{Transformations}

The following procedures perform transformations on one or more @racket[data-set]
structures and return a new @racket[data-set]. These are typically concerned with
partitioning a data set or optimizing the feature vectors.

@defproc[#:kind "transform"
         (partition-equally
           [partition-count exact-positive-integer?]
           [entropy-features (listof string?) '()])
         data-set?]{
TBD
}

@defproc[#:kind "transform"
         (partition-for-test
           [partition-percentage (real-in 1.0 50.0)]
           [entropy-features (listof string?) '()])
         data-set?]{
TBD
}

@defproc[#:kind "transform"
         (standardize
           [features (listof string?)])
         data-set?]{
TBD
}

@defproc[#:kind "transform"
         (fuzzify
           [features (listof string?)])
         data-set?]{
TBD
}

@;{============================================================================}
@section[#:tag "rml:snapshots"]{Snapshots}

Loading and manipulating data sets from source files may not always be efficient
and so the parsed in-memory format can be saved and loaded externally. These
saved forms are termed @italic{snapshots}, they are serialized forms of the
@racket[data-set] structure.

@defproc[#:kind "io"
         (write-snapshot
           [dataset data-set?]
           [out output-port?])
         void?]{
Write a @italic{snapshot} of the data set @racket[dataset] to the output port
@racket[out]. The snapshot also contains a version number representing the data
set structure; this ensures that the snapshot can be read correctly in the
future.
}

@defproc[#:kind "io"
         (read-snapshot
           [dataset data-set?]
           [in input-port?])
         data-set?]{
Read a @italic{snapshot} from the input port @racket[in] and returning a
@racket[data-set] structure. Reading will cause an exception if the data set
version number is incompatible.
}

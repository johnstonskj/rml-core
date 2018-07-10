#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          rml/data
          rml/individual
          (for-label rml/data
                     rml/individual
                     rml/not-implemented
                     racket/contract
                     math/statistics))

@;{============================================================================}

@(define example-eval (make-base-eval
                       '(require rml/data rml/individual rml/results rml/not-implemented)))

@(interaction-eval #:eval example-eval
  (begin
    (define (make-my-classifier a b) (λ (ds i) '("Iris-versicolor")))
    (define iris-data-set
      (load-data-set "test/iris_training_data.csv"
                     'csv
                     (list
                       (make-feature "sepal-length" #:index 0)
                       (make-feature "sepal-width" #:index 1)
                       (make-feature "petal-length" #:index 2)
                       (make-feature "petal-width" #:index 3)
                       (make-classifier "classification" #:index 4))))
    (define an-iris
      (make-individual
        "sepal-length" 6.3
        "sepal-width" 2.5
        "petal-length" 4.9
        "petal-width" 1.5
        "classification" "Iris-versicolor"))))

@;{============================================================================}

@title[#:tag "ml" #:version "1.0"]{Racket Machine Learning --- Core}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

This Package is part of an expected set of packages implementing machine learning capabilities
for Racket. The core of this package is the management of @italic{data sets}, @italic{individuals},
and @italic{results} and the @italic{classification} of individuals against known data sets.

@itemlist[
  @item{@bold{classify} --- Provides algorithm-neutral higher-order functions for classification.}
  @item{@bold{data-set} --- An abstraction to load the features, vector data, and classifiers
      used by learning capabilities.}
  @item{@bold{individual} --- An individual to be classified or otherwise passed to a Learning
      capability.}
  @item{@bold{results} --- A matrix to record the results of training or classification.}
]

This package does not assume anything about specific capabilities, and uses an expansive notion of
machine learning that should cover statistical inferencing, tree and decision matrix models, as
well as deep leaning approaches.

@table-of-contents[]

@;{============================================================================}
@;{============================================================================}
@section[]{Package rml/data}
@defmodule[rml/data]

This module deals with two opaque structure types, @racket[data-set] and @racket[data-set-field].
These are not available to clients directly although certain accessors are exported by this
module. Conceptually a @racket[data-set] is a table of data, columns represent fields that are
either @italic{features} that represent properties of an instance, and @italic{classifiers} or
@italic{labels} that are used to train and match instances.

@examples[ #:eval example-eval
(require rml/data)
(define dataset
  (load-data-set "test/iris_training_data.csv"
                 'csv
                 (list
                   (make-feature "sepal-length" #:index 0)
                   (make-feature "sepal-width" #:index 1)
                   (make-feature "petal-length" #:index 2)
                   (make-feature "petal-width" #:index 3)
                   (make-classifier "classification" #:index 4))))

(displayln (data-set? dataset))

(displayln (features dataset))

(displayln (classifiers dataset))

(displayln (partition-count dataset))

(displayln (data-count dataset))

(displayln (classifier-product dataset))
]

In this code block a training data set is loaded and the columns within the CSV data are
described.

@;{============================================================================}
@subsection[#:tag "rml:data-types"]{Types and Predicates}

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
@subsection[#:tag "rml:data-construct"]{Construction}

@defproc[(load-data-set
           [file-name string?]
           [format symbol?]
           [fields (listof data-set-field?)])
         data-set?]{
Returns a new @racket[data-set], with the specified @italic{features} and
@italic{classifiers}, from the specified file.
}

@defthing[supported-formats (listof symbol?)]{
Returns a list of file formats supported by the @racket[load-data-set] function.
}

@defproc[(make-feature
          [name string?]
          [#:index integer? 0])
         (data-set-field?)]{
Create a new @racket[data-set-field] as a feature, with the name @racket[name], and
the source column index of @racket[index]. The index value is important for formats
that do not support name mapping such as CSV.
}

@defproc[(make-classifier
          [name string?]
          [#:index integer? 0])
         (data-set-field?)]{
Create a new @racket[data-set-field] as a classifier, with the name @racket[name], and
the source column index of @racket[index]. The index value is important for formats
that do not support name mapping such as CSV.
}


@;{============================================================================}
@subsection[#:tag "rml:data-access"]{Accessors}

@defproc[#:kind "accessor"
         (classifiers
           [dataset data-set?])
         (listof string?)]{
The name of all @italic{classifier} features in the data set.
}

@defproc[#:kind "accessor"
         (classifier-product
           [dataset data-set?])
         (listof string?)]{
Returns a list with each row being the cartesian product of the unique values of each classifier
feature. All classifier features are treated as strings and the product is separated by the
Unicode times character "⨉".
}

@defproc[#:kind "accessor"
         (features
           [dataset data-set?])
         (listof string?)]{
The name of all @italic{feature} features in the data set.
}

@defproc[#:kind "accessor"
         (data-count
           [dataset data-set?])
         exact-nonnegative-integer?]{
The number of data rows in the data set, in all partitions.
}

@defproc[#:kind "accessor"
         (feature-vector
           [dataset data-set?]
           [partition-id (or/c exact-nonnegative-integer? symbol?)]
           [feature-name string?])
         (vectorof number?)]{
The vector of underlying data for the feature @racket[feature-name]. Note that @racket[partition-id]
may either be an integer representing the index of the partition (o-based), or one of the following
supported symbols.

@itemlist[
  @item{@racket['default] --- by default no partitioning is done when reading a @racket[data-set].
    This symbol is used to denote the identity of this default, single, partition.}
  @item{@racket['training] --- after a call to @racket[partition-for-test] two partitions are returned.
    This symbol is used to denote the identity of training data.}
  @item{@racket['testing] --- after a call to @racket[partition-for-test] two partitions are returned.
    This symbol is used to denote the identity of test data.}
]
}

@defproc[#:kind "accessor"
         (partition-count
           [dataset data-set?])
         exact-nonnegative-integer?]{
The number of partitions in the data set, when initially created this is usually 1.
}

@defproc[#:kind "accessor"
         (partition
           [dataset data-set?]
           [partition-id (or/c exact-nonnegative-integer? symbol?)])
         (vectorof vector?)]{
The partition (vector of feature vectors) data itself. See @racket[feature-vector] for details of
@racket[partition-id] symbols.
}

@;{============================================================================}
@subsection[]{Statistics}

@defproc[#:kind "accessor"
         (feature-statistics
           [dataset data-set?]
           [feature-name string?])
         statistics?]{
A @racket[statistics] structure (from @racket[math/statistics]) for the feature @racket[feature-name].
}

@defproc[#:kind "transform"
         (standardize
           [features (listof string?)])
         data-set?]{
Standardization requires statistics be computed for all features listed in
@racket[features], and will normalize the values to reduce the effect of large
outlyer values and enable more efficient distance measures.

From @hyperlink["http://www.scholarpedia.org/article/K-nearest_neighbor" "Scholarpedia"]}:

@italic{… removes scale effects caused by use of features with different measurement
scales. For example, if one feature is based on patient weight in units of kg and
another feature is based on blood protein values in units of ng/dL in the range
[-3,3], then patient weight will have a much greater influence on the distance
between samples and may bias the performance of the classifier. Standardization
transforms raw feature values into z-scores using the mean and standard deviation
of a feature values over all input samples}
}

@;{============================================================================}
@subsection[#:tag "rml:data-transforms"]{Transformations}

The following procedures perform transformations on one or more @racket[data-set]
structures and return a new @racket[data-set]. These are typically concerned with
partitioning a data set or optimizing the feature vectors.

@defproc[#:kind "transform"
         (partition-equally
           [partition-count exact-positive-integer?]
           [entropy-features (listof string?) '()])
         data-set?]{
Return a new @racket[data-set] that attempts to partition the original data into
@racket[partition-count] equal groups (equal in number of rows in their feature
vectors). If specified, the @racket[entropy-features] list denotes the names of
features, or classifiers, that should be randomly spread across partitions.
}

@defproc[#:kind "transform"
         (partition-for-test
           [test-percentage (real-in 1.0 50.0)]
           [entropy-features (listof string?) '()])
         data-set?]{
Return a new @racket[data-set] that attempts to partition the original data into
two new partitions with @racket[test-percentage] of rows separated out to act as
test data and the remainder the training data.

If specified, the @racket[entropy-features] list denotes the names of
features, or classifiers, that should be randomly spread across partitions.
}

@;{============================================================================}
@subsection[#:tag "rml:data-snapshots"]{Snapshots}

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

@;{============================================================================}
@;{============================================================================}
@section[]{Package rml/individual}
@defmodule[rml/individual]

This module implements a single type, @racket[individual], that is simply a
constrained @racket[hash]. The hash is constrained to have keys that are all
@racket[string]s, and should have the same keys as the union of feature and
classifier names from the data set it will be used with. This module also
provides a generator to return individuals for all rows in a partition.

@examples[ #:eval example-eval
(require rml/individual)
(define iris
  (make-individual
    "sepal-length" 6.3
    "sepal-width" 2.5
    "petal-length" 4.9
    "petal-width" 1.5
    "classification" "Iris-versicolor"))

(displayln (individual? iris))

(displayln (hash-keys iris))
]

This code block shows the creation of a simple @racket[individual] matching the
Iris data set.

@;{============================================================================}
@subsection[#:tag "rml:ind-types"]{Types and Predicates}

@defproc[#:kind "predicate"
         (individual?
           [a any/c])
         boolean?]{
Returns @racket[#t] if the value @racket[a] is an @racket[individual].
}

@;{============================================================================}
@subsection[#:tag "rml:ind-construct"]{Construction}

@defproc[#:kind "constructor"
         (make-individual [key any/c] [val any/c] ... ...
                          [#:data-set data-set? #f])
                          individual?]{
Creates an immutable @racket[individual] in much the same way as the standard @racket[hash]
procedure, taking an even number of parameters assumed to be a repeating pair of
@italic{key value}. In this case however all keys will be checked to ensure they are
strings.

If specified, the value for the keyword parameter @racket[#:data-set] will be
used to validate the names of the individual against the names of features and
classifiers in the corresponding data set.
}

@defproc[#:kind "constructor"
         (data-set-individual
           [dataset data-set?])
         individual?]{
Creates a mutable @racket[individual] by taking all the names from the features
and classifiers from @racket[dataset]. All values are initialized to @racket[#f].

@examples[ #:eval example-eval
(define blank-iris (data-set-individual dataset))

(displayln (individual? blank-iris))

(displayln (hash-keys blank-iris))
]
}

@;{============================================================================}
@subsection[#:tag "rml:ind-generate"]{Partition Generator}

@defthing[no-more-individuals symbol?]{
A symbol that acts as the @italic{stop-value} for @racket[individuals].
}

@defproc[#:kind "generator"
         (individuals
           [dataset data-set?]
           [partition-id exact-nonnegative-integer?])
         generator?]{
This procedure implements a generator and returns each row of a partition as an
@racket[individual].

@examples[ #:eval example-eval
(for ([row (in-producer (individuals dataset 0) no-more-individuals)])
     (displayln row))
]
}

@;{============================================================================}
@;{============================================================================}
@section[]{Package rml/classify}
@defmodule[rml/classify]

This module provides higher order functions to run classifiers over data sets.
Specific @italic{algorithm} modules are expected to provide classifier functions
that this module can use over data sets.

@examples[#:eval example-eval
(require rml/data rml/individual rml/classify)
(define my-classifier (make-my-classifier 5 95.0))
(displayln (classify iris-data-set an-iris my-classifier))
]

In this example we create a classifier using the algorithm-specific function
@racket[make-my-classifier] and use it in the call to @racket[classify] to
predict classification values for the individual @racket[an-iris].

@;{============================================================================}

@defthing[classifier/c contract?]{
Supplies a contract that defines classifier functions that are used by the higher
order functions in this module. Typically one would expect that an algorithm
provider would include a factory function, of the form
@racket[(-> args ... classifier/c)].
}

@defproc[(classify
           [dataset data-set?]
           [individual individual?]
           [classifier classifier/c])
         list?]{
This procedure will return a list of classifier values predicted for the provided
@racket[individual] based on the specific algorithm implemented by @racket[classifier].
}

@;{============================================================================}
@subsection[]{Partitioned Classification}

@defproc[(partitioned-test-classify
           [dataset data-set?]
           [train-percentage (real-in 1.0 50.0)]
           [classifier classifier/c])
         result-matrix?]{
This form of training uses the @racket[partition-for-test] procedure to create two
partitions, a training data partition and a test data partition. It then classifies
all the individuals in the test partition against the training partition and records
the results in a @racket[result-matrix]. The result matrix can be inspected to determine
the accuracy of the classifier.
}

@defproc[(cross-classify
           [dataset data-set?]
           [partition-count exact-positive-integer?]
           [classifier classifier/c])
         result-matrix?]{
This form of training uses the @racket[partition-equally] procedure to create
@racket[partition-count] partitions. Each partition is then classified against
all the others and the results are collated into a single @racket[result-matrix].
The result matrix can then be inspected to determine the accuracy of the classifier.
}

@;{============================================================================}
@;{============================================================================}
@section[]{Package rml/results}
@defmodule[rml/results]

This packages implements a results matrix, sometimes referred to as a @italic{confusion
matrix} to record the outcome of classification operations. Specifically the matrix
uses all distinct classification values (ω) as both rows and columns and maps the
predicted values to the labeled values incrementing the integer value at the intersection
for each operation.

@examples[ #:eval example-eval
; Create a result matrix from the Iris data set
(define results (make-result-matrix dataset))
; Display the (empty) matrix
(for ([row (result-matrix-formatted results)])
  (displayln row))
]

@;{============================================================================}
@subsection[#:tag "rml:res-types"]{Types and Predicates}

@defproc[#:kind "predicate"
         (result-matrix?
           [a any/c])
         boolean?]{
Returns @racket[#f] if the value of @racket[a] is a @racket[result-matrix].
}

@;{============================================================================}
@subsection[#:tag "rml:res-matrix"]{Construction}

@defproc[#:kind "constructor"
         (make-result-matrix
           [dataset data-set?])
         result-matrix?]{
Create a new @racket[result-matrix]}  using the values provided by
@code[#:lang "racket"]|{(classifier-product dataset)}| as row and column
indices.

@;{============================================================================}
@subsection[#:tag "rml:res-record"]{Recording Results}

@defproc[(record-result
          [C result-matrix?]
          [true-ω any/c]
          [predicted-ω any/c])
         result-matrix?]{
Increment the result count for in the @racket[result-matrix] for the combination
of predicted and true/known classifiers.
}

@defproc[(result-value
          [C result-matrix?]
          [true-ω any/c]
          [predicted-ω any/c])
         integer?]{
Return the current result count in @racket[result-matrix] for the combination
of predicted and true/known classifiers.
}

@defproc[#:kind "accessor"
         (result-matrix-formatted)
         result-matrix?]{
Return a formatted version of the result matrix with label data suitable for display
or printing.
}

@;{============================================================================}
@;{============================================================================}
@section[]{Package rml/not-implemented}
@defmodule[rml/not-implemented]

This is a common utility module, it provides a single procedure,
@racket[raise-not-implmented], and a single predicate @racket[exn:fail:not-implmented?].
These provide a simple way to mark incomplete procedures in the current implementation
of this, or any related package.

@examples[ #:eval example-eval
(require rml/not-implemented)

(define (fuzzify data-set features)
  (raise-not-implemented))

(fuzzify dataset '())
]


@defproc[#:kind "predicate"
         (exn:fail:not-implmented?
          [a any/c])
         boolean?]{
Returns @racket[#t] if the value of @racket[a] is an instance of the
@racket[exn:fail:not-implmented] exception.
}

@defform[(raise-not-implemented)]{
Raises a @racket[exn:fail:not-implmented] exception.
}

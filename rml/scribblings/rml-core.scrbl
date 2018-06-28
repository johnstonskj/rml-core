#lang scribble/manual

@require racket/sandbox
          scribble/eval
          [@for-label[rml
                      racket/base]]

@title[#:tag "ml" #:version "1.0"]{Package rml-core}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

@defmodule[rml  #:use-sources ("../data.rkt")]

This Package is part of an expected set of packages implementing machine learning capabilities
for Racket. The core of this package is the management of 'datasets', these datasets are assumed
to be for training and testing of machine learning capabilities. This package does not assume 
anything about the capabilities, and uses an expansive notion of machine learning that should
cover statistical inferencing, tree and decision matrix models, as well as deep leaning approaches.

@hyperlink[
  "https://github.com/johnstonskj/rml-core"
  "GitHub"].

@;{============================================================================}
@section[#:tag "rml:types"]{Types and Predicates}

@;{============================================================================}
@section[#:tag "rml:construct"]{Construction}

@;{============================================================================}
@section[#:tag "rml:access"]{Accessors}

@;{============================================================================}
@section[#:tag "rml:transforms"]{Transformations}

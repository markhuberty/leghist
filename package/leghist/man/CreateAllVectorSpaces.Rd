\name{CreateAllVectorSpaces}
\alias{CreateAllVectorSpaces}
\title{CreateAllVectorSpaces}
\usage{
  CreateAllVectorSpaces(doc.initial, doc.final,
    amendments = NULL, ngram.min = 1, ngram.max = 1,
    stem = FALSE, rm.stopwords = FALSE,
    rm.whitespace = FALSE, rm.punctuation = FALSE,
    filter = NULL, filter.thres = NULL,
    weighting = weightTf)
}
\arguments{
  \item{doc.initial}{the first version of a document.}

  \item{doc.final}{the final version of the same document.}

  \item{amendments}{an optional list of proposed changes to
  the initial document.}

  \item{ngram.min}{the minimum-length ngram to use in the
  document-term matrix}

  \item{ngram.max}{the maximum-length ngram to use in the
  document-term matrix}

  \item{stem}{should words in the documents be stemmed?}

  \item{rm.stopwords}{boolean, should english stopwords be
  removed?}

  \item{rm.whitespace}{boolean, should excess whitespace be
  stripped?}

  \item{rm.punctuation}{boolean, should punctuation be
  removed?}

  \item{filter}{one of 'NULL', 'sparse', 'tf' and 'tfidf'
  specifying the base value for filtering.}

  \item{filter.thres}{numeric, indicating filter threshold
  appropriate for the filter chosen.}

  \item{weighting}{one of weightTf, weightTfIdf, or
  weightBin.}

  \item{ngram}{the length of the word set that should be
  used for the document-term matrix (1-grams are single
  words, 2-grams are unique 2-word combinations, etc).}
}
\value{
  a list of document-term matrices, for the initial and
  final documents and any proposed amendments, formatted as
  sparse Matrix objects. The terms in each matrix are
  consistent with the set of unique terms in the final
  document.
}
\description{
  Creates the document-term vector space representations of
  the initial and final documents, and any (optional)
  amendments. Vector space representations are baselined to
  a common dictionary based on the final document.
}
\author{
  Mark Huberty
}


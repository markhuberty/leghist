\name{CreateVectorSpace}
\alias{CreateVectorSpace}
\title{CreateVectorSpace}
\usage{
  CreateVectorSpace(docs, ngram.min, ngram.max,
    stem = FALSE, dictionary = NULL, rm.stopwords = FALSE,
    rm.whitespace = FALSE, rm.punctuation = FALSE,
    filter = NULL, filter.thres = NULL,
    weighting = weightTf)
}
\arguments{
  \item{docs}{a string vector of documents.}

  \item{ngram.min}{the minimum-length ngram to use in the
  document-term matrix}

  \item{ngram.max}{the maximum-length ngram to use in the
  document-term matrix}

  \item{stem}{boolean, should the document be stemmed?}

  \item{dictionary}{character vector of terms on which to
  base the document-term matrix. Terms not in the
  dictionary will be dropped.See the tm package for
  documentation.}

  \item{rm.stopwords}{boolean, should stopwords be
  removed?}

  \item{rm.whitespace}{boolean, should excess whitespace be
  removed?}

  \item{rm.punctuation}{boolean, should punctuation be
  removed?}

  \item{filter}{one of 'NULL', 'sparse', 'tf' and 'tfidf'
  specifying the base value for filtering.}

  \item{filter.thres}{numeric, indicating filter threshold
  appropriate for the filter chosen.}

  \item{weighting}{one of weightTf, weightTfIdf, or
  weightBin}
}
\value{
  a document-term matrix in sparse representation where
  each row is a document and each col is an ngram. Cell
  values are term frequency counts.
}
\description{
  Creates the vector space model of a document.
}
\author{
  Mark Huberty
}


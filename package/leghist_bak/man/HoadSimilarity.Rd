\name{HoadSimilarity}
\alias{HoadSimilarity}
\title{HoadSimilarity}
\usage{
  HoadSimilarity(vec.d, vec.q, N, ft)

  HoadSimilarity(vec.d, vec.q, N, ft)
}
\arguments{
  \item{dtm}{a document-term matrix, such as is output from
  CreateAllVectorSpaces(), containing the term frequency
  vectors of both the documents for which matches are
  needed, and the set of candidate match documents D.}

  \item{idx.query}{the indices in dtm of the documents
  being matched}

  \item{idx.compare}{the indices in dtm of the potential
  matches}

  \item{idx.collection}{a vector of indices indicating
  which rows in dtm are for the set of comparison documents
  D (as opposed to the documents requiring matches.}

  \item{vec.d}{a term-freqency vector from a set of
  candidate matches D.}

  \item{vec.q}{a term-frequency vector of a document being
  matched to candidates in D.}

  \item{N}{The number of documents in D.}

  \item{ft}{The number of documents in D containing term
  t.}
}
\value{
  a similarity matrix of dimension idx.query * idx.compare,
  containing the output of similiarty()

  a numeric similarity measure, where larger numbers
  indicate more similar documents.
}
\description{
  A wrapper function for HoadSimilarityCore() that takes a
  standard set of inputs and handles pre-processing for the
  outputs

  Computes similarity measure #5 from Hoad & Zobel (2003).
}
\author{
  Mark Huberty

  Mark Huberty
}


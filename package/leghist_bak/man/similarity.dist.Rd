\name{similarity.dist}
\alias{similarity.dist}
\title{similarity.dist}
\usage{
  similarity.dist(dtm, idx.query, idx.compare,
    idx.collection)
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
}
\value{
  a similarity matrix of dimension idx.query * idx.compare,
  containing the output of similiarty()
}
\description{
  A wrapper function for similarity() that takes a standard
  set of inputs and handles pre-processing for the outputs
}
\author{
  Mark Huberty
}


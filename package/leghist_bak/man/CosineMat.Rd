\name{CosineMat}
\alias{CosineMat}
\title{CosineMat}
\usage{
  CosineMat(dtm, idx.query, idx.compare, idx.collection)
}
\arguments{
  \item{dtm}{A document-term matrix as output from
  CreateAllVectorSpaces}

  \item{idx.query}{an integer index vector indicating the
  rows in dtm corresponding to a set of target documents
  for which matches are desired}

  \item{idx.compare}{an integer index vector indicating
  rows in dtm corresponding to potential matches for the
  target documents}

  \item{idx.collection}{all rows in dtm corresponding to
  potential matches}
}
\value{
  A matrix of dimension idx.query * idx.compare, with
  values as the pairwise cosine similarity
}
\description{
  Vectorized version the cosine similarity measure. This
  computes the entire cosine similarity between two
  conformable row-major matrices at once using matrix
  algebra
}
\author{
  Mark Huberty
}


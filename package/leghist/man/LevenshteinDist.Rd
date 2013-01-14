\name{LevenshteinDist}
\alias{LevenshteinDist}
\title{LevenshteinDist}
\usage{
  LevenshteinDist(dtm, idx.query, idx.compare,
    idx.collection)
}
\arguments{
  \item{dtm}{a leghistCVS object as output from
  CreateAllVectorSpaces}

  \item{idx.query}{the index of the final bill in the
  corpus}

  \item{idx.compare}{the index of potential matches to the
  final bill}

  \item{idx.collection}{all entries in the corpus
  corresponding to potential matches}
}
\value{
  A distance matrix of form idx.query * idx.compare
}
\description{
  Computes the levenshtein distance between each element of
  the a character vector of query documents and an entire
  vector of sources. Supports use of a parallel backend via
  the foreach() library to speed computation for large data
  sizes. Produces output compatible with the MapBills
  function and can be passed as dist.fun.
}
\author{
  Mark Huberty
}


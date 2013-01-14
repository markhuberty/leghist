\name{MapFun}
\alias{MapFun}
\title{MapFun}
\usage{
  MapFun(cvsobject, distance.fun = "CosineMat", idx.final,
    idx.compare, idx.collection, filter.fun)
}
\arguments{
  \item{cvsobject}{an object of class leghistCVS, as output
  by CreateAllVectorSpaces}

  \item{distance.fun}{a distance or similarity function. It
  may either use the "vs.out" object (for bag-of-words
  based metrics) or the "corpus" object (for string-based
  metrics). It should return a matrix of form idx.final *
  idx.compare.}

  \item{idx.final}{an integer index indicating the location
  of the target documents to be matched to}

  \item{idx.compare}{an integer index vector indicating
  rows in dtm corresponding to potential matches for the
  target documents}

  \item{idx.collection}{all rows in dtm corresponding to
  potential matches (e.g., initial + amendments if both are
  present)}

  \item{filter.fun}{one of "min" or "max", indicating how
  the best match should be chosen. The choice should depend
  on whether distance.fun returns returns distance (min) or
  similarity (max)}
}
\value{
  a 3-column matrix of form idx.query:idx.match:distance
}
\description{
  Does the actual pairwise mapping of bills. Maps from
  doc1:doc2 via nearest-neighbor matching on the basis of a
  user-supplied distance / similarity function.
}
\author{
  Mark Huberty
}


\name{MapBills}
\alias{MapBills}
\title{MapBills}
\usage{
  MapBills(cvsobject, distance.fun = "CosineMat",
    filter.fun = "max")
}
\arguments{
  \item{cvsobject}{The output from from
  CreateAllVectorSpaces().}

  \item{distance.fun}{A similarity or distance function,
  which can take two vectors and return a matrix. See
  CosineMat() for details of the return format. metric
  should return larger values for more similar objects.}

  \item{filter.fun}{one of "min" or "max", indicating how
  the best match should be chosen. The choice should depend
  on whether distance.fun returns returns distance (min) or
  similarity (max)}
}
\value{
  A matrix mapping from the sections of the final document
  to the sections of both the initial document and any
  proposed amendments, with distance values for each
  matched pair.
}
\description{
  Maps the final bill to both the original bill and any
  proposed amendments. Returns a matrix that maps from the
  final bill to the initial bill and (if supplied) the
  amendments, with distance metrics for the best match from
  each source.
}
\author{
  Mark Huberty
}


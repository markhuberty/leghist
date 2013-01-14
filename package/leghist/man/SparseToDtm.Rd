\name{SparseToDtm}
\alias{SparseToDtm}
\title{SparseToDtm}
\usage{
  SparseToDtm(sparseM, weighting = weightTf)
}
\arguments{
  \item{sparseM}{: a sparse Matrix of form dgCMatrix.}

  \item{weighting}{one of weightTf, weightTfIdf, or
  weightBin}
}
\value{
  a simple_triplet_matrix as described in the slam package,
  with the same dimensions and properties as sparseM.
}
\description{
  Helper function to translate a sparse Matrix into a
  document-term matrix equivalent to that produced by the
  tm package.
}
\author{
  Mark Huberty
}


\name{DtmToMatrix}
\alias{DtmToMatrix}
\title{DtmToMatrix}
\usage{
  DtmToMatrix(dtm)
}
\arguments{
  \item{dtm}{a document-term matrix as output from the
  DocumentTermMatrix from the tm() package.}
}
\value{
  an equivalent sparse matrix as represented in the Matrix
  package.
}
\description{
  Converts a DocumentTermMatrix from the tm() package into
  a sparse Matrix. This is the same function as John Myles
  White included in his textregression() library.
}
\author{
  Mark Huberty
}


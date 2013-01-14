\name{cosine.dist}
\alias{cosine.dist}
\title{cosine.dist}
\usage{
  cosine.dist(dtm, idx.dtm1, idx.dtm2, idx.collection)
}
\arguments{
  \item{dtm}{a document-term matrix, such as is output from
  CreateAllVectorSpaces(), containing the term frequency
  vectors of both the documents for which matches are
  needed, and the set of candidate match documents D}

  \item{idx.dtm1}{a row index indicating the location of
  the first document.}

  \item{idx.dtm2}{a row indiex indicating the location of a
  second document.}

  \item{idx.collection}{a vector of indices indicating
  which rows in dtm are for the set of comparison documents
  D (as opposed to the documents requiring matches.}
}
\value{
  the output of cosine()
}
\description{
  A wrapper function for the cosine() function in the lsa
  package, which allows it to work with the MapBills
  function.
}
\author{
  Mark Huberty
}


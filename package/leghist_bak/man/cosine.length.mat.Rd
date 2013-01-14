\name{cosine.length.mat}
\alias{cosine.length.mat}
\title{cosine.length.mat}
\usage{
  cosine.length.mat(dtm, idx.query, idx.compare,
    idx.collection)
}
\arguments{
  \item{dtm}{a document-term matrix}

  \item{idx.query}{the indices of one set of documents in
  dtm}

  \item{idx.compare}{the indices of the set of comparison
  documents in dtm}

  \item{idx.collection}{the indices of all non-query
  documents in dtm}
}
\value{
  A similarity matrix of dimension idx.query * idx.compare
}
\description{
  Provides a length-weighted cosine similarity measure
}
\author{
  Mark Huberty
}


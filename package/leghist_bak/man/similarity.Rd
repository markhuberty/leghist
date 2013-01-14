\name{similarity}
\alias{similarity}
\title{similarity}
\usage{
  similarity(vec.d, vec.q, N, ft)
}
\arguments{
  \item{vec.d}{a term-freqency vector from a set of
  candidate matches D.}

  \item{vec.q}{a term-frequency vector of a document being
  matched to candidates in D.}

  \item{N}{The number of documents in D.}

  \item{ft}{The number of documents in D containing term
  t.}
}
\value{
  a numeric similarity measure, where larger numbers
  indicate more similar documents.
}
\description{
  Computes similarity measure #5 from Hoad & Zobel (2003).
}
\author{
  Mark Huberty
}


\name{GetWordsToHightlight}
\alias{GetWordsToHightlight}
\title{GetWordsToHighlight}
\usage{
  GetWordsToHightlight(vector1, vector2, terms)
}
\arguments{
  \item{vector1}{a term-frequency vector of integers
  representing one document}

  \item{vector2}{a term-frequency vector of integers
  representing a second document with the same dictionary
  and term ordering}

  \item{terms}{the terms represented by the vectors}
}
\value{
  A list of terms found in vector 1 but not vector 2
}
\description{
  Returns the set difference terms from two term-frequency
  vectors
}
\author{
  Mark Huberty
}


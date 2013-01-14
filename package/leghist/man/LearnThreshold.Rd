\name{LearnThreshold}
\alias{LearnThreshold}
\title{LearnThreshold}
\usage{
  LearnThreshold(map.bills.out, initial.bill, final.bill,
    amendments, labels, filter = "max", threshold.values,
    encoder.out, type = "overall")
}
\arguments{
  \item{map.bills.out}{the output of MapBills for this
  document.}

  \item{initial.bill}{the text of the initial bill.}

  \item{final.bill}{the text of the final bill.}

  \item{amendments}{amendment text, in the same order as
  was passed to MapBills.}

  \item{labels}{the origin of the amendments.}

  \item{filter}{one of "min" or "max" depending on the
  distance function used in MapBills.}

  \item{threshold.values}{a numeric vector of potential
  threshold values. See GetLikelyComposite for the
  definition of the threshold value. A suitably granular
  vector is recommended, as in seq(0,0.5, 0.005)}

  \item{encoder.out}{the output of RunEncoder for the
  human-coded matches of these documents.}

  \item{type}{one of "overall" (overall accuracy) or
  "tradeoff" (false positive/negative).}
}
\value{
  a list containing the entire output of the algorithm and
  the best threshold value.
}
\description{
  Provides a function interface for learning the best match
  quality threshold from a human-coded document. Given a
  mapping of source to target document and a sequence of
  threshold values, it will return the optimum based on
  either maximization of the accuracy rate or miniminzation
  of the false positive/negative rate.
}
\author{
  Mark Huberty
}


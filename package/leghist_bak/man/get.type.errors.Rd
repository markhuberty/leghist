\name{get.type.errors}
\alias{get.type.errors}
\title{get.type.error}
\usage{
  get.type.errors(map.bills.out, initial.bill, final.bill,
    amendments, labels, filter = "max", threshold.values,
    encoder.out)
}
\arguments{
  \item{map.bills.out}{the output of MapBills}

  \item{initial.bill}{the character vector representation
  of the initial bill}

  \item{final.bill}{the character vector representation of
  the final bill}

  \item{amendments}{the character vector representation of
  the amendments}

  \item{labels}{committee labels for the amendments}

  \item{filter}{one of min or max, depending on the use of
  a distance or similarity metric}

  \item{threshold.values}{a vector of similarity
  thresholds, as in seq(0, 0.5, 0.005)}

  \item{encoder.out}{the output of run.encoder for the bill
  in question, using the same bill and amendment arguments
  in the same order}
}
\value{
  Type 1 and Type 2 accuracy rates by threshold value
}
\description{
  Optimizes the tradeoff between false negative values
  (rejecting matches that should have been matched to
  source documents) and false positive values (accepting
  matches for which no match existed), on the basis of the
  threshold value. See learn.threshold for more detail.
}
\author{
  Mark Huberty
}


\name{get.accuracy.measures}
\alias{get.accuracy.measures}
\title{get.accuracy.measures
##'}
\usage{
  get.accuracy.measures(map.bills.out, initial.bill,
    final.bill, amendments, labels, filter = "max",
    threshold.values, encoder.out)
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
  Source and source+index accuracy of the automated match,
  compared with the hand-coded version
}
\description{
  Calculates the overall accuracy rate by threshold value
  for a set of documents based on a human-coded set of
  matches. See learn.threshold() for more details.
}
\author{
  Mark Huberty
}


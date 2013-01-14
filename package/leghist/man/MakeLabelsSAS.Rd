\name{MakeLabelsSAS}
\alias{MakeLabelsSAS}
\title{MakeLabelsSAS}
\usage{
  MakeLabelsSAS(amends.idx, a, f, labels = NULL)
}
\arguments{
  \item{amends.idx}{1:a, where a = the number of
  amendments.}

  \item{a}{The number of amendments.}

  \item{f}{the number of paragraphs in the final bill.}

  \item{labels}{an optional vector of labels which the user
  may supply.}
}
\value{
  A vector of labels for a SAS graph.
}
\description{
  Creates a vector of node (vertex) labels for a
  PlotAmendsSuccess() graph.
}


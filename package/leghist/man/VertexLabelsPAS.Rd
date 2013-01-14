\name{VertexLabelsPAS}
\alias{VertexLabelsPAS}
\title{VertexLabelsPAS}
\usage{
  VertexLabelsPAS(a, f, a.lab, f.lab, vertex.label = NULL,
    rejected.label = NULL)
}
\arguments{
  \item{a}{The number of amendments.}

  \item{f}{the number of paragraphs in the final bill.}

  \item{a.lab}{the number of visible labels on the bottom
  amendments tier.}

  \item{f.lab}{the number of visible labels on the top
  final bill tier.}

  \item{vertex.label}{an optional vector of labels which
  the user may supply.}

  \item{rejected.label}{character, the rejected node's
  label. If left NULL, the label will be "Rejected".}
}
\value{
  A vector of labels for a PAS graph.
}
\description{
  Creates a vector of node (vertex) labels for a
  PlotAmendsSuccess() graph.
}
\author{
  Hillary Sanders
}


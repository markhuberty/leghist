\name{VertexSizes}
\alias{VertexSizes}
\title{VertexSizes}
\usage{
  VertexSizes(A, num.com, num.top, scale.c, scale.t,
    scale.fin)
}
\arguments{
  \item{A}{ax4 information matrix}

  \item{num.com}{number of committees}

  \item{num.top}{number of topics}

  \item{scale.c}{Size scale for committee nodes.}

  \item{scale.t}{Size scale for topic nodes.}

  \item{scale.fin}{Size scale for final destination nodes:
  "Final" and "Junk".}
}
\value{
  A matrix of dimension a by 2. The first column is a
  vector of node sizes for each vertex in the graph., the
  second is a vector of second node sizes (e.g. for
  rectanges).
}
\description{
  A function called within PlotCommitteeTopics() to
  calculate vertex (node) sizes.
}


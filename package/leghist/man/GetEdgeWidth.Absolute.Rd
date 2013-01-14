\name{GetEdgeWidth.Absolute}
\alias{GetEdgeWidth.Absolute}
\title{GetEdgeWidth.Absolute}
\usage{
  GetEdgeWidth.Absolute(x, A, num.arrows.to.topics)
}
\arguments{
  \item{x}{A vector of length three, for the ith edge,
  indicating which node the edge came from, where it is
  going, and its index.}

  \item{A}{An ax4 matrix, where a = number of amendments.
  Each row represents an amendment: its index (on of 1:a),
  it's committee (one of 1:c), its topic (one of 1:t), and
  its final destination (rejected or final bill: 0 or 1).
  See PlotCommitteeTopics() for more details.}

  \item{num.arrows.to.topics}{The number of distinct edges
  (arrows) that are going to topic nodes (the middle layer)
  in the PlotCommitteeTopics() plot.}
}
\value{
  the width of the x[3]th edge (arrow) according to the
  absolute number of amendments represented by the given
  edge.
}
\description{
  A small function called within PlotCommitteeTopics() to
  calculate edge widths (arrow widths) for
  PlotCommitteeTopics(), where the edge.width argument is
  set to "absolute" (default). Similar to
  GetEdgeWidth.Relative and GetEdgeWidth.Success.
}
\author{
  Hillary Sanders
}


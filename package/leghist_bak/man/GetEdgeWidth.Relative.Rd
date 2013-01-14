\name{GetEdgeWidth.Relative}
\alias{GetEdgeWidth.Relative}
\title{GetEdgeWidth.Relative}
\usage{
  GetEdgeWidth.Relative(x, A, num.arrows.to.topics)
}
\arguments{
  \item{x}{A vector of length three, for the ith edge,
  indicating which node the edge came from, where it is
  going, and its index.}

  \item{A}{An ax4 matrix, where a = number of amendments.
  Each row represents an amendment: its index (on of 1:a),
  it's committee (one of 1:c), its topic (one of 1:t), and
  its final destination (junk or final bill: 0 or 1). See
  PlotCommitteeTopics() for more details.}

  \item{num.arrows.to.topics}{The number of distinct edges
  that are going to topic nodes (the middle layer) in the
  PlotCommitteeTopics() plot.}
}
\value{
  the width of the x[3]th edge, according to the percentage
  of amendments that the the given edge is carrying with
  respect to the total number of amendments coming from the
  edge's source (either a committee or topic node).
}
\description{
  A small function called within PlotCommitteeTopics() if
  the argument edge.width is set to "relative". This
  function creates edge (arrow) widths for
  PlotCommitteeTopics() relative to an edge's origin.
  Similar to GetEdgeWidth.Absolute() and
  GetEdgeWidth.Success().
}
\author{
  Hillary Sanders
}


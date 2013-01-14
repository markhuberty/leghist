\name{GetEdgeWidth.Success}
\alias{GetEdgeWidth.Success}
\title{GetEdgeWidth.Success}
\usage{
  GetEdgeWidth.Success(x, A, num.arrows.to.topics, num.com,
    num.top)
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

  \item{num.com}{The number of committees in the PCT graph
  to be plotted.}

  \item{num.top}{The number of topics in the PCT graph to
  be plotted.}
}
\value{
  the width of the x[3]th edge (arrow), according to the %
  of successful amendments the edge carries (with respect
  to all of the amendments it carries).
}
\description{
  A small function called within PlotCommitteeTopics() if
  the argument edge.width is set to "success". Creates edge
  (arrow) widths for PlotCommitteeTopics() based on the
  percentage of successful (headed for the final bill)
  amendments in the given edge. PlotCommitteeTopics().
}
\author{
  Hillary Sanders
}


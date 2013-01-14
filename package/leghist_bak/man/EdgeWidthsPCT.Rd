\name{EdgeWidthsPCT}
\alias{EdgeWidthsPCT}
\title{EdgeWidthsPCT}
\usage{
  EdgeWidthsPCT(A, num.com, num.top,
    edge.width = "absolute", edge.width.scale = 1)
}
\arguments{
  \item{A}{A matrix (created inside PlotCommitteeTopics())
  of dimensions a by 4, where a = the number of amendments.
  The columns 1:4 respectively indicate amendment index,
  committee, topic, and logical success or failure. Each
  row corresponds to one non-discarded (so rejected and
  accepted) amendment.}

  \item{num.com}{The number of committees in the PCT graph
  to be plotted.}

  \item{num.top}{The number of topics in the PCT graph to
  be plotted.}

  \item{edge.width}{The method used to calculate edge
  widths. The default, "absolute", means that edge widths
  will correspond to the absolute number of amendments they
  represent. "relative" means that edge widths will
  correspond to the % of amendments each edge holds with
  respect to the node (vertex) they are coming from.
  "success" means edge widths will correspond to the % of
  amendments in each edge which are destined for the final
  bill (with respect to the total number of amendments each
  edge is carrying).}

  \item{edge.width.scale}{Default = 1. Thicker edges =
  bigger number.}
}
\value{
  A vector of edge widths for each arrow to be drawn.
}
\description{
  A function called within PlotCommitteeTopics() to
  calculate all edge widths.
}
\author{
  Hillary Sanders
}


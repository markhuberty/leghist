\name{LayoutSCT}
\alias{LayoutSCT}
\title{LayoutSCT}
\usage{
  LayoutSCT(x, num.com, num.top, mid.layer = 0.6)
}
\arguments{
  \item{x}{the index of the coordinates to be created.}

  \item{num.com}{the number of committees (number of nodes
  wanted in the bottom layer of the graph).}

  \item{num.top}{the number of topics (number of nodes
  wanted in the middle layer of the graph).}

  \item{mid.layer}{The y-axis placement of the middle layer
  on the graph. Defaults to .6. Note that the bottom and
  top layers are at 0 and 1.}
}
\value{
  the xth pair of coordinates for the default layout of
  PlotCommitteeTopics().
}
\description{
  Creates the "x"th layout coordinates for
  PlotCommitteeTopics(). This function is called inside of
  PlotCommitteeTopics() to create the layout: three layers
  consisting of 1) committees (c of them), 2) topics (t of
  them), and the final destinations of the amendments (junk
  and final).
}
\author{
  Hillary Sanders
}


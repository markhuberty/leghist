\name{EdgeColorSCT}
\alias{EdgeColorSCT}
\title{EdgeColorSCT}
\usage{
  EdgeColorSCT(A, num.com, num.top, edge.col = NULL,
    edge.transparency = NULL)
}
\arguments{
  \item{A}{An ax4 matrix, where a = number of amendments.
  Each row represents an amendment: its index (on of 1:a),
  it's committee (one of 1:c), its topic (one of 1:t), and
  its final destination (junk or final bill: 0 or 1). See
  PlotCommitteeTopics() for more details.}

  \item{num.com}{number of committees}

  \item{num.top}{number of topics}

  \item{edge.col}{optional vector of colors (length 2).}

  \item{edge.transparency}{Optional integer in 10:99
  designating level of transparency}
}
\value{
  A vector of edge widths for each arrow to be drawn
}
\description{
  A function called within PlotCommitteeTopics() to
  calculate edge colors.
}


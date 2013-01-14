\name{VertexColorsPAS}
\alias{VertexColorsPAS}
\title{VertexColorsPAS}
\usage{
  VertexColorsPAS(a, f, merged, vertex.color.by,
    vertex.rejected.color, vertex.col = NULL)
}
\arguments{
  \item{a}{number of amendments.}

  \item{f}{number of paragraphs in the final bill.}

  \item{merged}{output of OutToInPAS}

  \item{vertex.color.by}{Should vertex colors be decided by
  an amendment's topic assigment or committee? "t",
  "topics", or "c", "committees".}

  \item{vertex.col}{Optional vector of colors.}
}
\value{
  A list of length two, consisting of: 1) a vector of all
  vertices' colors, and 2) a (shorter) vector of the
  individual colors used.
}
\description{
  A function called within PlotAmendsSuccess(). Creates
  vertex colors based on either the committee related to
  each node, or the topics related to each node, as chosen
  by the user.
}
\author{
  Hillary Sanders
}


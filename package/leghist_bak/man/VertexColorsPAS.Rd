\name{VertexColorsPAS}
\alias{VertexColorsPAS}
\title{VertexColorsPAS}
\usage{
  VertexColorsPAS(a, f, merged, vertex.color.by = "c",
    vertex.junk.color, vertex.col = NULL)
}
\arguments{
  \item{a}{number of amendments.}

  \item{f}{number of paragraphs in the final bill.}

  \item{merged}{output of OutToInPAS}

  \item{vertex.color.by}{}

  \item{vertex.col}{Optional vector of colors.}
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


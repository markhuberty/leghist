\name{Layout.SAS}
\alias{Layout.SAS}
\title{Layout.SAS}
\usage{
  Layout.SAS(x, a, f)
}
\arguments{
  \item{x}{the index of the coordinates to be created.}

  \item{a}{the number of amendments.}

  \item{f}{the number of text chunks (generally,
  paragraphs) in the final bill.}
}
\value{
  the xth layout coordinates for PlotAmendsSuccess().
}
\description{
  Creates the "x"th layout coordinates for
  PlotAmendsSuccess(). This function is called inside of
  PlotAmendsSuccess() to create the layout: coordinates for
  two layers consisting of 1) amendments (a of them), 2)
  the final bill (all of the paragraphs (or other text
  chunks) in the final bill, as well as a junk bin placed
  in the middle of the graph.
}
\author{
  Hillary Sanders
}


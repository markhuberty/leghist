\name{PlotAmendsSuccess}
\alias{PlotAmendsSuccess}
\title{PlotAmendsSuccess}
\usage{
  PlotAmendsSuccess(model.amend.hierarchy.out,
    get.likely.composite.out, committees,
    edge.color.by = "t", edge.col = NULL,
    edge.width.scale = 1, arrowhead.size = 0,
    junk.shape = "rectangle", af.scale = 1, junk.scale = 1,
    label.font = 3, label.cex = 0.75, vertex.label = NULL,
    a.lab = 10, f.lab = 10, vertex.color.by = "c",
    vertex.col = NULL,
    vertex.junk.color = "cornflowerblue", main = NULL,
    legend.x = -1.25, legend.y = 0.75, legend.cex = 0.5)
}
\arguments{
  \item{model.amend.hierarchy.out}{the output of
  ModelAmendHierarchy().}

  \item{get.likely.composite.out}{the output of
  get.likely.composite().}

  \item{committees}{the object "committees", used in other
  parts of this package, consisting of a vector of
  committee names for each ith amendment (accepted,
  rejected, and discarded amendments).}

  \item{edge.color.by}{Either "topics" ("t") or
  "committees" ("c") may be chosen by the user. If
  "topics", edge color will be based on the topic each
  amendment pertains to. If "committees", edge color will
  be based on the committee each amendment was submitted
  by.}

  \item{edge.col}{An optional vector of colors, the length
  of which should be equal to the number of either topics
  or committees being represented. If NULL, colors from
  RColorBrewer will be used. NOTE that if c or t (whichever
  matters in your case) is greater than 8, colors will be
  recycled! If c or t > 8, users should pass their own
  color vectors of length c or t.}

  \item{edge.width.scale}{Scale edge widths. Default = 1}

  \item{arrowhead.size}{Size of edge arrowheads. Default =
  0.}

  \item{junk.shape}{The node shape of the junk node.
  Possible shapes are "circle", "square", "csquare",
  "rectangle", "crectangle", "vrectangle", and "none".
  Default = "none".}

  \item{af.scale}{Scale the size of the amendment and final
  nodes.}

  \item{junk.scale}{Scale the size of the junk node.}

  \item{label.font}{Font type for the labels. Default = 3.}

  \item{label.cex}{Font size for the labels. Default =
  .75.}

  \item{vertex.label}{Vector of labels for each amendments,
  each final bill paragraph, and the junk bin. If left
  NULL, ten (relatively) equidistant nodes will be labeled
  by their paragraph indices for both amendments and the
  final bill, while the rejected amendments bin will be
  labeled "Junk".}

  \item{a.lab}{the number of visible labels on the bottom
  amendments tier. default = 10.}

  \item{f.lab}{the number of visible labels on the top
  final bill tier. default = 10.}

  \item{vertex.color.by}{the method used to calculate
  vertex.colors. Either "topics" ("t") or "committees"
  ("c") may be passed. If "topics", vertex colors will be
  based on the topic each amendment pertains to.
  "committees", vertex colors will be based on the
  committee each amendment was submitted by.}

  \item{vertex.col}{A vector of colors, of length x where x
  is the number of topics if vertex.color.by = "t" or where
  x is the number of committees if vertex.color.by = "c".
  If NULL, colors from RColorBrewer will be used. NOTE that
  if c or t (whichever matters in your case) is greater
  that 8, colors will be recycled! If c or t > 8, users
  should pass their own color vectors, of length > 8 one
  color for each committee or topic. Those paragraphs in
  the final bill which did come come from an amendment stay
  white.}

  \item{main}{The plot title. Default = "Amendments'
  Destinations".}

  \item{legend.x}{X axis placement of the legend.}

  \item{legend.y}{Y axis placement of the legend.}

  \item{legend.cex}{size of the legend.}
}
\value{
  A hopefully pretty graph!
}
\description{
  Creates a three tiered directed acyclic graph to
  visualize bill evolution. Individual amendments are
  either connected (with an arrow) to a junk bin if the
  amendment was not accepted into the final bill, or if it
  was, to its place in the final bill.
}
\author{
  Hillary Sanders
}


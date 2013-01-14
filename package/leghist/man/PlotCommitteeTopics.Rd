\name{PlotCommitteeTopics}
\alias{PlotCommitteeTopics}
\title{PlotCommitteeTopics}
\usage{
  PlotCommitteeTopics(model.amend.hierarchy.out,
    get.likely.composite.out, committees,
    edge.width.scale = 1, edge.width = "absolute",
    scale.c = 1, scale.t = 1, scale.fin = 1,
    edge.transparency = 70, edge.col = NULL, main = NULL,
    arrowhead.size = 0, layout = NULL, mid.layer = 0.65,
    plot.terms = TRUE, terms.cex = 0.5,
    terms.col = "grey30", terms.x.offset = 0,
    terms.y.offset = -0.05, terms.spread = 1,
    terms.text.close = 1, vertex.label = NULL,
    vertex.label.font = 3, vertex.label.cex = 0.75,
    rejected.final.label = NULL,
    vertex.color = "cornflowerblue",
    vertex.shape = "rectangle")
}
\arguments{
  \item{model.amend.hierarchy.out}{The object created by
  ModelAmendHierarchy().}

  \item{get.likely.composite.out}{The object created by
  GetLikelyComposite().}

  \item{committees}{The object created by
  ModelAmendHierarchy.}

  \item{edge.width.scale}{Scales the width of the arrows.
  Default = 1.}

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

  \item{scale.c}{Scales the size of the bottom layer
  committee nodes (vertices). By default, scale.c, scale.t,
  and scale.fin = 1, and their area are equally
  proportional to the number of amendments they represent.}

  \item{scale.t}{Scales the size of the middle layer topic
  nodes. Default = 1.}

  \item{scale.fin}{Scales the size of the top layer final
  nodes (rejected and Final). Default = 1.}

  \item{edge.transparency}{A number in 00:99, representing
  the wanted transparency in edges (lower number = more
  transparent). If left NULL (the default), edges will be
  kept opaque.}

  \item{edge.col}{Two edge colors to signify edges which
  contain 1) rejected destined amendments, 2) final
  destined amendments, or 3) both. Both RGB codes and
  character vectors are accepted. If NULL (the default)
  PlotCommitteeTopics() will use "cornflowerblue",
  "darkgoldenrod1", and varying shades of green to
  respectively signify if each edge's amendments were all
  successfull (in making it to the final bill), were all
  rejected, or had some combination of successful and
  failed amendments.}

  \item{main}{The graph's title. Defaults to NULL.}

  \item{arrowhead.size}{The size of arrowheads (edge
  arrowheads). Defaults to 0, i.e. no arrowhead.}

  \item{layout}{The layout of the graph. If left NULL,
  PlotCommitteeTopics() will create the default tricyclic
  graph described above. But you may also pass graphing
  algorithms (e.g. layout.fruchterman.reingold, or
  layout.circle) for a different layout. A layout matrix
  may also be passed. For more information on layout
  algorithms, seek igraph package documentation.}

  \item{mid.layer}{The placement of the middle layer of the
  graph on the y axis. Defaults to .65. Note that the
  bottom and top layers are at 0 and 1. Helpful if topic
  terms are being plotted (see plot.terms=TRUE below), and
  space is needed.}

  \item{plot.terms}{Logical. Should topic terms be plotted?
  Default = TRUE. Note that PlotCommitteeTopics() plots the
  terms beneath each topic node.}

  \item{terms.cex}{Text size for the topic terms plotted,
  if plot.terms=TRUE.}

  \item{terms.col}{Text color for the topic terms plotted,
  if plot.terms=TRUE.}

  \item{terms.x.offset}{X axis adjustment for the topic
  terms plotted, if plot.terms=TRUE.}

  \item{terms.y.offset}{Y axis adjustment for the topic
  terms plotted, if plot.terms=TRUE.}

  \item{terms.spread}{Measure of horizontal spread between
  the plotted topic}

  \item{terms.text.close}{Measure of vertical spread
  between the plotted topic terms.}

  \item{vertex.label}{An optional character vector
  representing the node names for each node (vertex). If
  NULL, the committee nodes (bottom layer) will be named
  with their full names, each ith topic node will be named
  Topic i, and the two final bins will be labeled "Adopted"
  and "Rejected" (for accepted and rejected amendments).}

  \item{vertex.label.font}{Type of font for the vertex
  labels.}

  \item{vertex.label.cex}{Size of the vertex label font.
  Vectorized.}

  \item{vertex.color}{The color vertex labels are filled
  with.}

  \item{vertex.shape}{The shape of the vertices. Default =
  "rectangle". Note that if another shape is passed, area
  may no longer be (one-to-one) related to the number of
  amendments represented. (For now.) Possible shapes are
  "circle", "square", "csquare", "rectangle", "crectangle",
  "vrectangle", and "none".}
}
\value{
  A hopefully pretty graph!
}
\description{
  The main graphing function for the legislative bill
  mapping package. This function creates a three layer
  directed acyclic graph. The first, bottom layer is a set
  of nodes representing committees which have each
  submitted a number of amendments. Edges (arrows) connect
  these committee nodes to the middle layer topic nodes to
  which each committee's amendment(s) pertain. Edges then
  connect these topic nodes to the third layer - two nodes:
  "Rejected" and "Adopted" - again according to the
  amendments that the edges represent. Edge width, as well
  as node area, by default correspond to the number of
  amendments they are representing.
}
\author{
  Hillary Sanders
}


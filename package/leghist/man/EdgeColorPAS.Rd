\name{EdgeColorPAS}
\alias{EdgeColorPAS}
\title{EdgeColorPAS}
\usage{
  EdgeColorPAS(merged, edge.color.by = "topics",
    edge.col = NULL)
}
\arguments{
  \item{edge.color.by}{Either "topics" ("t") or
  "committees" ("c") may be chosen by the user. If
  "topics", edge color will be based on the topic each
  amendment pertains to. If "committees", edge color will
  be based on the committee each amendment was submitted
  by.}

  \item{merged}{Output of OutToInPAS}

  \item{edge.col}{An optional vector of colors, the length
  of which should be equal to the number of either topics
  or committees being represented.}
}
\value{
  the character and numeric color vectors
}
\description{
  Called within PlotCommitteeTopics to calculate a vector
  of colors for the graph's edges based on the
  "edge.color.by" argument chosen.
}
\author{
  Hillary Sanders
}


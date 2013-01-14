\name{EdgeColorSAS}
\alias{EdgeColorSAS}
\title{EdgeColorSAS}
\usage{
  EdgeColorSAS(color.by = "topics", col = NULL, coms, tops)
}
\arguments{
  \item{color.by}{Either "topics" ("t") or "committees"
  ("c") may be chosen by the user. If "topics", edge color
  will be based on the topic each amendment pertains to. If
  "committees", edge color will be based on the committee
  each amendment was submitted by.}

  \item{col}{An optional vector of colors, the length of
  which should be equal to the number of either topics or
  committees being represented.}

  \item{coms}{A vector of committees corresponding to each
  amendment i.}

  \item{tops}{A vector of topics corresponding to each
  amendment i.}
}
\description{
  Called within PlotCommitteeTopics to calculate a vector
  of colors for the graph's edges based on the "color.by"
  argument chosen.
}


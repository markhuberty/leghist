\name{VertexLabels}
\alias{VertexLabels}
\title{VertexLabels}
\usage{
  VertexLabels(labels, merged, topics.matrix)
}
\arguments{
  \item{labels}{An optional vector of labels, usually NULL.
  If not NULL, the function will only output this same
  object.}

  \item{merged}{Output of OutToInSCT}

  \item{topics.matrix}{An object defined inside
  PlotCommitteeTopics():
  model.amend.hierarchy.out[[1]][[1]][[2]]}
}
\value{
  a vector of labels for each node in a
  PlotCommitteeTopics() graph.
}
\description{
  A function called within PlotCommitteeTopics() to creates
  vertex (node) labels.
}


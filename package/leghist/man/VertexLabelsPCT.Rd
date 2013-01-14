\name{VertexLabelsPCT}
\alias{VertexLabelsPCT}
\title{VertexLabelsPCT}
\usage{
  VertexLabelsPCT(vertex.label, merged, topics.matrix,
    rejected.final.label)
}
\arguments{
  \item{vertex.label}{An optional vector of labels, usually
  NULL. If not NULL, the function will only output this
  same object.}

  \item{merged}{Output of OutToInPCT}

  \item{topics.matrix}{An object defined inside
  PlotCommitteeTopics():
  model.amend.hierarchy.out[[1]][[1]][[2]]}

  \item{rejected.final.label}{character of length 2, the
  rejected node's label and the final bill's node label. If
  left NULL, the labels will be "Recjected" and "Adopted".}
}
\value{
  a vector of labels for each node in a
  PlotCommitteeTopics() graph.
}
\description{
  A function called within PlotCommitteeTopics() to creates
  vertex (node) labels.
}
\author{
  Hillary Sanders
}


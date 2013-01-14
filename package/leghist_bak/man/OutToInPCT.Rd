\name{OutToInPCT}
\alias{OutToInPCT}
\title{OutToInPCT}
\usage{
  OutToInPCT(model.amend.hierarchy.out,
    get.likely.composite.out, committees)
}
\arguments{
  \item{model.amend.hierarchy.out}{the output of
  ModelAmendHierarchy()}

  \item{get.likely.composite.out}{the output of
  get.likely.composite()}

  \item{committees}{the object "committees", used in other
  parts of this package, consisting of a vector of
  committee names for each ith amendment (accepted,
  rejected, and discarded amendments).}
}
\value{
  A dataframe of dimension ax4, where a equals the number
  of non- discarded amendments (so accepted or rejected
  amendments). The first column is the amendment indices,
  the second is the topic assignments, the third is the
  committees, and the fourth is a logical vector for
  amendment success (made it into the final bill) or
  failure.
}
\description{
  A function called within PlotCommitteeTopics() to take
  the output of various bill mapping functions and create
  an easily usable matrix carrying the information
  PlotCommitteeTopics() needs.
}
\author{
  Hillary Sanders
}


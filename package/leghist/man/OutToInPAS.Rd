\name{OutToInPAS}
\alias{OutToInPAS}
\title{OutToInPAS}
\usage{
  OutToInPAS(model.amend.hierarchy.out,
    get.likely.composite.out, committees)
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
}
\value{
  A four column dataframe, consiting of amendment index,
  topic index, committee, and a final destinations column:
  either a final bill index or 0, for rejected amendments.
}
\description{
  Takes output from various bill mapping functions and
  prepares the data for the PlotCommitteeTopics() function.
}
\author{
  Hillary Sanders
}


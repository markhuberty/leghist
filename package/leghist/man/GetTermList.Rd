\name{GetTermList}
\alias{GetTermList}
\title{GetTermList}
\usage{
  GetTermList(model.list, k.primary)
}
\arguments{
  \item{model.list}{A list of topic models as output from
  ModelAmendHierarchy}

  \item{k.primary}{The number of primary topics}
}
\value{
  Returns a nested list containing the top terms of each
  primary topic and its associated secondary topics
}
\description{
  GetTermList
}


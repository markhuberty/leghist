\name{CtabAmendHierarchy}
\alias{CtabAmendHierarchy}
\title{CtabAmendHierarchy}
\usage{
  CtabAmendHierarchy(amend.topic.hierarchy, composite.bill,
    committees, doc.list, tab.idx = 1)
}
\arguments{
  \item{amend.topic.hierarchy}{an object as returned from
  ModelAmendHierarchy}

  \item{composite.bill}{the composite bill}

  \item{committees}{the committee list for amendments}

  \item{doc.list}{the original document list containing a
  doc-term matrix}

  \item{tab.idx}{which axis proportions should be
  calculated on, one of 1 (rows) or 2 (columns)}
}
\value{
  a list of crosstabs. Element 1 count of amendments by
  committee and primary topic that were accepted or
  rejected. Element 2 is the aggregate count of amendments
  by primary topic that were accepted or rejected. Element
  3 returns the proportion of amendments accepted or
  rejected for each primary topic. Element 4 contains N
  elements, where N is the number of primary topics, each
  with N_i crosstabs indicating the breakdown of amendments
  for that primary topic by committee, secondary topic, and
  accept/reject.
}
\description{
  Takes as input a topic structure from ModelAmendHierarchy
  and returns a set of crosstabs indicating the proportion
  of amendments in each topic that were accepted or
  rejected.
}
\author{
  Mark Huberty
}


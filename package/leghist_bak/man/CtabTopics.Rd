\name{CtabTopics}
\alias{CtabTopics}
\title{CtabTopics}
\usage{
  CtabTopics(topics, committees, master.idx, this.idx)
}
\arguments{
  \item{topics}{the topic vector as returned by
  ModelAmendHierarchy}

  \item{committees}{the committee list corresponding to the
  committees responsible for the amendments}

  \item{master.idx}{The index of matched amendments in the
  composite bill, specifically the match.origin element of
  a composite.bill object}

  \item{this.idx}{Index of the amendments as they appear in
  the topic model, on the same interval as master.idx. Note
  that this is not the same as the dtm.idx, because of
  dropped cases due to amendment length or different
  stopwords lists.}
}
\value{
  A list of crosstabs: by topic and accepted/rejected
  (count.by.topic.status); by committee and
  accepted/rejected (count.by.committee.status); and
  proportion by topic status
}
\description{
  Crosstabs topics, committees, and acceptance/rejection
  for matched amendments. Called within CtabAmendHierarchy.
}
\author{
  Mark Huberty
}


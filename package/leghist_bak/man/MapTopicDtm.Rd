\name{MapTopicDtm}
\alias{MapTopicDtm}
\title{MapTopicDtm}
\usage{
  MapTopicDtm(topic.model, doc.list, composite, committees,
    sources, all.idx)
}
\arguments{
  \item{topic.model}{A topic model as output from
  ModelTopics}

  \item{doc.list}{A document-term matrix object as output
  from CreateAllVectorSpaces}

  \item{composite}{The composite bill, using doc.list and
  output from GetLikelyComposite}

  \item{committees}{Committees that originated the
  amendments in the composite bill}

  \item{sources}{Formal (system-assigned) sources of each
  row in the document-term matrix}

  \item{all.idx}{Index (1:N) of each row in the
  document-term matrix}
}
\value{
  A matrix mapping the source, committee, index, status,
  and topic of each row in the document-term matrix used to
  construct the composite bill.
}
\description{
  Maps topics from a topic model run on the full document
  corpus, to
}
\author{
  Mark Huberty
}


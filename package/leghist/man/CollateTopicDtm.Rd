\name{CollateTopicDtm}
\alias{CollateTopicDtm}
\title{CollateTopicDtm}
\usage{
  CollateTopicDtm(composite, topic.model, doc.list,
    committees)
}
\arguments{
  \item{composite}{Output of GetLikelyComposite}

  \item{topic.model}{Output of ModelTopics for the
  document-term in doc.list. Its recommended that
  topic.model be created with ModelTopics(..., na.rm=TRUE)}

  \item{doc.list}{Output of CreateAllVectorSpaces}

  \item{committees}{Committees list responsible for
  amendments in the doc.list object}
}
\value{
  A list with two objects. The doc.topic object lists each
  document section in the order provided in doc.list,
  mapped to their committee, status, and assigned topic
  from the topic model. The "composite" object contains an
  extended version of the output of GetLikelyComposite,
  providing the topics for both the final document section
  and its best match.
}
\description{
  Collates the topic model for the entire corpus of
  document sections, the assigned status of each section
  based on the composite match, and the origin of each
  document section.
}
\author{
  Mark Huberty
}


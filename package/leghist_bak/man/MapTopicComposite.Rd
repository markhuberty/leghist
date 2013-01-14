\name{MapTopicComposite}
\alias{MapTopicComposite}
\title{MapTopicComposite}
\usage{
  MapTopicComposite(topic.model, composite, doc.list,
    doc.topic)
}
\arguments{
  \item{topic.model}{the output of ModelTopics for the
  entire corpus of document sections as output from
  CreateAllVectorSpaces. Note that it's recommended the
  topic.model be created with na.rm=TRUE in ModelTopics.}

  \item{composite}{the output of GetLikelyComposite}

  \item{doc.topic}{the output of MapTopicDtm for this
  composite bill and vector space}
}
\value{
  An augmented composite bill object containing extra
  columns mapping the final bill section and its match to
  the topics assigned to them in the topic model.
}
\description{
  Maps the topics from a model of the entire
  document-section corpus onto the composite bill. Returns
  an augmented composite object with the assigned topics of
  both the final sections and their matches.
}
\author{
  Mark Huberty
}


\name{ModelDocSet}
\alias{ModelDocSet}
\title{ModelDocSet}
\usage{
  ModelDocSet(doc.list, composite.mat, type = "incl.amend",
    k = NULL, topic.method = "LDA",
    sampling.method = "VEM", n.terms = 5,
    addl.stopwords = "NULL", weighting = weightTf, control,
    na.rm = TRUE, ...)
}
\arguments{
  \item{doc.list}{the output from CreateAllVectorSpaces.}

  \item{composite.mat}{the output from GetLikelyComposite.}

  \item{type}{the subset of the text to be clustered by
  topic: one of "incl.amend" (default), "rej.amend",
  "incl.orig", "rej.orig", "all.amend", or "final".}

  \item{k}{the number of topics to model.}

  \item{topic.method}{one of "LDA" or "CTM"}

  \item{sampling.method}{one of "VEM" or "Gibbs"}

  \item{n.terms}{the number of terms to show in the
  returned object}

  \item{addl.stopwords}{specific stopwords to include not
  in the generic stopwords list used to generate the
  document-term matrix in doc.list}

  \item{weighting}{one of weightTf, weightTfIdf, or
  weightBin, depending on the weighting used to construct
  the document-term matrix in doc.list. Note that only
  weightTf is supported at present.}

  \item{control}{a list of control statements appropriate
  for topic.method. See the topicmodels documentation for
  more detail.}

  \item{na.rm}{Should documents of length 0 be discarded in
  the output? See ModelTopics for detail. Defaults to
  TRUE.}

  \item{...}{other arguments as required; see ModelTopics.}
}
\value{
  a ModelTopics object, and additionally an index of of the
  documents as it points to the text inputs, rather than
  the document-term matrix.
}
\description{
  Provides a facility to easily model the content of
  different classes of matched content.
}
\author{
  Mark Huberty
}


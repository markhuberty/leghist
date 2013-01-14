\name{ModelTopics}
\alias{ModelTopics}
\title{ModelTopics}
\usage{
  ModelTopics(dtm, idx, k = NULL, topic.method = "LDA",
    sampling.method, addl.stopwords = NULL, n.terms,
    weighting = weightTf, control = control, na.rm = TRUE)
}
\arguments{
  \item{dtm}{A document-term matrix as output from
  CreateAllVectorSpaces.}

  \item{idx}{The indices of the document-term matrix to
  model.}

  \item{k}{The number of topics to model.}

  \item{topic.method}{One of "LDA" or "CTM".}

  \item{sampling.method}{One of "VEM" or "Gibbs.}

  \item{addl.stopwords}{Additional stopwords to remove.}

  \item{n.terms}{the number of terms by topic to return.
  See the terms() function in topicmodels for details.}

  \item{weighting}{one of weightTf, weightTfIdf, or
  weightBin, corresponding to the weighting used to
  construct dtm. Note that at present only weightTf is
  supported.}

  \item{control}{a set of control parameters appropriate
  for topic.method. See the topicmodels package
  documentation for details.}

  \item{na.rm}{Should documents of length 0 after stopword
  removal be dropped (TRUE) or kept and assigned topic NA
  (FALSE)}

  \item{...}{other arguments as passed to the LDA or CTM
  methods}
}
\value{
  A list containing the topic model, the top N terms by
  topic, and the topic assignments for each document
  indicated by idx.
}
\description{
  Provides an interface to model the content of different
  amendment slices using Latent Dirichelet Allocation
  methods as supported in the topicmodels package. See the
  topicmodels package documentation for more details.
}
\author{
  Mark Huberty
}


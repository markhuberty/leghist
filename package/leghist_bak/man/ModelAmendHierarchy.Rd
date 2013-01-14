\name{ModelAmendHierarchy}
\alias{ModelAmendHierarchy}
\title{ModelAmendHierarchy}
\usage{
  ModelAmendHierarchy(doc.list, composite.mat, k = NULL,
    topic.method = "LDA", sampling.method = "VEM",
    n.terms = 5, addl.stopwords = "NULL",
    weighting = weightTf, ngram = 2,
    sparseness.probs = c(0.01, 0.99), control = control,
    na.rm = TRUE, ...)
}
\arguments{
  \item{doc.list}{the output of CreateAllVectorSpaces}

  \item{composite.mat}{the output of GetLikelyComposite,
  for this bill}

  \item{k}{the number of topics to model. May be one of
  NULL, an integer value, or an integer vector. If NULL,
  then the number of topics is assumed to be D/10, where D
  is either the count of amendments (level 1) or the count
  of amendments assigned to a topic (level 2). If a vector,
  the first element of the vector specifies how many topics
  should be used for modeling level 1, and the remaining
  elements how many topics should be used for modeling the
  amendments assigned to those topics. In this case, the
  vector must be of length V[1]}

  \item{topic.method}{One of "LDA" or "CTM"}

  \item{sampling.method}{One of "VEM", or "Gibbs"}

  \item{n.terms}{An integer value indicating how many terms
  should be returned for the purpose of inspecting the
  topics}

  \item{addl.stopwords}{Any additional stopwords that
  should be removed prior to modeling}

  \item{weighting}{One of weightTf, weightTfIdf, or
  weightBin}

  \item{ngram}{What n-gram should be used for topic
  modeling? Important if doc.list uses a range of ngrams.}

  \item{sparseness.probs}{Quantile range values to use in
  removing very sparse and very common terms. Defaults to
  c(0.01, 0.99).}

  \item{control}{A vector of control parameters for the
  topic model function; see the topicmodels documentation
  for more detail.}

  \item{na.rm}{Boolean, should documents of length 0 be
  discarded? See ModelTopics for detail. Defaults to TRUE.}

  \item{...}{}
}
\value{
  A list of of length 2. The first element in the list is a
  list of all models (both the top-level model and each of
  the secondary models). Model objects are of the form
  returned by ModelTopics(). The second element in the list
  is list of lists of terms, wherein the first element in
  each list is the set of terms that characterize that
  level-one category, and the second element contains the
  sets of terms that characterize the sub-categories within
  that level-one category.
}
\description{
  Provides a 2-level hierarchical topic modeling interface
  for modeling the amendment content
}
\author{
  Mark Huberty
}


\name{run.encoder}
\alias{run.encoder}
\title{run.encoder}
\usage{
  run.encoder(target.text = NULL, original.text = NULL,
    amendments = NULL, ngram.min = 1, ngram.max = 3,
    stem = FALSE, rm.stopwords = TRUE,
    rm.whitespace = TRUE, rm.punctuation = TRUE,
    filter = NULL, filter.thres = NULL,
    dist.fun = "cosine.mat", n.matches.to.show = 5,
    encode.random = FALSE, pct.encode = NULL)
}
\arguments{
  \item{target.text}{a character string of pargraphs
  needing matches}

  \item{original.text}{a character string of the original
  proposed text.}

  \item{amendments}{a character string of proposed
  amendments}

  \item{ngram.min}{the minimum-length ngram to use in the
  document-term matrix}

  \item{ngram.max}{the maximum-length ngram to use in the
  document-term matrix}

  \item{stem}{should words be stemmed?}

  \item{rm.stopwords}{should English stopwords be removed?}

  \item{rm.whitespace}{should excess whitespace be
  removed?}

  \item{rm.punctuation}{should punctuation be removed?}

  \item{filter}{Should a tfidf filter be applied?}

  \item{filter.thres}{What filter threshold should be
  used?}

  \item{dist.fun}{a distance function consistent with that
  of cosine.mat}

  \item{n.matches.to.show}{integer, how many potential
  matches should be shown to the user?}

  \item{encode.random}{should only a random subset of the
  target text be encoded?}

  \item{pct.encode}{If a random subset is to be encoded,
  what percent of the text should be encoded?}

  \item{ngram}{the n-gram to be used in creating a vector
  space of each document.}
}
\value{
  Returns a matrix of the form
  targetidx:match.idx:match.dist:match.source. For ease of
  automated comparison, the values in each are equivalent
  to similar values in the output of GetLikelyComposite.
}
\description{
  Takes the text to be matched, the initial text and
  amendment candidate matches, and settings for
  CreateAllVectorSpaces. Generates a set of candidate
  matches and asks the user to select the best (or no good
  match). Returns a data frame that maps from the final
  paragraph to both the initial bill and the amendments.
}
\author{
  Mark Huberty
}


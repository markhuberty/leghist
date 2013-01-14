\name{Encoder}
\alias{Encoder}
\title{Encoder}
\usage{
  Encoder(target.text, initial.match.text,
    amend.match.text, initial.distance.mat,
    amend.distance.mat = NULL, n.matches.to.show = 5,
    target.idx, similarity = TRUE)
}
\arguments{
  \item{target.text}{the final bill.}

  \item{initial.match.text}{the initial bill.}

  \item{amend.match.text}{any amendments (should be passed
  in the same order as they are passed to
  CreateAllVectorSpaces).}

  \item{initial.distance.mat}{the pairwise distance matrix
  between initial.match.text and target.text.}

  \item{amend.distance.mat}{the pairwise distance matrix
  between amend.match.text and target text.}

  \item{n.matches.to.show}{the number of potential matches
  to show}

  \item{target.idx}{the index of the target text.}

  \item{similarity}{boolean for whether the distance metric
  used for the distance.mat objects was a similarity
  measure or a distance measure}
}
\value{
  A matrix mapping from each entry in the target text to a
  user-supplied index of the best match in either the
  initial text or the amendment text.
}
\description{
  Provides a streamlined interface to manually match text
  under the same conditions as used for the automated
  MapBills process. Output is directly comparable with the
  output of GetLikelyComposite.
}
\author{
  Mark Huberty
}


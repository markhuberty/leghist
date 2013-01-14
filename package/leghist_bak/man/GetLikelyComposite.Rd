\name{GetLikelyComposite}
\alias{GetLikelyComposite}
\title{GetLikelyComposite}
\usage{
  GetLikelyComposite(mapbills.out, doc.initial, doc.final,
    amendments = NULL, amendment.origin = NULL,
    filter = "max", dist.threshold = 0)
}
\arguments{
  \item{mapbills.out}{the output of MapBills}

  \item{doc.initial}{a character string, representing
  doc.initial as used in MapBills.}

  \item{doc.final}{a character string, representing
  doc.final as used in MapBills.}

  \item{amendments}{a character string, representing
  amendments as used in MapBills.}

  \item{amendment.origin}{a character vector, the same
  length as amendments, indicating the origin of each
  amendment.}

  \item{filter}{One of "max" or "min", indicating how the
  best match should be selected.}

  \item{dist.threshold}{A threshold distance; if the best
  match does not pass the threshold, then the best match is
  the same section of the final document itself.}
}
\value{
  A matrix mapping from the index of each section in
  doc.final to its most likely match from doc.initial and
  amendments. The origin, index, text, and distance of the
  best match are all returned.
}
\description{
  Takes the initial and final versions of your documents
  (bills 1 ans 2), along with (possibly) the amendments,
  and outputs a matrix mapping from the index of each
  section in doc.final to its most likely match from
  doc.initial and amendments. The origin, index, text, and
  distance of the best match are all returned.
}
\author{
  Mark Huberty
}


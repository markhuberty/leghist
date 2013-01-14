\name{WriteTexSection}
\alias{WriteTexSection}
\title{WriteTexSection}
\usage{
  WriteTexSection(section, highlight.words = NULL,
    origin = NULL, origin.idx = NULL, dist = NULL,
    marginnote = TRUE)
}
\arguments{
  \item{section}{a string representing a document section.}

  \item{highlight.words}{a character vector of words to
  highlight}

  \item{origin}{a character string representing the origin
  of the section.}

  \item{origin.idx}{the index of the document in the origin
  source (e.g. paragraph number).}

  \item{dist}{The distance or similarity value to be
  displayed with origin in the margin note}

  \item{marginnote}{boolean, should margin notes be
  printed.}
}
\value{
  A character string with all LaTeX-sensitive characters
  appropriately escaped, and all highlights and origin
  information inserted with LaTeX-appropriate data. This
  assumes that the final LaTeX document will have a defined
  command of form texthighlight{} defined. Both
  WriteSideBySide and WriteComposite provide for this in
  their preamble.
}
\description{
  For a given text string and a list of words to highlight,
  WriteTexSection reforms the string to be LaTeX-valid and
  highlights words. If the necessary information is
  provided, the source of the string and a distance or
  similarity value are printed as margin notes. indicated
  in a margin note.
}
\author{
  Mark Huberty
}


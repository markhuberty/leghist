\name{WriteCompositeFinal}
\alias{WriteCompositeFinal}
\title{WriteCompositeFinal}
\usage{
  WriteCompositeFinal(composite.match, cavs.out,
    col.highlight = "red", box.amendments = TRUE,
    col.box = "lightyellow", file.out = "generic.tex",
    pdflatex = TRUE)
}
\arguments{
  \item{composite.match}{the output of GetLikelyComposite.}

  \item{cavs.out}{the output of CreateAllVectorSpaces used
  to generate the composite match.}

  \item{col.highlight}{the color to use in highlighting
  word differences}

  \item{box.amendments}{boolean, should matches that
  originate from amendments be boxed?}

  \item{col.box}{the color to use in shading amendment
  boxes}

  \item{file.out}{a valid filename for the latex output
  file.}

  \item{pdflatex}{boolean, should PDFLATEX be run on the
  output file?}
}
\value{
  Returns silently. If PDFLATEX is true, returns both the
  .tex file and its PDF output. Otherwise, returns only the
  .tex file.
}
\description{
  Writes out a LaTeX representation of the final document,
  as built up from components of the initial document plus
  amendments. Provides options to highlight both word
  differences and sections that result from amendments. The
  origin of each section is called out in a margin note.
}
\author{
  Mark Huberty
}


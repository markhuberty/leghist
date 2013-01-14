\name{WriteSideBySide}
\alias{WriteSideBySide}
\title{WriteSideBySide}
\usage{
  WriteSideBySide(composite.match, doc.final, cavs.out,
    dir.out, col.highlight = "red", box.amendments = TRUE,
    col.box = "lightyellow", file.out = "generic.tex",
    pdflatex = TRUE)
}
\arguments{
  \item{composite.match}{the output of GetLikelyComposite}

  \item{doc.final}{the original character string
  representing the final document.}

  \item{cavs.out}{the output of CreateAllVectorSpaces used
  to generate the composite match.}

  \item{dir.out}{the directory in which to place the .tex
  files}

  \item{col.highlight}{the color to highlight word changes}

  \item{box.amendments}{boolean, should amendments be
  boxed?}

  \item{col.box}{the color to shade amendment boxes.}

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
  Returns a LaTeX document that presents the side-by-side
  comparison of the final document sections and their
  matched pairs, with sources for the matched document.
  Takes as input the ComputeVectorAllVectorSpaces output
  and the result from that output from MapBills.
}
\author{
  Mark Huberty
}


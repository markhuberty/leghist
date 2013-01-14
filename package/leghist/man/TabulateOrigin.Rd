\name{TabulateOrigin}
\alias{TabulateOrigin}
\title{TabulateOrigin}
\usage{
  TabulateOrigin(ctd, model, n.round = 2, n.terms = 5,
    generate.latex = FALSE, print.latex = FALSE,
    latex.filename = NULL, latex.landscape = TRUE,
    latex.caption = NULL, latex.label = NULL,
    latex.size = NULL)
}
\arguments{
  \item{ctd}{The output of CollateTopicDtm}

  \item{model}{The output of ModelTopics for the entire
  corpus}

  \item{n.round}{Number of digits to round topic
  proportions to}

  \item{n.terms}{Number of terms to display in the final
  table}

  \item{generate.latex}{Boolean, should LaTeX table output
  be generated?}

  \item{print.latex}{Boolean, should the xdvi version of
  the table be shown?}

  \item{latex.filename}{String with valid filename for
  LaTeX output}

  \item{latex.landscape}{Boolean, should table be
  landscape?}

  \item{latex.caption}{String, caption for the LaTeX table}

  \item{latex.label}{String, valid LaTeX label for the
  table}

  \item{latex.size}{Valid LaTeX fontsize (e.g. "normal",
  "small")}

  \item{latex}{Boolean, should a LaTeX table be generated?}
}
\value{
  If latex==TRUE, returns a file with the LaTeX table;
  otherwise returns a data frame with the same information
}
\description{
  Given the topic distribution of the final bill, and the
  origin of each bill section, generates nicely-formatted
  tables summarizing contributions and optionally outputs
  to LaTeX.
}
\author{
  Mark Huberty
}


\name{SanitizeTex}
\alias{SanitizeTex}
\title{SanitizeTex}
\usage{
  SanitizeTex(string)
}
\arguments{
  \item{string}{a string to be inserted in a LaTeX file.}
}
\value{
  the same string, with the LaTeX special characters
  (%$_{}\~^&) replaced with backslash-escaped equivalents.
}
\description{
  Sanitizes special LaTeX characters in a string so that
  pdflatex will handle them correctly. All special
  characters are replaced with their escaped equivalents.
}
\author{
  Mark Huberty
}


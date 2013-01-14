\name{CheckAndFixRGB}
\alias{CheckAndFixRGB}
\title{CheckAndFixRGB}
\usage{
  CheckAndFixRGB(x)
}
\arguments{
  \item{x}{Presumably an RGB code or a character vector
  representing a color.}
}
\value{
  the input, but in RGB form, if possible.
}
\description{
  If the passed vector doesn't look like an RGB code,
  CheckAndFixRGB assumes the input is a color and tries to
  extract the RGB code so transparency can be added. Called
  within EdgeColorPCT(), which is called within
  PlotCommitteeTopics().
}


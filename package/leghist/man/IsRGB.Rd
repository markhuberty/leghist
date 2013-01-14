\name{IsRGB}
\alias{IsRGB}
\title{IsRGB}
\usage{
  IsRGB(x)
}
\arguments{
  \item{x}{a character vector}
}
\value{
  logical, does x start with a "#" sign?
}
\description{
  Is x an RGB code? Called within CheckAndFixRGB, which is
  called within EdgeColorPCT(), which is called within
  PlotCommitteeTopics().
}
\author{
  Hillary Sanders
}


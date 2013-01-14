\name{PlotTopicWords}
\alias{PlotTopicWords}
\title{PlotTopicWords}
\usage{
  PlotTopicWords(words.list, layout, cex = 0.5,
    col = "grey30", x.offset = 0, y.offset = -0.05,
    spread = 1, text.close = 1)
}
\arguments{
  \item{words.list}{A list (of length num.top or less,
  where num.top is the number of topics in your
  PlotCommitteeTopics() graph.}

  \item{layout}{A layout matrix, created inside
  PlotCommitteeTopics().}

  \item{cex}{Text size, default = .5.}

  \item{col}{Text color, default = "grey30".}

  \item{x.offset}{Adjust the x axis placement of the terms.
  Default = 0.}

  \item{y.offset}{Adjust the y axis placement of the terms.
  Default = -.05.}

  \item{spread}{Adjust the breadth of the terms to be
  plotted. Default = 1.}

  \item{text.close}{How close should each term for a given
  topic be? Default = 1.}
}
\value{
  Text on plotted onto a PlotCommitteeTopics() graph (with
  default layout style).
}
\description{
  Plots a list of words next to each topic node in the
  graph created by this package's PlotCommitteeTopics()
  function. To be used within PlotCommitteeTopics().
}
\author{
  Hillary Sanders
}


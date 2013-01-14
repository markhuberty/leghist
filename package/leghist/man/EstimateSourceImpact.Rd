\name{EstimateSourceImpact}
\alias{EstimateSourceImpact}
\title{EstimateSourceImpact}
\usage{
  EstimateSourceImpact(tab.topic.status.out,
    prop.margin = 1, dist.quantiles = seq(0, 1, 0.2))
}
\arguments{
  \item{tab.topic.status.out}{output from CollateTopicDtm
  containing the topic and status attributes of all
  documents in the corpus, and the mapping of topics to
  sections in the final bill.}

  \item{prop.margin}{Specifies whether the proportion table
  should use row (1) or column (2) percentages.}
}
\value{
  A list containing the topic distribution for the final
  bill, the cross-tabulation of topic counts by committee
  for the synthetic bill, and the proportion cross-tab of
  topic counts by committee for the final bill.
}
\description{
  Given a composite bill and a topic model for the entire
  legislative corpus, EstimateSourceImpact assigns
  attribution of topics--those found in the final but not
  initial legislation--by committee origin.
}
\author{
  Mark Huberty
}


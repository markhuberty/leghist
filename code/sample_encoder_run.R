## A sample of how this works

## Set the directory to where you have the code files (encoder.R
## and leg_functions.R)
setwd("~/Documents/Research/Papers/leghist/code")
## Load the encoder functions
source("encoder.R")

## Change the directory to where you've put the text files
setwd("~/Documents/Research/Dissertation/master/notes/leg_hist_sources/intl_mkt/")
## Load in the plain text files 
ep.first.reading <- read.csv("./2007/txt/ep_first_reading_report.txt",
                             header=TRUE,
                             stringsAsFactors=FALSE
                             )
ep.second.reading <-
  read.csv("./2007/txt/ep_second_reading_report.txt",
           header=TRUE,
           stringsAsFactors=FALSE
           )

initial.bill <- readLines("./2003/txt/directive_2003_54_ec.txt")
commission.proposal <- readLines("./2007/txt/com_2007_528_final.txt")
council.common.position <- readLines("./2007/txt/council_common_position.txt")
final.bill <- readLines("./2007/txt/directive_2009_72_ec.txt")

## Run the encoder. Pass it the final bill, the
## initial bill, the amendments, and the right settings to use in
## generating the distance matrices, etc. This will then
## run you through the entire bill.

## Note that the code prints out some information while you are going
## through the loop:
## Valid choices (usually 1:n.matches.to.show, plus NA)
## Whether your choice was valid
df.test <- run.encoder(final.bill,
                       initial.bill,
                       c(ep.first.reading$text,
                         ep.second.reading$text,
                         commission.proposal
                         ),
                       ngram=2,
                       stem=FALSE,
                       rm.stopwords=TRUE,
                       rm.whitespace=TRUE,
                       rm.punctuation=TRUE,
                       filter=NULL,
                       filter.thres=NULL,
                       dist.fun="cosine.mat",
                       n.matches.to.show=5,
                       encode.random=TRUE
                       )



save(df.test, file="directive_2009_72_matches.RData")

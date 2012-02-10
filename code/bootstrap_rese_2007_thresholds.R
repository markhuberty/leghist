setwd("~/Documents/Research/Papers/leghist/code")
source("leg_functions.R")
setwd("~/Documents/Research/Dissertation/master/notes/leg_hist_sources/rese/")

## Parse the 2007-2008 files
ep.first.reading <- read.csv("./2007/txt/ep_first_reading_report.txt",
                             header=TRUE,
                             stringsAsFactors=FALSE
                             )

initial.bill <- readLines("./2001/orig/directive_2001_77_ec.txt")
commission.proposal <- readLines("./2007/txt/com_2008_0019.txt")
final.bill <- readLines("./2007/txt/directive_2009_28_ec.txt")

doc.list <- CreateAllVectorSpaces(initial.bill,
                                  final.bill,
                                  c(ep.first.reading$text,
                                    commission.proposal
                                    ),
                                  ngram=3,
                                  stem=FALSE,
                                  rm.stopwords=TRUE,
                                  rm.whitespace=TRUE,
                                  rm.punctuation=TRUE,
                                  filter=NULL,
                                  filter.thres=NULL,
                                  weighting=weightTf
                                  )

map.bills.cos <- MapBills(doc.list,
                          distance.fun="cosine.mat"
                          )





## Try some calibration
load("rese_2007_encoding.RData")
## Fix some cruft, this won't be in there in future versions.
df.test$match.source <- as.character(df.test$match.source)
df.test$match.source[is.na(df.test$match.source)] <- "doc.final"
df.test$match.source <- as.factor(df.test$match.source)
levels(df.test$match.source) <- c("amendment", "doc.final", "doc.initial")

df.test$match.index <- ifelse(is.na(df.test$match.index),
                              df.test$target.index,
                              df.test$match.index
                              )

threshold.seq <- seq(from=0, to=0.5, by=0.005)

type.acc.bs <- foreach(i = 1:1000, .combine=c) %dopar% {

  thres.acc <- learn.threshold(map.bills.cos,
                               initial.bill,
                               final.bill,
                               c(ep.first.reading$text,
                                 commission.proposal),
                               c(ep.first.reading$committee,
                                 rep("Commission 2007",
                                     length(commission.proposal)
                                     )
                                 ),
                               filter="max",
                               threshold.values=threshold.seq,
                               df.test[sample(1:nrow(df.test), 100, replace=FALSE),],
                               type="overall"
                               )

  
  type.acc <- learn.threshold(map.bills.cos,
                            initial.bill,
                            final.bill,
                            c(ep.first.reading$text,
                              commission.proposal),
                            c(ep.first.reading$committee,
                              rep("Commission 2007",
                                  length(commission.proposal)
                                  )
                              ),
                            filter="max",
                            threshold.values=threshold.seq,
                            df.test[sample(1:nrow(df.test), 100, replace=FALSE),],
                            type="tradeoff"
                            )

  out <- list(thres.acc, type.acc)
  return(out)

}



setwd("~/Documents/Research/Papers/leghist/code")
source("leg_functions.R")
setwd("~/Documents/Research/Dissertation/master/notes/leg_hist_sources/intl_mkt/2003/")
 
## parse.eurlex.parl.html <- function(url,
##                                    amend.num.string,
##                                    amend.string,
##                                    amendment.cleanup.strings,
##                                    label.cleanup.strings,
##                                    committees
##                                    ){

##   input <- readLines(url, encoding="UTF-8")
##   input <- paste(input, collapse=" ")
##   input <- str_replace_all(input, "\u00A0", " ")
##   input <- str_replace_all(input, "</{0,1}span.*?>", "")

##   amendment.labels <- unlist(str_extract_all(input, amend.num.string))
##   amendments <- unlist(str_split(input, amend.num.string))[-1]
  
##   print(length(amendments))

##   for(i in 1:length(amendments))
##     {

##       amendments[i] <- str_split(amendments[i], amend.string)

##     }
  
##   stopifnot(length(amendment.labels) == length(amendments))

##   print("Cleaning amendments")
##   for(i in 1:length(amendment.cleanup.strings))
##     {
##       amendments <- str_replace_all(amendments,
##                                     amendment.cleanup.strings[i],
##                                     " "
##                                     )
##     }
  
##   print("Cleaning labels")
##   for(i in 1:length(label.cleanup.strings))
##     {
      
##       amendment.labels <- str_replace_all(amendment.labels,
##                                           label.cleanup.strings[i],
##                                           ""
##                                           )

##     }
##   print(length(amendment.labels))
  
##   print("Getting positions of first amendments")
##   first.amendment <- which(!is.na(str_match(amendment.labels,
##                                             "Amendment\\s*?1$"
##                                             )
##                                   )
##                            )
##   first.amendment <- c(first.amendment, length(amendments))
  
##   print(length(first.amendment))

##   print("Getting committee list")
##   if(length(first.amendment) == (length(committees) + 1))
##     {

##       committee.list = sapply(1:length(committees), function(x){

##         label.length <-
##           ifelse(x == length(committees),
##                  first.amendment[x+1] - first.amendment[x] + 1,
##                  first.amendment[x+1] - first.amendment[x]
##                  )
##         print(label.length)
                               
##         rep(committees[x], label.length)
        
##       })

##       committee.list <- unlist(committee.list)
        
      
##     }else{

##       committee.list <- paste("C", 1:length(amendments))
      
##     }
##   out <- data.frame(committee.list, amendment.labels, amendments)

##   return(out)

## }

## test.url <-
## "http://www.europarl.europa.eu/sides/getDoc.do?type=REPORT&mode=XML&reference=A6-2008-0191&language=EN"

## test.parse <- parse.eurlex.parl.html(test.url,
##                                      amend.num.string="Amendment\\s*?[0-9]+?<",
##                                      amend.string="Amendment",
##                                      amendment.cleanup.strings=c("<.*?>",
##                                        "\342\200\223",
##                                        "\342\200\231"
##                                        ),
##                                      label.cleanup.strings=c(">", "<"),
##                                      committees=c("A", "B", "C")
##                                      )

## Load in the data and bills

ep.first.reading <- read.csv("ep_first_reading_report.txt",
                             header=TRUE,
                             stringsAsFactors=FALSE
                             )
ep.second.reading <- read.csv("ep_second_reading_report.txt",
                              header=TRUE,
                              stringsAsFactors=FALSE
                              )


initial.bill <- readLines("../1996/directive_96_92_ec.txt")
commission.proposal <- readLines("com_2001_0125_final_0077.txt")
final.bill <- readLines("directive_2003_54_ec.txt")


doc.list <- CreateAllVectorSpaces(initial.bill,
                                  final.bill,
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
                                  filter.thres=NULL
                                  )

## map.bills.cos <- MapBills(doc.list,
##                           distance.fun="cosine.dist"
##                           )

## map.bills.sim <- MapBills(doc.list,
##                           distance.fun="similarity.dist"
##                           )

map.bills.cos <- MapBills(doc.list,
                          distance.fun="cosine.mat"
                          )

map.bills.cos.len <- MapBills(doc.list,
                              distance.fun="cosine.length.mat"
                              )

composite.bill.cos <- GetLikelyComposite(map.bills.cos,
                                         initial.bill,
                                         final.bill,
                                         c(ep.first.reading$text,
                                           ep.second.reading$text,
                                           commission.proposal
                                           ),
                                         c(ep.first.reading$committee,
                                           ep.second.reading$committee,
                                           rep("Commission 2001",
                                               length(commission.proposal)
                                               )
                                           ),
                                         filter="max",
                                         dist.threshold=0.2
                                         )


write.side.by.side.cos <- WriteSideBySide(composite.bill.cos,
                                          final.bill,
                                          cavs.out=doc.list,
                                          file.out="ep_2003_intl_mkt_sbs_cos.tex",
                                          pdflatex=TRUE
                                          )

## composite.bill.sim <- GetLikelyComposite(map.bills.sim,
##                                          initial.bill,
##                                          final.bill,
##                                          c(ep.first.reading$text,
##                                            ep.second.reading$text,
##                                            commission.proposal
##                                            ),
##                                          c(ep.first.reading$committee,
##                                            ep.second.reading$committee,
##                                            rep("Commission 2001",
##                                                length(commission.proposal)
##                                                )
##                                            ),
##                                          filter="max",
##                                          dist.threshold=0.5
##                                          )


## write.side.by.side.sim <- WriteSideBySide(composite.bill.sim,
##                                           final.bill,
##                                           cavs.out=doc.list,
##                                           file.out="ep_2003_intl_mkt_sbs_sim.tex",
##                                           pdflatex=TRUE
##                                           )

composite.bill.cos.len <- GetLikelyComposite(map.bills.cos.len,
                                             initial.bill,
                                             final.bill,
                                             c(ep.first.reading$text,
                                               ep.second.reading$text,
                                               commission.proposal
                                               ),
                                             c(ep.first.reading$committee,
                                               ep.second.reading$committee,
                                               rep("Commission 2001",
                                                   length(commission.proposal)
                                                   )
                                               ),
                                             filter="max",
                                             dist.threshold=0.1
                                             )


write.side.by.side.cos.len <- WriteSideBySide(composite.bill.cos.len,
                                              final.bill,
                                              cavs.out=doc.list,
                                              file.out="ep_2003_intl_mkt_sbs_cos_len.tex",
                                              pdflatex=TRUE
                                              )




save.image("~/Documents/Research/Papers/leghist/data/cos_sim_compare_intlmkt_2003.RData")

## Encoder.R: a library for user-encoding of text matches
## Author: Mark Huberty
## Begun: 21 January 2012


source("leg_functions.R")
## Basic idea: generate the set of all possible matches to a target;
## then show user the target and N closest matches from a category;
## then ask user which is the right one.
##' <description>
##' Provides a streamlined interface to manually match text under the
##' same conditions as used for the automated MapBills process. Output
##' is directly comparable with the output of GetLikelyComposite.
##' <details>
##' @title encoder 
##' @param target.text the final bill 
##' @param initial.match.text the initial bill
##' @param amend.match.text any amendments (should be passed in the
##' same order as they are passed to CreateAllVectorSpaces)
##' @param initial.distance.mat the pairwise distance matrix between
##' initial.match.text and target.text
##' @param amend.distance.mat the pairwise distance matrix between
##' amend.match.text and target text.
##' @param n.matches.to.show the number of potential matches to show
##' @param target.idx the index of the target text 
##' @return A matrix mapping from each entry in the target text to a
##' user-supplied index of the best match in either the initial text or
##' the amendment text.
##' @author Mark Huberty
encoder <- function(target.text,
                    initial.match.text,
                    amend.match.text,
                    initial.distance.mat,
                    amend.distance.mat=NULL,
                    n.matches.to.show=5,
                    target.idx
                    ){

  if(!is.null(amend.distance.mat) & !is.null(amend.match.text))
    {
    
      has.amend <- TRUE
      n.matches.to.show <- ceiling(n.matches.to.show / 2)
      print(n.matches.to.show)
      
    }else{

      has.amend <- FALSE

    }

  idx.initial.mat <- sapply(1:ncol(initial.distance.mat), function(x){

    order(initial.distance.mat[,x], decreasing=TRUE)[1:n.matches.to.show]

  })

  if(has.amend)
     {
       idx.amend.mat <- sapply(1:ncol(amend.distance.mat), function(x){
         
         order(amend.distance.mat[,x], decreasing=TRUE)[1:n.matches.to.show]
         
       })
     }

  source.selections <- c()
  match.selections <- c()
  dist.selections <- c()
  sep.string <- "***************"

  print("starting loop")
  for(r in 1:length(target.text))
    {

      ## Get the potential targets

      ## Issue here: need to handle the 'no amendments' case
      target <- target.text[r]

      potential.initial.match.idx <-
        idx.initial.mat[,r]
      potential.initial.matches <-
        initial.match.text[potential.initial.match.idx]
      potential.initial.match.distances <-
        initial.distance.mat[potential.initial.match.idx, r]
      
      ## potential.initial.matches <-
      ##   potential.initial.matches[shuffle.vec]
      ## potential.initial.match.distances <-
      ##   dist.vec[shuffle.vec]
      ## potential.initial.match.idx <-
      ##   match.idx[shuffle.vec]


      if(has.amend)
        {

          potential.amend.match.idx <-
            idx.amend.mat[,r]
          potential.amend.matches <-
            amend.match.text[potential.amend.match.idx]
          potential.amend.match.distances <-
            amend.distance.mat[potential.amend.match.idx, r]

        }

      ## Generate the list of matches

      if(has.amend)
        {
          match.len <- 2 * n.matches.to.show
          shuffle.master <- sample(1:match.len, replace=FALSE)
          
          match.source <- c(rep("amendment", n.matches.to.show),
                            rep("doc.initial", n.matches.to.show)
                            )[shuffle.master]
          
          potential.matches <-
            c(potential.amend.matches,
              potential.initial.matches
              )[shuffle.master]

          potential.match.distances <-
            c(potential.amend.match.distances,
              potential.initial.match.distances
              )[shuffle.master]

          potential.match.idx <-
            c(potential.amend.match.idx,
              potential.initial.match.idx
              )[shuffle.master]
          
        }else{
          match.len <- n.matches.to.show
          shuffle.master <- sample(1:match.len, replace=FALSE)

          match.source <-
            rep("initial", n.matches.to.show)[shuffle.master]

          potential.matches <-
            potential.initial.matches[shuffle.master]
          potential.match.distances <-
            potential.initial.match.distances[shuffle.master]
          potential.match.idx <-
            potential.initial.match.idx[shuffle.master]
          
        }
      ## Run the print statements
      print(sep.string)
      print("TEXT TO MATCH:")
      print(target)
      print(sep.string)

      print("POTENTIAL MATCHES:")
      for(i in 1:length(potential.matches))
        {
          print(sep.string)
          print(paste("Index: ", i, sep=""))
          print(potential.matches[i])
        }

      ## Loop over the target text to get user input
      valid.selection <- FALSE
      valid.matches <- c(as.character(1:match.len), "NA")
      prompt.text <-
        paste("Enter index of best match (valid entries:",
              paste(1:match.len, collapse=" "),
              "or NA): ",
              sep=" "
              )
              
      while(!valid.selection)
        {
          
          selection <- readline(prompt=prompt.text)

          if(selection == "")
            {

              print("Empty entries not valid, please try again")
              valid.selection <- FALSE

            }else{

              if(selection %in% valid.matches)
                {
                  valid.selection <- TRUE
                  selection <- as.integer(selection)
                }else{
                  print("Invalid choice. Please try again")
                }
            }
        }

      if(!is.na(selection))
         {
           match.selections <- append(match.selections,
                                      potential.match.idx[selection])
           dist.selections <- append(dist.selections,
                                     potential.match.distances[selection]
                                     )
           source.selections <- append(source.selections,
                                       match.source[selection]
                                       )
         }else{ ## No match, append as final

           match.selections <- append(match.selections, r)
           dist.selections <- append(dist.selections, selection)
           source.selections <- append(source.selections, "doc.final")
           print(match.selections)
           print(dist.selections)
           print(source.selections)
           
         }

      print(paste("Matched",
                  r,
                  "of",
                  length(target.text),
                  "necessary matches"
                  )
            )
    }
  match.selections <- as.integer(match.selections)
  dist.selections <- as.numeric(dist.selections)
  source.selections <- as.character(source.selections)
  
  df.out <- data.frame(target.idx,
                       match.selections,
                       dist.selections,
                       source.selections
                       )

  names(df.out) <- c("target.index", "match.index", "match.dist", "match.source")
  return(df.out)

}

## run.encoder
## 
## Input: target.text: 
##        original.text: 
##        amendmnets: 
##          ngram, stem, rm.stopwords, rm.whitespace, rm.punctuation,
##          filter, filter.thres: see CreateAllVectorSpaces
##        n.matches.to.show: how many candidate matches should be
##          provided?
##        encode.random: should only a random subset of the target
##          text be coded?
##        pct.encode: If encode.random, then what percentage of the
##          target text should be coded? Assumes 10% if not specified
##' <description>
##' Takes the text to be matched, the initial text and amendment
##' candidate matches, and settings for
##' CreateAllVectorSpaces. Generates a set of candidate matches and
##' asks the user to select the best (or no good match). Returns
##' a data frame that maps from the final paragraph to both the initial
##' bill and the amendments.
##' <details>
##' @title 
##' @param target.text a character string of pargraphs needing matches
##' @param original.text a character string of the original proposed
##'                      text
##' @param amendments a character string of proposed amendments
##' @param ngram the n-gram to be used in creating a vector space of
## each document
##' @param stem should words be stemmed?
##' @param rm.stopwords should English stopwords be removed?
##' @param rm.whitespace should excess whitespace be removed?
##' @param rm.punctuation should punctuation be removed?
##' @param filter Should a tfidf filter be applied?
##' @param filter.thres What filter threshold should be used?
##' @param dist.fun a distance function consistent with that of cosine.mat
##' @param n.matches.to.show integer, how many potential matches
##' should be shown to the user?
##' @param encode.random should only a random subset of the target text be
##' encoded?
##' @param pct.encode If a random subset is to be encoded, what
##' percent of the text should be encoded?
##' @return Returns a matrix of the form
## targetidx:match.idx:match.dist:match.source. For ease of automated
##' comparison, the values in each are
##' equivalent to similar values in the output of GetLikelyComposite. 
##' @author Mark Huberty
run.encoder <- function(target.text=NULL,
                        original.text=NULL,
                        amendments=NULL,
                        ngram=2,
                        stem=FALSE,
                        rm.stopwords=TRUE,
                        rm.whitespace=TRUE,
                        rm.punctuation=TRUE,
                        filter=NULL,
                        filter.thres=NULL,
                        dist.fun="cosine.mat",
                        n.matches.to.show=5,
                        encode.random=FALSE,
                        pct.encode=NULL
                        ){

  if(encode.random)
    {
      if(is.null(pct.encode))
        {

          idx.to.test <-
            sample(1:length(target.text),
                   ceiling(length(target.text)/10)
                   )
          
        }else{

          idx.to.test <-
            sample(1:length(target.text),
                   ceiling(pct.encode * length(target.text))
                   )
          
        }

      target.text <- target.text[idx.to.test]
      target.idx <- idx.to.test
      
    }else{

      target.idx <- 1:length(target.text)
                
    }

  stopifnot(!is.null(original.text) &
            !is.null(target.text)
            )
  
  doc.list <- CreateAllVectorSpaces(original.text,
                                    target.text,
                                    amendments,
                                    ngram=ngram,
                                    stem=stem,
                                    rm.stopwords=rm.stopwords,
                                    rm.whitespace=rm.whitespace,
                                    rm.punctuation=rm.punctuation,
                                    filter=filter,
                                    filter.thres=filter.thres
                                    )

  print("Vector space created")
  dist.fun <- match.fun(dist.fun)

  distance.mat.orig <- dist.fun(doc.list$vs.out,
                                doc.list$idx.final,
                                doc.list$idx.initial
                                )
  if(is.null(amendments))
    {
      distance.mat.amend <- NULL
    }else{
      
      distance.mat.amend <- dist.fun(doc.list$vs.out,
                                     doc.list$idx.final,
                                     doc.list$idx.amendments
                                     )
    }
  
  print("Distance matrices created, starting the encoding sequence")

  match.df.orig <- encoder(target.text,
                           original.text,
                           amendments,
                           distance.mat.orig,
                           distance.mat.amend,
                           n.matches.to.show=n.matches.to.show,
                           target.idx
                           )

  return(match.df.orig)
  
}


test.accuracy <- function(automated.match,
                          human.match,
                          doc.initial,
                          doc.final,
                          amendments,
                          amendment.origin=NULL,
                          filter="max",
                          dist.threshold=0
                          ){

  
  best.automated.match <- GetLikelyComposite(automated.match,
                                             doc.initial,
                                             doc.final,
                                             amendments,
                                             filter=filter,
                                             dist.threshold=dist.threshold
                                             )

  best.human.match <- GetLikelyComposite(human.match,
                                         doc.initial,
                                         doc.final,
                                         amendments,
                                         filter=filter,
                                         dist.threshold=dist.threshold
                                         )

  ## Not right, needs to compare both idx and origin...
  match.pct <- sum(best.automated.match$match.idx ==
                   best.human.match$match.idx) /
                     length(best.automated.match$match.idx)

  


}

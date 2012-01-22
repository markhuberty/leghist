## Encoder.R: a library for user-encoding of text matches
## Author: Mark Huberty
## Begun: 21 January 2012


source("leg_functions.R")
## Basic idea: generate the set of all possible matches to a target;
## then show user the target and N closest matches from a category;
## then ask user which is the right one.

encoder <- function(target.text,
                    match.text,
                    distance.mat,
                    n.matches.to.show=5,
                    target.idx
                    ){

  idx.mat <- sapply(1:ncol(distance.mat), function(x){

    order(distance.mat[,x], decreasing=TRUE)[1:n.matches.to.show]

  })


  match.selections <- c()
  dist.selections <- c()
  sep.string <- "***************"

  print(paste("VALID ENTRIES ARE:",
              paste(1:n.matches.to.show, collapse=","),
              "or NA if no good match is shown"
              )
        )
  for(r in 1:length(target.text))
    {

      ## Get the potential targets
      target <- target.text[r]
      match.idx <- idx.mat[,r]
      potential.matches <- match.text[match.idx]
     
      dist.vec <- distance.mat[match.idx,r]

      ## Randomize the order so
      ## the user doesn't know which potential
      ## match was the closest
      shuffle.vec <- sample(1:n.matches.to.show)
      potential.matches <-
        potential.matches[shuffle.vec]
      potential.match.distances <-
        dist.vec[shuffle.vec]
      potential.match.idx <-
        match.idx[shuffle.vec]

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
      while(!valid.selection)
        {
          selection <- readline(prompt="Enter index of best match: ")

          if(selection == "")
            {
              print("Empty entries not valid, please try again")
              valid.selection <- FALSE
            }else{
              selection <- as.integer(selection)
              if(selection %in% c(1:n.matches.to.show) |
                 is.na(selection))
                {
                  valid.selection <- TRUE
                }else{
                  print("Invalid choice. Please try again")
                }
            }
        }

      if(is.integer(selection))
         {
           match.selections <- append(match.selections,
                                      potential.match.idx[selection])
           dist.selections <- append(dist.selections,
                                     potential.match.distances[selection]
                                     )
         }else{
           match.selections <- append(match.selections, selection)
           dist.selections <- append(dist.selections, selection)
         }
        
    }
  match.selections <- as.integer(match.selections)
  dist.selections <- as.numeric(dist.selections)
  
  df.out <- data.frame(target.idx, match.selections, dist.selections)
  names(df.out) <- c("target.index", "match.index", "match.dist")
  return(df.out)

}

## run.encoder
## Takes the text to be matched, the initial text and amendment
## candidate matches, and settings for
## CreateAllVectorSpaces. Generates a set of candidate matches and
## asks the user to select the best (or no good match). Returns
## a data frame that maps from the final paragraph to both the initial
## bill and the amendments.
## Input: target.text: a character string of pargraphs needing matches
##        original.text: a character string of the original proposed
##                       text
##        amendmnets: a character string of amendment paragraphs
##          ngram, stem, rm.stopwords, rm.whitespace, rm.punctuation,
##          filter, filter.thres: see CreateAllVectorSpaces
##        n.matches.to.show: how many candidate matches should be provided?  
run.encoder <- function(target.text,
                        original.text,
                        amendments,
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
  distance.mat.amend <- dist.fun(doc.list$vs.out,
                                 doc.list$idx.final,
                                 doc.list$idx.amendments
                                 )

  print("Distance matrices created, starting the encoding sequence")
  print("Encoding matches to the original text")
  match.df.orig <- encoder(target.text,
                           original.text,
                           distance.mat.orig,
                           n.matches.to.show=n.matches.to.show,
                           target.idx
                           )
  print("Encoding matches to the amendment text")
  match.df.amend <- encoder(target.text,
                            amendments,
                            distance.mat.amend,
                            n.matches.to.show=n.matches.to.show,
                            target.idx
                            )

  df.out <- cbind(match.df.orig,
                  match.df.amend[,2:3]
                  )

  names(df.out) <- c("idx.final",
                     "idx.orig",
                     "dist.orig",
                     "idx.amend",
                     "dist.amend"
                     )
  return(df.out)
  
}

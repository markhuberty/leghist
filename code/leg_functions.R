#################################################################
## Mark Huberty
## Automated analysis of legislative history
## Begun: 4 November 2011

## This file contains functions that will become an R package to
## automate the textual evolution of legislation from its introduction as
## a bill, through the amendment process, until finalization.

## Four core pieces of functionality are envisioned:
## 1. Mapping of sections between editions of bills
## 2. Mapping of amendments to their location in bills
## 3. Identification of discarded material
## 4. Modeling of the content of added and discarded material

## Additionally, the project may ultimately provide:
## 1. Classification of changes as either "substantive" or
## "administrative" (that is, the content of the law, or how the law is
##                   executed / administered)
##
## Coding conventions should follow the Google R style guide, at
## http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
## END
#################################################################
require(gdata)
require(tm)
require(topicmodels)
require(stringr)
require(lsa)
require(Matrix)
require(RWeka)
#require(openNLP) ## For the sentence tokenizer functionality

##' ##' <description>
##' Maps the final bill to both the original bill and any proposed
##amendments. Returns a matrix that maps from the final bill to the
##initial bill and (if supplied) the amendments, with distance metrics
##for the best match from each source. 
##' <details>
##' @title MapBills
##' @param doc.list A list of document-term matrices, as output from
##ComputeAllVectorSpaces()
##' @param distance.fun A distance function. The underlying distance
##metric should return larger values for more similar objects. 
##' @return A matrix mapping from the sections of the final document
##to the sections of both the initial document and any proposed
##amendments, with distance values for each matched pair.
##' @author Mark Huberty
MapBills <- function(doc.list,
                     distance.fun="cosine.dist"
                     ){


  idx.collection <- ifelse(is.na(doc.list[["idx.amendments"]]),
                           doc.list[["idx.initial"]],
                           c(doc.list[["idx.initial"]],
                             doc.list[["idx.amendments"]]
                             )
                           )
                    
  ## Create mapping by loop
  print("mapping final to initial")
  map.initial.final <- MapFun(doc.list[["vs.out"]],
                              distance.fun=distance.fun,
                              idx.final=doc.list[["idx.final"]],
                              idx.compare=doc.list[["idx.initial"]],
                              idx.collection=idx.collection
                              )

  

  if(!is.na(doc.list[["idx.amendments"]]))
    {
      print("mapping final to amendments")
      map.amend.final <- MapFun(doc.list[["vs.out"]],
                                distance.fun=distance.fun,
                                idx.final=doc.list[["idx.final"]],
                                idx.compare=doc.list[["idx.amendments"]],
                                idx.collection=idx.collection
                                )

      map.all <- cbind(map.initial.final,
                       map.amend.final[,2:3]
                       )
      
  
    }else{

      map.all <- cbind(map.initial.final,
                       NA,
                       NA
                       )
  
    }

  names(map.all) <- c("bill2.idx", "bill1.idx", "bill1.dist",
                      "amend.idx", "amend.dist"
                      )

  
  return(map.all)

}
## End MapBills

## Should add roxygen comments here...
ModelChanges <- function(bill1,
                         bill2,
                         map.output,
                         n.topics=NULL,
                         topic.fun="LDA", ...){
  ## Models the additions and deletions between two bills based on a
  ## mapping between the two as provided by MapBills
  ## Uses either LDA or CTM topic modeling to summarize additions and
  ## deletions
  ## Inputs:
  ##    bill1, bill2: character vectors, each element of which is a
  ##     section of a bill. Any level of disaggregation can be used.
  ##    map.output: the output of MapBills for bill1 and bill2
  ##    n.topics: the number of topics to pass to the topic modeler.
  ##     If NULL, then the default is set to 1/5 (?) of the section
  ##     count, separately for added and excluded text
  ##    topic.fun: the topic modeling function to be used. Can be one
  ##    of LDA, CTM, or a user-supplied function
  ## Outputs:
  ##    Two topic models with n.topics topics in each:
  ##     model.deleted: a topic model of the deleted text
  ##     model.added: a topic model of the added text
  
  





}
## End ModelChanges

##' <description>
##' Does the actual pairwise mapping of bills. Maps from doc1:doc2
##via nearest-neighbor matching on the basis of a user-supplied distance / similarity function
##' <details>
##' @title MapFun
##' @param doc1 a document-term matrix 
##' @param doc2 a document-term matrix
##' @param distance.fun a distance function. The underlying distance
##metric should return larger values when two objects are closer
##' @param ... any other arguments to be passed to distance.fun
##' @return a 3-column matrix mapping the row index of doc2 to its
##best match in doc1, and the distance between them
##' @author Mark Huberty
MapFun <- function(dtm,
                   distance.fun="cosine.dist",
                   idx.final,
                   idx.compare,
                   idx.collection){

  if("DocumentTermMatrix" %in% class(dtm))
    dtm <- DtmToMatrix(dtm)
  else
    stopifnot(class(dtm) %in% c("data.frame", "matrix") |
              "dgCMatrix" %in% class(dtm)
              )
    
  dist.fun <- match.fun(distance.fun)

  dist.mat <- dist.fun(dtm, idx.final, idx.compare, idx.collection)

  print("Getting match indices")
  match.idx <- sapply(1:ncol(dist.mat), function(x){

    dist.vec = dist.mat[,x]

    ## TODO: add threshold value here
    ## as in if dist > thresh, idx, else NA
    ## TODO: this is too inflexible for use with min/max
    ## Should pass T/F to decreasing based on min/max
    idx.vec <- order(dist.vec, decreasing=TRUE)

    return(c(idx.vec[1], dist.vec[idx.vec[1]]))
    
  })

  df.out <- data.frame(1:length(idx.final),
                       t(match.idx)
                       )

  names(df.out) <- c("idx.doc.2", "idx.doc.1", "distance")
  return(df.out)
  
}
## End MapFun

##' <description>
##' Creates the document-term vector space representations of the
##' initial and final documents, and any (optional) amendments. Vector
##' space representations are baselined to a common dictionary based on
##' the final document.
##' <details>
##' @title CreateAllVectorSpaces
##' @param doc.initial the first version of a document
##' @param doc.final the final version of the same document
##' @param amendments an optional list of proposed changes to the
##' initial document
##' @param ngram the length of the word set that should be used for
##' the document-term matrix (1-grams are single words, 2-grams are
##' unique 2-word combinations, etc)
##' @param stem should words in the documents be stemmed?
##' @param rm.stopwords boolean, should english stopwords be removed?
##' @param rm.whitespace boolean, should excess whitespace be
##' stripped?
##' @param rm.punctuation boolean, should punctuation be removed?
##' @param filter one of 'NULL', 'sparse', 'tf' and 'tfidf' specifying
##' the           base value for filtering
##' @param filter.thres numeric, indicating filter threshold
##' appropriate for the filter chosen
##' @param weighting one of weightTf, weightTfIdf, or weightBin
##' @return a list of document-term matrices, for the initial and
##' final documents and any proposed amendments, formatted as sparse
##' Matrix objects. The terms in each matrix are consistent with the set
##' of unique terms in the final document.
CreateAllVectorSpaces <- function(doc.initial, doc.final,
                                  amendments=NULL,
                                  ngram=1,
                                  stem=FALSE,
                                  rm.stopwords=FALSE,
                                  rm.whitespace=FALSE,
                                  rm.punctuation=FALSE,
                                  filter=NULL,
                                  filter.thres=NULL,
                                  weighting=weightTf
                                  ){

  ## Check to ensure that the require args are there
  stopifnot(!is.null(doc.initial) &
            !is.null(doc.final) &
            !is.null(ngram)
            )

  ## Check to ensure types are right
  stopifnot(is.character(doc.initial) &
            is.character(doc.final) &
            is.numeric(ngram)
            )

  
  if(!is.null(amendments))
    {
      
      stopifnot(is.character(amendments))
      text.vec <- c(doc.final, doc.initial, amendments)
      
    }else{
      
      text.vec <- c(doc.final, doc.initial)

    }
  
  ## Check to ensure that ngram is structured correctly
  stopifnot(ngram >= 1)

  ## Ensure that ngram is integer-valued
  ngram <- round(ngram, 0)

  vs.all <- CreateVectorSpace(text.vec,
                              ngram=ngram,
                              stem=stem,
                              rm.stopwords=rm.stopwords,
                              rm.whitespace=rm.whitespace,
                              rm.punctuation=rm.punctuation,
                              filter=filter,
                              filter.thres=filter.thres,
                              weighting=weighting
                              )

  idx.final <- 1:length(doc.final)
  idx.initial <-
    (length(doc.final) + 1):(length(doc.final) + length(doc.initial))
  
  if(is.null(amendments))
    {

      idx.amendments <- NA
     
    }else{

       idx.amendments <-
        (length(doc.final) + length(doc.initial) + 1):vs.all$nrow
      
    }

  vs.out <- sparseMatrix(i=vs.all$i,
                         j=vs.all$j,
                         x=vs.all$v,
                         dimnames=vs.all$dimnames
                         )

  #DtmToMatrix(vs.all)  
  list.out <- list(idx.final,
                   idx.initial,
                   idx.amendments,
                   vs.out
                   )
  
  names(list.out) <- c("idx.final", "idx.initial", "idx.amendments", "vs.out")
                   
  return(list.out)
  

}
  ## End CreateAllVectorSpaces

##' <description>
##' Creates the vector space model of a document 
##' <details>
##' @title CreateVectorSpace
##' @param docs a string vector of documents
##' @param ngram an integer specifying the n-gram to use for
  ##                  the vector space
##' @param stem boolean, should the document be stemmed?
##' @param dictionary character vector of terms on which to base
##           the document-term matrix. Terms not in the dictionary
##           will be dropped.See the tm package for
##           documentation. 
##' @param rm.stopwords boolean, should stopwords be removed?
##' @param rm.whitespace boolean, should excess whitespace be removed?
##' @param rm.punctuation boolean, should punctuation be removed?
##' @param filter one of 'NULL', 'sparse', 'tf' and 'tfidf' specifying the
  ##           base value for filtering
##' @param filter.thres numeric, indicating filter threshold
##appropriate for the filter chosen
##' @return  a document-term matrix in sparse representation
##           where each row is a document and each col is an
##           ngram. Cell values are term frequency counts. 
##' @author Mark Huberty
CreateVectorSpace <- function(docs,
                              ngram,
                              stem=FALSE,
                              dictionary=NULL,
                              rm.stopwords=FALSE,
                              rm.whitespace=FALSE,
                              rm.punctuation=FALSE,
                              filter=NULL,
                              filter.thres=NULL,
                              weighting=weightTf
                              ){

  tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngram,
                                                          max = ngram)
                                          )

  corpus.in <-Corpus(VectorSource(docs),
                     readerControl=list(readPlain),
                     language="en",
                     load=TRUE
                     )

  ## Ensure everything is utf-8 compliant
  corpus.in <- tm_map(corpus.in, function(x) iconv(enc2utf8(x), sub = "byte"))
  corpus.in <- tm_map(corpus.in, tolower)

  
  if(rm.stopwords)
    {
      corpus.in <- tm_map(corpus.in,
                          removeWords,
                          stopwords("english")
                          )
    }

  if(rm.whitespace)
    {
      corpus.in <- tm_map(corpus.in, stripWhitespace)
    }

  if(rm.punctuation)
    {
      corpus.in <- tm_map(corpus.in, removePunctuation)
    }

  if(stem)
    {
      corpus.in <- tm_map(corpus.in, stemDocument)
    }

  ## if(ngram > 1)
  ##   {

  ##   }else{

  ##     tokenize=scan(..., what="character")
      
  ##   }
  dtm.corpus <- DocumentTermMatrix(corpus.in,
                                   control=list(dictionary=dictionary,
                                     weighting=weighting,
                                     tokenizer=tokenizer)
                                   )


  return(dtm.corpus)
  
}
## End CreateVectorSpace

##' <description>
##'
##' <details>
##' @title ModelImpact
##' @return TBD
##' @author Mark Huberty
ModelImpact <- function(){
  ## Classifies changes as substantive or administrative
  ## <Question here is _how_: what could you exploit to differentiate
  ## between substance and administrative procedure; preambles and
  ## stated purposes there might provide a good means of doing this
  ## later. Could, for instance, topic model the preamble, assign
  ## paragraphs from the preamble to topics, and then use that model
  ## to classify the rest of the text / changes to the text.>

  ## Should return a class vector (S, A, ?)  w/ probabilities 
  

}
## End ModelImpact

##' ##' <description>
##'
##' <details>
##' @title ComputeIdentity
##' @return TBD
##' @author Mark Huberty
ComputeIdentity <- function(){
  
  ## See Hoad and Zobel (2003) on plagiarism for background. This is
  ## their identity measure 5. 
  ## Computes an "identity" measure that goes beyond cosine distance
  ## to observe the length and complexity of a "document" as well as
  ## its terms.
  ## Inputs: vec1: a vector, representing a document, which is to be
  ## compared to a set of possible matches
  ##         vec2: a vector of the same length as vec1, representing
  ##         one possible match to vec1, drawn from a larger corpus of
  ##         N documents 
  ##         N: total number of documents in the corpus from which
  ##         vec2 was drawn
  ##         tf: a vector of length vec1, with integer counts of the
  ##         number of documents in the comparison corpus that contain
  ##         the terms represented by each entry in vec2 

  



}

##' <description>
##' Takes as input the ComputeVectorAllVectorSpaces output and the
##result from that output from MapBills Returns a LaTeX document that
##presents the side-by-side comparison of the final document sections
##and their matched pairs, with sources for the matched document
##' <details>
##' @title WriteSideBySide
##' @param composite.match the output of GetLikelyComposite
##' @param doc.final the original character string representing the
##final document
##' @param cavs.out the output of CreateAllVectorSpaces used to
##generate the composite match
##' @param col.highlight the color to highlight word changes
##' @param box.amendments boolean, should amendments be boxed?
##' @param col.box the color to shade amendment boxes
##' @param file.out a valid filename for the latex output file
##' @param pdflatex boolean, should PDFLATEX be run on the output file?
##' @return Returns silently. If PDFLATEX is true, returns both the
##.tex file and its PDF output. Otherwise, returns only the .tex file.
##' @author Mark Huberty
WriteSideBySide <- function(composite.match,
                            doc.final,
                            cavs.out,
                            dir.out,
                            col.highlight="red",
                            box.amendments=TRUE,
                            col.box="lightyellow",
                            file.out="generic.tex",
                            pdflatex=TRUE
                            ){

  composite.match <-
    composite.match[order(composite.match$doc.final.idx),]

  preamble="\\documentclass{article}\n\\usepackage{framed}\n\\usepackage{color}\n\\usepackage{marginnote}\n"
  texthighlight <- paste("\\newcommand{\\texthighlight}[1]{\\textcolor{",
                         col.highlight,
                         "}{#1}}\n",
                         sep=""
                         )

  
  sink(paste(dir.out, file.out, sep=""))

  cat(preamble)
  cat(texthighlight)
  cat("\\begin{document}")
  cat("\n\n")

  cat("\\begin{minipage}[t]{0.45\\linewidth}")
  cat("\n")
  cat("Final Bill")
  cat("\n")
  cat("\\end{minipage}")
  cat("\n")
  cat("\\begin{minipage}[t]{0.08\\linewidth}")
  cat("\n")
  cat("\\end{minipage}")
  cat("\\begin{minipage}[t]{0.45\\linewidth}")
  cat("\n")
  cat("Matched Segment")
  cat("\n")
  cat("\\end{minipage}")
  cat("\n\n")
  cat("\\vspace{12pt}")
  cat("\n\n")
  
  for(i in 1:nrow(composite.match))
    {
      origin <- composite.match[i, "match.origin"]
      alt.origin <- composite.match[i, "alt.origin"]
      origin.idx <- composite.match[i, "match.idx"]
      dist <- composite.match[i, "match.dist"]
      
      if(origin == "doc.initial")
        {
          idx.subset <- cavs.out$idx.initial
        }else if(origin == "amendment"){  
          idx.subset <- cavs.out$idx.amendments
        }else{
          idx.subset <- cavs.out$idx.final
        }
      
      ## This stuff is now wrong b/c of changes to the structure of
      ## the cavs output
      word.diff <- ( cavs.out[["vs.out"]][cavs.out$idx.final,][i,] > 0 ) -
        ( cavs.out[["vs.out"]][idx.subset,][origin.idx,] > 0 )
      
      highlight.words <-
        colnames(cavs.out[["vs.out"]])[word.diff > 0]

      highlight.words <- highlight.words[highlight.words != " "]
      
      string.in <- SanitizeTex(doc.final[composite.match$doc.final.idx[i]])
      this.section <-  WriteTexSection(string.in,
                                       highlight.words=highlight.words,
                                       origin=alt.origin,
                                       origin.idx=origin.idx,
                                       dist=NULL,
                                       marginnote=FALSE
                                       )
      
      cat("\\begin{minipage}[t]{0.45\\linewidth}")
      cat("\n")
      cat(this.section)
      cat("\n")
      cat("\\end{minipage}")
      cat("\n")
      cat("\\begin{minipage}[t]{0.08\\linewidth}")
      cat("\n")
      cat("\\end{minipage}")

      word.diff <- ( cavs.out[["vs.out"]][idx.subset,][origin.idx,] > 0 ) -
        ( cavs.out[["vs.out"]][cavs.out$idx.final,][i,] > 0 ) 
             
      highlight.words <-
        colnames(cavs.out[["vs.out"]])[word.diff > 0]

      highlight.words <- highlight.words[highlight.words != " "]
      
      string.in <- SanitizeTex(composite.match$match.txt[i])
      this.section <-  WriteTexSection(string.in,
                                       highlight.words=highlight.words,
                                       origin=alt.origin,
                                       origin.idx=origin.idx,
                                       dist=dist,
                                       marginnote=TRUE
                                       )
      
      cat("\\begin{minipage}[t]{0.45\\linewidth}")
      cat("\n")
      cat(trim(this.section))
      cat("\n")
      cat("\\end{minipage}")
      cat("\n\n")
      cat("\\vspace{12pt}")
      cat("\n\n")
      ##cat("\\rule{\\linewidth}{0.5mm}\n")

    }

  cat("\\end{document}")
  sink()

  
  if(pdflatex)
    {
      call <- paste("pdflatex -output-directory=",
                    dir.out, " ",
                    dir.out,
                    file.out,
                    sep=""
                    )
      system(call)
      system(call)
    }

  print("Done")
  
}
##' <description>
##'
##' <details>
##' @title GetLikelyComposite
##' @param mapbills.out the output of MapBills 
##' @param doc.initial a character string, representing doc.initial as
##used in MapBills
##' @param doc.final a character string, representing doc.final as
##used in MapBills
##' @param amendments a character string, representing amendments as
##used in MapBills 
##' @param amendment.origin a character vector, the same length as
##amendments, indicating the origin of each amendment
##' @param filter One of "max" or "min", indicating how the best match
##should be selected
##' @param dist.threshold A threshold distance; if the best match does
##not pass the threshold, then the best match is the same section of
##the final document itself. 
##' @return A matrix mapping from the index of each section in
##doc.final to its most likely match from doc.initial and
##amendments. The origin, index, text, and distance of the best match
##are all returned.
##' @author Mark Huberty
GetLikelyComposite <- function(mapbills.out,
                               doc.initial,
                               doc.final,
                               amendments=NULL,
                               amendment.origin=NULL,
                               filter="max",
                               dist.threshold=0
                               ){

  ## Make sure that if the amendment origin list is provided,
  ## it's the the same length as the amendments
  stopifnot((!is.null(amendment.origin) &
            length(amendments) == length(amendment.origin))##  |
            ## is.null(amendment.origin)
            )
  
  ## Check distance function and then grab it
  stopifnot(filter %in% c("min", "max"))
  filter.fun <- match.fun(filter)

  
 
  ## Placeholder for amendments--should never be necessary
  if(is.null(amendments))
    amendments <- rep(NA, nrow(mapbills.out))
  
  
  mat.out <- sapply(mapbills.out$bill2.idx, function(x){

    dist.idx <-
      which(mapbills.out[x, c("bill1.dist", "amend.dist")] == 
            filter.fun(mapbills.out[x, c("bill1.dist", "amend.dist")],
                na.rm=TRUE)
            )[1] ## This is a hack, see if it works

    #print(dist.idx)
    
    match.dist <- mapbills.out[x,
                               c("bill1.dist","amend.dist")[dist.idx]]
    match.dist <- ifelse(is.null(match.dist), NA, match.dist)
    #print(match.dist)
    
    pass.threshold <- ifelse(filter == "min",
                             match.dist < dist.threshold,
                             match.dist > dist.threshold
                             )
    
    if(is.na(dist.idx) | is.na(pass.threshold) |
       !pass.threshold | length(dist.idx) == 0){

        vec.out <- c(x, x, "doc.final", "Final", match.dist, doc.final[x])

      }else{

        match.idx <- mapbills.out[x, c("bill1.idx",
                                       "amend.idx")[dist.idx]
                                  ]
        #print(match.idx)
        match.origin <- c("doc.initial", "amendment")[dist.idx]
        #print(match.origin)
        match.txt <- ifelse(match.origin=="doc.initial",
                            doc.initial[match.idx],
                            amendments[match.idx]
                            )

        
        if(dist.idx == 1)
          {
            
            alt.origin <- "Original"
            
          }else{

            if(!is.null(amendment.origin))
              {

                alt.origin <- amendment.origin[match.idx]

              }else{

                alt.origin <- "Amendment"
                
              }
            
          }
        
       
        vec.out <- c(x, match.idx, match.origin, alt.origin,
                     match.dist,
                     match.txt)

        
      }
                  

    return(vec.out)
    
  }) ## End sapply

  
  mat.out <- data.frame(t(mat.out), stringsAsFactors=FALSE)
  colnames(mat.out) <- c("doc.final.idx",
                         "match.idx",
                         "match.origin",
                         "alt.origin",
                         "match.dist",
                         "match.txt"
                         )

  mat.out$doc.final.idx <- as.integer(mat.out$doc.final.idx)
  mat.out$match.dist <- as.numeric(mat.out$match.dist)
  mat.out$match.idx <- as.integer(mat.out$match.idx)

  return(mat.out)

}

##' <description>
##' Writes out a LaTeX representation of the final document, as built
##up from components of the initial document plus amendments. Provides
##options to highlight both word differences and sections that result
##from amendments. The origin of each section is called out in a
##margin note.
##' <details>
##' @title WriteCompositeFinal
##' @param composite.match the output of GetLikelyComposite
##' @param cavs.out the output of CreateAllVectorSpaces used to
##generate the composite match
##' @param col.highlight the color to use in highlighting word differences
##' @param box.amendments boolean, should matches that originate from
##amendments be boxed?
##' @param col.box the color to use in shading amendment boxes
##' @param file.out a valid filename for the latex output file
##' @param pdflatex boolean, should PDFLATEX be run on the output file?
##' @return Returns silently. If PDFLATEX is true, returns both the
##.tex file and its PDF output. Otherwise, returns only the .tex file.
##' @author Mark Huberty
WriteCompositeFinal <- function(composite.match,
                                cavs.out,
                                col.highlight="red",
                                box.amendments=TRUE,
                                col.box="lightyellow",
                                file.out="generic.tex",
                                pdflatex=TRUE
                                ){

  ## Write out the preamble

  composite.match <- composite.match[order(composite.match$doc.final.idx),]

  for(idx in 1:length(cavs.out))
    {
      if("DocumentTermMatrix" %in% class(cavs.out[[idx]]))
        cavs.out[[idx]] <- DtmToMatrix(cavs.out[[idx]])
    }
  preamble="\\documentclass{article}\n\\usepackage{framed}\n\\usepackage{color}\n\\usepackage{marginnote}\n"

  texthighlight <- paste("\\newcommand{\\texthighlight}[1]{\\textcolor{",
                         col.highlight,
                         "}{#1}}\n",
                         sep=""
                         )

  
  sink(file.out)

  cat(preamble)
  cat(texthighlight)
  cat("\\begin{document}")
  cat("\n\n")
  
  for(idx in 1:length(composite.match$doc.final.idx))
    {

      origin <- composite.match[idx, "match.origin"]
      alt.origin <- composite.match[idx, "alt.origin"]
      
      if(origin == "doc.final")
        {
          string.in <- SanitizeTex(composite.match$match.txt[idx])
          this.out <- WriteTexSection(string.in,
                                      highlight.words=NULL,
                                      origin=alt.origin,
                                      origin.idx=idx
                                      )

          cat(this.out)
          cat("\n\n")

        }else{
          
          match.idx <- composite.match[idx,"match.idx"]

          ## Does a diff on the binary final and binary initial
          ## records to detect new words. Does not detect word order.
          ## TODO: insert a better function here to handle this.
          ##       this is really crude.
          word.diff <- ( cavs.out[["doc.final"]][idx,] > 0 ) -
            ( cavs.out[[origin]][match.idx,] > 0 )
          
          highlight.words <-
            cavs.out[["doc.final"]]$colnames[word.diff > 0]
          
          this.out <- WriteTexSection(composite.match$match.txt[idx],
                                      highlight.words=highlight.words,
                                      origin=alt.origin,
                                      origin.idx=match.idx
                                      )

          
          if(origin == "doc.initial"){

            cat(this.out)
            cat("\n\n")

          }else{ ## Then it's an amendment

            if(box.amendments)
              {
                
                cat("\\begin{framed}",
                    this.out,
                    "\\end{framed}",
                    sep="\n"
                    )
                cat("\n\n")
                
              }else{

                cat(this.out)
                cat("\n\n")

              }

          }


        } ## End if / else
    } ## End for loop

  cat("\\end{document}")
  sink()


  if(pdflatex)
    {
      call <- paste("pdflatex", file.out, sep=" ")
      system(call)
      system(call)
    }

  print("Done")
  ## finally write the \end{document}

  ## Then if pdflatex=TRUE, issue a system call to pdflatex <filename>


}

##' ##' <description>
##' For a given text string and a list of words to highlight, it
##reforms the string to be LaTeX-valid and highlights words. If the
##necessary information is provided, the source of the string is
##indicated in a margin note.
##' <details>
##' @title WriteTexSection
##' @param section a string representing a document section
##' @param highlight.words a character vector of words to highlight
##' @param origin a character string representing the origin of the section
##' @param origin.idx the index of the document in the origin source
##(e.g. paragraph number)
##' @param marginnote boolean, should margin notes be printed
##' @return A character string with all LaTeX-sensitive characters
##appropriately escaped, and all highlights and origin information
##inserted with LaTeX-appropriate data. This assumes that the final
##LaTeX document will have a defined command of form \texthighlight{}
##defined. Both WriteSideBySide and WriteComposite provide for this in
##their preamble.
##' @author Mark Huberty
WriteTexSection <- function(section,
                            highlight.words=NULL,
                            origin=NULL,
                            origin.idx=NULL,
                            dist=NULL,
                            marginnote=TRUE
                            ){
  

  string.out <- SanitizeTex(section)

  if(!is.null(highlight.words) & length(highlight.words) > 0)
    {

      for(i in 1:length(highlight.words))
        {

          regexp.in <- paste("(", highlight.words[i], ")", sep="")
          regexp.out <- paste("\\\\\\texthighlight\\{\\1\\}")
          
          string.out <- str_replace_all(string.out,
                                        regexp.in,
                                        regexp.out
                                        )
          
        }

    }
  
  if(!is.null(origin) & marginnote)
    {

      string.out <- paste("\\marginnote{",
                          origin,
                          "\n",
                          origin.idx,
                          "\n",
                          round(dist, 3),
                          "}",
                          trim(string.out),
                          sep=""
                          )

    }

  return(string.out)
    

}
##' <description>
##' Converts a DocumentTermMatrix from the tm() package into a sparse
##Matrix. This is the same function as John Myles White included in
##his textregression() library.
##' <details>
##' @title DtmToMatrix
##' @param dtm a document-term matrix as output from the
##DocumentTermMatrix from the tm() package
##' @return an equivalent sparse matrix as represented in the Matrix package.
##' @author Mark Huberty
DtmToMatrix <- function(dtm){
  
  m <- Matrix(0, nrow = dtm$nrow, ncol = dtm$ncol, sparse = TRUE,
              dimnames=dtm$dimnames
              )
  
  for (index in 1:length(dtm$i))
    {
      m[dtm$i[index], dtm$j[index]] <- dtm$v[index]
    }
  
  return(m)
}

##' <description>
##' Sanitizes special LaTeX characters in a string so that pdflatex
##' will handle them correctly. All special characters are replaced
##' with their escaped equivalents.
##' <details>
##' @title SanitizeTex
##' @param string a string to be inserted in a LaTeX file
##' @return the same string, with the LaTeX special characters
##(%$_{}\~^&) replaced with backslash-escaped equivalents
##' @author Mark Huberty
SanitizeTex <- function(string){

  out <- str_replace_all(string, "([%$_{}\\~\\^&])", "\\\\\\1")
  return(out)
  
}

## TODO Need to be modified to take dtm, not done yet.
##' <description>
##' A wrapper function for the cosine() function in the lsa package,
##which allows it to work with the MapBills function
##' <details>
##' @title cosine.dist
##' @param dtm a document-term matrix, such as is output from
##CreateAllVectorSpaces(), containing the term frequency vectors of
##both the documents for which matches are needed, and the set of
##candidate match documents D
##' @param idx.dtm1 a row index indicating the location of the first document
##' @param idx.dtm2 a row indiex indicating the location of a second document
##' @param idx.collection a vector of indices indicating which rows in
##dtm are for the set of comparison documents D (as opposed to the
##documents requiring matches
##' @return the output of cosine()
##' @author Mark Huberty
cosine.dist <- function(dtm, idx.dtm1, idx.dtm2, idx.collection){


  out = cosine(dtm[idx.dtm1,], dtm[idx.dtm2,])

  return(out)

}


##' <description>
##' A wrapper function for similarity() that takes a standard set of
##' inputs and handles pre-processing for the outputs
##' <details>
##' @title similarity.dist
##' @param dtm a document-term matrix, such as is output from
##' CreateAllVectorSpaces(), containing the term frequency vectors of
##' both the documents for which matches are needed, and the set of
##' candidate match documents D
##' @param idx.dtm1 a row index indicating the location of the first document
##' @param idx.dtm2 a row indiex indicating the location of a second document
##' @param idx.collection a vector of indices indicating which rows in
##dtm are for the set of comparison documents D (as opposed to the
##documents requiring matches
##' @return the output of similarity()
##' @author Mark Huberty
similarity.dist <- function(dtm, idx.query, idx.compare, idx.collection){

  N <- length(idx.collection)
  ft <- colSums(dtm[idx.collection,] > 0)
  
  out <- sapply(idx.query, function(x){
    sapply(idx.compare, function(y){
      similarity(dtm[x,], dtm[y,], N, ft)
    })
  })

  return(out)

}
##' <description>
##' Computes similarity measure #5 from Hoad & Zobel (2003)
##' <details>
##' @title similarity
##' @param vec.d a term-freqency vector from a set of candidate
##matches D
##' @param vec.q a term-frequency vector of a document being matched
##to candidates in D
##' @param N The number of documents in D
##' @param ft The number of documents in D containing term t
##' @return a numeric similarity measure, where larger numbers
##indicate more similar documents
##' @author Mark Huberty
similarity <-function(vec.d,
                      vec.q,
                      N,
                      ft
                      ){

  ## These two lines were wrong--don't want number of
  ## unique terms, want number of terms. 
  #fd <- sum(vec.d > 0)
  #fq <- sum(vec.q > 0)

  
  fd <- sum(vec.d)
  fq <- sum(vec.q)
  
  T <- (vec.q > 0 & vec.d > 0)
  
  ft_sub = ft[T]
  
  vec.weights <- (N / ft_sub) / (1 + abs(vec.d[T] - vec.q[T]))
  
  weight<- sum(vec.weights)
  
  out = 1 / (1 + log(1 + abs(fd - fq))) * weight

  return(out)

}



cosine.length.mat <- function(dtm, idx.query, idx.compare, idx.collection){

  rq <- rowSums(dtm[idx.query,])
  rd <- rowSums(dtm[idx.compare,])

  len.coef <- 1 / (1 + log(1 + outer(rd, rq, vector.diff)))
  mat <- cosine.mat(dtm, idx.query, idx.compare, idx.collection)

  out <- len.coef * mat

  return(out)

}

vector.diff <- function(x, y){

  abs(x - y)

}


## Could do the matrix/vector diffs with outer(x, y, fun)

## This isn't tested. Trying for a vectorized version of the
## cosine similarity to cut out the loop function. Notice that this
## returns the entire distance matrix, so it doesn't slot in correctly
## just yet. Would have to rewrite everything else to take as inputs
## the dtm, and idx.final / idx.initial, idx.collection. Then
## could return the sets of distance matrices, etc. Trying to make
## this much faster, to cut out the 130k loops that result.


##' <description>
##'
##' <details>
##' @title 
##' @param dtm 
##' @param idx.query 
##' @param idx.compare 
##' @param idx.collection 
##' @return 
##' @author Mark Huberty
cosine.mat <- function(dtm, idx.query, idx.compare, idx.collection){

  dtm.query <- dtm[idx.query,]
  dtm.compare <- dtm[idx.compare,]
  
  numerator <- dtm.compare %*% t(dtm.query)

  denominator.a <- sqrt(rowSums(dtm[idx.query,]^2))
  denominator.b <- sqrt(rowSums(dtm[idx.compare,]^2))

  denominator <- denominator.b %*% t(denominator.a)
  
  out <- numerator / denominator

  return(out)

}




##' <description>
##' Provides a facility to easily model the content of different
##classes of matched content
##' <details>
##' @title ModelDocSet
##' @param doc.list the output from CreateAllVectorSpaces
##' @param composite.mat the output from GetLikelyComposite
##' @param type the subset of the text to be clustered by topic: one
##' of "incl.amend" (default), "rej.amend", "incl.orig", "rej.orig"
##' @param k the number of topics to model
##' @param method the topicmodeling method to use (one of "LDA" or
##' "CTM")
##' @param n.terms the number of terms to show in the returned object
##' @param ... other arguments as required; see ModelTopics
##' @return a ModelTopics object, and additionally an index of
## of the documents as it points to the text inputs, rather than the
##document-term matrix
##' @author Mark Huberty
ModelDocSet <- function(doc.list,
                        composite.mat,
                        type="incl.amend",
                        k=NULL,
                        topic.method="LDA",
                        sampling.method="VEM",
                        n.terms=5,
                        addl.stopwords="NULL",
                        ...){

  stopifnot(type %in% c("incl.amend", "rej.amend",
                        "incl.orig", "rej.orig",
                        "final"
                        )
            )
  
  if(type == "incl.amend")
    {

      dtm.idx <- doc.list$idx.amendments
      orig.idx <-
        composite.mat$match.idx[composite.mat$match.origin=="amendment"]

      topic.idx <- dtm.idx[orig.idx]
      
    }else if(type=="rej.amend"){

      dtm.idx <- doc.list$idx.amendments
      orig.idx <-
        composite.mat$match.idx[composite.mat$match.origin=="amendment"]

      topic.idx <- dtm.idx[-orig.idx]
      
    }else if(type=="incl.orig"){

      dtm.idx <- doc.list$idx.initial
      orig.idx <-
        composite.mat$match.idx[composite.mat$match.origin=="doc.initial"]

      topic.idx <- dtm.idx[orig.idx]


    }else if(type=="rej.orig"){

      dtm.idx <- doc.list$idx.initial
      orig.idx <-
        composite.mat$match.idx[composite.mat$match.origin=="doc.initial"]

      topic.idx <- dtm.idx[-orig.idx]
      
    }else{

      dtm.idx <- doc.list$idx.final
      orig.idx <-
        composite.mat$match.idx[composite.mat$match.origin=="doc.final"]

      topic.idx <- dtm.idx[orig.idx]


    }

  #print(type)
  #print(topic.idx)
  
  model.out <- ModelTopics(doc.list$vs.out,
                           topic.idx,
                           k=k,
                           topic.method=topic.method,
                           sampling.method=sampling.method,
                           n.terms=n.terms,
                           addl.stopwords,
                           ...
                           )

  model.out$txt.idx <- model.out$dtm.idx - min(dtm.idx) + 1
  
  return(model.out)


}


###
##' <description>
##' Provides an interface to model the content of different amendment
##' slices using Latent Dirichelet Allocation methods as supported in
##' the topicmodels package. See the topicmodels package documentation
##' for more details.
##' <details>
##' @title ModelTopics
##' @param dtm A document-term matrix as output from
##' CreateAllVectorSpaces
##' @param idx The indices of the document-term matrix to model
##' @param k The number of topics to model
##' @param topic.method One of "LDA" or "CTM"
##' @param sampling.method One of "VEM" or "Gibbs
##' @param addl.stopwords Additional stopwords to remove
##' @param n.terms the number of terms by topic to return. See the
##' terms() function in topicmodels for details.
##' @param ... other arguments as passed to the LDA or CTM methods
##' @param method one of "LDA" (default) or "CTM. See the topicmodels
##' documentation for details. The LDA default assumes independence in
##' term distributions across topics. This may not be appropriate. 
##' @return A list containing the topic model, the top N terms by topic, and the topic assignments for each document indicated by idx
##' @author Mark Huberty
ModelTopics <- function(dtm, idx, k=NULL, topic.method="LDA",
                        sampling.method, addl.stopwords=NULL,
                        n.terms, ...){

  if(!is.null(addl.stopwords))
    {

      to.remove <-
        lapply(addl.stopwords, function(x){

          grepl(x, colnames(dtm))
          
          })

      remove.vec <- rep(FALSE, ncol(dtm))
      for(i in 1:length(to.remove))
        {
          remove.vec <- remove.vec | to.remove[[i]]
        }
      print(sum(remove.vec))
      dtm <- dtm[, !remove.vec]
  
    }

  
  dtm.sub <- dtm[idx,]
  print(dim(dtm.sub))
  ## Check to ensure that all rows have at least one term
  has.terms <- rowSums(dtm.sub) > 0
  dtm.sub <- dtm.sub[has.terms,]
  idx <- idx[has.terms]

  
  this.dtm <- sparseToDtm(dtm.sub)
  print(dim(this.dtm))

  if(is.null(k))
    k <- ceiling(nrow(this.dtm) / 10)

  topic.fun <- match.fun(topic.method)

  out <- topic.fun(this.dtm, method=sampling.method, k=k, ...)

  terms.out <- terms(out, n.terms)
  topics.out <- topics(out)
  list.out <- list(out, terms.out, topics.out, idx)
  names(list.out) <- c("topic.model", "terms", "topics", "dtm.idx")
  return(list.out)

}

##' <description>
##' Helper function to translate a sparse Matrix into
##' a document-term matrix equivalent to that produced by the tm package
##' <details>
##' @title sparseToDtm
##' @param sparseM : a sparse Matrix of form dgCMatrix
##' @return a simple_triplet_matrix as described in the slam package,
##with the same dimensions and properties as sparseM
##' @author Mark Huberty
sparseToDtm <- function(sparseM){

  require(slam)
  stm <- as.simple_triplet_matrix(sparseM)
  class(stm) <- append(class(stm), "DocumentTermMatrix")

  return(stm)

}

##' <description>
##' Provides a function interface for learning the best match quality
##' threshold from a human-coded document. Given a mapping of source
##' to target document and a sequence of threshold values, it will
##' return the optimum based on either maximization of the accuracy rate
##' or miniminzation of the false positive/negative rate.
##' <details>
##' @title 
##' @param map.bills.out the output of MapBills for this document 
##' @param initial.bill the text of the initial bill
##' @param final.bill the text of the final bill
##' @param amendments amendment text, in the same order as was passed
##' to MapBills
##' @param labels the origin of the amendments
##' @param filter one of "min" or "max" depending on the distance
##' function used in MapBills
##' @param threshold.values a numeric vector of potential threshold
##' values. See GetLikelyComposite for the definition of the threshold
##' value. A suitable granular vector is recommended, as in seq(0,0.5, 0.005)
##' @param encoder.out the output of run.encoder for the human-coded
##' matches of these documents
##' @param type one of "overall" (overall accuracy) or "tradeoff"
##' (false positive/negative)
##' @return a list containing the entire output of the algorithm and
##' the best threshold value
##' @author Mark Huberty
learn.threshold <- function(map.bills.out,
                            initial.bill,
                            final.bill,
                            amendments,
                            labels,
                            filter="max",
                            threshold.values,
                            encoder.out,
                            type="overall"){

  
  if(type == "overall")
    {
      accuracy.measures <- get.accuracy.measures(map.bills.out,
                                                 initial.bill,
                                                 final.bill,
                                                 amendments,
                                                 labels,
                                                 filter="max",
                                                 threshold.values,
                                                 encoder.out
                                                 )

      best.idx <- which(accuracy.measures$overall ==
                        max(accuracy.measures$overall)
                        )
    }else if(type == "tradeoff"){

      accuracy.measures <- get.type.errors(map.bills.out,
                                           initial.bill,
                                           final.bill,
                                           amendments,
                                           labels,
                                           filter="max",
                                           threshold.values,
                                           encoder.out
                                           )
      ## Minimize the absolute difference between the
      ## negative match accuracy rate and the positive match accuracy rate
      accuracy.dif <- abs(accuracy.measures$pct.no.match -
                          accuracy.measures$pct.positive.match)
      best.idx <- which(accuracy.dif == min(accuracy.dif))
    }

  optimum.threshold <- accuracy.measures$threshold.value[best.idx]

  return(list(optimum.threshold, accuracy.measures))

}
##' <description>
##' Calculates the overall accuracy rate by threshold value for a set
##' of documents based on a human-coded set of matches. See
##' learn.threshold() for more details.
##' <details>
##' @title get.accuracy.measures 
##' @param map.bills.out 
##' @param initial.bill 
##' @param final.bill 
##' @param amendments 
##' @param labels 
##' @param filter 
##' @param threshold.values 
##' @param encoder.out 
##' @return 
##' @author Mark Huberty
get.accuracy.measures <- function(map.bills.out,
                                  initial.bill,
                                  final.bill,
                                  amendments,
                                  labels,
                                  filter="max",
                                  threshold.values,
                                  encoder.out){

  accuracy.measures <- sapply(threshold.values, function(x){

    cb <- GetLikelyComposite(map.bills.out,
                             initial.bill,
                             final.bill,
                             amendments,
                             labels,
                             filter=filter,
                             dist.threshold=x
                             )
    ## Subset in case only some of the bill was hand-coded
    cb <- cb[encoder.out$target.index,]
    
    tab.source.acc <- table(cb$match.origin,
                            encoder.out$match.source
                            )
    pct.source.acc <- sum(diag(tab.source.acc)) / sum(tab.source.acc)

    overall.acc <- (cb$match.origin == encoder.out$match.source &
                    cb$match.idx == encoder.out$match.index
                    )
    pct.overall.acc <- sum(overall.acc) / length(overall.acc)

    out <- c(pct.overall.acc, pct.source.acc)


  })

  accuracy.measures <- data.frame(t(accuracy.measures))
  names(accuracy.measures) <- c("overall", "source")
  accuracy.measures$threshold.value <- threshold.values

  return(accuracy.measures)
}

##' <description>
##' Optimizes the tradeoff between false negative values (rejecting
##' matches that should have been matched to source documents) and false
##' positive values (accepting matches for which no match existed), on
##' the basis of the threshold value. See learn.threshold for more detail.
##' <details>
##' @title get.type.error
##' @param map.bills.out 
##' @param initial.bill 
##' @param final.bill 
##' @param amendments 
##' @param labelsfilter 
##' @param threshold.values 
##' @param encoder.out 
##' @return 
##' @author Mark Huberty
get.type.errors <- function(map.bills.out,
                            initial.bill,
                            final.bill,
                            amendments,
                            labels,
                            filter="max",
                            threshold.values,
                            encoder.out){
  
  accuracy.measures <- sapply(threshold.values, function(x){

    cb <- GetLikelyComposite(map.bills.out,
                             initial.bill,
                             final.bill,
                             amendments,
                             labels,
                             filter=filter,
                             dist.threshold=x
                             )
    ## Subset in case only some of the bill was hand-coded
    cb <- cb[encoder.out$target.index,]
    
    pct.no.match <- sum(cb$match.origin == "doc.final" &
                        encoder.out$match.source == "doc.final"
                        ) / sum(encoder.out$match.source ==
                                "doc.final"
                                )

    pct.positive.match <- sum((cb$match.origin != "doc.final" &
                               encoder.out$match.source != "doc.final") &
                              cb$match.idx == encoder.out$match.index
                              ) / sum(encoder.out$match.source !=
                                      "doc.final")
    
    return(c(pct.no.match, pct.positive.match))
  })
  
  accuracy.measures <- data.frame(t(accuracy.measures))
  names(accuracy.measures) <- c("pct.no.match", "pct.positive.match")
  accuracy.measures$threshold.value <- threshold.values
  return(accuracy.measures)
}

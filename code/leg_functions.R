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

require(tm)
require(topicmodels)
require(stringr)
require(lsa)
require(Matrix)
#require(openNLP) ## For the sentence tokenizer functionality

MapBills <- function(doc.list,
                     distance.fun="cosine"
                     ){
  ## Maps content of bill1 to bill2. Returns a vector of indices
  ## mapping the sections of bill1 (however provided by the user) to
  ## the sections of bill2. Mapping is accompanied by a distance
  ## measure to provide a confidence interval.
  ## Inputs:
  ##    doc.list: the output of CreateAllVectorSpaces, containing the
  ##     vector spaces for the initial and final bills and
  ##     (optionally) the amendments.
  ##    
  ##    dist.fun: the distance function to be used in mapping bill
  ##     sections to each other. Default is "raw"
  ##    n.to.return: number of matched clauses to return. Default is
  ##     1, meaning that only the closest match is returned.
  ##    dist.threshold: a distance threshold to use for determining
  ##    matches. If NULL, this defaults to assuming the
  ##    nearest-neighbor match and returning the match + distance
  ## Outputs:
  ##    bill.map: a data frame of indices mapping _from_ bill1 and (if
  ##    provided) the amendments, _to_
  ##     bill2. New sections are indicated by "N". Deleted sections
  ##     are indicated by "D". If amendments are passed, then "N"
  ##     values are also paired with the amendment that most likely
  ##     became the new section.
  ##     A normalized distance measure (on [0,1]) should be provided for
  ##     all matches

    ## Various checks


  ## maps bills bill1:bill2

  ## Create mapping by loop
  
  map.initial.final <- MapFun(doc.list[["doc.initial"]],
                              doc.list[["doc.final"]],
                              distance.fun=distance.fun
                              )
  

  if(!is.null(doc.list[["amendments"]]))
    {
      map.amend.final <- MapFun(doc.list[["amendments"]],
                                doc.list[["doc.final"]],
                                distance.fun=distance.fun
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

MapFun <- function(doc1, doc2, distance.fun="cosine"){
  ## Does the actual pairwise mapping of bills. Maps from doc1:doc2
  ## Input: doc1, doc2: two doc-term matrices. "docs" must be bills
  ##    or amendments at same level of disaggregation
  ##    (i.e. article, paragraph, etc).
  ##        distance.fun: a string denoting the distance metric function; must return a
  ##        scalar distance value
  ## Output: a data frame with three columns, of length(doc2)
  ##    Format: doc1.row:doc2.row:distance
  ## Process: constructs a pairwise distance matrix from all
  ##    entries in doc1 to all entries in doc2. Then picks nearest-
  ##    neighbor match from doc1 for doc2.

  ## TODO: notice that the sapply scales linearly with document
  ## size. Can this be improved?

  ## TODO: this is going to require the use of matrices or sparse
  ## matrices.
  ## The DocumentTermMatrix object from tm() doesn't allow row calls
  ## and so can't be referenced as done here. should probably use
  ## Matrix representation.

  if("DocumentTermMatrix" %in% class(doc1))
    doc1 <- DtmToMatrix(doc1)
  if("DocumentTermMatrix" %in% class(doc2))
    doc2 <- DtmToMatrix(doc2)

  stopifnot(class(doc1) %in% c("Matrix", "data.frame", "matrix") |
            class(doc2) %in% c("Matrix", "data.frame", "matrix")
            )
  
  dist.fun <- match.fun(distance.fun)

  dist.mat <- sapply(1:nrow(doc2), function(x){
    sapply(1:nrow(doc1), function(y){
      
      dist.out <- dist.fun(doc2[x,], doc1[y,])

      return(dist.out)

    })
  })

  match.idx <- sapply(1:ncol(dist.mat), function(x){

    dist.vec = dist.mat[,x]

    ## TODO: add threshold value here
    ## as in if dist > thresh, idx, else NA
    idx.vec <- order(dist.vec, decreasing=TRUE)

    return(c(idx.vec[1], dist.vec[idx.vec[1]]))
    
  })

  df.out <- data.frame(1:nrow(doc2),
                       t(match.idx)
                       )

  names(df.out) <- c("idx.doc.2", "idx.doc.1", "distance")
  return(df.out)
  
}
## End MapFun


CreateAllVectorSpaces <- function(doc.initial, doc.final,
                                  amendments=NULL,
                                  ngram=1,
                                  stem=FALSE,
                                  rm.stopwords=FALSE,
                                  rm.whitespace=FALSE,
                                  rm.punctuation=FALSE,
                                  filter=NULL,
                                  filter.thres=NULL
                                  ){

  ## Creates a mutually consistent set of vector spaces across a
  ## series of documents. Takes as input the initial and final
  ## documents and an optional vector of amendments. Returns a list
  ## with the vector spaces of each document provided, plus optional
  ## information
  ## Inputs: doc.initial: the first version of the document
  ##         doc.final:   the final version of the same document
  ##         amendments:  an optional list of potential changes to
  ##                      doc.initial, all or some of which may have
  ##                      occurred to create doc.final
  ##         stem:        should the Porter stemmer be used to stem
  ##                      words?
  ##         rm.stopwords: should english stopwords be removed?
  ##         rm.whitespace: should excess whitespace be removed?
  ##         rm.punctuation: should punctuation be removed?
  ##         filter: one of 'NULL', 'sparse', 'tf', and 'tfidf'
  ##                 specifying the filter that should be applied to doc.final to
  ##                 generate the baseline vocabulary
  ##         filter.thres: a threshold value for filter, if filter is
  ##                       not null
  ## Process: Runs CreateVectorSpace on doc.final to generate the
  ## master dictionary. Then creates vector spaces for doc.initial
  ## and amendments based on this dictionary. Only doc.final is
  ## filtered for sparseness--the other documents inherit the term
  ## list that results from the doc.final vector space.

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
    stopifnot(is.character(amendments))

  ## Check to ensure that ngram is structured correctly
  stopifnot(ngram >= 1)

  ## Ensure that ngram is integer-valued
  ngram <- round(ngram, 0)
  
  vs.final <- CreateVectorSpace(doc.final,
                                ngram=ngram,
                                stem=stem,
                                rm.stopwords=rm.stopwords,
                                rm.whitespace=rm.whitespace,
                                rm.punctuation=rm.punctuation,
                                filter=filter,
                                filter.thres=filter.thres
                                )

  vs.initial <- CreateVectorSpace(doc.initial,
                                  ngram=ngram,
                                  stem=stem,
                                  rm.stopwords=rm.stopwords,
                                  rm.whitespace=rm.whitespace,
                                  rm.punctuation=rm.punctuation,
                                  filter=NULL,
                                  filter.thres=NULL,
                                  dictionary=vs.final$dimnames$Terms
                                  )

  if(!is.null(amendments))
    {
      vs.amend <- CreateVectorSpace(amendments,
                                    ngram=ngram,
                                    stem=stem,
                                    rm.stopwords=rm.stopwords,
                                    rm.whitespace=rm.whitespace,
                                    rm.punctuation=rm.punctuation,
                                    filter=NULL,
                                    filter.thres=NULL,
                                    dictionary=vs.final$dimnames$Terms
                                    )
    }else{

      vs.amend <- NULL

    }
  
  list.out <- list(vs.final,
                   vs.initial,
                   vs.amend
                   )
  
  names(list.out) <- c("doc.final", "doc.initial", "amendments")
                   
  return(list.out)
  

}
  ## End CreateAllVectorSpaces

CreateVectorSpace <- function(docs,
                              ngram,
                              stem=FALSE,
                              dictionary=NULL,
                              rm.stopwords=FALSE,
                              rm.whitespace=FALSE,
                              rm.punctuation=FALSE,
                              filter=NULL,
                              filter.thres=NULL
                              ){
  ## Creates the vector space model of a document set
  ## Inputs:   docs: a string vector of documents
  ##           ngram: an integer specifying the n-gram to use for
  ##                  the vector space
  ##           stem: should the document be stemmed?
  ##           dictionary: character vector of terms on which to base
  ##           the document-term matrix. Terms not in the dictionary
  ##           will be dropped.See the tm package for
  ##           documentation. 
  ##           filter: one of 'NULL', 'sparse', 'tf' and 'tfidf' specifying the
  ##           base value for filtering
  ##           filter.thresh: a filter threshold value consistent with
  ##           the filter. e.g., for sparseness, a decimal value on
  ##           (0,1] that indicates the sparseness threshold.
  ## Outputs:  dtm: a document-term matrix in sparse representation
  ##           where each row is a document and each col is an
  ##           ngram. Cell values are term frequency counts. 

  corpus.in <-Corpus(VectorSource(docs),
                     readerControl=list(readPlain),
                     language="en",
                     load=TRUE
                     )

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

  dtm.corpus <- DocumentTermMatrix(corpus.in,
                                   control=list(dictionary=dictionary)
                                   )


  return(dtm.corpus)
  
}
## End CreateVectorSpace


## This may not be necessary -- given the dictionary argument
## available to DocumentTermMatrix.
OrderVectorSpace <- function(dtm, master.term.vec){
  ## Orders the columns of a doc-term matrix to match
  ## a master vector. Inserts 0-valued columns if necessary
  ## Output: a dtm with documents as rows and length(master.term.vec) columns



}
## End OrderVectorSpace

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

WriteSideBySide <- function(){
  ## Takes as input the ComputeVectorAllVectorSpaces output and the
  ## result from that output from MapBills
  ## Returns a LaTeX document that presents the initial: final +
  ## amendments mapping, color coded as desired.



}

GetLikelyComposite <- function(mapbills.out,
                               doc.initial,
                               doc.final,
                               amendments=NULL,
                               amendment.origin=NULL,
                               filter="max",
                               dist.threshold=0
                               ){

  ## Inputs: mapbills.out: the output of mapbills
  ##         doc.initial: the character string of the initial document
  ##         doc.final: the character string of the final document
  ##         amendments: the character string of amendments (optional)
  ##         amendment.origin: a character vector as long as
  ##         amendments, containing alternate descriptions of where
  ##           the amendments came from (i.e. Committee for the
  ##           Environment)
  ##         filter: one of "min" or "max".
  ##         dist.threshold: the threshold for determining whether any
  ##           given match is in fact a real match (should be chosen
  ##           with consideration of the distance function in mapbills,
  ##           and the filter function.
  ## Output: bill2.idx, match.idx, match.origin, match.txt

  ## Make sure that if the amendment origin list is provided,
  ## it's the the same length as the amendments
  stopifnot((!is.null(amendment.origin) &
            length(amendments) == length(amendment.origin)) |
            is.null(amendment.origin)
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
            )
    
    match.dist <- mapbills.out[x, c("bill1.dist","amend.dist")[dist.idx]]
    
    pass.threshold <- ifelse(filter == "min",
                             match.dist < dist.threshold,
                             match.dist > dist.threshold
                             )
    
    if(is.na(dist.idx) | is.na(pass.threshold) | !pass.threshold)
      {

        vec.out <- c(x, x, "doc.final", "Final", doc.final[x])

      }else{

        match.idx <- mapbills.out[x, c("bill1.idx", "amend.idx")[dist.idx]]
        match.origin <- c("doc.initial", "amendment")[dist.idx]
            
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
        
       
        vec.out <- c(x, match.idx, match.origin, alt.origin, match.txt)

      }
                  

    return(vec.out)
    
  }) ## End sapply

  mat.out <- data.frame(t(mat.out))
  colnames(mat.out) <- c("doc.final.idx",
                         "match.idx",
                         "match.origin",
                         "alt.origin",
                         "match.txt"
                         )
  mat.out$doc.final.idx <- as.integer(mat.out$doc.final.idx)
  mat.out$match.idx <- as.integer(mat.out$match.idx)

  return(mat.out)

}


WriteCompositeFinal <- function(composite.match,
                                cavs.out,
                                col.highlight="red",
                                box.amendments=TRUE,
                                col.box="lightyellow",
                                file.out="generic.tex",
                                pdflatex=TRUE
                                ){

  ## Inputs:
  ## composite.match: the output of GetLikelyComposite
  ## col.highlight: the color of highlighted words
  ## box.amendments: should sections that result from amendments be
  ##                 boxed?
  ## col.box: what color should the box be?
  ## file.out: where should the resulting TeX file be written.
  ## Returns a composite version of the final document, built up from
  ## the initial document + the amendments
  ## The composite version is marked up to be typeset in LaTeX
  ## Changes are highlighed; the source of new material is highlighted
  ## in margin notes. The resulting LaTeX document can be compiled
  ## directly if pdflatex=TRUE. Guaranteed to work for OS X and *nix
  ## options.

  ## Write out the preamble

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

          this.out <- WriteTexSection(composite.match$match.txt[idx],
                                      highlight.words=NULL,
                                      origin=alt.origin,
                                      origin.idx=idx
                                      )

          cat(this.out)

        }else{
          
          match.idx <- composite.match[idx,"match.idx"]

          ## Does a diff on the binary final and binary initial
          ## records to detect new words. Does not detect word order.
          ## TODO: insert a better function here to handle this.
          ##       this is really crude.
          word.diff <- ( cavs.out[["doc.final"]][idx,] > 0 ) -
            ( cavs.out[[origin]][match.idx,] > 0 )
          
          highlight.words <-
            colnames(cavs.out[["doc.final"]])[word.diff > 0]
          
          this.out <- WriteTexSection(composite.match$match.txt[idx],
                                      highlight.words=highlight.words,
                                      origin=alt.origin,
                                      origin.idx=match.idx
                                      )

          
          if(origin == "doc.initial"){

            cat(this.out)
            cat("\n\n")

          }else{ ## Then it's an amendment

            if(box.amendment)
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
    }

  print("Done")
  ## finally write the \end{document}

  ## Then if pdflatex=TRUE, issue a system call to pdflatex <filename>


}

WriteTexSection <- function(section,
                            highlight.words=NULL,
                            origin=NULL,
                            origin.idx=NULL
                            ){
  
  ## Inputs: section: a text section for insertion into a TeX document
  ##         highlight.words: a list of unique words to be highlighted
  ##         origin: the origin of the section (author or source)
  ## Outputs: a marked-up string, with a margin note indicating origin
  ##          and the highlighted words colored.
  ## Note:   the function assumes that the final TeX document will
  ##         have a command defined as \texthighlight{} that colors a
  ##         word appropriately. 
string.out <- section
if(!is.null(highlight.words))
    {

      for(i in 1:length(highlight.words))
        {

          regexp.in <- paste("(", highlight.words[i], ")", sep="")
          regexp.out <- paste("\\\\\\texthighlight\\{\\1\\}")
          
          string.out <- gsub(regexp.in,
                             regexp.out,
                             string.out,
                             fixed=FALSE
                             )

        }

    }
  
  if(!is.null(origin))
    {

      string.out <- paste(string.out,
                          "\\marginnote{",
                          origin,
                          " ",
                          origin.idx,
                          "}",
                          sep=""
                          )

    }

  return(string.out)
    

}

DtmToMatrix <- function(dtm)
{
  m <- Matrix(0, nrow = dtm$nrow, ncol = dtm$ncol, sparse = TRUE)
  
  for (index in 1:length(dtm$i))
  {
    m[dtm$i[index], dtm$j[index]] <- dtm$v[index]
  }
  
  return(m)
}

## ## Not at all clear this will work. See the .py
## ## code for doing this with EU bills. Very very messy.
## FormatText <- function(candidate.text,
##                        disagg.level="p",
##                        disagg.lables=NULL,
##                        sep="\\n\\n"){
##   ## Takes as input an undifferentiated character string with the
##   ## text of interest and formats it for use by other models

##   ## Note that the input strings should be clean--no page nos, artifacts
##   ## of OCR'ing, etc.

##   ## sep refers to the distinguishing feature that divides the string
##   ## at the disagg level (one of c:clause, p:paragraph; s:sentence)


## }
## End FormatText



## Other potential functions:
## FormatText: takes as input some text in a particular format (clean)
##             and returns the files needed as input to the main functions
##             This could be a little weird--would ideally want to map
##             to actual clause/paragraph stuff, but could use dummies
##             if not provided. What about the deletion problem(see
##             below)?
##             Maybe provides a means of choosing the level of disagg
##             you want (clause, paragraph, sentence) with defaults
##             for the requisite separator
##                     (clause: ?, paragraph: \n\n, sentence: [.]\w)
## AmendmentSimilarity: provides a measure of similarty among accepted
##      amendments
## PrintAnnotatedBill: Stuffs final bill text into a LaTeX file,
##      color-coded by origin, with legend and (maybe) margin notes
##      (marginpar package) about
##      where sections came from or distance measure?
## TabulateContributions: Tabulate contributions by provided origin
## (i.e. party or committee or whatever) and impact (substance, admin)


## Questions:
## How to handle deletions? As in, amendments that push to drop a
## paragraph? would want some way to formalize this

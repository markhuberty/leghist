#################################################################
## Mark Huberty and Hillary Sanders
## leghist: Automated analysis of legislative history
## Begun: 4 November 2011

## leghist provides functionality within the R programming language
## to automate the textual evolution of legislation from its introduction as
## a bill, through the amendment process, until finalization.

## leghist provides five core pieces of functionality:
## 1. Mapping of sections between editions of bills
## 2. Mapping of amendments to their location in bills
## 3. Identification of discarded material
## 4. Modeling of the content of added and discarded material
## 5. Visualization of the flow of bill content, by subject matter
##    area, from the initial to final bill.

## Coding conventions follow the Google R style guide, at
## http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html

## LICENSE:
## Created by Mark Huberty and Hillary Sanders on 2012-05-19
## Copyright (c) 2012, under the Simplified BSD License.
## For more information on FreeBSD see:
## http://www.opensource.org/licenses/bsd-license.php All rights reserved.
## END
#################################################################
## require(gdata)
## require(tm)
## require(topicmodels)
## require(stringr)
## require(lsa)
## require(Matrix)
## require(RWeka)

##' @import tm topicmodels stringr lsa Matrix RWeka gdata catspec
##' @import foreach RecordLinkage igraph plyr
##' @export weightTf weightTfIdf weightBin
NULL ## terminates the import statement, don't take it out.

##' Maps the final bill to both the original bill and any proposed
##' amendments. Returns a matrix that maps from the final bill to the
##' initial bill and (if supplied) the amendments, with distance metrics
##' for the best match from each source. 
##' @title MapBills
##' @param cvsobject The output from from CreateAllVectorSpaces().
##' @param distance.fun A similarity or distance function, which can
##' take two vectors and return a matrix. See CosineMat() for details of
##' the return format. 
##' metric should return larger values for more similar objects. 
##' @param filter.fun one of "min" or "max", indicating how the best
##' match should be chosen. The choice should depend on whether
##' distance.fun returns returns distance (min) or similarity (max)
##' @return A matrix mapping from the sections of the final document
##' to the sections of both the initial document and any proposed
##' amendments, with distance values for each matched pair.
##' @export
##' @author Mark Huberty
MapBills <- function(cvsobject,
                     distance.fun="CosineMat",
                     filter.fun="max"
                     ){

  stopifnot(class(cvsobject) == "leghistCVS")
  idx.collection <- ifelse(is.na(cvsobject[["idx.amendments"]]),
                           cvsobject[["idx.initial"]],
                           c(cvsobject[["idx.initial"]],
                             cvsobject[["idx.amendments"]]
                             )
                           )
  
  ## Create mapping by loop
  print("mapping final to initial")
  map.initial.final <- MapFun(cvsobject,
                              distance.fun=distance.fun,
                              idx.final=cvsobject[["idx.final"]],
                              idx.compare=cvsobject[["idx.initial"]],
                              idx.collection=idx.collection,
                              filter.fun
                              )

  

  if (!is.na(cvsobject[["idx.amendments"]]))
    {
      print("mapping final to amendments")
      map.amend.final <- MapFun(cvsobject,
                                distance.fun=distance.fun,
                                idx.final=cvsobject[["idx.final"]],
                                idx.compare=cvsobject[["idx.amendments"]],
                                idx.collection=idx.collection,
                                filter.fun
                                )

      map.all <- cbind(map.initial.final,
                       map.amend.final[,2:3]
                       )
      
      
    } else {

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



##' Does the actual pairwise mapping of bills. Maps from doc1:doc2
##' via nearest-neighbor matching on the basis of a user-supplied
##' distance / similarity function.
##' @title MapFun
##' @param cvsobject an object of class leghistCVS, as output
##' by CreateAllVectorSpaces
##' @param distance.fun a distance or similarity function. It may
##' either use the "vs.out" object (for bag-of-words based metrics) or
##' the "corpus" object (for string-based metrics). It should return a
##' matrix of form idx.final * idx.compare.
##' @param idx.final an integer index indicating the location of the
##' target documents to be matched to
##' @param idx.compare an integer index vector indicating rows in dtm
##' corresponding to potential matches for the target documents
##' @param idx.collection all rows in dtm corresponding to potential
##' matches (e.g., initial + amendments if both are present)
##' @param filter.fun one of "min" or "max", indicating how the best
##' match should be chosen. The choice should depend on whether
##' distance.fun returns returns distance (min) or similarity (max)
##' @return a 3-column matrix of form idx.query:idx.match:distance
##' @export
##' @author Mark Huberty
MapFun <- function(cvsobject,
                   distance.fun="CosineMat",
                   idx.final,
                   idx.compare,
                   idx.collection,
                   filter.fun){

  stopifnot(class(cvsobject) == "leghistCVS")
  
  ## Set the sort order flag based on the filter function
  d <- ifelse(filter.fun == "max", TRUE, FALSE)
  dist.fun <- match.fun(distance.fun)
  
  ## Generate the distance matrix
  dist.mat <- dist.fun(cvsobject, idx.final, idx.compare, idx.collection)

  ## Retrieve the match indices and the distance 
  match.idx <- sapply(1:ncol(dist.mat), function(x){

    dist.vec = dist.mat[,x]
    idx.vec <- order(dist.vec, decreasing=d)

    return(c(idx.vec[1], dist.vec[idx.vec[1]]))
    
  })

  df.out <- data.frame(1:length(idx.final),
                       t(match.idx)
                       )

  names(df.out) <- c("idx.doc.2", "idx.doc.1", "distance")
  return(df.out)
  
}
## End MapFun

##' Creates the document-term vector space representations of the
##' initial and final documents, and any (optional) amendments. Vector
##' space representations are baselined to a common dictionary based on
##' the final document.
##' @title CreateAllVectorSpaces
##' @param doc.initial the first version of a document.
##' @param doc.final the final version of the same document.
##' @param amendments an optional list of proposed changes to the
##' initial document.
##' @param ngram.min the minimum-length ngram to use in the
##' document-term matrix
##' @param ngram.max the maximum-length ngram to use in the
##' document-term matrix
##' @param stem should words in the documents be stemmed?
##' @param rm.stopwords boolean, should english stopwords be removed?
##' @param rm.whitespace boolean, should excess whitespace be
##' stripped?
##' @param rm.punctuation boolean, should punctuation be removed?
##' @param filter one of 'NULL', 'sparse', 'tf' and 'tfidf' specifying
##' the base value for filtering.
##' @param filter.thres numeric, indicating filter threshold
##' appropriate for the filter chosen.
##' @param weighting one of weightTf, weightTfIdf, or weightBin.
##' @param ngram the length of the word set that should be used for
##' the document-term matrix (1-grams are single words, 2-grams are
##' unique 2-word combinations, etc).
##' @return a list of document-term matrices, for the initial and
##' final documents and any proposed amendments, formatted as sparse
##' Matrix objects. The terms in each matrix are consistent with the set
##' of unique terms in the final document.
##' @export
##' @author Mark Huberty
CreateAllVectorSpaces <- function(doc.initial, doc.final,
                                  amendments=NULL,
                                  ngram.min=1,
                                  ngram.max=1,
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
            !is.null(ngram.min) &
            !is.null(ngram.max)
            )

  ## Check to ensure types are right
  stopifnot(is.character(doc.initial) &
            is.character(doc.final) &
            is.numeric(ngram.min) &
            is.numeric(ngram.max)
            )

  
  if (!is.null(amendments))
    {
      
      stopifnot(is.character(amendments))
      text.vec <- c(doc.final, doc.initial, amendments)
      
    } else {
      
      text.vec <- c(doc.final, doc.initial)

    }
  
  ## Check to ensure that ngram is structured correctly
  stopifnot(ngram.min >= 1)
  stopifnot(ngram.max >= 1)

  ## Ensure that ngram is integer-valued
  ngram.min <- round(ngram.min, 0)
  ngram.max <- round(ngram.max, 0)

  vs.all <- CreateVectorSpace(text.vec,
                              ngram.min=ngram.min,
                              ngram.max=ngram.max,
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
  
  if (is.null(amendments))
    {

      idx.amendments <- NA
      
    } else {

      idx.amendments <-
        (length(doc.final) + length(doc.initial) + 1):vs.all$dtm$nrow
      
    }

  ## Convert the dtm to a sparse matrix so I can do math on it later.
  ## Note that this is much faster than the SparseToDtm code
  vs.out <- sparseMatrix(i=vs.all$dtm$i,
                         j=vs.all$dtm$j,
                         x=vs.all$dtm$v,
                         dimnames=vs.all$dtm$dimnames
                         )

  list.out <- list(idx.final,
                   idx.initial,
                   idx.amendments,
                   vs.out,
                   vs.all[["corpus"]]
                   )
  
  names(list.out) <- c("idx.final",
                       "idx.initial",
                       "idx.amendments",
                       "vs.out",
                       "corpus.out"
                       )
  attr(list.out, "class") <- "leghistCVS"
  return(list.out)
  

}
## End CreateAllVectorSpaces


##' Creates the vector space model of a document. 
##' @title CreateVectorSpace
##' @param docs a string vector of documents.
##' @param ngram.min the minimum-length ngram to use in the
##' document-term matrix
##' @param ngram.max the maximum-length ngram to use in the
##' document-term matrix
##' @param stem boolean, should the document be stemmed?
##' @param dictionary character vector of terms on which to base
##' the document-term matrix. Terms not in the dictionary
##' will be dropped.See the tm package for           documentation. 
##' @param rm.stopwords boolean, should stopwords be removed?
##' @param rm.whitespace boolean, should excess whitespace be removed?
##' @param rm.punctuation boolean, should punctuation be removed?
##' @param filter one of 'NULL', 'sparse', 'tf' and 'tfidf' specifying
##' the           base value for filtering.
##' @param filter.thres numeric, indicating filter threshold
##' appropriate for the filter chosen.
##' @param weighting one of weightTf, weightTfIdf, or weightBin
##' @return  a document-term matrix in sparse representation
##'           where each row is a document and each col is an
##'           ngram. Cell values are term frequency counts. 
##' @author Mark Huberty
CreateVectorSpace <- function(docs,
                              ngram.min,
                              ngram.max,
                              stem=FALSE,
                              dictionary=NULL,
                              rm.stopwords=FALSE,
                              rm.whitespace=FALSE,
                              rm.punctuation=FALSE,
                              filter=NULL,
                              filter.thres=NULL,
                              weighting=weightTf
                              ){

  tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngram.min,
                                                          max = ngram.max)
                                          )

  corpus.in <-Corpus(VectorSource(docs),
                     readerControl=list(readPlain),
                     language="en",
                     load=TRUE
                     )

  ## Ensure everything is utf-8 compliant and lowercase
  corpus.in <- tm_map(corpus.in, function(x) iconv(enc2utf8(x), sub = "byte"))
  corpus.in <- tm_map(corpus.in, tolower)

  
  if (rm.stopwords)
    {
      corpus.in <- tm_map(corpus.in,
                          removeWords,
                          stopwords("english")
                          )
    }

  if (rm.punctuation)
    {
      corpus.in <- tm_map(corpus.in, removePunctuation)
    }
  
  if (rm.whitespace)
    {
      corpus.in <- tm_map(corpus.in, stripWhitespace)
    }

  if (stem)
    {
      corpus.in <- tm_map(corpus.in, stemDocument)
    }

  dtm.corpus <- DocumentTermMatrix(corpus.in,
                                   control=list(dictionary=dictionary,
                                     weighting=weighting,
                                     tokenizer=tokenizer)
                                   )

  corpus.dtm.out <- list(corpus.in, dtm.corpus)
  names(corpus.dtm.out) <- c("corpus", "dtm")
  return(corpus.dtm.out)
  
}
## End CreateVectorSpace


##' Returns a LaTeX document that presents the side-by-side comparison of the final
##' document sections and their matched pairs, with sources for the matched document. 
##' Takes as input the ComputeVectorAllVectorSpaces output and the
##' result from that output from MapBills.
##' @title WriteSideBySide
##' @param composite.match the output of GetLikelyComposite
##' @param doc.final the original character string representing the
##' final document.
##' @param cavs.out the output of CreateAllVectorSpaces used to
##' generate the composite match.
##' @param dir.out the directory in which to place the .tex files
##' @param col.highlight the color to highlight word changes
##' @param box.amendments boolean, should amendments be boxed?
##' @param col.box the color to shade amendment boxes.
##' @param file.out a valid filename for the latex output file.
##' @param pdflatex boolean, should PDFLATEX be run on the output
##' file?
##' @return Returns silently. If PDFLATEX is true, returns both the
##' .tex file and its PDF output. Otherwise, returns only the .tex
##' file.
##' @export
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
    composite.match[order(composite.match$doc.final.idx), ]

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
  
  for (i in 1:nrow(composite.match)) {
    origin <- composite.match[i, "match.origin"]
    alt.origin <- composite.match[i, "alt.origin"]
    origin.idx <- composite.match[i, "match.idx"]
    dist <- composite.match[i, "match.dist"]
    
    if (origin == "doc.initial")
      {
        idx.subset <- cavs.out$idx.initial
      } else if (origin == "amendment"){  
        idx.subset <- cavs.out$idx.amendments
      } else {
        idx.subset <- cavs.out$idx.final
      }

    ## Return word diffs to highlight
    highlight.words <-
      GetWordsToHighlight(cavs.out[["vs.out"]][cavs.out$idx.final, ][i,],
                          cavs.out[["vs.out"]][idx.subset, ][origin.idx,],
                          colnames(cavs.out[["vs.out"]])
                          )
    
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

    highlight.words <-
      GetWordsToHighlight(cavs.out[["vs.out"]][idx.subset, ][origin.idx,],
                          cavs.out[["vs.out"]][cavs.out$idx.final, ][i,],
                          colnames(cavs.out[["vs.out"]])
                          )
    
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

  }

  cat("\\end{document}")
  sink()

  
  if (pdflatex)
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

##' Returns the set difference terms from two term-frequency vectors
##' @title GetWordsToHighlight
##' @param vector1 a term-frequency vector of integers representing
##' one document
##' @param vector2 a term-frequency vector of integers representing a
##' second document with the same dictionary and term ordering
##' @param terms the terms represented by the vectors
##' @return A list of terms found in vector 1 but not vector 2
##' @author Mark Huberty
GetWordsToHightlight <- function(vector1, vector2, terms){

  highlight.words <- setdiff(terms[vector1 > 0],
                             terms[vector2 > 0]
                             )
  highlight.words <- highlight.words[highlight.words != " "]
  return(highlight.words)

}


##' Takes the initial and final versions of your documents (bills 1 ans 2),
##' along with (possibly) the amendments, and outputs a matrix mapping
##' from the index of each section in doc.final to its most likely match
##' from doc.initial and amendments. The origin, index, text, and distance
##' of the best match are all returned.
##' @title GetLikelyComposite
##' @param mapbills.out the output of MapBills 
##' @param doc.initial a character string, representing doc.initial as
##' used in MapBills.
##' @param doc.final a character string, representing doc.final as
##' used in MapBills.
##' @param amendments a character string, representing amendments as
##' used in MapBills. 
##' @param amendment.origin a character vector, the same length as
##' amendments, indicating the origin of each amendment.
##' @param filter One of "max" or "min", indicating how the best match
##' should be selected.
##' @param dist.threshold A threshold distance; if the best match does
##' not pass the threshold, then the best match is the same section of
##' the final document itself. 
##' @return A matrix mapping from the index of each section in
##' doc.final to its most likely match from doc.initial and
##' amendments. The origin, index, text, and distance of the best match
##' are all returned.
##' @export
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
  
  ## Validate the distance function and then grab it
  stopifnot(filter %in% c("min", "max"))
  filter.fun <- match.fun(filter)
  
  ## Placeholder for amendments
  if (is.null(amendments))
    amendments <- rep(NA, nrow(mapbills.out))
  
  ## Loop across the mapbills object and generate the
  ## correct "best match" object.
  mat.out <- sapply(mapbills.out$bill2.idx, function(x){

    ## Get the index (1 or 2) of the best match out of the original and
    ## amendment matches 
    dist.idx <-
      which(mapbills.out[x, c("bill1.dist", "amend.dist")] == 
            filter.fun(mapbills.out[x, c("bill1.dist", "amend.dist")],
                       na.rm=TRUE)
            )[1] 

    ## Get the corresponding distance and check whether NA
    match.dist <-
      mapbills.out[x, c("bill1.dist","amend.dist")[dist.idx]]
    match.dist <- ifelse(is.null(match.dist), NA, match.dist)

    ## Check if this distance passes the threshold
    pass.threshold <- ifelse(filter == "min",
                             match.dist < dist.threshold,
                             match.dist > dist.threshold
                             )

    ## If it doesn't, then assign "final" as the origin; else assign
    ## the correct origin
    if (is.na(dist.idx) | is.na(pass.threshold) |
        !pass.threshold | length(dist.idx) == 0){
      
      vec.out <- c(x, x, "doc.final", "Final", match.dist, doc.final[x])
      
    }else{

      match.idx <- mapbills.out[x, c("bill1.idx",
                                     "amend.idx")[dist.idx]
                                ]
      match.origin <- c("doc.initial", "amendment")[dist.idx]
      match.txt <- ifelse(match.origin=="doc.initial",
                          doc.initial[match.idx],
                          amendments[match.idx]
                          )
      if (dist.idx == 1)
        {
          
          alt.origin <- "Original"
          
        }else{
          
          if (!is.null(amendment.origin))
            {
              
              alt.origin <- amendment.origin[match.idx]
              
            }else{
              
              alt.origin <- "Amendment"
              
            }
          
        }
      
      
      vec.out <- c(x,
                   match.idx,
                   match.origin,
                   alt.origin,
                   match.dist,
                   match.txt
                   )
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

##' Writes out a LaTeX representation of the final document, as built
##' up from components of the initial document plus amendments. Provides
##' options to highlight both word differences and sections that result
##' from amendments. The origin of each section is called out in a
##' margin note.
##' @title WriteCompositeFinal
##' @param composite.match the output of GetLikelyComposite.
##' @param cavs.out the output of CreateAllVectorSpaces used to
##' generate the composite match.
##' @param col.highlight the color to use in highlighting word differences
##' @param box.amendments boolean, should matches that originate from
##' amendments be boxed?
##' @param col.box the color to use in shading amendment boxes
##' @param file.out a valid filename for the latex output file.
##' @param pdflatex boolean, should PDFLATEX be run on the output file?
##' @return Returns silently. If PDFLATEX is true, returns both the
##' .tex file and its PDF output. Otherwise, returns only the .tex
##' file.
##' @export
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

  composite.match <-
    composite.match[order(composite.match$doc.final.idx), ]

  for (idx in 1:length(cavs.out)) {
    if ("DocumentTermMatrix" %in% class(cavs.out[[idx]]))
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
  
  for (idx in 1:length(composite.match$doc.final.idx)) {

    origin <- composite.match[idx, "match.origin"]
    alt.origin <- composite.match[idx, "alt.origin"]
    
    if (origin == "doc.final")
      {
        string.in <- SanitizeTex(composite.match$match.txt[idx])
        this.out <- WriteTexSection(string.in,
                                    highlight.words=NULL,
                                    origin=alt.origin,
                                    origin.idx=idx
                                    )

        cat(this.out)
        cat("\n\n")

      } else {
        
        match.idx <- composite.match[idx,"match.idx"]

        ## Does a diff on the binary final and binary initial
        ## records to detect new words. Does not detect word order.
        ## TODO: insert a better function here to handle this.
        ##       this is really crude.
        highlight.words <-
          GetWordsToHightlight(cavs.out[["doc.final"]][idx, ],
                               cavs.out[[origin]][match.idx, ],
                               colnames(cavs.out)
                               )
        
        this.out <- WriteTexSection(composite.match$match.txt[idx],
                                    highlight.words=highlight.words,
                                    origin=alt.origin,
                                    origin.idx=match.idx
                                    )

        
        if (origin == "doc.initial"){

          cat(this.out)
          cat("\n\n")

        } else { ## Then it's an amendment

          if (box.amendments)
            {
              
              cat("\\begin{framed}",
                  this.out,
                  "\\end{framed}",
                  sep="\n"
                  )
              cat("\n\n")
              
            } else {

              cat(this.out)
              cat("\n\n")

            }

        }


      } ## End if / else
  } ## End for loop

  cat("\\end{document}")
  sink()

  ## If desired, call pdflatex to generate the PDF version of the output.
  if (pdflatex)
    {
      call <- paste("pdflatex", file.out, sep=" ")
      system(call)
      system(call)
    }
  
  print("Done")

}

##' For a given text string and a list of words to highlight, WriteTexSection
##' reforms the string to be LaTeX-valid and highlights words. If the
##' necessary information is provided, the source of the string and
##' a distance or similarity value are printed as margin notes.
##' indicated in a margin note.
##' @title WriteTexSection
##' @param section a string representing a document section.
##' @param highlight.words a character vector of words to highlight
##' @param origin a character string representing the origin of the
##' section.
##' @param origin.idx the index of the document in the origin source
##' (e.g. paragraph number).
##' @param dist The distance or similarity value to be displayed with
##' origin in the margin note
##' @param marginnote boolean, should margin notes be printed.
##' @return A character string with all LaTeX-sensitive characters
##' appropriately escaped, and all highlights and origin information
##' inserted with LaTeX-appropriate data. This assumes that the final
##' LaTeX document will have a defined command of form texthighlight{}
##' defined. Both WriteSideBySide and WriteComposite provide for this in
##' their preamble.
##' @author Mark Huberty
WriteTexSection <- function(section,
                            highlight.words=NULL,
                            origin=NULL,
                            origin.idx=NULL,
                            dist=NULL,
                            marginnote=TRUE
                            ){
  

  string.out <- SanitizeTex(section)

  if (!is.null(highlight.words) & length(highlight.words) > 0)
    {

      for (i in 1:length(highlight.words)) {

        regexp.in <- paste("(", highlight.words[i], ")", sep="")
        regexp.out <- paste("\\\\\\texthighlight\\{\\1\\}")
        
        string.out <- str_replace_all(string.out,
                                      regexp.in,
                                      regexp.out
                                      )
        
      }

    }
  
  if (!is.null(origin) & marginnote)
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
##' Converts a DocumentTermMatrix from the tm() package into a sparse
##' Matrix. This is the same function as John Myles White included in
##' his textregression() library.
##' @title DtmToMatrix
##' @param dtm a document-term matrix as output from the
##' DocumentTermMatrix from the tm() package.
##' @return an equivalent sparse matrix as represented in the Matrix
##' package.
##' @author Mark Huberty
DtmToMatrix <- function(dtm){
  
  m <- Matrix(0, nrow = dtm$nrow, ncol = dtm$ncol, sparse = TRUE,
              dimnames=dtm$dimnames
              )
  
  for (index in 1:length(dtm$i)) {
    m[dtm$i[index], dtm$j[index]] <- dtm$v[index]
  }
  
  return(m)
}

##' Sanitizes special LaTeX characters in a string so that pdflatex
##' will handle them correctly. All special characters are replaced
##' with their escaped equivalents.
##' @title SanitizeTex
##' @param string a string to be inserted in a LaTeX file.
##' @return the same string, with the LaTeX special characters
##' (%$_{}\~^&) replaced with backslash-escaped equivalents.
##' @author Mark Huberty
SanitizeTex <- function(string){

  out <- str_replace_all(string, "([%$_{}\\~\\^&])", "\\\\\\1")
  return(out)
  
}


##' A wrapper function for HoadSimilarityCore() that takes a standard set of
##' inputs and handles pre-processing for the outputs
##' @title HoadSimilarity
##' @param dtm a document-term matrix, such as is output from
##' CreateAllVectorSpaces(), containing the term frequency vectors of
##' both the documents for which matches are needed, and the set of
##' candidate match documents D.
##' @param idx.query the indices in dtm of the documents being matched
##' @param idx.compare the indices in dtm of the potential matches
##' @param idx.collection a vector of indices indicating which rows in
##' dtm are for the set of comparison documents D (as opposed to the
##' documents requiring matches.
##' @return a similarity matrix of dimension idx.query * idx.compare,
##' containing the output of similiarty()
##' @export
##' @author Mark Huberty
HoadSimilarity <- function(dtm, idx.query, idx.compare, idx.collection){
  dtm <- dtm[["vs.out"]]
  N <- length(idx.collection)
  ft <- colSums(dtm[idx.collection, ] > 0)
  
  out <- sapply(idx.query, function(x){
    sapply(idx.compare, function(y){
      HoadSimilarityCore(dtm[x, ], dtm[y, ], N, ft)
    })
  })

  return(out)

}
##' Computes similarity measure #5 from Hoad & Zobel (2003).
##' @title HoadSimilarityCore
##' @param vec.d a term-freqency vector from a set of candidate
##' matches D.
##' @param vec.q a term-frequency vector of a document being matched
##' to candidates in D.
##' @param N The number of documents in D.
##' @param ft The number of documents in D containing term t.
##' @return a numeric similarity measure, where larger numbers
##' indicate more similar documents.
##' @export
##' @author Mark Huberty
HoadSimilarity <-function(vec.d,
                          vec.q,
                          N,
                          ft
                          ){
  
  fd <- sum(vec.d)
  fq <- sum(vec.q)
  
  T <- (vec.q > 0 & vec.d > 0)
  
  ft_sub = ft[T]
  
  vec.weights <- (N / ft_sub) / (1 + abs(vec.d[T] - vec.q[T]))
  
  weight<- sum(vec.weights)
  
  out = 1 / (1 + log(1 + abs(fd - fq))) * weight

  return(out)

}


##' Provides a length-weighted cosine similarity measure
##' @title CosineInvLength
##' @param dtm a document-term matrix
##' @param idx.query the indices of one set of documents in dtm
##' @param idx.compare the indices of the set of comparison documents
##' in dtm
##' @param idx.collection the indices of all non-query documents in
##' dtm
##' @return A similarity matrix of dimension idx.query * idx.compare
##' @export
##' @author Mark Huberty
CosineInvLength <- function(dtm, idx.query, idx.compare, idx.collection){
  dtm <- dtm[["corpus.out"]]
  rq <- rowSums(dtm[idx.query, ])
  rd <- rowSums(dtm[idx.compare, ])

  len.coef <- 1 / (1 + log(1 + outer(rd, rq, VectorDiff)))
  mat <- cosine.mat(dtm, idx.query, idx.compare, idx.collection)

  out <- len.coef * mat
  return(out)

}


##' Returns the vector of absolute differences of two vectors
##' @title VectorDiff
##' @param x a numeric or integer vector
##' @param y a numeric or integer vector of length x
##' @return A vector of length x with the absolute pairwise
##' differences between x and y
##' @author Mark Huberty
VectorDiff <- function(x, y){
  abs(x - y)
}



##' Vectorized version the cosine similarity
##' measure. This computes the entire cosine similarity
##' between two conformable row-major matrices at once using matrix algebra
##' @title CosineMat
##' @param dtm A document-term matrix as output from
##' CreateAllVectorSpaces 
##' @param idx.query an integer index vector indicating the rows in
##' dtm corresponding to a set of target documents for which matches
##' are desired
##' @param idx.compare an integer index vector indicating rows in dtm
##' corresponding to potential matches for the target documents
##' @param idx.collection all rows in dtm corresponding to potential
##' matches
##' @return A matrix of dimension idx.query * idx.compare, with values
##' as the pairwise cosine similarity
##' @export
##' @author Mark Huberty
CosineMat <- function(dtm, idx.query, idx.compare, idx.collection){
  dtm <- dtm[["vs.out"]]
  dtm.query <- dtm[idx.query, ]
  dtm.compare <- dtm[idx.compare, ]
  
  numerator <- dtm.compare %*% t(dtm.query)

  denominator.a <- sqrt(rowSums(dtm[idx.query, ]^2))
  denominator.b <- sqrt(rowSums(dtm[idx.compare, ]^2))

  denominator <- denominator.b %*% t(denominator.a)
  
  out <- numerator / denominator

  return(out)

}

##' Computes the levenshtein distance between each element of the
##' a character vector of query documents and an entire vector of
##' sources. Supports use of a parallel backend via the foreach()
##' library to speed computation for large data sizes. Produces output
##' compatible with the MapBills function and can be passed as dist.fun.
##' @title LevenshteinDist
##' @param dtm a leghistCVS object as output from CreateAllVectorSpaces 
##' @param idx.query the index of the final bill in the corpus
##' @param idx.compare the index of potential matches to the final
##' bill
##' @param idx.collection all entries in the corpus corresponding to potential matches
##' @return A distance matrix of form idx.query * idx.compare
##' @author Mark Huberty
##' @export
LevenshteinDist <- function(dtm, idx.query, idx.compare, idx.collection){
  stopifnot(class(dtm) == "leghistCVS")
  txt <- unlist(dtm$corpus.out)
  out <- foreach(x=idx.query, .combine=rbind) %dopar% {

    levenshteinSim(txt[x], txt[idx.compare])
    
  }
  
  return(t(out))
}



##' Provides a facility to easily model the content of different
##' classes of matched content.
##' @title ModelDocSet
##' @param doc.list the output from CreateAllVectorSpaces.
##' @param composite.mat the output from GetLikelyComposite.
##' @param type the subset of the text to be clustered by topic: one
##' of "incl.amend" (default), "rej.amend", "incl.orig", "rej.orig",
##' "all.amend", or "final".
##' @param k the number of topics to model.
##' @param topic.method one of "LDA" or "CTM"
##' @param sampling.method one of "VEM" or "Gibbs"
##' @param n.terms the number of terms to show in the returned object
##' @param addl.stopwords specific stopwords to include not in the
##' generic stopwords list used to generate the document-term matrix
##' in doc.list
##' @param weighting one of weightTf, weightTfIdf, or weightBin,
##' depending on the weighting used to construct the document-term
##' matrix in doc.list. Note that only weightTf is supported at
##' present.
##' @param control a list of control statements appropriate for
##' topic.method. See the topicmodels documentation for more detail.
##' @param ... other arguments as required; see ModelTopics.
##' @return a ModelTopics object, and additionally an index of
##' of the documents as it points to the text inputs, rather than the
##' document-term matrix.
##' @export
##' @author Mark Huberty
ModelDocSet <- function(doc.list,
                        composite.mat,
                        type="incl.amend",
                        k=NULL,
                        topic.method="LDA",
                        sampling.method="VEM",
                        n.terms=5,
                        addl.stopwords="NULL",
                        weighting=weightTf,
                        control,
                        ...){

  stopifnot(type %in% c("incl.amend", "rej.amend",
                        "incl.orig", "rej.orig",
                        "final", "all.amend",
                        "composite.bill", "actual.bill"
                        )
            )
  
  if (type == "incl.amend")
    {

      dtm.idx <- doc.list$idx.amendments
      orig.idx <-
        composite.mat$match.idx[composite.mat$match.origin=="amendment"]

      topic.idx <- dtm.idx[orig.idx]
      
    }else if (type=="rej.amend"){

      dtm.idx <- doc.list$idx.amendments
      orig.idx <-
        composite.mat$match.idx[composite.mat$match.origin=="amendment"]

      topic.idx <- dtm.idx[-orig.idx]
      
    }else if (type=="incl.orig"){

      dtm.idx <- doc.list$idx.initial
      orig.idx <-
        composite.mat$match.idx[composite.mat$match.origin=="doc.initial"]

      topic.idx <- dtm.idx[orig.idx]


    }else if (type=="rej.orig"){

      dtm.idx <- doc.list$idx.initial
      orig.idx <-
        composite.mat$match.idx[composite.mat$match.origin=="doc.initial"]

      topic.idx <- dtm.idx[-orig.idx]
      
    }else if (type=="all.amend"){

      dtm.idx <- topic.idx <- doc.list$idx.amendments
      
    }else if (type=="final"){

      dtm.idx <- doc.list$idx.final
      orig.idx <-
        composite.mat$match.idx[composite.mat$match.origin=="doc.final"]
      
      topic.idx <- dtm.idx[orig.idx]
      
    }else if (type=="composite.bill"){
      topic.idx <- sapply(1:nrow(composite.mat), function(x){
        origin <- composite.mat$match.origin[x]
        match.idx <- composite.mat$match.idx[x]
        if (origin == "doc.initial")
          {
            out <- doc.list$idx.initial[match.idx]
          }else if (origin == "amendment"){
            out <- doc.list$idx.amendment[match.idx]
          }else{
            doc.list$idx.final[match.idx]
          }
      })
      
    }else{
      topic.idx <- doc.list$idx.final
    }
  
  model.out <- ModelTopics(doc.list$vs.out,
                           topic.idx,
                           k=k,
                           topic.method=topic.method,
                           sampling.method=sampling.method,
                           n.terms=n.terms,
                           addl.stopwords,
                           weighting=weighting,
                           control=control
                           )
  ## Provide an index to the text (1:N) rather than the dtm
  model.out$txt.idx <- model.out$dtm.idx - min(dtm.idx) + 1
  
  return(model.out)


}


##' Provides an interface to model the content of different amendment
##' slices using Latent Dirichelet Allocation methods as supported in
##' the topicmodels package. See the topicmodels package documentation
##' for more details.
##' @title ModelTopics
##' @param dtm A document-term matrix as output from
##' CreateAllVectorSpaces.
##' @param idx The indices of the document-term matrix to model.
##' @param k The number of topics to model.
##' @param topic.method One of "LDA" or "CTM".
##' @param sampling.method One of "VEM" or "Gibbs.
##' @param addl.stopwords Additional stopwords to remove.
##' @param n.terms the number of terms by topic to return. See the
##' terms() function in topicmodels for details.
##' @param weighting one of weightTf, weightTfIdf, or weightBin,
##' corresponding to the weighting used to construct dtm. Note that at
##' present only weightTf is supported.
##' @param control a set of control parameters appropriate for
##' topic.method. See the topicmodels package documentation for details.
##' @param ... other arguments as passed to the LDA or CTM methods
##' @return A list containing the topic model, the top N terms by topic, and the topic
##' assignments for each document indicated by idx.
##' @export
##' @author Mark Huberty
ModelTopics <- function(dtm, idx, k=NULL, topic.method="LDA",
                        sampling.method, addl.stopwords=NULL,
                        n.terms, weighting=weightTf,control=control){

  if (!is.null(addl.stopwords))
    {

      to.remove <-
        lapply(addl.stopwords, function(x){

          grepl(x, colnames(dtm))
          
        })

      remove.vec <- rep(FALSE, ncol(dtm))
      for (i in 1:length(to.remove))
        {
          remove.vec <- remove.vec | to.remove[[i]]
        }
      print(sum(remove.vec))
      dtm <- dtm[, !remove.vec]
      
    }

  dtm.sub <- dtm[idx, ]
  print(dim(dtm.sub))
  ## Check to ensure that all rows have at least one term
  has.terms <- rowSums(dtm.sub) > 0
  dtm.sub <- dtm.sub[has.terms, ]
  idx <- idx[has.terms]

  this.dtm <- SparseToDtm(dtm.sub, weighting=weighting)
                                        #print(dim(this.dtm))
  
  ## If no K provided, ensure that each topic has on average 10 documents
  if (is.null(k))
    k <- ceiling(nrow(this.dtm) / 10)

  ## Match and execute the topic modeling function
  topic.fun <- match.fun(topic.method)
  out <- topic.fun(this.dtm, method=sampling.method, k=k, control=control)

  terms.out <- terms(out, n.terms)
  topics.out <- topics(out)
  list.out <- list(out, terms.out, topics.out, idx)
  names(list.out) <- c("topic.model", "terms", "topics", "dtm.idx")
  return(list.out)

}

##' Helper function to translate a sparse Matrix into
##' a document-term matrix equivalent to that produced by the tm package.
##' @title SparseToDtm
##' @param sparseM : a sparse Matrix of form dgCMatrix.
##' @param weighting one of weightTf, weightTfIdf, or weightBin
##' @return a simple_triplet_matrix as described in the slam package,
##' with the same dimensions and properties as sparseM.
##' @author Mark Huberty
SparseToDtm <- function(sparseM, weighting=weightTf){

  stm <- as.DocumentTermMatrix(sparseM, weighting=weighting)
  return(stm)

}

##' Provides a function interface for learning the best match quality
##' threshold from a human-coded document. Given a mapping of source
##' to target document and a sequence of threshold values, it will
##' return the optimum based on either maximization of the accuracy rate
##' or miniminzation of the false positive/negative rate.
##' @title LearnThreshold
##' @param map.bills.out the output of MapBills for this document. 
##' @param initial.bill the text of the initial bill.
##' @param final.bill the text of the final bill.
##' @param amendments amendment text, in the same order as was passed
##' to MapBills.
##' @param labels the origin of the amendments.
##' @param filter one of "min" or "max" depending on the distance
##' function used in MapBills.
##' @param threshold.values a numeric vector of potential threshold
##' values. See GetLikelyComposite for the definition of the threshold
##' value. A suitably granular vector is recommended, as in seq(0,0.5, 0.005)
##' @param encoder.out the output of RunEncoder for the human-coded
##' matches of these documents.
##' @param type one of "overall" (overall accuracy) or "tradeoff"
##' (false positive/negative).
##' @return a list containing the entire output of the algorithm and
##' the best threshold value.
##' @export
##' @author Mark Huberty
LearnThreshold <- function(map.bills.out,
                           initial.bill,
                           final.bill,
                           amendments,
                           labels,
                           filter="max",
                           threshold.values,
                           encoder.out,
                           type="overall"){

  
  if (type == "overall")
    {
      accuracy.measures <- GetAccuracyMeasures(map.bills.out,
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
    } else if (type == "tradeoff"){

      accuracy.measures <- GetTypeErrors(map.bills.out,
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

##' Calculates the overall accuracy rate by threshold value for a set
##' of documents based on a human-coded set of matches. See
##' LearnThreshold() for more details.
##' @title GetAccuracyMeasures
##' @param map.bills.out the output of MapBills
##' @param initial.bill the character vector representation of the initial bill
##' @param final.bill the character vector representation of the final bill
##' @param amendments the character vector representation of the amendments
##' @param labels committee labels for the amendments
##' @param filter one of min or max, depending on the use of a
##' distance or similarity metric
##' @param threshold.values a vector of similarity thresholds, as in
##' seq(0, 0.5, 0.005)
##' @param encoder.out the output of RunEncoder for the bill in
##' question, using the same bill and amendment arguments in the same order
##' @return Source and source+index accuracy of the automated match,
##' compared with the hand-coded version
##' @export
##' @author Mark Huberty
GetAccuracyMeasures <- function(map.bills.out,
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
    cb <- cb[encoder.out$target.index, ]

    ## Tabulate the source accuracy data
    tab.source.acc <- table(cb$match.origin,
                            encoder.out$match.source
                            )
    pct.source.acc <- sum(diag(tab.source.acc)) / sum(tab.source.acc)

    ## Tabulate the source + index accuracy data
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

##' Optimizes the tradeoff between false negative values (rejecting
##' matches that should have been matched to source documents) and false
##' positive values (accepting matches for which no match existed), on
##' the basis of the threshold value. See LearnThreshold for more detail.
##' @title GetTypeErrors
##' @param map.bills.out the output of MapBills
##' @param initial.bill the character vector representation of the initial bill
##' @param final.bill the character vector representation of the final bill
##' @param amendments the character vector representation of the amendments
##' @param labels committee labels for the amendments
##' @param filter one of min or max, depending on the use of a
##' distance or similarity metric
##' @param threshold.values a vector of similarity thresholds, as in
##' seq(0, 0.5, 0.005)
##' @param encoder.out the output of RunEncoder for the bill in
##' question, using the same bill and amendment arguments in the same order
##' @return Type 1 and Type 2 accuracy rates by threshold value
##' @export
##' @author Mark Huberty
GetTypeErrors <- function(map.bills.out,
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
    cb <- cb[encoder.out$target.index, ]
    
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



##' Provides a streamlined interface to manually match text under the
##' same conditions as used for the automated MapBills process. Output
##' is directly comparable with the output of GetLikelyComposite.
##' @title Encoder 
##' @param target.text the final bill. 
##' @param initial.match.text the initial bill.
##' @param amend.match.text any amendments (should be passed in the
##' same order as they are passed to CreateAllVectorSpaces).
##' @param initial.distance.mat the pairwise distance matrix between
##' initial.match.text and target.text.
##' @param amend.distance.mat the pairwise distance matrix between
##' amend.match.text and target text.
##' @param n.matches.to.show the number of potential matches to show
##' @param target.idx the index of the target text.
##' @param similarity boolean for whether the distance metric used for
##' the distance.mat objects was a similarity measure or a distance measure 
##' @return A matrix mapping from each entry in the target text to a
##' user-supplied index of the best match in either the initial text or
##' the amendment text.
##' @export
##' @author Mark Huberty
Encoder <- function(target.text,
                    initial.match.text,
                    amend.match.text,
                    initial.distance.mat,
                    amend.distance.mat=NULL,
                    n.matches.to.show=5,
                    target.idx,
                    similarity=TRUE
                    ){

  if (!is.null(amend.distance.mat) & !is.null(amend.match.text))
    {
      
      has.amend <- TRUE
      n.matches.to.show <- ceiling(n.matches.to.show / 2)
      print(n.matches.to.show)
      
    } else {

      has.amend <- FALSE

    }

  idx.initial.mat <- sapply(1:ncol(initial.distance.mat), function(x){

    order(initial.distance.mat[,x], decreasing=similarity)[1:n.matches.to.show]

  })

  if (has.amend)
    {
      idx.amend.mat <- sapply(1:ncol(amend.distance.mat), function(x){
        
        order(amend.distance.mat[,x], decreasing=similarity)[1:n.matches.to.show]
        
      })
    }

  source.selections <- c()
  match.selections <- c()
  dist.selections <- c()
  sep.string <- "***************"

  print("starting loop")
  for (r in 1:length(target.text)) {

    ## Get the potential targets
    target <- target.text[r]

    potential.initial.match.idx <-
      idx.initial.mat[,r]
    potential.initial.matches <-
      initial.match.text[potential.initial.match.idx]
    potential.initial.match.distances <-
      initial.distance.mat[potential.initial.match.idx, r]

    if (has.amend)
      {

        potential.amend.match.idx <-
          idx.amend.mat[,r]
        potential.amend.matches <-
          amend.match.text[potential.amend.match.idx]
        potential.amend.match.distances <-
          amend.distance.mat[potential.amend.match.idx, r]

      }

    ## Generate the list of matches and shuffle order
    if (has.amend)
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
        
      } else {
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
    for (i in 1:length(potential.matches)) {
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

    selection <- readline(prompt=prompt.text)
    valid.selection <- ValidateSelection(selection,
                                         valid.matches
                                         )
    while(!valid.selection)
      {
        
        selection <- readline(prompt=prompt.text)
        valid.selection <- ValidateSelection(selection,
                                             valid.matches
                                             )

      }

    selection <- as.integer(selection)
    if (!is.na(selection))
      {
        match.selections <- append(match.selections,
                                   potential.match.idx[selection])
        dist.selections <- append(dist.selections,
                                  potential.match.distances[selection]
                                  )
        source.selections <- append(source.selections,
                                    match.source[selection]
                                    )
      } else { ## No match, append as final

        match.selections <- append(match.selections, r)
        dist.selections <- append(dist.selections, selection)
        source.selections <- append(source.selections, "doc.final")
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

##' Validates the user input for the encoder routine and returns a
##' boolean 
##' @title ValidateSelection
##' @param selection the user input from the command line 
##' @param valid.matches a set of valid user inputs
##' @return TRUE if the input is valid, else FALSE
##' @author Mark Huberty
ValidateSelection <- function(selection, valid.matches){

  if (selection == "")
    {

      print("Empty entries not valid, please try again")
      valid.selection <- FALSE

    }else{

      if (selection %in% valid.matches)
        {
          valid.selection <- TRUE
        }else{
          valid.selection <- FALSE
          print("Invalid choice. Please try again")
        }
    }
  print(valid.selection)
  return(valid.selection)
}

##' Takes the text to be matched, the initial text and amendment
##' candidate matches, and settings for
##' CreateAllVectorSpaces. Generates a set of candidate matches and
##' asks the user to select the best (or no good match). Returns
##' a data frame that maps from the final paragraph to both the initial
##' bill and the amendments.
##' @title RunEncoder
##' @param target.text a character string of pargraphs needing matches
##' @param original.text a character string of the original proposed
##' text.
##' @param amendments a character string of proposed amendments
##' @param ngram.min the minimum-length ngram to use in the
##' document-term matrix
##' @param ngram.max the maximum-length ngram to use in the
##' document-term matrix
##' @param stem should words be stemmed?
##' @param rm.stopwords should English stopwords be removed?
##' @param rm.whitespace should excess whitespace be removed?
##' @param rm.punctuation should punctuation be removed?
##' @param filter Should a tfidf filter be applied?
##' @param filter.thres What filter threshold should be used?
##' @param dist.fun a distance function consistent with that of
##' CosineMat
##' @param n.matches.to.show integer, how many potential matches
##' should be shown to the user?
##' @param encode.random should only a random subset of the target
##' text be encoded?
##' @param pct.encode If a random subset is to be encoded, what
##' percent of the text should be encoded?
##' @param ngram the n-gram to be used in creating a vector space of
##' each document.
##' @return Returns a matrix of the form
##' targetidx:match.idx:match.dist:match.source. For ease of automated
##' comparison, the values in each are
##' equivalent to similar values in the output of GetLikelyComposite.
##' @export
##' @author Mark Huberty
RunEncoder <- function(target.text=NULL,
                       original.text=NULL,
                       amendments=NULL,
                       ngram.min=1,
                       ngram.max=3,
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

  if (encode.random)
    {
      if (is.null(pct.encode))
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
                                    ngram.min=ngram.min,
                                    ngram.max=ngram.max,
                                    stem=stem,
                                    rm.stopwords=rm.stopwords,
                                    rm.whitespace=rm.whitespace,
                                    rm.punctuation=rm.punctuation,
                                    filter=filter,
                                    filter.thres=filter.thres
                                    )

  print("Vector space created")
  dist.fun <- match.fun(dist.fun)

  distance.mat.orig <- dist.fun(doc.list,
                                doc.list$idx.final,
                                doc.list$idx.initial
                                )
  if (is.null(amendments))
    {
      distance.mat.amend <- NULL
    } else {
      
      distance.mat.amend <- dist.fun(doc.list,
                                     doc.list$idx.final,
                                     doc.list$idx.amendments
                                     )
    }
  
  print("Distance matrices created, starting the encoding sequence")

  match.df.orig <- Encoder(target.text,
                           original.text,
                           amendments,
                           distance.mat.orig,
                           distance.mat.amend,
                           n.matches.to.show=n.matches.to.show,
                           target.idx
                           )

  return(match.df.orig)
  
}



##' Provides a 2-level hierarchical topic modeling interface for
##' modeling the amendment content
##' @title ModelAmendHierarchy
##' @param doc.list the output of CreateAllVectorSpaces
##' @param composite.mat the output of GetLikelyComposite, for this
##' bill
##' @param k the number of topics to model. May be one of NULL, an
##' integer value, or an integer vector. If NULL, then the number of
##' topics is assumed to be D/10, where D is either the count of
##' amendments (level 1) or the count of amendments assigned to a
##' topic (level 2). If a vector, the first element of the vector
##' specifies how many topics should be used for modeling level 1, and
##' the remaining elements how many topics should be used for modeling
##' the amendments assigned to those topics. In this case, the vector
##' must be of length V[1]  
##' @param topic.method One of "LDA" or "CTM"
##' @param sampling.method One of "VEM", or "Gibbs"
##' @param n.terms An integer value indicating how many terms should
##' be returned for the purpose of inspecting the topics
##' @param addl.stopwords Any additional stopwords that should be
##' removed prior to modeling
##' @param weighting One of weightTf, weightTfIdf, or weightBin
##' @param ngram 
##' @param sparseness.probs 
##' @param control A vector of control parameters for the topic model
##' function; see the topicmodels documentation for more detail.
##' @param ... 
##' @return A list of of length 2. The first element in the list is a
##' list of all models (both the top-level model and each of the
##' secondary models). Model objects are of the form returned by
##' ModelTopics(). The second element in the list is list of lists of terms,
##' wherein the first element in each list is the set of terms that
##' characterize that level-one category, and the second element
##' contains the sets of terms that characterize the sub-categories
##' within that level-one category.
##' @author Mark Huberty
##' @export
ModelAmendHierarchy <- function(doc.list,
                                composite.mat,
                                k=NULL,
                                topic.method="LDA",
                                sampling.method="VEM",
                                n.terms=5,
                                addl.stopwords="NULL",
                                weighting=weightTf,
                                ngram=2,
                                sparseness.probs=c(0.01, 0.99),
                                control=control,
                                ...
                                ){

  ## Fix the K values appropriately
  ## Three cases: No K; single K, vector of K's, one per
  ## level-1 topic
  if (is.null(k))
    {

      k <- ceiling(length(doc.list$idx.amendments) / 50)
      k.all <- c(k, rep(NULL, k))
      
    } else if (length(k) == 1){
      
      k.all <- rep(k, k + 1)
      
    } else {

      stopifnot(length(k) == k[1] + 1)
      k.all <- k

    }

  ## Filter for ngram length by getting indices of the
  ## desired ngram (for handling multi-ngram-length dtms
  ## used for bill matching
  ngram.idx <- GetNgramIdx(colnames(doc.list$vs.out), ngram)
  tf.idx <- GetTfIdx(doc.list$vs.out, sparseness.probs)
  
  cols.to.keep <- intersect(ngram.idx, tf.idx)
  
  ## Run the level one model with k.all[1] topics
  model.all <- ModelTopics(doc.list$vs.out[,cols.to.keep],
                           doc.list$idx.amendments,
                           topic.method=topic.method,
                           sampling.method=sampling.method,
                           addl.stopwords=addl.stopwords,
                           weighting=weighting,
                           k=k.all[1],
                           n.terms=n.terms,
                           control=control,
                           ...
                           )

  ## Now run the models for each sub-class of amendments, determined
  ## by the topic to which model.all assigned them.
  topic.labels <- sort(unique(model.all$topics))
  ## rebase the dtm index to the 1:A amendment 
  dtm.idx <- model.all$dtm.idx - min(doc.list$idx.amendments) + 1
  
  models.sub <- lapply(topic.labels, function(x){

    ## Restrict the index to those rows that received this topic
    this.idx <- dtm.idx[which(model.all$topics == x)]
    ## Model the amendments portion of the dtm, for those amendments
    ## that received this topic assignment
    model.sub <- ModelTopics(doc.list$vs.out[doc.list$idx.amendments,
                                             cols.to.keep
                                             ],

                             this.idx,
                             topic.method=topic.method,
                             sampling.method=sampling.method,
                             addl.stopwords=addl.stopwords,
                             weighting=weighting,
                             k=k.all[x+1],
                             n.terms=n.terms,
                             control=control,
                             ...
                             )
  })

  ## Return a list with the level 1 and level 2 models.
  out <- list(model.all, models.sub)
  names(out) <- c("model.primary", "model.secondary")

  ## Then pull out the term sets of each topic, and structure by the
  ## topic hierarchy
  terms.list <- GetTermList(out, k[1])

  out <- list(out, terms.list)
  names(out) <- c("models", "terms")
  return(out)
  
}

## Get the terms for each model and return the primary:secondary
## termlist pairs
##' @title GetTermList
##' @param model.list A list of topic models as output from ModelAmendHierarchy
##' @param k.primary The number of primary topics
##' @return Returns a nested list containing the top terms of each primary
##' topic and its associated secondary topics
GetTermList <- function(model.list, k.primary){

  out <- lapply(1:k.primary, function(x){
    terms.primary <- model.list[[1]]$terms[,x]
    terms.secondary <- model.list[[2]][[x]]$terms
    this.out <- list(terms.primary, terms.secondary)
    names(this.out) <- c("terms.primary", "terms.secondary")
    return(this.out)
  })
  return(out)

}

## For a dtm, check the ngram length of the column
## headers and return the indices of n-length ngrams
##' @title GetNgramIdx
##' @param str A character vector whose components are strings
##' containing one or more words
##' @param n The length of the n-grams for which vector indices are to
##' be returned. 
GetNgramIdx <- function(str, n){
 
  str.split <- strsplit(str, " ")
  l <- sapply(strsplit(str, " "), length)
  idx.out <- which(l == n)
  
  return(idx.out)
}

## For a dtm, check the term frequency quantiles
## for each term and return the indices of frequencies w/in
## a quantile range defined by probs
##' @title GetTfIdx
##' @param dtm A document-term matrix
##' @param probs a character vector of length 2 indicating the minimum
## and maximum quantile ranges to keep.
GetTfIdx <- function(dtm, probs){
  stopifnot(length(probs) == 2)
  if (probs[1] > probs[2])
    probs <- rev(probs)
  tf <- colSums(as.matrix(dtm))
  q.tf <- quantile(tf, probs)
  idx.out <- which(tf > q.tf[1] & tf < q.tf[2])
  return(idx.out)
  
}

##' Takes as input a topic structure from ModelAmendHierarchy and
##' returns a set of crosstabs indicating the proportion of amendments
##' in each topic that were accepted or rejected.
##' @title CtabAmendHierarchy
##' @param amend.topic.hierarchy an object as returned from
##' ModelAmendHierarchy 
##' @param composite.bill the composite bill
##' @param committees the committee list for amendments
##' @param doc.list the original document list containing a doc-term
##' matrix
##' @param tab.idx which axis proportions should be calculated on, one
##' of 1 (rows) or 2 (columns)
##' @return a list of crosstabs. Element 1 count of amendments by
##' committee and primary topic that were accepted or
##' rejected. Element 2 is the aggregate count of amendments by primary
##' topic that were accepted or rejected. Element 3 returns the
##' proportion of amendments accepted or rejected for each primary
##' topic. Element 4 contains N elements, where N is the number of
##' primary topics, each with N_i crosstabs indicating the breakdown of
##' amendments for that primary topic by committee, secondary topic,
##' and accept/reject.
##' @author Mark Huberty
##' @export
CtabAmendHierarchy <- function(amend.topic.hierarchy,
                               composite.bill,
                               committees,
                               doc.list,
                               tab.idx=1
                               ){
  
  ## Baseline the primary index to 1:N
  min.amend.idx <- length(doc.list$idx.final) + length(doc.list$idx.initial)
  model.amend.idx <-
    amend.topic.hierarchy[[1]][[1]]$dtm.idx - min.amend.idx

  tab.primary <-
    CtabTopics(amend.topic.hierarchy[[1]][[1]]$topics,
               committees,
               composite.bill$match.idx[composite.bill$match.origin=="amendment"],
               model.amend.idx
               )
  
  ## Generate subtopic crosstabs
  tab.secondary <- lapply(amend.topic.hierarchy[[1]][[2]], function(x){

    model.amend.idx <- x$dtm.idx

    CtabTopics(x$topics,
               committees,
               composite.bill$match.idx[composite.bill$match.origin=="amendment"],
               model.amend.idx
               )
  })

  ## Return the entire list
  out <- list(tab.primary, tab.secondary)
  names(out) <- c("tab.primary", "tab.secondary")
  return(out)
  
}

##' Crosstabs topics, committees, and acceptance/rejection for matched
##' amendments. Called within CtabAmendHierarchy.
##' @title CtabTopics
##' @param topics the topic vector as returned by ModelAmendHierarchy
##' @param committees the committee list corresponding to the
##' committees responsible for the amendments
##' @param master.idx The index of matched amendments in the composite
##' bill, specifically the match.origin element of a composite.bill object
##' @param this.idx Index of the amendments as they appear in the
##' topic model, on the same interval as master.idx. Note that this is
##' not the same as the dtm.idx, because of dropped cases due to
##' amendment length or different stopwords lists.
##' @return A list of crosstabs: by topic and accepted/rejected
##' (count.by.topic.status); by committee and accepted/rejected
##' (count.by.committee.status); and proportion by topic status
##' @author Mark Huberty
CtabTopics <- function(topics, committees, master.idx, this.idx){

  labels <- rep(NA, length(this.idx))
  acc.amend.idx <- this.idx %in% master.idx
  rej.amend.idx <- !(this.idx %in% master.idx)
  labels[acc.amend.idx] <- "acc"
  labels[rej.amend.idx] <- "rej"
  committees.sub <- committees[this.idx]

  count.by.topic.status <- table(topics,
                                 labels
                                 )
  count.by.committee.status <- table(committees.sub,
                                     labels
                                     )
  prop.by.topic.status <- prop.table(count.by.topic.status,
                                     margin=1
                                     )
  prop.by.topic.committee.status <-
    my.print.ctab(ctab(factor(topics),
                       factor(committees.sub),
                       factor(labels),
                       dec.places=1,
                       type="column"
                       )
                  )
  
  out <- list(count.by.topic.status,
              count.by.committee.status,
              prop.by.topic.status,
              prop.by.topic.committee.status
              )
  names(out) <- c("count.by.topic.status",
                  "count.by.committee.status",
                  "prop.by.topic.status",
                  "prop.by.topic.committee.status"
                  )
  return(out)
  
}

## This is a custom implementation of print.ctab() from the catspec
## library. The original version calls all() w/o specifing the NA
## handling. However, all()'s NA handling is inconsistent. all(TRUE,
## NA, FALSE) returns FALSE, while all(TRUE, NA, TRUE) returns
## NA. This provides a facility for specifying the NA handling
## explicitly, so that it will be consistent across cases.
##' @title my.print.ctab
##' @param x An object output from ctab()
##' @param dec.places Decimal places to print in output
##' @param addmargins Should margins be calculated?
##' @param all.NA Boolean, should NA values be recognized or skipped
##' @param ... 
my.print.ctab <- function (x, dec.places = x$dec.places, addmargins =
                           x$addmargins,
                           all.NA=TRUE,
                           ...) 
{
  if (length(dim(x$ctab)) == 1) {
    tbl <- x$ctab
    if (addmargins) 
      tbl <- addmargins(tbl)
    if (x$style == "long") {
      tbl <- as.matrix(tbl)
      colnames(tbl) <- names(dimnames(x$ctab))
    }
  }
  else {
    row.vars <- x$row.vars
    col.vars <- x$col.vars
    a = length(row.vars)
    if (length(x$type) > 1) {
      z <- length(names(dimnames(x$ctab)))
      if (x$style == "long") 
        row.vars <- c(row.vars, z)
      else col.vars <- c(z, col.vars)
    }
    b = length(col.vars)
    tbl <- x$ctab
    mrgn <- c(row.vars[a], col.vars[b])
    if (length(dim(x$table)) == 1) 
      mrgn <- 1
    if (addmargins) 
      tbl <- addmargins(tbl, margin = mrgn)
    tbl <- ftable(tbl, row.vars = row.vars, col.vars = col.vars)
  }
  if (!all(as.integer(tbl) == as.numeric(tbl), na.rm=all.NA)) 
    tbl <- round(tbl, dec.places)
  out <- print(tbl, ...)
  return(out)
}


#################################################################
## BEGIN VISUALIZATION CODE FOR AMENDMENT AND TOPIC FLOW
#################################################################
## require(igraph)
## require(plyr)


##' A function called within PlotCommitteeTopics() to take the output of
##' various bill mapping functions and create an easily usable matrix 
##' carrying the information PlotCommitteeTopics() needs.
##' @title OutToInSCT
##' @param model.amend.hierarchy.out the output of ModelAmendHierarchy()
##' @param get.likely.composite.out the output of get.likely.composite()
##' @param committees the object "committees", used in other parts of this
##' package, consisting of a vector of committee names for each ith 
##' amendment (accepted, rejected, and discarded amendments).
##' @return A dataframe of dimension ax4, where a equals the number of non-
##' discarded amendments (so accepted or rejected amendments). The first
##' column is the amendment indices, the second is the topic assignments, 
##' the third is the committees, and the fourth is a logical vector for 
##' amendment success (made it into the final bill) or failure.
OutToInSCT <- function(model.amend.hierarchy.out,
                       get.likely.composite.out,
                       committees){
  
  ## Create a matrix of each amendment index and their topic assignments.
  amend.top.index <- cbind( model.amend.hierarchy.out[[1]][[1]][[4]]
                           -min(model.amend.hierarchy.out[[1]][[1]][[4]])+1
                           , as.numeric(model.amend.hierarchy.out[[1]][[1]][[3]]))
  ## Note that here, only those amendments that are not thrown out due to length 
  ## (e.g. ":") are represented.
  colnames(amend.top.index) <- c("idx","topic #")

  ## Find the indices of those amendments which made it to the composite final bill.
  successful<- get.likely.composite.out[ get.likely.composite.out[,3]=="amendment",2:3]
  
  unique.successful<- unique(successful) 
  ## Create a matrix of the successful amendment indices. The second row
  ## becomes helpful in a moment!
  y <- unique.successful[order(unique.successful[,1]), ]
  x<- data.frame(1:length(committees),committees)
  names(x)<-c("match.idx","committees")

  joined <- join( x, y, type="left")
  ## All of the elements in the third row that are <NA> (not "amendment") 
  ## must be rejected amendments or amendments discarded by the computer (due to
  ## their very short length).
  joined[,3][is.na(joined[,3])]<- 0
  joined[,3][joined[,3]=="amendment"]<- 1
  ## Three columns: amendment index, committee, logical: was the amendment accepted?
  
  ## However, amendments that were discarded still need to be removed:
  ## Use amend.top.index, as it only shows non-discarded amendments, and has topic info:
  merged<- merge(amend.top.index,joined,by=1)

  return(merged)
}
## end OutToInSCT()


##' Is x an RGB code? Called within CheckAndFixRGB, which is called within EdgeColorSCT(),
##' which is called within PlotCommitteeTopics().
##' @title IsRGB
##' @param x a character vector
##' @return logical, does x start with a "#" sign?
##' @author Hillary Sanders
IsRGB <- function(x){
  y <- grepl("^#",x)
  return(y)
}
## end IsRGB


##' If the passed vector doesn't look like an RGB code, CheckAndFixRGB assumes the
##' input is a color and tries to extract the RGB code so transparency can be added.
##' Called within EdgeColorSCT(), which is called within PlotCommitteeTopics().
##' @title CheckAndFixRGB
##' @param x Presumably an RGB code or a character vector representing a color.
##' @return the input, but in RGB form, if possible.
##" @author Hillary Sanders
CheckAndFixRGB <- function (x) {
  
  if (!IsRGB (x)){
    x <- rgb(col2rgb(x)[1],
             col2rgb(x)[2],
             col2rgb(x)[3], maxColorValue=255)
  }
  return (x)
}
## end CheckAndFixRGB


##' A function called within PlotCommitteeTopics() to calculate edge colors.
##' @title EdgeColorSCT
##' @param A An ax4 matrix, where a = number of amendments. Each row represents an
##' amendment: its index (on of 1:a), it's committee (one of 1:c), its topic (one
##' of 1:t), and its final destination (junk or final bill: 0 or 1). See
##' PlotCommitteeTopics() for more details. 
##' @param num.com number of committees
##' @param num.top number of topics
##' @param edge.col optional vector of colors (length 2).
##' @param edge.transparency Optional integer in 10:99 designating level of 
##' transparency
##' @return A vector of edge widths for each arrow to be drawn
##' @author Hillary Sanders
EdgeColorSCT <- function(A, num.com, num.top, edge.col=NULL, edge.transparency=NULL){ 
  if (is.null(edge.col)){
    colors <- c("#FFB90F","#6495ED")
    ## "darkgoldenrod1", "cornflowerblue" : (Failure, Success)
  } else { 
    colors <- rep(edge.col,2) [1:2]
    
    colors <- as.character (sapply(colors, CheckAndFixRGB))
  }
  
  ## The final destination (1 or 2: junk or final) of each unique edge (arrow):
  edge.color.idx <- c( (A[!duplicated(A[,2:3]),4]),
                      (A[!duplicated(A[,3:4]),4]) ) -num.com-num.top+1
  
  edge.color <- colors[edge.color.idx]
  
  ## Are the amendment(s) that a committee-to-topics arrow is representing heading to both 
  ## the final bill AND junk? If both, then the arrow color should be some shade of (default) green.
  for ( i in which(!duplicated(A[,2:3]))){

    identical <- c( which ( ( (A[i,2]==A[,2]) * (A[i,3]==A[,3])) ==1) )

    destinations <- A [ identical,4]
    ## If their final destinations are not all the same, then make their arrow be green.
    if (length(unique(destinations))!=1) {
      
      success.rate <- mean(destinations)-min(destinations)
      
      lum <- ((1-success.rate)*100)
      shade <- hcl(110,c=100,l=lum)
      ## So if success rate is high, edges will be dark green, if low, light yellow/green.         
      
      edge.color[ order((i==which(!duplicated(A[,2:3])))==0)[1]] <- shade
    }
  }
  
  if (!is.null(edge.transparency)){ 
    for (i in 1:length(edge.color)){
      ## Add a transparency number (in 00:99)
      edge.color[i] <- paste( edge.color[i], as.character(edge.transparency), sep="")
    }
  }
  return (edge.color)
}
## end Edge.Color()


##' A function called within PlotCommitteeTopics() to calculate vertex (node) 
##' sizes.
##' @title VertexSizes
##' @param A ax4 information matrix
##' @param num.com number of committees
##' @param num.top number of topics
##' @param scale.c Size scale for committee nodes.
##' @param scale.t Size scale for topic nodes.
##' @param scale.fin Size scale for final destination nodes: "Final" and "Junk".
##' @return A matrix of dimension a by 2. The first column is a vector of node sizes 
##' for each vertex in the graph., the second is a vector of second node sizes (e.g.
##' for rectanges).
##' @author Hillary Sanders
VertexSizes <- function(A, num.com, num.top, scale.c, scale.t, scale.fin){
  
  vertex.size <- rep(0,(num.com+num.top+2))
  
  for (i in 1:num.com){
    vertex.size[i] <- sum(A[,2]==(i-1))
  }
  for (i in (num.com+1):(num.com+num.top)) {
    vertex.size[i] <- sum(A[,3]==(i-1))
  }
  for (i in (num.com+num.top+1):(num.com+num.top+2)) {
    vertex.size[i] <- sum(A[,4]==(i-1))
  }
  ## Here, both dimensions of the default rectangle vertex shape are created, and scaled
  ## by how large the biggest vertex is on the graph. 
  biggest <- max(vertex.size)
  v.size <- ((sqrt(vertex.size))/(sqrt(biggest))*(60))
  v.size2 <- ((sqrt(vertex.size)/(sqrt(biggest))*(45)))
  
  ## Vertex sizes can also be rescaled by the user by scale.c, scale.t, and
  ## scale.fin inputs. Defaults = 1.

  v.size[1:num.com] <- v.size[1:num.com]*sqrt(scale.c)
  v.size2[1:num.com] <- v.size2[1:num.com]*sqrt(scale.c)

  v.size[(num.com+1):(num.com+num.top)] <- 
    v.size[(num.com+1):(num.com+num.top)]*sqrt(scale.t)
  
  v.size2[(num.com+1):(num.com+num.top)] <- 
    v.size2[(num.com+1):(num.com+num.top)]*sqrt(scale.t)

  v.size[(num.com+num.top+1):(num.com+num.top+2)] <-
    v.size[(num.com+num.top+1):(num.com+num.top+2)]*sqrt(scale.fin)
  
  v.size2[(num.com+num.top+1):(num.com+num.top+2)] <-
    v.size2[(num.com+num.top+1):(num.com+num.top+2)]*sqrt(scale.fin)
  
  
  return(cbind(v.size,v.size2))
}
# end VertexSizes()


##' A function called within PlotCommitteeTopics() to creates vertex 
##' (node) labels.
##' @title VertexLabels
##' @param labels An optional vector of labels, usually NULL. If not NULL,
##' the function will only output this same object.
##' @param merged Output of OutToInSCT
##' @param topics.matrix An object defined inside PlotCommitteeTopics(): 
##' model.amend.hierarchy.out[[1]][[1]][[2]]
##' @return a vector of labels for each node in a PlotCommitteeTopics()
##' graph.
##' @author Hillary Sanders
VertexLabels <- function(labels, merged, topics.matrix) {
  
  if (is.null(labels)) {
    
    com <- levels(merged[,3])
    top <- paste( "Topic", 1:ncol(topics.matrix))
    final <- c("Junk", "Final")  
    
    labels <- c(com, top, final)
  }

  return(labels)
}
## end VertexLabels()


##' Creates the "x"th layout coordinates for PlotCommitteeTopics(). This function
##' is called inside of PlotCommitteeTopics() to create the layout: three layers 
##' consisting of 1) committees (c of them), 2) topics (t of them), and the final
##' destinations of the amendments (junk and final). 
##' @title LayoutSCT
##' @param x the index of the coordinates to be created.
##' @param num.com the number of committees (number of nodes wanted in the bottom
##' layer of the graph).
##' @param num.top the number of topics (number of nodes wanted in the middle
##' layer of the graph).
##' @param mid.layer The y-axis placement of the middle layer on the graph. 
##' Defaults to .6. Note that the bottom and top layers are at 0 and 1. 
##' @return the xth pair of coordinates for the default layout of 
##' PlotCommitteeTopics().
##' @author Hillary Sanders
LayoutSCT <- function(x,num.com,num.top,mid.layer=.6){
  
  if (x<(num.com+1)) {
    cords <- c(x/(1+num.com),0)
    
  } else {
    
    if (x>(num.com+num.top)) 
      { cords<- c( (x-num.com-num.top)/3,1)
        
      } else {
        cords <- c( (x-num.com)/(1+num.top),mid.layer)
      }
  }
  return (cords)
}
## end LayoutSCT


##' A small function called within PlotCommitteeTopics() to calculate edge widths
##' (arrow widths) for PlotCommitteeTopics(), where the edge.width argument is set
##' to "absolute" (default). Similar to GetEdgeWidth.Relative and GetEdgeWidth.Success.
##' @title GetEdgeWidth
##' @param x A vector of length three, for the ith edge, indicating which node the
##' edge came from, where it is going, and its index.
##' @param A An ax4 matrix, where a = number of amendments. Each row represents an
##' amendment: its index (on of 1:a), it's committee (one of 1:c), its topic (one
##' of 1:t), and its final destination (junk or final bill: 0 or 1). See
##' PlotCommitteeTopics() for more details. 
##' @param num.arrows.to.topics The number of distinct edges (arrows) that are going
##' to topic nodes (the middle layer) in the PlotCommitteeTopics() plot.
##' @return the width of the x[3]th edge (arrow) according to the absolute number of
##' amendments represented by the given edge.
##' @author Hillary Sanders
GetEdgeWidth <- function(x,A,num.arrows.to.topics){
  
  if (x[3]< num.arrows.to.topics+1){
    width <- sum(
                 (x[1]==A[,2]) & 
                 (x[2]==A[,3])
                 ) 
    
  } else {
    
    width <- sum(
                 (x[1]==A[,3]) &
                 (x[2]==A[,4])
                 )
  }
  return (width)
}
## End GetEdgeWidth


##' A small function called within PlotCommitteeTopics() if the argument
##' edge.width is set to "relative". This function creates edge (arrow) widths for 
##' PlotCommitteeTopics() relative to an edge's origin. Similar to GetEdgeWidth() and
##' GetEdgeWidth.Success().
##' @title GetEdgeWidth.Relative
##' @param x A vector of length three, for the ith edge, indicating which node the
##' edge came from, where it is going, and its index.
##' @param A An ax4 matrix, where a = number of amendments. Each row represents an
##' amendment: its index (on of 1:a), it's committee (one of 1:c), its topic (one
##' of 1:t), and its final destination (junk or final bill: 0 or 1). See
##' PlotCommitteeTopics() for more details. 
##' @param num.arrows.to.topics The number of distinct edges that are going
##' to topic nodes (the middle layer) in the PlotCommitteeTopics() plot.
##' @return the width of the x[3]th edge, according to the percentage
##' of amendments that the the given edge is carrying with respect to the total 
##' number of amendments coming from the edge's source (either a committee or topic
##' node).
##' @author Hillary Sanders
GetEdgeWidth.Relative <- function(x,A,num.arrows.to.topics){
  if (x[3]< num.arrows.to.topics+1){
    
    width <- sum(
                 (x[1]==A[ ,2]) &
                 (x[2]==A[ ,3]) ) /
                   sum(x[1]==A[ ,2]) *15 
    ## The 15 is so that the default with of 1 (more user friendly) makes
    ## reasonably sized edges
  } else {    
    width <- sum(
                 (x[1]==A[ ,3]) &
                 (x[2]==A[ ,4])) /
                   sum(x[1]==A[ ,3]) *15
  } 
  
  return (width)
}
## End GetEdgeWidth.Relative


##' A small function called within PlotCommitteeTopics() if the argument
##' edge.width is set to "success". Creates edge (arrow) widths for
##' PlotCommitteeTopics() based on the percentage of successful (headed for
##' the final bill) amendments in the given edge.
##' PlotCommitteeTopics().
##' @title GetEdgeWidth.Success
##' @param x A vector of length three, for the ith edge, indicating which node the
##' edge came from, where it is going, and its index.
##' @param A An ax4 matrix, where a = number of amendments. Each row represents an
##' amendment: its index (on of 1:a), it's committee (one of 1:c), its topic (one
##' of 1:t), and its final destination (junk or final bill: 0 or 1). See
##' PlotCommitteeTopics() for more details.
##' @param num.arrows.to.topics The number of distinct edges (arrows) that are
##' going to topic nodes (the middle layer) in the PlotCommitteeTopics() plot.
##' @param num.com The number of committees in the SCT graph to be plotted.
##' @param num.top The number of topics in the SCT graph to be plotted.
##' @return the width of the x[3]th edge (arrow), according to the % of
##' successful amendments the edge carries (with respect to all of the amendments
##' it carries).
##' @author Hillary Sanders
GetEdgeWidth.Success <- function(x, A, num.arrows.to.topics, num.com, num.top){
  
  if (x[3]< num.arrows.to.topics+1){
    width <- sum(
                 (x[1]==A[, 2]) &
                 (x[2]==A[, 3]) &
                 (A[, 4] == (num.com+num.top+1))
                 ) / 
                   sum((x[1]==A[, 2]) &
                       (x[2]==A[, 3])) *15
    
  } else {
    
    width <- sum(
                 (x[1]==A[, 3]) &
                 (x[2]==A[, 4]) &
                 (A[, 4]==(num.com+num.top+1))
                 ) /
                   sum(
                       (x[1]==A[, 3]) &
                       (x[2]==A[, 4])) *15
    ## The 15 is so that the default with of 1 (more user friendly) makes
    ## reasonably sized edges
  }
  return(width)
} 
## End GetEdgeWidth.Sucess


##' A function called within PlotCommitteeTopics() to calculate all edge widths.
##' @title EdgeWidths
##' @param A A matrix (created inside PlotCommitteeTopics()) of dimensions a by 4,
##' where a = the number of amendments. The columns 1:4 respectively indicate 
##' amendment index, committee, topic, and logical success or failure. Each row
##' corresponds to one non-discarded (so rejected and accepted) amendment.
##' @param num.com The number of committees in the SCT graph to be plotted.
##' @param num.top The number of topics in the SCT graph to be plotted.
##' @param edge.width The method used to calculate edge widths. The default, "absolute",
##' means that edge widths will correspond to the absolute number of amendments they
##' represent. "relative" means that edge widths will correspond to the % of
##' amendments each edge holds with respect to the node (vertex) they are coming from. 
##' "success" means edge widths will correspond to the % of amendments in each edge
##' which are destined for the final bill (with respect to the total number of
##' amendments each edge is carrying).
##' @param edge.width.scale Default = 1. Thicker edges = bigger number.
##' @return A vector of edge widths for each arrow to be drawn. 
##' @author Hillary Sanders
EdgeWidths <- function(A, num.com, num.top, edge.width="absolute", edge.width.scale=1){
  
  arrows.mat <- rbind( unique( A[, 2:3] ), unique(A[, 3:4]) )
  num.arrows.to.topics <- nrow(unique( A[, 2:3]))
  num.arrows.to.jf <- nrow(unique(A[, 3:4]))
  num.arrows <- nrow(arrows.mat) # the total number of arrows to be drawn
  
  if ((edge.width == "absolute") | (edge.width == "a")){
    width <- apply(cbind(arrows.mat,1:num.arrows), 1, GetEdgeWidth,
                   A, num.arrows.to.topics)
  }
  
  if ((edge.width == "relative") | (edge.width == "r")){
    width <- apply(cbind(arrows.mat,1:num.arrows), 1, GetEdgeWidth.Relative,
                   A, num.arrows.to.topics)
  }
  
  if ((edge.width == "success") | (edge.width == "s")){ 
    width <- apply(cbind(arrows.mat,1:num.arrows), 1, GetEdgeWidth.Success,
                   A, num.arrows.to.topics, num.com, num.top)
  }
                                        # Scale it:
  width <- ceiling(edge.width.scale*width)
  if ((edge.width == "absolute") | (edge.width == "a")) width <- width/10
  
  return(width)
}
# end Edge.Width()


##' The main graphing function for the legislative bill mapping package. This
##' function creates a three layer directed acyclic graph. The first, bottom 
##' layer is a set of nodes representing committees which have each submitted 
##' a number of amendments. Edges (arrows) connect these committee nodes to the
##' middle layer topic nodes to which each committee's amendment(s) pertain. 
##' Edges then connect these topic nodes to the third layer - two nodes: "Junk"
##' and "Final" - again according to the amendments that the edges represent.
##' Edge width, as well as node area, by default correspond to the number of
##' amendments they are representing.
##' @title PlotCommitteeTopics
##' @param model.amend.hierarchy.out The object created by ModelAmendHierarchy().
##' @param get.likely.composite.out The object created by GetLikelyComposite().
##' @param committees The object created by ModelAmendHierarchy.
##' @param edge.width.scale Scales the width of the arrows. Default = 1.
##' @param edge.width The method used to calculate edge widths. The default, "absolute",
##' means that edge widths will correspond to the absolute number of amendments they
##' represent. "relative" means that edge widths will correspond to the % of
##' amendments each edge holds with respect to the node (vertex) they are coming from. 
##' "success" means edge widths will correspond to the % of amendments in each edge
##' which are destined for the final bill (with respect to the total number of
##' amendments each edge is carrying).
##' @param scale.c Scales the size of the bottom layer committee nodes (vertices). 
##' By default, scale.c, scale.t, and scale.fin = 1, and their area are equally 
##' proportional to the number of amendments they represent.
##' @param scale.t Scales the size of the middle layer topic nodes. Default = 1.
##' @param scale.fin Scales the size of the top layer final nodes (Junk and Final).
##' Default = 1.
##' @param edge.transparency A number in 00:99, representing the wanted transparency
##' in edges (lower number = more transparent). If left NULL (the default), edges 
##' will be kept opaque.
##' @param edge.col Two edge colors to signify edges which contain 1) junk destined
##' amendments, 2) final destined amendments, or 3) both. Both RGB codes and 
##' character vectors are accepted. If NULL (the default) PlotCommitteeTopics()
##' will use "cornflowerblue", "darkgoldenrod1", and varying shades of green to
##' respectively signify if each edge's amendments were all successfull (in
##' making it to the final bill), were all rejected, or had some combination
##' of successful and failed amendments.
##' @param main The graph's title. Defaults to NULL.
##' @param arrowhead.size The size of arrowheads (edge arrowheads). Defaults
##' to 0, i.e. no arrowhead.
##' @param layout The layout of the graph. If NULL (the default), 
##' PlotCommitteeTopics() will create the three layers decribed above. But users 
##' can also pass graphing algorithms (e.g. layout.fruchterman.reingold) for a
##' different layout.
##' @param mid.layer The placement of the middle layer of the graph on the y 
##' axis. Defaults to .65. Note that the bottom and top layers are at 0 and 1.
##' Helpful if topic terms are being plotted (see plot.terms=TRUE below),
##' and space is needed.
##' @param plot.terms Logical. Should topic terms be plotted? Default = TRUE.
##' Note that PlotCommitteeTopics() plots the terms beneath each topic node.
##' @param terms.cex Text size for the topic terms plotted, if plot.terms=TRUE.
##' @param terms.col Text color for the topic terms plotted, if plot.terms=TRUE.
##' @param terms.x.offset X axis adjustment for the topic terms plotted, if 
##' plot.terms=TRUE.
##' @param terms.y.offset Y axis adjustment for the topic terms plotted, if 
##' plot.terms=TRUE.
##' @param terms.spread Measure of horizontal spread between the plotted topic
##' @param terms.text.close Measure of vertical spread between the plotted 
##' topic terms.
##' @param labels An optional character vector representing the node names for each 
##' node (vertex). If NULL, the committee nodes (bottom layer) will be named with 
##' their full names, each ith topic node will be named Topic i, and the two final
##' bins will be labeled "Final" and "Junk" (for accepted and rejected amendments).
##' @param vertex.label.font Type of font for the vertex labels.
##' @param vertex.label.cex Size of the vertex label font. Vectorized.
##' @param vertex.color The color vertex labels are filled with.
##' @param vertex.shape The shape of the vertices. Default = "rectangle". Note that
##' if another shape is passed, area may no longer be (one-to-one) related to the 
##' number of amendments represented. (For now.) Possible shapes are "circle",
##' "square", "csquare", "rectangle", "crectangle", "vrectangle", and "none".
##' @return A hopefully pretty graph!
##' @author Hillary Sanders
##' @export
PlotCommitteeTopics <- function(model.amend.hierarchy.out,get.likely.composite.out,
                               committees,
                               edge.width.scale=1, edge.width = "absolute",
                               scale.c=1, scale.t=1, scale.fin=1,
                               edge.transparency=70, edge.col=NULL,
                               main=NULL, arrowhead.size=0, 
                               layout=NULL, mid.layer=.65, 
                               plot.terms=TRUE, terms.cex=.5, terms.col="grey30",
                               terms.x.offset=0, terms.y.offset=-.05, 
                               terms.spread=1, terms.text.close=1,
                               labels=NULL, vertex.label.font=3, vertex.label.cex=.75,
                               vertex.color="cornflowerblue", vertex.shape = "rectangle"
                               ) {
  
  merged <- OutToInSCT(model.amend.hierarchy.out,get.likely.composite.out,committees)
  
  committees <- merged[,3]
  ## Need to make the committees column numeric.
  ## Also need to make sure that if there are any skipped indices, everything 
  ## gets re-ordered properly.
  amend.committees <- as.numeric(factor(merged[,3]))
  amend.topics <- as.numeric(factor((merged[,2])))
  amend.final <- as.numeric(factor(merged[,4])) -1
  
  a <- length(amend.committees)
  
  stopifnot(length(amend.committees)==length(amend.topics),
            length(amend.topics)==length(amend.final))
  
  A <- matrix(c(1:a,amend.committees,amend.topics,amend.final),ncol=4)
  
  ## num.amd = # of amendments, num.com = # of committees, num.top = # of topics.
  num.amd <- nrow(A)
  num.com <- length(unique(A[,2]))
  num.top <- length(unique(A[,3])) 
  
  ## reindex. Note that igraph takes numbers starting at 0, not 1.
  ## Now the last 3 columns of A represent the nodes each amendment will touch.
  A[,1:4] <- c( (A[,1]),
               (A[,2]-1),
               (A[,3]+num.com-1),
               (A[,4]+num.com+num.top))
  
  ## A matrix of all of the unique arrows that need to be drawn:
  arrows.mat <- rbind( unique( A[,2:3] ), unique(A[,3:4]) )
  
  ## Calculate edge widths
  width <- EdgeWidths(A, num.com, num.top, edge.width, edge.width.scale)
  
  ## Calculate edge colors
  edge.color <- EdgeColorSCT(A, num.com, num.top, edge.col, edge.transparency)  

  ## Calculate vertex sizes
  size <- VertexSizes(A, num.com, num.top, scale.c, scale.t, scale.fin)
  v.size <- size[,1]
  v.size2 <- size[,2]
  
  topics.matrix <- model.amend.hierarchy.out[[1]][[1]][[2]]

  ## Calculate vertex labels
  labels <- VertexLabels(labels,merged,topics.matrix)
  
  ## The actual object to be graphed:
  g. <- arrows.mat-min(arrows.mat)
  g.. <- as.numeric(t(g.))
  g <- graph(g..)
  
  ## To create the layout, an nx2 matrix denoting the coordinates of each x vertices, you can use
  ## a function or a matrix. The default is to use the following lines to create the matrix: 
  
  if (is.null(layout)){
    x <- 1:(num.com+num.top+2)
    lay.mat <- t(sapply(x,FUN=LayoutSCT,num.com=num.com,num.top=num.top,
                        mid.layer=mid.layer))
    ## So currently the graph is plotted on a (0,0),(1,1) screen, more or less.
  } else {
    lay.mat <- layout(g)
  }
  
  if (is.null(main)) main <- ""
  
  ## graph it!                
  plot(g,
       layout = lay.mat,
       edge.arrow.width = arrowhead.size,
       edge.width = width,
       edge.color = edge.color,
       vertex.label = labels,
       vertex.shape = vertex.shape,
       vertex.size = v.size,
       vertex.size2 = v.size2,
       vertex.label.dist = 0,
       vertex.label.font = vertex.label.font,
       vertex.label.cex = vertex.label.cex,
       vertex.color = vertex.color,
       main = main
       )
  
  if ( plot.terms == TRUE){
    
    terms.list <- list()
    for (i in 1:ncol(topics.matrix)){
      terms.list[[i]] <- topics.matrix[,i]
    }
    
    PlotTopicWords(terms.list, layout=lay.mat,
                   terms.cex, terms.col,
                   terms.x.offset, terms.y.offset,
                   terms.spread, terms.text.close)
  }
}


##' Plots a list of words next to each topic node in the graph created by this
##' package's PlotCommitteeTopics() function. To be used within 
##' PlotCommitteeTopics().
##' @title PlotTopicWords
##' @param words.list A list (of length num.top or less, where num.top is the
##' number of topics in your PlotCommitteeTopics() graph.
##' @param layout A layout matrix, created inside PlotCommitteeTopics().
##' @param cex Text size, default = .5.
##' @param col Text color, default = "grey30".
##' @param x.offset Adjust the x axis placement of the terms. Default = 0.
##' @param y.offset Adjust the y axis placement of the terms. Default = 0.
##' @param spread Adjust the breadth of the terms to be plotted. Default = 1.
##' @param text.close How close should each term for a given topic be? Default = 1.
##' @return Text on plotted onto a PlotCommitteeTopics() graph (with default 
##' layout style).
##' @author Hillary Sanders
PlotTopicWords <- function(words.list, layout,
                           cex=.5, col="grey30",
                           x.offset=0, y.offset=-.05,
                           spread=1, text.close=1
                           ) {
  
  num.top <- length(words.list)
  x.axis <- ( (layout[ ( layout[,2] == unique (layout[, 2])[2]), 1] # the 2nd level
               )*2.8*spread) - 1.4*spread
                                        # The constants 2.8 and -4/25 help to make the more-user-friendly defaults of 1
                                        # look good on a graph plotting a reasonably sized bill.
  y.axis <- seq(.1,
                by=(-4/25)*cex*text.close,length=length(words.list[[1]])) +
                  y.offset

  for (i in 1:num.top){
    x <- (x.axis[i]+x.offset )  # scale
    text(x=x, y=y.axis,
         labels=c(words.list[[i]]), col=col, cex=cex)
  }
}
# end PlotTopicWords


##' Takes output from various bill mapping functions and prepares the data 
##' for the PlotCommitteeTopics() function.
##' @title OutToInSAS
##' @param model.amend.hierarchy.out the output of ModelAmendHierarchy().
##' @param get.likely.composite.out the output of get.likely.composite().
##' @param committees the object "committees", used in other parts of this
##' package, consisting of a vector of committee names for each ith 
##' amendment (accepted, rejected, and discarded amendments).
##' @return A four column matrix, consiting of amendment index, topic index,
##' committee, and a final destinations column: either a final bill index or
##' 0, for rejected amenments.
##' @author Hillary Sanders
OutToInSAS <- function(model.amend.hierarchy.out,
                       get.likely.composite.out,
                       committees){
  
  ## Create a matrix of each amendment index and their topic assignments.
  amend.top.index <- cbind( model.amend.hierarchy.out[[1]][[1]][[4]]
                           -min(model.amend.hierarchy.out[[1]][[1]][[4]])+1
                           , as.numeric(model.amend.hierarchy.out[[1]][[1]][[3]]))
  ## Note that here, only those amendments that are not thrown out due to length 
  ## (e.g. ":") are represented.
  colnames(amend.top.index) <- c("idx","topic #")

  ## Find the indices of those amendments which made it to the composite final bill.
  successful<- get.likely.composite.out[ get.likely.composite.out[,3]=="amendment",c(1,2)]
  colnames(successful) <- c("final.idx","amend.idx")
  

  ## Create a matrix of the successful amendment indices. The second row
  ## becomes helpful in a moment!
  x <- data.frame(1:length(committees),committees)
  names(x) <-c("amend.idx","committees")
  
  joined <- join( x, successful, type="left")
  
  ## All of the elements in the third row that are <NA> (not "amendment") 
  ## must be rejected amendments or amendments discarded by the computer (due to
  ## their very short length).
  joined[,3][is.na(joined[,3])] <- 0
  
  ## Three columns: amendment index, committee, logical: was the amendment accepted?
  
  ## However, amendments that were discarded still need to be removed:
  ## Use amend.top.index, as it only shows non-discarded amendments, and has topic info:
  merged <- merge(amend.top.index,joined,by=1)
  colnames(merged) <- c("amendment.idx","topic.idx","committee","final.idx.or.junk")

  return(merged)
}
## end OutToInSAS()


##' Called within PlotCommitteeTopics to calculate a vector of colors for the graph's
##' edges based on the "edge.color.by" argument chosen.
##' @title EdgeColorSAS
##' @param edge.color.by Either "topics" ("t") or "committees" ("c") may be chosen by the user.
##' If "topics", edge color will be based on the topic each amendment pertains to. If
##' "committees", edge color will be based on the committee each amendment was submitted
##' by.
##' @param col An optional vector of colors, the length of which should be equal to the
##' number of either topics or committees being represented.
##' @param coms A vector of committees corresponding to each amendment i.
##' @param tops A vector of topics corresponding to each amendment i.
##' @author Hillary Sanders
EdgeColorSAS <- function(edge.color.by ="topics", coms, tops, edge.col=NULL){
  

  if (edge.color.by == "topics" | edge.color.by == "t"){
    if (is.null(edge.col)) {
      num.tops <- length (unique(tops))
      edge.col <- colors()[seq(425,600,length=num.tops)]
    }
    edge.color <- edge.col[tops] 
  }
  
  if (edge.color.by == "committees" | edge.color.by == "c"){
    if (is.null(edge.col)){
      num.coms <- length(unique(coms))
      edge.col.possible <- colors()[seq(425,600,length=num.coms)]
    }
    edge.color <- edge.col[as.numeric(factor(coms))]
  }
  
  return(list(edge.color,edge.col))
}
## end EdgeColorSAS


##' Creates a vector of node (vertex) labels for a PlotAmendsSuccess() graph.
##' @title MakeLabelsSAS
##' @param a The number of amendments.
##' @param f the number of paragraphs in the final bill.
##' @param a.lab the number of visible labels on the bottom amendments tier.
##' @param f.lab the number of visible labels on the top final bill tier.
##' @param labels an optional vector of labels which the user may supply.
##' @return A vector of labels for a SAS graph.
##' @author Hillary Sanders
MakeLabelsSAS <- function(a, f, a.lab, f.lab, labels=NULL){
  
  if (is.null(labels)) {
    
    a.labeled <- floor( c( seq(1, a, length=a.lab) ))
    f.labeled <- floor(seq(1, f, length=f.lab) )
    
    labels <- rep("", a+f+1)
    
    for (i in a.labeled) labels[i] <- i
    for (i in f.labeled) labels[a+i] <- i
    labels[a+f+1] <- "Junk"
  }
  return(labels)
}
# end MakeLabelsSAS

##' Creates a vector of node (vertex) shapes for a PlotAmendsSuccess() graph.
##' @title MakeShapesSAS
##' @param a the number of amendments.
##' @param f the number of paragraphs in the final bill.
##' @param junk.shape the shape of the junk node.
##' @return A vector of shapes for a SAS graph.
##' author Hillary Sanders
MakeShapesSAS <- function(a, f, junk.shape){ 
  
  shape.a <- rep("rectangle", a)
  
  shape.f <- rep("rectangle", f)
    
  shape <- c(shape.a, shape.f, junk.shape)
    
  return (shape)
  }
# end MakeShapesSAS


##' Creates a vector of node (vertex) sizes for a PlotAmendsSuccess() graph.
##' @title MakeAmendsSizes
##' @param a the number of amendments.
##' @param f the number of paragraphs in the final bill.
##' @param junk.shape the shape of the junk node.
##' @return A vector of sizes for a SAS graph.
##' author Hillary Sanders
MakeAmendsSizes <- function(a, f, junk.scale){

  size.a <- rep(2/a, a)
  size.f <- rep(2/f, f)
  size.junk <- 30*junk.scale
  
  vertex.sizes <- c(size.a, size.f, size.junk)
  
  return(vertex.sizes)
  }
# end vertex.sizes


##' A function called within PlotAmendsSuccess(). Creates vertex colors
##' based on either the committee related to each node, or the topics
##' related to each node, as chosen by the user.
##' @title MakeVertexColors
##' @param a number of amendments.
##' @param f number of paragraphs in the final bill.
##' @param merged
##' @param vertex.color.by
##' @param vertex.col
##' @author Hillary Sanders
##' @export
MakeVertexColors <- function(a, f, merged,
                             vertex.color.by="c", vertex.col=NULL,
                             vertex.junk.color){
                             	
    
  succeeded <- merged[merged$final.idx.or.junk != 0,2:4]
    
  final.order <- order(succeeded[,3])
    
  succeeded.ordered <- succeeded[final.order,]
  colnames(succeeded.ordered) <- c("topic","committee","final.idx")
  
  final.idx <- data.frame(1:f)
  colnames(final.idx) <- "final.idx"
  
  final <- join (final.idx, succeeded.ordered)
          
  if(vertex.color.by == "c" | vertex.color.by == "committees"){
    
    coms.amends <- as.numeric(factor(merged$committee))
    
    coms.final <- as.numeric(factor(final$committee))
    
    coms.final[is.na(coms.final)] <- max(coms.final, na.rm=TRUE) + 1
    
    if(is.null(vertex.col)){ 
      vertex.col <- colors()[seq(500,650,length=10)]
    }
    
    vertex.col.short <- c(vertex.col[1:max(coms.final, na.rm=TRUE)-1],"white")
    
    vertex.color <- c(vertex.col.short[c(coms.amends,coms.final)],vertex.junk.color)
  }
  
  if(vertex.color.by == "t" | vertex.color.by == "topics"){
    
    tops.amends <- merged$topic.idx
    
    tops.final <- final$topic
    
    tops.final[is.na(tops.final)] <- max(tops.final,na.rm=TRUE) + 1
    
    if(is.null(vertex.col)){ 
      vertex.col <- colors()[seq(500,650,length=10)]
    }
    
    vertex.col <- c(vertex.col[1:max(tops.final, na.rm=TRUE)-1], "white")
    
    vertex.color <- c(vertex.col[c(coms.amends,coms.final)],"cornflowerblue")
    }
  
  return(vertex.color)
}


##' Creates the "x"th layout coordinates for PlotAmendsSuccess(). This function
##' is called inside of PlotAmendsSuccess() to create the layout: coordinates
##' for two layers consisting of 1) amendments (a of them), 2) the final bill 
##' (all of the paragraphs (or other text chunks) in the final bill, as well as
##' a junk bin placed in the middle of the graph.
##' @title LayoutSAS
##' @param x the index of the coordinates to be created. 
##' @param a the number of amendments.
##' @param f the number of text chunks (generally, paragraphs) in the final bill.
##' @return the xth layout coordinates for PlotAmendsSuccess().
##' @author Hillary Sanders
LayoutSAS <- function(x, a, f){
  if (x<=a) { cords <- c(x/(1+a), .4)
                 } else {
                   if (x==(a+f+1)) { cords <- c(.5, .2)
                              } else {
                                cords <- c( ((x-a)/(1+f)), .8) }}
  return (cords)
}
# End LayoutSAS


##' Creates a three tiered directed acyclic graph to visualize bill evolution.
##' Individual amendments are either connected (with an arrow) to a junk bin if the
##' amendment was not accepted into the final bill, or if it was, to its place in 
##' the final bill.
##' @title PlotAmendsSuccess
##' @param model.amend.hierarchy.out the output of ModelAmendHierarchy().
##' @param get.likely.composite.out the output of get.likely.composite().
##' @param committees the object "committees", used in other parts of this
##' package, consisting of a vector of committee names for each ith 
##' amendment (accepted, rejected, and discarded amendments).
##' @param edge.color.by Either "topics" ("t") or "committees" ("c") may be chosen by the user.
##' If "topics", edge color will be based on the topic each amendment pertains to. If
##' "committees", edge color will be based on the committee each amendment was submitted
##' by.
##' @param col An optional vector of colors, the length of which should be equal to the
##' number of either topics or committees being represented.
##' @param edge.width.scale Scale edge widths. Default = 1
##' @param arrowhead.size Size of edge arrowheads. Default = 0.
##' @param junk.shape The node shape of the junk node. Possible 
##' shapes are "circle", "square", "csquare", "rectangle", "crectangle", "vrectangle",
##' and "none". Default = "none".
##' @param af.scale Scale the size of the amendment and final nodes.
##' @param junk.scale Scale the size of the junk node.
##' @param label.font Font type for the labels. Default = 3.
##' @param label.cex Font size for the labels. Default = .75.
##' @param labels Vector of labels for each amendments, each final bill 
##' paragraph, and the junk bin. If left NULL, ten (relatively) equidistant
##' nodes will be labeled by their paragraph indices for both amendments and 
##' the final bill, while the rejected amendments bin will be labeled "Junk".
##' @param a.lab the number of visible labels on the bottom amendments tier.
##' default = 10.
##' @param f.lab the number of visible labels on the top final bill tier.
##' default = 10.
##' @param main The plot title. Default = "Amendments' Destinations".
##' @param legend.x X axis placement of the legend.
##' @param legend.y Y axis placement of the legend.
##' @param legend.cex size of the legend.
##' @return A hopefully pretty graph!
##' @author Hillary Sanders
##' @export
PlotAmendsSuccess <- function(model.amend.hierarchy.out, get.likely.composite.out, committees,
                             edge.color.by="topics", edge.col=NULL,
                             edge.width.scale=1, arrowhead.size=0,
                             af.shape="none", junk.shape="rectangle",
                             af.scale=1, junk.scale=1,
                             label.font=3,label.cex=.75, labels=NULL,
                             a.lab=10, f.lab=10, vertex.color.by="c",
                             vertex.col=NULL, vertex.junk.color="cornflowerblue",
                             main="Amendments' Destinations",
                             legend.x=-1.25, legend.y=.75, legend.cex=.5
                             ){
  
  merged <- OutToInSAS (model.amend.hierarchy.out, get.likely.composite.out, committees)
  amends.idx <- merged[,1]
  tops <- merged$topic.idx
  coms <- merged$committee
  destinations <- merged$final.idx.or.junk

  ## number of paragraphs in final bill.
  f <- nrow(get.likely.composite.out) 
  ## number of amendments.
  a <- length(destinations) 
  amends.plot.idx <- 1:a
  final.idx <- 1:f
  
  
  colors <- EdgeColorSAS(edge.color.by, coms, tops, edge.col=NULL)
  edge.color <- colors[[1]]
  col <- colors[[2]]
  
  destinations[destinations==0] <- f+1
  ## So that each amendment destined for the junk bin will go to that last,
  ## (a+f+1)th, vertex, with index a+f, since igraph indices start at 0:
  
  mat <- matrix(c(amends.plot.idx-1, 0, 0, destinations+a-1, (f+a-1), a), ncol=2)
  g <- as.numeric(t(mat))          
  graph <- graph(g)
  ## the 0 -> f+a-1 and 0 -> a are to ensure that all 1:f final paragraphs are
  ## shown in the graph. They will have no color, so will be invisible. The 
  ## (f+a+1)th vertex will be the junk bin, with graph index (f+a), since igraph 
  ## indices start at 0.
  
  x <- (a+f+1)
  y <- 1:x
  lay.mat <- t(sapply(y,FUN=LayoutSAS, a=a, f=f))
  
  labels <- MakeLabelsSAS(a, f, a.lab, f.lab, labels)  
  
  v.shape <- MakeShapesSAS(a, f, junk.shape)
  
  vertex.size <- MakeAmendsSizes(a, f, junk.scale)
  
  vertex.color <- MakeVertexColors(a, f, merged, vertex.color.by,
                                   vertex.col, vertex.junk.color)
  
  vertex.frame.color <- vertex.color

  plot(graph, layout=lay.mat, edge.arrow.width=arrowhead.size,
       edge.width=edge.width.scale, edge.color=edge.color, vertex.color=vertex.color,
       vertex.frame.color = vertex.frame.color, vertex.shape=v.shape, vertex.size=vertex.size,
       vertex.label=labels, vertex.label.font=label.font, vertex.label.cex=label.cex,
       main=main)


  if (edge.color.by == "topics" | edge.color.by == "t"){
    leg.text <- paste("Topic",1:length(unique(tops)), sep=" ")
  }
  
  if (edge.color.by == "committees" | edge.color.by =="c"){
    leg.text <- levels(factor(committees))
  }
  
  legend(legend.x,legend.y, leg.text, col, bg="lavenderblush",cex=legend.cex)
}

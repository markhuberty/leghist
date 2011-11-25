## Unit test functions for the legist code
## Begun 11 November 2011
## Mark Huberty

library(RUnit)

test.MapBills <- function(){

  ## TODO HERE: per Adirenne Hosek, would be better for the US
  ## Congress process to return something like:
  ## bill2.idx, bill1.idx, bill1.dist, amend.idx, amend.dist
  ## In other words, return both the best match to the original, and
  ## the best match to the amendments. 
  vec.in <- c("abcd", "efgh", "ijkl", "mnop")
  list.in <- list(vec.in, vec.in)
  dtm.list.in <- CreateAllVectorSpaces(vec.in,
                                       vec.in,
                                       amendments=NULL,
                                       ngram=1,
                                       stem=FALSE,
                                       rm.stopwords=FALSE,
                                       rm.whitespace=FALSE,
                                       rm.punctuation=FALSE,
                                       filter=NULL,
                                       filter.thres=NULL
                                       )
  
  df.out <- data.frame(c(1:4),
                       c(1:4),
                       rep(1, 4),
                       NA,
                       NA
                       )
  
  names(df.out) <- c("bill2.idx", "bill1.idx", "bill1.dist",
                       "amend.idx", "amend.dist")
  
  ## checkEquals(MapBills(doc.list=list.in
  ##                      dist.fun="raw",
  ##                      n.to.return=1,
  ##                      dist.threshold=0
  ##                      ),
  ##             df.out
  ##             )

  checkEquals(MapBills(doc.list=dtm.list.in,
                       distance.fun="cosine"
                       ),
              df.out
              )
               
  
}## End test.MapBills

test.GetLikelyComposite <- function(){

  vec.in <- c("abcd", "efgh", "ijkl", "mnop")
  list.in <- list(vec.in, vec.in)
  dtm.list.in <- CreateAllVectorSpaces(vec.in,
                                       vec.in,
                                       amendments=NULL,
                                       ngram=1,
                                       stem=FALSE,
                                       rm.stopwords=FALSE,
                                       rm.whitespace=FALSE,
                                       rm.punctuation=FALSE,
                                       filter=NULL,
                                       filter.thres=NULL
                                       )

  mapbills.in <- MapBills(doc.list=dtm.list.in,
                          distance.fun="cosine"
                          )

  test.out <- data.frame(1:4,
                         1:4,
                         "doc.initial",
                         "Original",
                         vec.in
                         )
  names(test.out) <- c("doc.final.idx",
                       "match.idx",
                       "match.origin",
                       "alt.origin",
                       "match.txt"
                       )
    
  checkEquals(GetLikelyComposite(mapbills.in,
                                 vec.in,
                                 vec.in
                                 ),
              test.out
              )

}

test.ModelChanges <- function(){

}## End test.ModelChanges

test.MapFun <- function(){

  ## TODO: these should be sparse representations
  matrix1 <- matrix(ncol = 300,nrow = 100, c(1:(100*300)))
  matrix2 <- matrix(ncol = 300,nrow = 100, c(1:(100*300)))

  df.out <- data.frame(c(1:100),
                       c(1:100),
                       rep(1,100)
                       )
  
  names(df.out) <- c("idx.doc.2","idx.doc.1","distance")


  checkEquals(MapFun(doc1 = matrix1 ,
                     doc2 = matrix2 ,
                     distance.fun="cosine"
                     ),
              df.out
              )
  
}## End test.MapFu

test.CreateAllVectorSpaces <- function(){

  doc.in <- c("Should I test this?", "I guess I should.")
  corpus.in <-Corpus(VectorSource(doc.in),
                     readerControl=list(readPlain),
                     language="en",
                     load=TRUE
                     )
  corpus.in <- tm_map(corpus.in, tolower)
  corpus.in <- tm_map(corpus.in, removeWords, stopwords("english"))
  corpus.in <- tm_map(corpus.in, stripWhitespace)
  corpus.in <- tm_map(corpus.in, removePunctuation)
  mat.out <- DocumentTermMatrix(corpus.in)

  list.out <- list(mat.out, mat.out, NULL)
  names(list.out) <- c("doc.final", "doc.initial", "amendments")

  checkEquals(CreateAllVectorSpaces(doc.initial=doc.in,
                                    doc.final=doc.in,
                                    amendments=NULL,
                                    ngram=1,
                                    stem=FALSE,
                                    rm.stopwords=TRUE,
                                    rm.whitespace=TRUE,
                                    rm.punctuation=TRUE,
                                    filter=NULL,
                                    filter.thres=NULL
                                    ),
              list.out
              )
  


}

test.CreateVectorSpace <- function(){

  doc.in <- c("Should I test this?", "I guess I should.")
  corpus.in <-Corpus(VectorSource(doc.in),
                     readerControl=list(readPlain),
                     language="en",
                     load=TRUE
                     )
  corpus.in <- tm_map(corpus.in, removeWords, stopwords("english"))
  corpus.in <- tm_map(corpus.in, tolower)
  corpus.in <- tm_map(corpus.in, stripWhitespace)
  corpus.in <- tm_map(corpus.in, removePunctuation)
  mat.out <- DocumentTermMatrix(corpus.in)

  ## Test with very basic filters
  checkEquals(CreateVectorSpace(doc.in,
                                ngram=1,
                                stem=FALSE,
                                dictionary=NULL,
                                rm.stopwords=FALSE,
                                rm.whitespace=TRUE,
                                rm.punctuation=TRUE,
                                filter=NULL,
                                filter.thres=NULL
                                ),
              mat.out
              )

  ## Test against a defined dictionary

}## End test.CreateVectorSpace

test.ComputeIdentity <- function(){

  
  




}## End test.ComputeIdentity

test.WriteTexSection <- function(){

  string.in <- "This is a test"
  highlight.words <- "test"
  origin="amendment"
  origin.idx=1

  string.out <-
  "This is a \\texthighlight{test}\\marginnote{amendment 1}"

  checkEquals(WriteTexSection(string.in,
                              highlight.words=highlight.words,
                              origin=origin,
                              origin.idx=origin.idx
                              ),
              string.out
              )



}


test.OrderVectorSpace <- function(){

}## End test.OrderVectorSpace

test.ModelImpact <- function(){

}## End test.ModelImpact


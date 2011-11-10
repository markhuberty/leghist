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
require(openNLP) ## For the sentence tokenizer functionality

MapBills <- function(bill1,
                     bill2,
                     amendments=NULL,
                     dist.fun="raw",
                     n.to.return=1,
                     dist.threshold=NULL, ...){
  ## Maps content of bill1 to bill2. Returns a vector of indices
  ## mapping the sections of bill1 (however provided by the user) to
  ## the sections of bill2. Mapping is accompanied by a distance
  ## measure to provide a confidence interval.
  ## Inputs:
  ##    bill1, bill2: character vectors, each element of which is a
  ##     section of a bill. Any level of disaggregation can be used (i.e.,
  ##     article, paragraph, clause, etc).
  ##    amendments: amendments proposed to bill 1, disaggregated at
  ##     the same level of detail as the bills themselves
  ##    dist.fun: the distance function to be used in mapping bill
  ##     sections to each other. Default is "raw"
  ##    n.to.return: number of matched clauses to return. Default is
  ##     1, meaning that only the closest match is returned.
  ##    dist.threshold: a distance threshold to use for determining
  ##    matches. If NULL, this defaults to assuming the
  ##    nearest-neighbor match and returning the match + distance
  ## Outputs:
  ##    bill.map: a data frame of indices mapping _from_ bill1 _to_
  ##     bill2. New sections are indicated by "N". Deleted sections
  ##     are indicated by "D". If amendments are passed, then "N"
  ##     values are also paired with the amendment that most likely
  ##     became the new section.
  ##     A normalized distance measures should be provided for
  ##     all matches


  ## NOTES: see
  ## http://finzi.psych.upenn.edu/R/library/arules/html/dissimilarity.html
  ## The R dissimilarity function provides cosine and jaccard options


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

MapFun <- function(){
  ## Does the actual pairwise mapping of bills
  

}
## End MapFun

ModelImpact <- function(){
  ## Classifies changes as substantive or administrative
  ## <Question here is _how_: what could you exploit to differentiate
  ## between substance and administrative procedure; preambles and
  ## stated purposes there might provide a good means of doing this
  ## later. Could, for instance, topic model the preamble, assign
  ## paragraphs from the preamble to topics, and then use that model
  ## to classify the rest of the text / changes to the text.

  ## Should return a class vector w/ probabilities 
  

}
## End ModelImpact

FormatText <- function(candidate.text,
                       disagg.level="p",
                       disagg.lables=NULL,
                       sep="\\n\\n"){
  ## Takes as input an undifferentiated character string with the
  ## text of interest and formats it for use by other models

  ## Note that the input strings should be clean--no page nos, artifacts
  ## of OCR'ing, etc.

  ## sep refers to the distinguishing feature that divides the string
  ## at the disagg level (one of c:clause, p:paragraph; s:sentence)


}




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


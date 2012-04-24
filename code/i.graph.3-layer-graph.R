#################################################################
## Hillary Sanders
## Automated analysis of legislative history - Graphing 
## Begun: March 2012

## This file contains functions that will be the graphing component of an R 
## package whose purpuse is to automate the textual evolution of legislation.
## The file contains two major functions which create different directed acyclic
## graphs, as well as a few smaller helper functions which are called within 
## the larger functions.
## END
#################################################################
## require(igraph)


##' Creates the "x"th layout coordinates for graph.1(). This function is called
##' inside of graph.1() to create the layout: three layers consisting of
##' 1) committees (c of them), 2) topics (t of them), and the final destinations
##' of the amendments (junk and final). 
##' @title Lay.Graph.1
##' @param x the index of the coordinates to be created.
##' @param c the number of committees (number of nodes wanted in the bottom layer
##' of the graph).
##' @param t the number of topics (number of nodes wanted in the middle layer of 
##' the graph).
##' @return the xth pair of coordinates for the layout of graph.1().
##' @author Hillary Sanders
Lay.Graph.1 <- function(x,c,t){
  if (x<(c+1)) { cords <- c(x/(1+c),.2)
                 } else {
                   if (x>(c+t)) { cords<- c( (x-c-t)/3,.8)
                                  } else {
                                    cords<- c( (x-c)/(1+t),.5)
                        }
                   }
        return (cords)
                }
## end Lay.Graph.1


##' A small function called within graph.1() to calculate edge widths (arrow widths)
##' for a default graph.1().
##' @title How.Wide
##' @param x the index of the arrow whose width will be returned by this function.
##' @param A An ax4 matrix, where a = number of amendments. Each row represents an
##' amendment: it's index (on of 1:a), it's committe (one of 1:c), it's topic (one
##' of 1:t), and its final destination (junk or final bill: 0 or 1). See graph.1()
##' for more details.
##' @param num.arrows.to.topics The number of distinct edges (arrows) that are going
##' to topics (the middle layer) in the graph.1() plot.
##' @return the width of the x[3]th edge (arrow).
##' @author Hillary Sanders
How.Wide <- function(x,A,num.arrows.to.topics){
                if (x[3]< num.arrows.to.topics+1){
                        width<- sum((x[1]==A[,2]) & (x[2]==A[,3]))
                } else {
                        width<- sum((x[1]==A[,3]) & (x[2]==A[,4]))
                } 
                }
# End How.Wide

# Similar to How.Wide, makes arrow widths relative to their origin.
How.Wide.Relative <- function(x,A,num.arrows.to.topics){
        if (x[3]< num.arrows.to.topics+1){
                width<- sum((x[1]==A[ ,2]) & (x[2]==A[ ,3])) / sum(x[1]==A[ ,2]) *15
                } else {
                        width<- sum((x[1]==A[ ,3]) & (x[2]==A[ ,4])) / sum(x[1]==A[ ,3]) *15
                        } 
        }
#End How.Wide.Relative


# Similar to How.Wide, How.Wide.Relative makes arrow widths relative to the % of
# successful amendments they carry.
How.Wide.Success <- function(x,A,num.arrows.to.topics){
  if (x[3]< num.arrows.to.topics+1){
    width <- sum((x[1]==A[, 2]) & (x[2]==A[, 3]) & (A[, 4] == (c+t+1))) / 
      sum((x[1]==A[, 2]) & (x[2]==A[, 3])) *15
    } else {
      width <- sum((x[1]==A[, 3]) & (x[2]==A[, 4]) & (A[, 4]==(c+t+1))) /
        sum((x[1]==A[, 3]) & (x[2]==A[, 4])) *15
      }
  }
#End How.Wide.Sucess



##' The main graphing function for the legislative bill mapping package. This function
##' creates a three layer directed acyclic graph. The first, bottom layer is a set of 
##' nodes representing committees which have each submitted a number of amendments. 
##' Arrows (edges) connect these committee nodes to the middle layer topic nodes to 
##' which each committee's amendemnt(s) pertain. Arrows then connect these topic nodes 
##' to the third layer - two nodes: "Junk" and "Final", again according to the amendments
##' that the arrows represent. Arrow width, as well as node size, by default represent 
##' the number of amendments they are representing.
##' @title graph.1
##' @param amend.committees A vector of length a, where a = the number of amendments
##' introduced. The ith element in amend.committees is an integer - one of 1:c, where c
##' is the number of committees - representing which comittee introduced the ith amendment.
##' @param amend.topics A vector of length a, where a = the number of amendments introduced.
##' The ith element in amend.topics is an integer - one of 1:t, where t is the number of 
##' topics - representing which topic pertains to the ith amendment. 
##' @param amend.final A vector of length a, where a = the number of amendments introduced.
##' The ith element in amend.committees is either 0 or 1 - representing whether the ith
##' amendment was rejected (0) or if it was accepted into the final bill (1). 
##' @param labels A character vector representing the node names for each node (vertex). If
##' NULL, labels will default to 1:c,1:t integers for committees and topics, and "Junk" and
##' "Final" for the two final bins (top level).
##' @param edge.width.scale Scales the width of the arrows. Defaults to 1.
##' @param edge.width The method used to calculate edge widths. The default, "absolute", means
##' that edge widths will correspond to the absolute number of amendments they represent. 
##' "relative": edge widths will correspond to the % of amendments each edge holds with respect
##' to the vertex they are coming from. "success": edge widths will correspond to the % of
##' amendments in each edge which are destined for the final bill.
##' @param scale.c Scales the size of the bottom layer committee nodes (vertices). By default,
##' scale.c, scale.t, and scale.fin = 1, and their area are equally proportional to the number
##' of amendments they represent.
##' @param scale.t Scales the size of the middle layer topic nodes.
##' @param scale.fin Scales the size of the top layer final nodes (Junk and Final).
##' @param edge.transparency A number in 00:99, representing the wanted transparency in edges
##' (lower number = more transparent). If left NULL (the default), edges will be kept opaque.
##' @param edge.col Three edge colors to signify edges which contain 1) junk destined
##' amendments, 2) final destined amendments, or 3) both. If NULL (the default) graph.1() will
##' use "cornflowerblue", "darkgoldenrod1", and "chartreuse3".
##' @param layout The layout of the graph. If NULL (the default), graph.1() will create the 
##' three layers decribed above. But users can also pass graphing algorithms (e.g 
##' layout.fruchterman.reingold) for a different layout.
##' @return A hopefully pretty graph!
##' @author Hillary Sanders
graph.1 <- function(amend.committees, amend.topics, amend.final,
                    labels=NULL,
                    edge.width.scale=1, edge.width = "absolute",
                    scale.c=1, scale.t=1, scale.fin=1,
                    edge.transparency=NULL, edge.col=NULL,
                    main=NULL, arrowhead.size=0, layout=NULL
                   ) {
  
  a <- length(amend.committees)
  # add an error message for if amend.committees, .topics, or .final are not of the
  # same length.
  
  A <- matrix(c(1:a,amend.committees,amend.topics,amend.final),ncol=4)
  # a for # of amendments, c for # of committees, t for number of topics.
  a <- nrow(A)
  c <- length(unique(A[,2]))
  t <- length(unique(A[,3])) 
     
  # reindex. Note that igraph takes numbers starting at 0, not 1.
  # Now the last 3 columns of A represent the nodes each amendment will touch.
  A[,1:4] <- c( (A[,1]),
                (A[,2]-1),
                (A[,3]+c-1),
                (A[,4]+c+t))
        
  # A matrix of all of the unique arrows that need to be drawn:
  arrows.mat <- rbind( unique( A[,2:3] ), unique(A[,3:4]) )
  num.arrows.to.topics <- nrow(unique( A[,2:3]))
  num.arrows.to.jf <- nrow(unique(A[,3:4]))
  num.arrows <- nrow(arrows.mat) # the total number of arrows to be drawn

  ### Edge Parameters. (Arrows)
  # 1) Edge width
  if(edge.width == "absolute"){
          width <- apply(cbind(arrows.mat,1:num.arrows),1,How.Wide,
                         A=A,num.arrows.to.topics=num.arrows.to.topics)
          }
  if(edge.width == "relative"){
                  width <- apply(cbind(arrows.mat,1:num.arrows),1,How.Wide.Relative,
                         A=A,num.arrows.to.topics=num.arrows.to.topics)
          }
  if(edge.width == "success"){ 
                width <- apply(cbind(arrows.mat,1:num.arrows),1,How.Wide.Success,
                               A=A,num.arrows.to.topics=num.arrows.to.topics)
  }
  # Scale it:
  width <- ceiling(edge.width.scale*width)

  
  # 2) Arrow colors.
  
  if (is.null(edge.col))  colors <- c("#6495ED","#FFB90F","#66CD00") 
  # "cornflowerblue", "darkgoldenrod1","chartreuse3" 
   
     
  if(!is.null(edge.transparency)){ 
          for (i in 1:3){
 # Extract the rgb code if color is passed as a character vector, as opposed to an rgb code.
                  if( strsplit(colors[i],"")[[1]][1] != "#") {
                          colors[i] <- rgb(col2rgb(colors[i])[1],
                                           col2rgb(colors[i])[2],
                                           col2rgb(colors[i])[3], maxColorValue=255)
                  }
          }
          for (i in 1:3){
# Add a transparency number (in 00:99)
                  colors[i] <- paste( colors[i], as.character(edge.transparency), sep="")
          }
  }
      
  # The final destination (1 or 2: junk or final) of each unique edge (arrow):
  # (So if there are 3 of the same arrow, the first one is represented.) 
  edge.color.idx <- c( (A[!duplicated(A[,2:3]),4]),
                       (A[!duplicated(A[,3:4]),4]) ) -c-t+1
            
  # The following lines look at each comittee-to-topics arrow to be drawn. The question is: 
  # are the (perhaps multiple) amendment(s) that this arrow is representing heading to both 
  # the final bill AND junk, or just one of those two? If both, then the arrow color should 
  # be green - a combination of yellow and blue. (Those colors = default colors)
  for ( i in which(!duplicated(A[,2:3]))){
  # i.e. for each non-duplicated arrow i (each row i in matrix A which corresponds to 
  # a "new" or "unique" arrow going from comittees to topics), which ones does it match to 
  # (if it matches to any)? in the set of all amendments:
    identical <- c( which ( ( (A[i,2]==A[,2]) * (A[i,3]==A[,3])) ==1) )
    # Where are these identical arrows going to? (i.e. junk or final)
    destinations <- A [ identical,4]
               # If their final destinations are not all the same, then make their drawn arrow be green.
    if(length(unique(destinations))!=1) {
                    # should not be identical[1] # fix
      edge.color.idx[ order((i==which(!duplicated(A[,2:3])))==0)[1]] <- 3
                  }
                }
 
    edge.color <- colors[edge.color.idx]
  
  # Note: Vertex label colors?        
        
  ### Vertex Parameters
  # 1) Vertex label size.
  vertex.size <- rep(0,sum(c+t+2))
      
      for (i in 1:c){
        vertex.size[i] <- sum(A[,2]==(i-1))
        }
      for (i in (c+1):(c+t)) {
        vertex.size[i] <- sum(A[,3]==(i-1))
        }
      for (i in (c+t+1):(c+t+2)) {
        vertex.size[i] <- sum(A[,4]==(i-1))
        }
  # Here, both dimensions of the default rectangle vertex shape are created, and scaled
  # by how large the biggest vertex is on the graph. 
  biggest <- max(vertex.size)
  v.size <- ((sqrt(vertex.size))/(sqrt(biggest))*(60))
  v.size2 <- ((sqrt(vertex.size)/(sqrt(biggest))*(45)))
        
  # Vertex sizes can also be rescaled by the user by scale.c, scale.t, and
  # scale.fin inputs. Defaults = 1.

  v.size[1:c] <- v.size[1:c]*scale.c
  v.size[(c+1):(c+t)] <- v.size[(c+1):(c+t)]*scale.t
  v.size[(c+t+1):(c+t+2)] <- v.size[(c+t+1):(c+t+2)]*scale.fin
       
        
  # 3) The vertex labels
  if (is.null(labels)) { 
    labels <- as.character(c(1:c,1:t,"Junk","Final"))
                           }
        
  # The actual object to be graphed:
  g. <- arrows.mat-min(arrows.mat)
  g.. <- as.numeric(t(g.))
  g <- graph(g..)
        
  ### Layout
  # To create the layout, an nx2 matrix denoting the coordinates of each x vertices, you can use
  # a function or a matrix. The default is to use the following lines to create the matrix: 
  # (where there are c+t+2 vertices, c of them committees, t of them topics, and 2 of them 
  # either the final or junk bin.)
              
  #  For the layout the matrix:
  if(is.null(layout)){
    x <- 1:(c+t+2)
    lay.mat <- t(sapply(x,FUN=Lay.Graph.1,c=c,t=t))
    # So currently the graph is plotted on a (0,0),(1,1) screen, more or less.
    } else {
      lay.mat <- layout
      }
        
  if(is.null(main)) main <- ""
  
  # graph it!                
  plot(g,
       layout=lay.mat,
       edge.arrow.width= arrowhead.size,
       edge.width= width,
       edge.color= edge.color,
       vertex.label= labels,
       vertex.shape= "rectangle",
       vertex.size= v.size,
       vertex.size2 = v.size2,
       vertex.label.dist= 0,
       vertex.label.font=.3,
       vertex.label.cex=.75,
       main=main
       )
      }
 
##' Plots a list of words next to each topic node in the graph created by this
##' package's graph.1() function. To be used after graph.1().
##' @title Plot.Topic.Words
##' @param words.list A list (of length t or less, where t is the number of topics
##' in your graph.1() graph.
##' @param layout A
##' @return Text on top of a graph.1() graph.
##' @author Hillary Sanders
  Plot.Topic.Words <- function(words.list, layout,
                               cex=.75, col="darkgrey", pos=2, offset=.2,
                               adjust=3.5, text.close=.75) {
        
  t <- length(words.list)
  
  for (i in 1:t){
    y <- seq(.1,by=(-4/30)*cex/text.close,length=10) [ 1:length(words.list[[i]]) ]
    x <- (( (lay.mat
            [ ( lay.mat[,2] == unique (lay.mat[,2])[2]),1] # the 2nd level
            - unique(lay.mat[,2])[2])*adjust)[i]-offset )  # scale
 text(x=x, y=y, pos=pos, offset=offset,
      labels=c(words.list[[i]]), col=col,cex=cex)
    }
  }
# end Plot.Topic.Words


##' Creates the "x"th layout coordinates for graph.2(). This function is called
##' inside of graph.2() to create the layout: coordinates for two layers consisting
##' of 1) amendments (a of them), 2) the final bill (all of the paragraphs (or other
##' text chunks) in the final bill, as well as a junk bin placed in the middle of 
##' the graph.
##' @title Lay.Graph.2
##' @param x the index of the coordinates to be created. 
##' @param a the number of amendments 
##' @param f the number of text chunks (e.g. paragraphs) in the final bill.
##' @return the xth layout coordinates for graph.2().
##' @author Hillary Sanders
Lay.Graph.2 <- function(x,a,f){
  if (x<(a+1)) { cords<- c(x/(1+a),.2)
                 } else {
                   if (x>(a+f)) { cords <- c(.5,.5)
                                  } else {
                                    cords <- c( ((x-a)/(1+f)),.8) }}
  return (cords)
  }
  # End Lay.Graph.2


  # NOTE: Question: Amendments either: 1) replace some paragraph(s) in the inital
  # bill, or 2) are shoved into the initial bill, and replace nothing. This could
  # be represented in the following graph by color, if the infomation were passed
  # to the function. Is that vector easy to get? Figure out.

##' Creates a two layered directed acyclic graph to visualize bill evolution.
##' Individual amendments are either connected (with an arrow) to a junk bin if the
##' amendment was not accepted into the final bill, or if it was, to its place in 
##' the final bill.
##' @title graph.2
##' @param amends A vector of all 1:a amendments' final destinations: either 0 for
##' junk (rejected amendments) or an integer i for the ith paragraph in the final bill
##' which the amendment replaced.
##' @param f Integer: the number of paragraphs (text chunks) in the final bill.
##' @param edge.width
##' @return 
##' @author Hillary Sanders
graph.2 <- function(amends,
                    f=NULL, 
                    edge.width.scale=3, edge.arrow.width=.25,
                    af.shape="circle", junk.shape="rectangle",
                    label.font=3,label.cex=.75,
                    main="Amendments' Destinations"
                    ){
  if (is.null(f)){f <- max(a)}
  a <- length(amends)
  amends.idx <- 1:a
  final.idx <- 1:f
        
  colors <- c("cornflowerblue","lightblue")
  edge.color <- colors[(amends == 0)+1]
        
  amends[amends==0] <- f+1
  
  g <- as.numeric(t(matrix(c(amends.idx-1,0,amends+a-1,(f+a-1)),ncol=2)))           
  graph <- graph(g)
  # the 0 -> f+a-1 is to ensure that all final paragraphs are shown in the graph.
  # It has no color, so is invisible.
         
  x <- (a+f+1)
  y <- 1:x
  lay.mat <- t(sapply(y,FUN=Lay.Graph.2,a=a,f=f))
        
  labels <- c(1:a,1:f,"junk")
        
  v.shape <- c(rep(af.shape,a+f),junk.shape)
  v.size <- c(rep(15,a+f),30)
        
  plot(graph, layout= lay.mat, edge.arrow.width = edge.arrow.width,
       edge.width= edge.width, edge.color= edge.color, vertex.shape=v.shape,
       vertex.size=v.size, vertex.label= labels, vertex.label.font=label.font,
       vertex.label.cex=label.cex, main=main)
  } 
  
  
  
  
  
  







 
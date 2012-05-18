#################################################################
## Hillary Sanders
## Automated analysis of legislative history - Graphing 
## Begun: March 2012

## This file contains functions that will be the graphing component of an R 
## package whose purpose is to automate the textual evolution of legislation.
## The file contains two major functions which create different directed acyclic
## graphs, as well as a few smaller helper functions which are called within 
## the larger functions.
## END
#################################################################
## require(igraph)
## require(plyr)


##' Creates the "x"th layout coordinates for See.Committee.Topics(). This function
##' is called inside of See.Committee.Topics() to create the layout: three layers 
##' consisting of 1) committees (c of them), 2) topics (t of them), and the final
##' destinations of the amendments (junk and final). 
##' @title Lay.See.Committee.Topics
##' @param x the index of the coordinates to be created.
##' @param num.com the number of committees (number of nodes wanted in the bottom
##' layer of the graph).
##' @param num.top the number of topics (number of nodes wanted in the middle
##' layer of the graph).
##' @param mid.layer The y-axis placement of the middle layer on the graph. 
##' Defaults to .6. Note that the bottom and top layers are at 0 and 1. 
##' @return the xth pair of coordinates for the default layout of 
##' See.Committee.Topics().
##' @author Hillary Sanders
Lay.See.Committee.Topics <- function(x,num.com,num.top,mid.layer=.6){
  
  if (x<(num.com+1)) { cords <- c(x/(1+num.com),0)
        } else {
          if (x>(num.com+num.top)) { cords<- c( (x-num.com-num.top)/3,1)
                } else {
                  cords <- c( (x-num.com)/(1+num.top) )
                  }
          }
        return (cords)
  }
## end Lay.See.Committee.Topics


##' A small function called within See.Committee.Topics() to calculate edge widths
##' (arrow widths) for See.Committee.Topics(), where the edge.width argument is set
##' to "absolute" (default). Similar to How.Wide.Relative and How.Wide.Success.
##' @title How.Wide
##' @param x A vector of length three, for the ith edge, indicating which node the
##' edge came from, where it is going, and its index.
##' @param A An ax4 matrix, where a = number of amendments. Each row represents an
##' amendment: its index (on of 1:a), it's committee (one of 1:c), its topic (one
##' of 1:t), and its final destination (junk or final bill: 0 or 1). See
##' See.Committee.Topics() for more details. 
##' @param num.arrows.to.topics The number of distinct edges (arrows) that are going
##' to topic nodes (the middle layer) in the See.Committee.Topics() plot.
##' @return the width of the x[3]th edge (arrow) according to the absolute number of
##' amendments represented by the given edge.
##' @author Hillary Sanders
How.Wide <- function(x,A,num.arrows.to.topics){
                if (x[3]< num.arrows.to.topics+1){
                        width<- sum((x[1]==A[,2]) & (x[2]==A[,3]))
                } else {
                        width<- sum((x[1]==A[,3]) & (x[2]==A[,4]))
                } 
                }
# End How.Wide


##' A small function called within See.Committee.Topics() if the argument
##' edge.width is set to "relative". This function creates edge (arrow) widths for 
##' See.Committee.Topics() relative to an edge's origin. Similar to How.Wide and
##' How.Wide.Success.
##' @title How.Wide.Relative
##' @param x A vector of length three, for the ith edge, indicating which node the
##' edge came from, where it is going, and its index.
##' @param A An ax4 matrix, where a = number of amendments. Each row represents an
##' amendment: its index (on of 1:a), it's committee (one of 1:c), its topic (one
##' of 1:t), and its final destination (junk or final bill: 0 or 1). See
##' See.Committee.Topics() for more details. 
##' @param num.arrows.to.topics The number of distinct edges that are going
##' to topic nodes (the middle layer) in the See.Committee.Topics() plot.
##' @return the width of the x[3]th edge, according to the percentage
##' of amenments that the the given edge is carrying with respect to the total 
##' number of amendments coming from the edge's source (either a committee or topic
##' node).
##' @author Hillary Sanders
How.Wide.Relative <- function(x,A,num.arrows.to.topics){
  if (x[3]< num.arrows.to.topics+1){
    width<- sum((x[1]==A[ ,2]) & (x[2]==A[ ,3])) / sum(x[1]==A[ ,2]) *15
    } else {
      width<- sum((x[1]==A[ ,3]) & (x[2]==A[ ,4])) / sum(x[1]==A[ ,3]) *15
      } 
  }
#End How.Wide.Relative


##' A small function called within See.Committee.Topics() if the argument
##' edge.width is set to "success". Creates edge (arrow) widths for
##' See.Committee.Topics() based on the percentage of successful (headed for
##' the final bill) amendments in the given edge.
##' See.Committee.Topics().
##' @title How.Wide.Success
##' @param x A vector of length three, for the ith edge, indicating which node the
##' edge came from, where it is going, and its index.
##' @param A An ax4 matrix, where a = number of amendments. Each row represents an
##' amendment: its index (on of 1:a), it's committee (one of 1:c), its topic (one
##' of 1:t), and its final destination (junk or final bill: 0 or 1). See
##' See.Committee.Topics() for more details. 
##' @param num.arrows.to.topics The number of distinct edges (arrows) that are
##'going to topic nodes (the middle layer) in the See.Committee.Topics() plot.
##' @return the width of the x[3]th edge (arrow), according to the % of
##' successful amendments the edge carries (with respect to all of the amendments
##' it carries).
##' @author Hillary Sanders
How.Wide.Success <- function(x,A,num.arrows.to.topics){
  
  if (x[3]< num.arrows.to.topics+1){
    width <- sum((x[1]==A[, 2]) & (x[2]==A[, 3]) & (A[, 4] == (num.com+num.top+1))) / 
      sum((x[1]==A[, 2]) & (x[2]==A[, 3])) *15
    } else {
      width <- sum((x[1]==A[, 3]) & (x[2]==A[, 4]) & (A[, 4]==(num.com+num.top+1))) /
        sum((x[1]==A[, 3]) & (x[2]==A[, 4])) *15
      }
  }
#End How.Wide.Sucess



##' The main graphing function for the legislative bill mapping package. This
##' function creates a three layer directed acyclic graph. The first, bottom 
##' layer is a set of nodes representing committees which have each submitted 
##' a number of amendments. Edges (arrows) connect these committee nodes to the
##' middle layer topic nodes to which each committee's amendment(s) pertain. 
##' Edges then connect these topic nodes to the third layer - two nodes: "Junk"
##' and "Final" - again according to the amendments that the edges represent.
##' Edge width, as well as node area, by default correspond to the number of
##' amendments they are representing.
##' @title See.Committee.Topics
##' @param model.amend.hierarchy.out The object created by model.amend.hierarchy().
##' This is used to help determine the width, color, and direction of the edges 
##' (arrows) on the graph to be created.
##' @param get.likely.composite.out The object created by GetLikelyComposite(). This
##' is used to help determine the width, color, and direction of the graph's edges.
##' @param committees The object created by model.amend.hierarchy. This
##' is used to help determine the width, color, and direction of the graph's edges.
##' @param labels An optional character vector representing the node names for each 
##' node (vertex). If NULL, the committee nodes (bottom layer) will be named with 
##' their full names, each ith topic node will be named Topic i, and the two final
##' bins will be labeled "Final", and "Junk" (for accepted and rejected amendments).
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
##' character vectors are accepted. If NULL (the default) See.Committee.Topics()
##' will use "cornflowerblue", "darkgoldenrod1", and varying shades of green to
##' respectively signify if each edge's amendments were all successfull (in
##' making it to the final bill), were all rejected, or had some combination
##' of successful and failed amendments.
##' @param main The graph's title. Defaults to NULL.
##' @param arrowhead.size The size of arrowheads (edge arrowheads). Defaults
##' to 0, i.e. no arrowhead.
##' @param layout The layout of the graph. If NULL (the default), 
##' See.Committee.Topics() will create the three layers decribed above. But users 
##' can also pass graphing algorithms (e.g. layout.fruchterman.reingold) for a
##' different layout. Note that if this is done, the plot.terms option will
##' not function correctly.
##' @param mid.layer The placement of the middle layer of the graph on the y 
##' axis. Defaults to .6. Note that the bottom and top layers are at 0 and 1.
##' Helpful if topic terms are being plotted (see plot.terms=TRUE below),
##' and space is needed.
##' @param plot.terms Logical. Should topic terms be plotted? Default = TRUE.
##' Note that See.Committee.Topics() plots the terms beneath each topic node.
##' If you want to adjust the color, size, placement, spacing, etc., of the
##' terms, set plot.terms to FALSE and use the Plot.Topic.Words() function 
##' after See.Committee.Topics, as it allows for more flexibility.
##' @return A hopefully pretty graph!
##' @author Hillary Sanders
See.Committee.Topics <- function(model.amend.hierarchy.out,get.likely.composite.out,
                                 committees,
                                 labels=NULL,
                                 edge.width.scale=1, edge.width = "absolute",
                                 scale.c=1, scale.t=1, scale.fin=1,
                                 edge.transparency=NULL, edge.col=NULL,
                                 main=NULL, arrowhead.size=0, 
                                 layout=NULL, mid.layer=.6,
                                 plot.terms=TRUE
                                 ) {
  
  # Which amendments were assigned to which topics? Create a matrix of each 
  # amendment index and their topic assignments. Note that here, the amendment
  # index corresponds to all of the amendments including those amendments 
  # (like ":") thrown out due to length. But only those not thrown out are 
  # represented.
  amend.top.index <- cbind( model.amend.hierarchy.out[[1]][[1]][[4]]
                            -min(model.amend.hierarchy.out[[1]][[1]][[4]])+1
                            , as.numeric(model.amend.hierarchy.out[[1]][[1]][[3]]))
  
  colnames(amend.top.index) <- c("idx","topic #")

  # Which amendments were successful? Look at those amendments which made it to the 
  # composite final bill, and find thier indices.
  successful<- get.likely.composite.out[ get.likely.composite.out[,3]=="amendment",2:3]
  
  unique.successful<- unique(successful) 
  # Create a nice clean matrix of the successful amendment indices. The second row
  # will become helpful in a moment!
  y <- unique.successful[order(unique.successful[,1]),]
  x<- data.frame(1:length(committees),committees)
  names(x)<-c("match.idx","committees")

  joined <- join( x, y, type="left")
  # Now, all of the elements in the third row that are <NA> (not "amendment") 
  # must be rejected amendments or amendments discarded by the computer (due to
  # their very short length), since the object successful only showed 
  # successful amendments.
  joined[,3][is.na(joined[,3])]<- 0
  joined[,3][joined[,3]=="amendment"]<- 1
  # So joined is a dataframe with three columns: amendment index, committee,
  # and a logical column indicating wether or not the amendment was accepted
  # into the final bill. But those amendments which were automatically 
  # discarded by the computer (e.g. ":") still should to be removed.

  # Since amend.top.index only shows non-discarded amendments, we can use that
  # to make sure only the non-discarded amendments are being plotted, and to 
  # add the topic assigments and committee assignments to the same object:
  merged<- merge(amend.top.idx,joined,by=1)

  # need to make committees numeric.
  # Also need to make sure that if there are any skipped indices, everything 
  # gets re-ordered properly.
  amend.committees <- as.numeric(factor(merged[,3]))
  amend.topics <- as.numeric(factor((merged[,2])))
  amend.final <- as.numeric(factor(merged[,4])) -1
  
  a <- length(amend.committees)
  
  stopifnot(length(amend.committees)==length(amend.topics),
            length(amend.topics)==length(amend.final))
  
  A <- matrix(c(1:a,amend.committees,amend.topics,amend.final),ncol=4)
  
  # num.amd = # of amendments, num.com = # of committees, num.top = # of topics.
  num.amd <- nrow(A)
  num.com <- length(unique(A[,2]))
  num.top <- length(unique(A[,3])) 
     
  # reindex. Note that igraph takes numbers starting at 0, not 1.
  # Now the last 3 columns of A represent the nodes each amendment will touch.
  A[,1:4] <- c( (A[,1]),
                (A[,2]-1),
                (A[,3]+num.com-1),
                (A[,4]+num.com+num.top))
        
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
  
  if (is.null(edge.col)){
    colors <- c("#FFB90F","#6495ED")
  # "darkgoldenrod1", "cornflowerblue" 
  # (Failure, Success)
  } else { colors <- rep(edge.col,2) [1:2]
  }
  
  if(!is.null(edge.transparency)){ 
    for (i in 1:2){
      # Extract the rgb code if color is passed as a character vector, as opposed 
      # to an rgb code. Needed in order to tack on the transparency bit.
      if( strsplit(colors[i],"")[[1]][1] != "#") {
        colors[i] <- rgb(col2rgb(colors[i])[1],
                         col2rgb(colors[i])[2],
                         col2rgb(colors[i])[3], maxColorValue=255)
        }
      }
    }
      
  # The final destination (1 or 2: junk or final) of each unique edge (arrow):
  # (So if there are 3 of the same arrow, the first one is represented.) 
  edge.color.idx <- c( (A[!duplicated(A[,2:3]),4]),
                       (A[!duplicated(A[,3:4]),4]) ) -num.com-num.top+1
  
  edge.color <- colors[edge.color.idx]
            
  # The following lines look at each comittee-to-topics arrow to be drawn. The question is: 
  # are the (perhaps multiple) amendment(s) that this arrow is representing heading to both 
  # the final bill AND junk, or just one of those two? If both, then the arrow color should 
  # be some shade of green - a combination of yellow and blue. (Those colors = default colors)
  for ( i in which(!duplicated(A[,2:3]))){
  # i.e. for each non-duplicated arrow i (each row i in matrix A which corresponds to 
  # a "new" or "unique" arrow going from comittees to topics), which ones does it match to 
  # (if it matches to any) in the set of all amendments? --> :
    identical <- c( which ( ( (A[i,2]==A[,2]) * (A[i,3]==A[,3])) ==1) )
    # Where are these identical arrows going to? (i.e. junk or final)
    destinations <- A [ identical,4]
    # If their final destinations are not all the same, then make their drawn arrow be green.
    if(length(unique(destinations))!=1) {
      
      success.rate <- mean(destinations-max(destinations)+1)
      
      lum <- 20 + (1-success.rate)*80
      shade <- hcl(110,c=100,l=lum)
      # So if success rate is high, edges will be dark green, if low, light yellow/green.         
      
      edge.color[ order((i==which(!duplicated(A[,2:3])))==0)[1]] <- shade
                  }
                }
 
    if(!is.null(edge.transparency)){ 
      for (i in 1:length(edge.color)){
        # Add a transparency number (in 00:99)
        edge.color[i] <- paste( edge.color[i], as.character(edge.transparency), sep="")
        }
      }
 
        
  ### Vertex Parameters
  # 1) Vertex label size.
  vertex.size <- rep(0,sum(num.com+num.top+2))
      
      for (i in 1:num.com){
        vertex.size[i] <- sum(A[,2]==(i-1))
        }
      for (i in (num.com+1):(num.com+num.top)) {
        vertex.size[i] <- sum(A[,3]==(i-1))
        }
      for (i in (num.com+num.top+1):(num.com+num.top+2)) {
        vertex.size[i] <- sum(A[,4]==(i-1))
        }
  # Here, both dimensions of the default rectangle vertex shape are created, and scaled
  # by how large the biggest vertex is on the graph. 
  biggest <- max(vertex.size)
  v.size <- ((sqrt(vertex.size))/(sqrt(biggest))*(60))
  v.size2 <- ((sqrt(vertex.size)/(sqrt(biggest))*(45)))
        
  # Vertex sizes can also be rescaled by the user by scale.c, scale.t, and
  # scale.fin inputs. Defaults = 1.

  v.size[1:num.com] <- v.size[1:num.com]*scale.c
  v.size[(num.com+1):(num.com+num.top)] <- v.size[(num.com+1):(num.com+num.top)]*scale.t
  v.size[(num.com+num.top+1):(num.com+num.top+2)] <- v.size[(num.com+num.top+1):(num.com+num.top+2)]*scale.fin
       
        
  # 3) The vertex labels
  if (is.null(labels)) {
    if (is.null(committees)){
      com <- 1:numcom
    } else {
      com <- levels(merged[,3])
    }
    
    topics.matrix <- model.amend.hierarchy.out[[1]][[1]][[2]]
    top <- paste( "Topic", 1:ncol(topics.matrix))
    final <- c("Junk","Final")  
    
    labels <- c(com,top,final)
        }
        
  # The actual object to be graphed:
  g. <- arrows.mat-min(arrows.mat)
  g.. <- as.numeric(t(g.))
  g <- graph(g..)
        
  ### Layout
  # To create the layout, an nx2 matrix denoting the coordinates of each x vertices, you can use
  # a function or a matrix. The default is to use the following lines to create the matrix: 
  # (where there are num.com+num.top+2 vertices, num.com of them committees, num.top of them topics, and 2 of them 
  # either the final or junk bin.)
              
  #  For the layout the matrix:
  if(is.null(layout)){
    x <- 1:(num.com+num.top+2)
    lay.mat <- t(sapply(x,FUN=Lay.See.Committee.Topics,num.com=num.com,num.top=num.top,
                        mid.layer=mid.layer,mid.layer.spread=mid.layer.spread))
    # So currently the graph is plotted on a (0,0),(1,1) screen, more or less.
    } else {
      lay.mat <- layout
      }
        
  if(is.null(main)) main <- ""
  
  # graph it!                
  plot(g,
       layout=lay.mat,
       edge.arrow.width= arrowhead.size,
       edge.width= width/10,
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
  
  if( plot.terms == TRUE){
    
    terms.list <- list()
    for (i in 1:ncol(topics.matrix)){
      terms.list[[i]] <- topics.matrix[,i]
    }
    Plot.Topic.Words(terms.list, layout=lay.mat)
      }
}
 
##' Plots a list of words next to each topic node in the graph created by this
##' package's See.Committee.Topics() function. To be used within 
##' See.Committee.Topics().
##' @title Plot.Topic.Words
##' @param words.list A list (of length num.top or less, where num.top is the
##' number of topics in your See.Committee.Topics() graph.
##' @param layout 
##' @param cex Text size, default = .5.
##' @param col Text color, default = "grey30".
##' @param pos The position of the terms to be plotted in relation to their topic
##' nodes. 1 = below, 2 = to the left of, 3 = above, 4 = to the right of.
##' Default = 1, below. (Note that if you change the positioning of the text, it
##' may be helpful to also modify the placement of the middle layer of the 
##' See.Committee.Topics graph, by using that function's mid.layer argument.)
##' @param x.offset Adjust the x axis placement of the terms. Default = 0.
##' @param y.offset Adjust the y axis placement of the terms. Default = 0.
##' @param adjust Adjust the breadth of the terms to be plotted. Default is
##' set at 2.8, which usually works well with See.Committee.Topics() graphs.
##' @param text.close How close should each term for a given topic be? Default
##' .8.
##' @return Text on plotted onto a See.Committee.Topics() graph (with default 
##' layout style).
##' @author Hillary Sanders
  Plot.Topic.Words <- function(words.list, layout,
                               cex=.5, col="grey30", pos=1,
                               x.offset=0, y.offset=-.05,
                               adjust=2.8, text.close=.8
                               ) {
        
  num.top <- length(words.list)
  x.axis <- ( (layout[ ( layout[,2] == unique (layout[,2])[2]),1] # the 2nd level
               )*adjust) - adjust*.5
  y.axis <- seq(.1,
                by=(-4/30)*cex/text.close,length=length(words.list[[i]])) +
                  y.offset

  for (i in 1:num.top){
    x <- (x.axis[i]-x.offset )  # scale
 text(x=x, y=y.axis, pos=pos, offset=x.offset,
      labels=c(words.list[[i]]), col=col,cex=cex)
    }
  }
# end Plot.Topic.Words


##' Creates the "x"th layout coordinates for See.Amends.Success(). This function
##' is called inside of See.Amends.Success() to create the layout: coordinates
##' for two layers consisting of 1) amendments (a of them), 2) the final bill 
##' (all of the paragraphs (or other text chunks) in the final bill, as well as
##' a junk bin placed in the middle of the graph.
##' @title Lay.See.Amends.Success
##' @param x the index of the coordinates to be created. 
##' @param a the number of amendments.
##' @param f the number of text chunks (generally, paragraphs) in the final bill.
##' @return the xth layout coordinates for See.Amends.Success().
##' @author Hillary Sanders
Lay.See.Amends.Success <- function(x,a,f){
  if (x<(a+1)) { cords<- c(x/(1+a),.2)
                 } else {
                   if (x>(a+f)) { cords <- c(.5,.5)
                                  } else {
                                    cords <- c( ((x-a)/(1+f)),.8) }}
  return (cords)
  }
  # End Lay.See.Amends.Success


##' Creates a two layered directed acyclic graph to visualize bill evolution.
##' Individual amendments are either connected (with an arrow) to a junk bin if the
##' amendment was not accepted into the final bill, or if it was, to its place in 
##' the final bill.
##' @title See.Amends.Success
##' @param amends A vector of all 1:a amendments' final destinations: either 0 for
##' junk (rejected amendments) or an integer i for the ith paragraph in the final bill
##' which the amendment replaced.
##' @param f Integer: the number of paragraphs (text chunks) in the final bill.
##' @param edge.width.scale Scale edge widths. Default = 1
##' @param af.shape The node shape of the amendment and final bill nodes. Possible 
##' shapes are “circle”, “square”, “csquare”, “rectangle”, “crectangle”,
##' “vrectangle” and “none”. Default = "none".
##' @param junk.shape The shape of the junk bin node. Default = "rectangle"
##' @param label.font Font type for the labels. Default = 3.
##' @param label.cex Font size for the labels. Default = .75.
##' @param labels Vector of labels for each amendments, each final bill 
##' paragraph, and the junk bin. If left null, ten (relatively) equidistant
##' nodes will be labeled by their paragraph indices for both amendments and 
##' the final bill, while the rejected amendments bin will be labeled "Junk".
##' @param main The plot title. Default = "Amendments' Destinations".
##' @return A hopefully pretty graph!
##' @author Hillary Sanders

See.Amends.Success <- function(amends,
                               f=NULL,
                               edge.width.scale=1, edge.arrow.width=.25,
                               af.shape="none", junk.shape="rectangle",
                               label.font=3,label.cex=.75, labels=NULL,
                               main="Amendments' Destinations"
                               ){
  
  if (is.null(f)){f <- max(amends)}
  a <- length(amends)
  amends.idx <- 1:a
  final.idx <- 1:f
        
  colors <- c("cornflowerblue","lightblue")
  edge.color <- colors[(amends == 0)+1]
        
  amends[amends==0] <- f+1
  # So that each amendment destined for the junk bin will go to that last,
  # (a+f+1)th, vertex, with index a+f, since igraph indices start at 0:
  
  mat<-matrix(c(amends.idx-1,0,0,amends+a-1,(f+a-1),a),ncol=2)
  g<-as.numeric(t(mat))          
  graph <- graph(g)
  # the 0 -> f+a-1 and 0 -> a are to ensure that all 1:f final paragraphs are
  # shown in the graph. They will have no color, so will be invisible. The 
  # (f+a+1)th vertex will be the junk bin, with graph index (f+a), since igraph 
  # indices start at 0.
         
  x <- (a+f+1)
  y <- 1:x
  lay.mat <- t(sapply(y,FUN=Lay.See.Amends.Success,a=a,f=f))
        
  if (is.null(labels)) {
    
    a.labeled <- floor( c( seq(1, a, length=10) ))
    f.labeled <- floor(seq(1, f, length=10) )
    
    lab.a <- rep ("",a)
    lab.b <- rep ("",f)
    
    labels <- rep("", a+f+1)
    
    for (i in a.labeled) labels[i] <- i
    for (i in f.labeled) labels[a+i] <- i
    labels[a+f+1] <- "Junk"
  }
          
  v.shape <- c(rep(af.shape,a+f),junk.shape)
  v.size <- c(rep(15,a+f),30)
        
  plot(graph, layout= lay.mat, edge.arrow.width = 3*edge.arrow.width,
       edge.width= edge.width.scale, edge.color= edge.color, vertex.shape=v.shape,
       vertex.size=v.size, vertex.label= labels, vertex.label.font=label.font,
       vertex.label.cex=label.cex, main=main)
  }
# End See.Amends.Success






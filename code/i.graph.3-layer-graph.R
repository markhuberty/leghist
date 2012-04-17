
# Error 1: Often only works properly if the function contents are pasted into the console, 
#         instead of just running the function.
# Error 2: green arrow?



        ### A few functions  to create directed acyclic graphs of committees, topics,
        ### and final/junk bins. 
        ### Line widths will represent amendment quantity, but actual amendments will not be
        ### represented in the my.graph() function. They will be in the two-layer function.
        ### The input this function takes is as follows:
#       A list (in the example below, called info), composed of:
#         
#         1) An a x 4 matrix A, where a is the number of amendments. Columns denote:
#                  1) amendnment number (1:a), 2) committee number (1:c), 3) topic number (1:t), 
#                  and 4) whether or not that amendment is junk or final bill material (0 or 1). e.g.
#
#                                      [,1] [,2] [,3] [,4]
#                                 [1,]    1    1    1    0
#                                 [2,]    2    1    1    1
#                                 [3,]    3    2    3    1
#                                 [4,]    4    3    1    0
#                                 [5,]    5    3    2    0
#                                 [6,]    6    1    3    0
# 
#         2) A vector, of length c+t+2 (# of committees + # of topics + 2 for the final/junk bins),
#            corresponding to the vertex label names. If not given, the label will default to the 
#            vertex index. If you do not want a label, "" returns a blank one.

 #library(igraph)

lay<-function(x,c,t){
         if (x<(c+1)) { cords<- c(x/(1+c),.2)
          } else {
             if (x>(c+t)) { cords<- c( (x-c-t)/3,.8)
                } else {
                        cords<- c( (x-c)/(1+t),.5)
                        }}
        return (cords)
                }

how.wide<-function(x,A,num.arrows.to.topics=num.arrows.to.topics){
                if (x[3]< num.arrows.to.topics+1){
                        width<- sum((x[1]==A[,2]) & (x[2]==A[,3]))
                } else {
                        width<- sum((x[1]==A[,3]) & (x[2]==A[,4]))
                } 
                }

graph.1<- function(info,
                   arrow.width.scale=4,
                   scale.c=1,
                   scale.t=1,
                   scale.fin=1,
                   main=NULL,
                   arrowhead.size=0,
                   layout=NULL
                   ) {
# Graph of committees to topics to final/junk. Line widths represent # of amendments being represented.
        
        # Remember that the columns of A describe the amendment index, committee number,
        # topic number, and final success for each 1:a amendments.      
        
        A<-info[[1]]
        if(!is.list(info)) A<-info
        # a for # of amendments, c for # of committees, t for number of topics.
        a<-nrow(A)
        c<-length(unique(A[,2]))
        t<-length(unique(A[,3])) 
     
        # reindex. Note that igraph takes numbers starting at 0, not 1.
        # Now the last 3 columns of A represent the nodes each amendment will touch.
        A[,1:4]<-c( (A[,1]),
                
                    (A[,2]-1),
                    (A[,3]+c-1),
                    (A[,4]+c+t))
        
        # A matrix of all of the unique arrows that need to be drawn:
        arrows.mat<- rbind( unique( A[,2:3] ), unique(A[,3:4]) )
        num.arrows.to.topics<-nrow(unique( A[,2:3]))
        num.arrows.to.jf<-nrow(unique(A[,3:4]))
        num.arrows<-nrow(arrows.mat)
        # num.arrows = the total number of arrows to be drawn

        ### Edge Parameters. (Arrows)
        # 1) Edge width
        width<- apply(cbind(arrows.mat,1:num.arrows),1,how.wide,
                      A=A,num.arrows.to.topics=num.arrows.to.topics)
        # Scale it:
        width<-ceiling(3*(arrow.width.scale*width/max(width,arrow.width.scale)))
        # So if the greatest width is more than width.scales, then 
        
         # 2) Arrow colors.
        # Note: in future, let the user define the colors, default to something like this if
        # user does not supply colors.
        # Problem. Multiple amendments may come from one committee going into the same topic. -->
        # Blend it? For now, it's just blue, yellow, and green. --> gradations in future.'

        colors<-c("cornflowerblue","darkgoldenrod1","chartreuse3")
        
        # The final destination (1 or 2: junk or final) of each unique arrow:
        # Unique = non repeating. So if there are 3 of the same arrow, here the first one 
        # is represented. # length = num.arrows = total number of arrows to be drawn.
       edge.color.idx<- c( (A[!duplicated(A[,2:3]),4]),
                    (A[!duplicated(A[,3:4]),4]) ) -c-t+1
            
         # The following lines look at each comittee-to-topics arrow to be drawn. The question is: 
         # are the (perhaps multiple) amendment(s) that this arrow is representing heading to both 
         # the final bill AND junk, or just one of those two? If both, then the arrow color should 
         # be green - a combination of yellow and blue. (Those colors = default colors)
        for ( i in which(!duplicated(A[,2:3]))){
                # i.e. for each non-duplicated arrow i (each row i in matrix A which corresponds to 
                # a "new" or "unique" arrow going from comittees to topics), which ones does it match to 
                # (if it matches to any)? in the set of all amendments:
            identical<- c( which ( ( (A[i,2]==A[,2]) * (A[i,3]==A[,3])) ==1) )
                # Where are these identical arrows going to? (i.e. junk or final)
            destinations<- A [ identical,4]
               # If their final destinations are not all the same, then make their drawn arrow be green.
            if(length(unique(destinations))!=1) {
                    # should not be identical[1] # fix
                    edge.color.idx[ order((i==which(!duplicated(A[,2:3])))==0)[1]]<- 3
                  }
                }
 
        edge.color<- colors[edge.color.idx]
        
        # Note: Vertex label colors?        
        
        ### Vertex Parameters
        # 1) Vertex label size.
        
                vertex.size<-rep(0,(c+t+2))
        
                for (i in 1:c){
                        vertex.size[i]<- sum(A[,2]==(i-1))
                        }
                for (i in (c+1):(c+t)) {
                        vertex.size[i]<- sum(A[,3]==(i-1))
                        }
                for (i in (c+t+1):(c+t+2)) {
                        vertex.size[i]<- sum(A[,4]==(i-1))
                        }
        # Here, both dimensions of the default rectangle vertex shape are created, and scaled
        # by how large the biggest vertex is on the graph. 
        biggest<-max(vertex.size)
        v.size<- ((sqrt(vertex.size))/(sqrt(biggest))*(60))
        v.size2<-((sqrt(vertex.size)/(sqrt(biggest))*(45)))
        
        # Vertex sizes can also be rescaled by the user by scale.c, scale.t, and
        # scale.fin inputs. Defaults = 1.

        v.size[1:c]<-v.size[1:c]*scale.c
        v.size[(c+1):(c+t)]<- v.size[(c+1):(c+t)]*scale.t
        v.size[(c+t+1):(c+t+2)]<-v.size[(c+t+1):(c+t+2)]*scale.fin
       
        
        # 3) The vertex labels
        if (length(info)==2) { labels<-info[[2]][1:(c+t+2)]
                } else { 
                        labels<-as.character(c(1:c,1:t,"Junk","Final"))}

        
        # The actual object to be graphed:
        g.<-arrows.mat-min(arrows.mat)
        g..<-as.numeric(t(g.))
        g<-graph(g..)
        
        ### Layout
        # To create the layout, an nx2 matrix denoting the coordinates of each x vertices, you can use
        # a function or a matrix. The default is to use the following lines to create the matrix: 
        # (where there are c+t+2 vertices, c of them committees, t of them topics, and 2 of them 
        # either the final or junk bin.)
              
        ##  For the layout the matrix:
                
        if(is.null(layout)){
        y<-(c+t+2)
        x<-1:y
        lay.mat<-t(sapply(x,FUN=lay,c=c,t=t))
        ## So currently the graph is plotted on a (0,0),(1,1) screen, more or less.
        } else {
                lay.mat<- layout(g)
                 }
        
        if(is.null(main)) main<-""
        
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
        # end graph.1 function

# Example 1.
#         A <- matrix(c(1:6,  1,1,2,3,3,1,
#                               1,2,3,1,3,2,
#                               0,1,1,0,0,0),nrow=6)
#           info<- list( A, c("c1","c2","c3","t1","t2","t3","junk","final"))
#          graph.1(info)


# Option: Add Topic Texts to graph.1 plot.
# words.list = a list of length t, where t = # of topics. Each element i corresponds 
# to the ith topic's k words. 
# layout = A layout matrix (lay.mat inside the graph.1() function.)
plot.topic.words<-function(words.list,
                      layout,
                      cex=.75,
                      col="darkgrey",
                      pos=2,
                      offset=.2,
                      adjust=3.5,
                      text.close=.75){
        
t<-length(words.list)

for (i in 1:t){
        y<- seq(.1,by=(-4/30)*cex/text.close,length=10) [ 1:length(words.list[[i]]) ]
        x<- (( (lay.mat
             [ ( lay.mat[,2] == unique (lay.mat[,2])[2]),1] # the 2nd level
             - unique(lay.mat[,2])[2])*adjust)[i]-offset )  # scale
 
        text(x=x,
             y=y,
             pos=pos,
             offset=offset,
             labels=c(words.list[[i]]),
             col=col,
             cex=cex)
                }
                }
# end plot.topic.words

## Now for a two layer graph: Amendments to final, with one node in the middle to 
## represent junk. Hmm. That's not really two layers. This needs a new name. graph.af ?


#############

lay.2<-function(x,a,f){
         if (x<(a+1)) { cords<- c(x/(1+a),.2)
          } else {
             if (x>(a+f)) { cords<- c(.5,.5)
             } else {
                     cords<- c( ((x-a)/(1+f)),.8) }}
        return (cords)
                }

# Another function to create a different sort of graph which represents bill evolution.
# This graph simply plots individual amendments to either a junk bin, or to their 
# places in the final bill. 
# amends = A vector of all 1:a amendments final destinations: either 0 for junk,
# or an integer i for the ith paragraph in the final bill.
# f = the number of paragraphs in the final bill.
graph.2<-function(amends,
                  f=NULL,
                  edge.width=3,
                  edge.arrow.width=.25,
                  af.shape="circle",
                  junk.shape="rectangle",
                  label.font=3,
                  label.cex=.75,
                  main="Amendments' Destinations"
                  ){
        if (is.null(f)){ f<-max(a)}
        a<- length(amends)
        amends.idx<- 1:a
        final.idx<- 1:f
        
        colors<- c("cornflowerblue","lightblue")
        edge.color<-colors[(amends == 0)+1]
        
        amends[amends==0]<- f+1
        
        g<-as.numeric(t(matrix(c(amends.idx-1,0,amends+a-1,(f+a-1)),ncol=2)))           
        graph<-graph(g)
        # the 0 -> f+a-1 is to ensure that all final paragraphs are shown in the
        # graph. It has no color, so is invisible.
         
        x<-(a+f+1)
        y<-1:x
        lay.mat<-t(sapply(y,FUN=lay.2layer,a=a,f=f))
        
        labels<-c(1:a,1:f,"junk")
        
        v.shape<-c(rep(af.shape,a+f),junk.shape)
        v.size<-c(rep(15,a+f),30)
        
        plot(graph,
             layout= lay.mat,
             edge.arrow.width = edge.arrow.width,
             edge.width= edge.width,
             edge.color= edge.color,
             vertex.shape=v.shape,
             vertex.size=v.size,
             vertex.label= labels,
             vertex.label.font=label.font,
             vertex.label.cex=label.cex,
             main=main
                )
        }

# example:
# amends<- c(0,8,0,1,0,0,10,18,0,7,2,15,0,20,3,0,9)
# f<-20
# graph.2(amends,f)



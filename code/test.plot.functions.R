#################################################################
## Hillary Sanders
## Automated analysis of legislative history - Testing 
## Begun: April 2012

## This file contains code to automate the testing of the leghist package.
## END
#################################################################

# load the three objects the graphing functions take as input.
library(plyr)
library(igraph)
load("~/Desktop/BRIE/ok.RData")
library(leghist)


PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees)

# input labels
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     labels=1:5) # should recycle, and spit warning messages.

# Thicker edges
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     edge.width.scale=2) 
# Default graph, edge width = absolute
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     edge.width="absolute") # should be the same 
# Edge width = success
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     edge.width="success") 
# Edge width = relative
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     edge.width="relative") 

# double the area of the bottom layer nodes.
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                      scale.c=2) 
# double the area of the mid-layer nodes.
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     scale.t=2) 
# double the area of the top layer nodes.
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     scale.fin=2) 
# Highly transparant edges
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     edge.transparency=10 )
# purple and green for junk and final
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     edge.col=c("purple","green")) 
# pink for both junk and final, no transparency.
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     edge.col="pink", edge.transparency=NULL) 
# Add title
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     main = "Yay for Random Titles")
# Make arrowheads show up, and have size 2.
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     arrowhead.size=2)
# Non-default layout.
# ?

# don't plot terms
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     plot.terms=FALSE)
# make plotted text smaller.
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     plot.terms=TRUE, terms.cex=.3)
# make plotted text red.
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     terms.col="red")
# make plotted text shift right.
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     terms.x.offset=.5)
# make plotted text shift down.
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     terms.y.offset=-.5)
# make plotted text spread out more (x axis).
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     terms.spread=2)
# make individual text lines less close together (y axis).
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     terms.text.close=1.5)
# Make labels bold
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     vertex.label.font=2)
# Make label text smaller
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     vertex.label.cex=.5)
# Make vertex nodes green
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     vertex.color="lightgreen")
# crectangle vertex node shapes
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     vertex.shape="crectangle")
# circle vertex node shapes
PlotCommitteeTopics(model.amend.hierarchy.out,get.likely.composite.out,committees,
                     vertex.shape="circle")

####################

# Default
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees)
# Committee Committee vertices and nodes
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                  edge.color.by="c", vertex.color.by="c")
# Topic Topic vertices and nodes
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                  edge.color.by="t", vertex.color.by="t")
# Committee topic vertices and nodes.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                  edge.color.by="c", vertex.color.by="t")
# Topic committee vertices and nodes
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                  edge.color.by="t",  vertex.color.by="c")
# Thicken edges
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   edge.width.scale=5)
# Input personal arrow colors.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   edge.col=c("pink","red","darkred","violetred1","violetred2",
                              "violetred3", "violetred4"))
# Input *TOO MANY* personal arrow colors. Should be the SAME as before.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   edge.col=c("pink","red","darkred","violetred1","violetred2",
                              "violetred3", "violetred4", "green"))
# Input *TOO FEW* personal arrow colors. Should issue warning, and recycle.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   edge.col=c("pink","red","darkred"))
# Input personal edge colors.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   vertex.col=c("pink","red"))
# Input *TOO MANY* personal arrow colors. Should be the SAME as before.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   vertex.col=c("pink","red","darkred","violetred1","violetred2",
                              "violetred3", "violetred4", "green"))
# Input *TOO FEW* personal arrow colors. Should issue warning, and recycle.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   edge.col="darkred")
# Increase edge widths
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   edge.width.scale=5)
# 
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   edge.col="darkred")
# increase arrowhead sizes
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   arrowhead.size=1)

# junk shape = square
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   junk.shape="square")
# increase initial/final node size.
# junk shape = "circle".
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   junk.shape="circle")
# increase junk node size.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   junk.scale=3,junk.shape="circle")
# junk node color
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                  vertex.junk.color="orange")

# make font bold/italic
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   label.font=4)
# increase font size
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   label.cex=1.05)
# user input labels. Recyles. Should produce warnings.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   vertex.label = 1:10)

# 5 visible nodes on the bottom, 15 on top.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                  a.lab=5, f.lab=15)
# Random Title
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   main = "Random Title")

# shift legend box right
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   legend.x=-1)
# shift legend box down
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   legend.y=0)
# increase legend size.
PlotAmendsSuccess(model.amend.hierarchy.out,get.likely.composite.out,committees,
                   legend.cex=1)
# end tests


                        
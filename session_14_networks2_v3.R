################################################################################
# UMD MSBA Program
# Data Visualization
# Fall 2020
# Professors Jessica Clark and Lauren Rhue
#
# Networks
################################################################################
# Load libraries and functions
library(tidyverse)
library(readxl)
library(scales)

#install.packages("GGally")
library(GGally)
library(sna)
library(network)
#ufheuofheoifeoi

###############################################################################
# KRACKHARDT
# Krackhardt studied high tech workers to understand the office dynamics
# and captured the organizational chart and the "advice network" among 
# the workers.  
#
# The file named Krackhardt_advice_edgelist.csv contains the edgelist of
# workers, where a link between worker W1 and worker W2 represents that
# W1 asks W2 for advice. 
#
# The file named Krackhardt_node_attributes.csv contains information
# about the workers, such as their age, tenure at the firm, level, and 
# department.

setwd("C:/Users/vivek_000/Desktop/Data Viz")
advice <- read.csv("Krackhardt_advice_edgelist.csv")
friendship <- read.csv("Krackhardt_friendship_edgelist.csv")
reports <- read.csv("Krackhardt_reports_edgelist.csv")
krack.attr <- read.csv("Krackhardt_node_attributes.csv")

###############################################################################
# Network measures in ggnet

# create the network with ggnet
adnet <- network(advice, directed=T)
repnet <- network(reports, directed = T)
ggnet2(repnet)
ggnet2(adnet)

# calculate the network measures
bc = betweenness(adnet)
cl = closeness(adnet)

# PLOT THE ADVICE NETWORK
# Label the person with the highest closeness centrality
closeness_label <- network.vertex.names(adnet)[which.max(cl)]
closeness_color <- rep("grey70",length(cl))
closeness_color[which.max(cl)] <- "royalblue"

adplot1 <- ggnet2(adnet,
                  label=closeness_label,
                  size=10,
                  label.color="white",
                  node.color= closeness_color,
                  label.size = 3,
                  edge.color = "grey80",
                  arrow.size = 2,
                  arrow.gap = 0.03,
                  legend.position = "none") 
adplot1

adplot1 +
  labs(title="Krackhardt Advice Network",
       subtitle = "Worker W15 is in center of the advice network for the office.",
       caption = "Source: Krackhardt office study") +
  theme(panel.border = element_rect(fill=NA,color="black"),
        plot.caption = element_text(face = "italic"))

###############################################################################
# MAKING GRAPHS WITH igraph

# Another popular networks package is igraph. 
# igraph allows for more robust network analysis, but its visualization is not
# as nice as ggnet.

# network/sna MASKS many of the functions of igraph. 
# By detaching network, those functions should not conflict with igraph
detach("package:sna", TRUE)
detach("package:network", TRUE)

library(igraph) 

# Create a graph from a data frame and include the vertex attributes
adgraph <- graph.data.frame(advice, vertices=krack.attr, directed=T)
repgraph <- graph.data.frame(reports, vertices=krack.attr, directed=T)

## Plot the networks
plot(adgraph) # default in igraph
plot(repgraph) # default in igraph

############################################################
# REPORTING NETWORK

## Get attributes
V(repgraph)$name # names of the vertices
E(repgraph) # edges of the vertices
V(repgraph)$Dept # Department of the worker

# CHANGE DEFAULT IN PLOTS
repplot_dept1 <- plot(repgraph,
                     vertex.size=25,
                     vertex.color = "royalblue",
                     vertex.frame.color='black', 		#the color of the border of the dots 
                     vertex.label.color='white',		#the color of the name labels
                     vertex.label.font=2,			#the font of the name labels
                     vertex.label=V(repgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                     vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)

# make smaller arrows, add title
repplot_dept2 <- plot(repgraph,
                     main='Krackhardt reporting network',	#specifies the title
                     edge.arrow.size=0.5,
                     vertex.size=25,
                     vertex.color = "royalblue",
                     vertex.frame.color='black', 		#the color of the border of the dots 
                     vertex.label.color='white',		#the color of the name labels
                     vertex.label.font=2,			#the font of the name labels
                     vertex.label=V(repgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                     vertex.label.cex=0.8,			#specifies the size of the font of the labels. can also be made to vary
                     edge.color = "grey80"    
)

# CHANGE THE VERTEX NODE COLOR BY ATTRIBUTES
# Change the vertex color by the Department
repplot_dept <- plot(repgraph,
                main='Krackhardt reporting network',	#specifies the title
                edge.arrow.size=0.5,
                vertex.size=25,
                vertex.color = as.factor(V(repgraph)$Dept),
                vertex.frame.color= as.factor(V(repgraph)$Dept), 		#the color of the border of the dots 
                vertex.label.color='white',		#the color of the name labels
                vertex.label.font=2,			#the font of the name labels
                vertex.label=V(repgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)

# adding the color palette
repplot_dept2 <- plot(repgraph,
                     main='Krackhardt reporting network',	#specifies the title
                     vertex.size=25,
                     vertex.color = as.factor(V(repgraph)$Dept),
                     vertex.frame.color = as.factor(V(repgraph)$Dept), 		#the color of the border of the dots 
                     palette = 'Set1',
                     vertex.label.color='white',		#the color of the name labels
                     vertex.label.dist=0.1,			#puts the name labels slightly off the dots
                     vertex.label.font=2,			#the font of the name labels
                     vertex.label=V(repgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                     edge.arrow.size=0.5,
                     vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)

# Changing the layout
repplot_layout <- plot(repgraph,
                      main='Krackhardt reporting network',	#specifies the title
                      layout=layout.circle,
                      vertex.size=25,
                      vertex.color = as.factor(V(repgraph)$Dept),
                      vertex.frame.color = as.factor(V(repgraph)$Dept), 		#the color of the border of the dots 
                      palette = 'Set1',
                      vertex.label.color='white',		#the color of the name labels
                      vertex.label.dist=0.1,			#puts the name labels slightly off the dots
                      vertex.label.font=2,			#the font of the name labels
                      vertex.label=V(repgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                      edge.arrow.size=0.5,
                      vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)

repplot_layout2 <- plot(repgraph,
                       main='Krackhardt reporting network',	#specifies the title
                       layout=layout.fruchterman.reingold,
                       vertex.size=25,
                       vertex.color = as.factor(V(repgraph)$Dept),
                       vertex.frame.color = as.factor(V(repgraph)$Dept), 		#the color of the border of the dots 
                       palette = 'Set1',
                       vertex.label.color='white',		#the color of the name labels
                       vertex.label.dist=0.1,			#puts the name labels slightly off the dots
                       vertex.label.font=2,			#the font of the name labels
                       vertex.label=V(repgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                       edge.arrow.size=0.5,
                       vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)

repplot_layout3 <- plot(repgraph,
                        main='Krackhardt reporting network',	#specifies the title
                        layout=layout.grid,
                        vertex.size=25,
                        vertex.color = as.factor(V(repgraph)$Dept),
                        vertex.frame.color = as.factor(V(repgraph)$Dept), 		#the color of the border of the dots 
                        palette = 'Set1',
                        vertex.label.color='white',		#the color of the name labels
                        vertex.label.dist=0.1,			#puts the name labels slightly off the dots
                        vertex.label.font=2,			#the font of the name labels
                        vertex.label=V(repgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                        edge.arrow.size=0.5,
                        vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)

repplot_layout4 <- plot(repgraph,
                        main='Krackhardt reporting network',	#specifies the title
                        layout=layout.davidson.harel,
                        vertex.size=25,
                        vertex.color = as.factor(V(repgraph)$Dept),
                        vertex.frame.color = as.factor(V(repgraph)$Dept), 		#the color of the border of the dots 
                        palette = 'Set1',
                        vertex.label.color='white',		#the color of the name labels
                        vertex.label.dist=0.1,			#puts the name labels slightly off the dots
                        vertex.label.font=2,			#the font of the name labels
                        vertex.label=V(repgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                        edge.arrow.size=0.5,
                        vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)

repplot_layout5 <- plot(repgraph,
                        main='Krackhardt reporting network',	#specifies the title
                        layout=layout_with_kk,
                        vertex.size=25,
                        vertex.color = as.factor(V(repgraph)$Dept),
                        vertex.frame.color = as.factor(V(repgraph)$Dept), 		#the color of the border of the dots 
                        palette = 'Set1',
                        vertex.label.color='white',		#the color of the name labels
                        vertex.label.dist=0.1,			#puts the name labels slightly off the dots
                        vertex.label.font=2,			#the font of the name labels
                        vertex.label=V(repgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                        edge.arrow.size=0.5,
                        vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)

################################################################
# TRY-IT-YOURSELF: DISCUSSION

# Describe the organizational hierarchy of this office. How would you know?
#3 level - all country - regional

# Who is the center of the hierarchy? What measure could you use?
#w7, department as color

# How deep is the hierarchy? Which measure could you use?
#graph diameter

# Who has the most direct reports? Which measure could you use?

# Consider how the layout affects the insights of the visualization. 
# Which measure makes it easiest to see the hierarchy?

# Plot the reporting network with the node color as the level.


#####################################################################
# ADVICE NETWORK
# learn about the network and compare to other networks
V(adgraph)$name # names of the vertices
E(adgraph) # edges of the vertices
V(adgraph)$Age # see the Age attribute
V(adgraph)$Dept # Department of the worker

# centrality measures
V(adgraph)$degree <- degree(adgraph) # lists the degree centrality for each node
betweenness(adgraph) # lists the betweenness centrality for each node
closeness(adgraph) # lists the closeness centrality for each node
degree.distribution(adgraph) # graph-level measure for degree distribution
diameter(adgraph) # graph diameter


## LAYOUTS
# try different layouts
pl1 <- plot(adgraph,
            layout=layout.fruchterman.reingold)

pl2 <- plot(adgraph,
            layout=layout.grid)

pl3 <- plot(adgraph,  			#the graph to be plotted
            layout= layout.circle)	# the layout method. see the igraph documentation for details

pl4 <- plot(adgraph,  			#the graph to be plotted
            layout= layout.davidson.harel)	# the layout method. see the igraph documentation for details

pl5 <- plot(adgraph,  			#the graph to be plotted
            layout= layout_with_kk)

pl6 <- plot(adgraph,  			#the graph to be plotted
            layout= layout.drl)

# Use a saved layout
saved_layout_drl <- layout_with_drl(adgraph)
pl7 <- plot(adgraph, layout=saved_layout_drl)

saved_layout_fr <- layout_with_fr(adgraph)
pl8 <- plot(adgraph, layout=saved_layout_fr)

# CHANGING THE OPTIONS
# choose the fruchterman reingold layout and change default options
plot(adgraph,
     layout=layout.fruchterman.reingold,
     main='Krackhardt advice network',	#specifies the title
     edge.arrow.size=0.5,
     vertex.size=22,
     vertex.color = "grey10",
     vertex.label.dist=0.1,			#puts the name labels slightly off the dots
     vertex.frame.color='black', 		#the color of the border of the dots 
     vertex.label.color='white',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(adgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)


# change the color by the department
adplot <- plot(adgraph,
     #layout=layout.fruchterman.reingold,
     main='Krackhardt advice network',	#specifies the title
     edge.arrow.size=0.5,
     vertex.size=22,
     vertex.color = as.factor(V(adgraph)$Dept),
     palette = 'Set1',
     vertex.label.dist=0.1,			#puts the name labels slightly off the dots
     vertex.frame.color='black', 		#the color of the border of the dots 
     vertex.label.color='white',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(adgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)



# Show network measures in the visualization
# Add the degree as the attribute
V(adgraph)$color <- scales::dscale(degree(adgraph) %>% cut(5), 
                                   sequential_pal)

V(adgraph)$color <- scales::dscale(degree(adgraph) %>% cut(5), 
                                   brewer_pal())

adplot2 <- plot(adgraph,
               main='Krackhardt advice network',	#specifies the title
               edge.arrow.size=0.5,
               vertex.size=22,
               vertex.color = V(adgraph)$color,
               vertex.frame.color='black', 		#the color of the border of the dots 
               vertex.label.color='black',		#the color of the name labels
               vertex.label.font=2,			#the font of the name labels
               vertex.label=V(adgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
               vertex.label.cex=0.8			#specifies the size of the font of the labels. can also be made to vary
)

# Change the node size by the degree
adplot3 <- plot(adgraph,
                main='Krackhardt advice network',	#specifies the title
                edge.arrow.size=0.5,
                vertex.size=as.numeric(V(adgraph)$degree)+7,
                vertex.color = V(adgraph)$color ,
                vertex.frame.color='black', 		#the color of the border of the dots 
                vertex.label.color='black',		#the color of the name labels
                vertex.label.font=2,			#the font of the name labels
                vertex.label=V(adgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                vertex.label.cex=0.7			#specifies the size of the font of the labels. can also be made to vary
)

brewer_colors <- scales::dscale(degree(adgraph) %>% cut(5), 
                                   brewer_pal(palette = 2))

# saving the graph to a file 

png("mygraph.png", height=800, width=800)

#functions to plot your graph
plot(adgraph,  			#the graph to be plotted
     #layout=layout.drl,	# the layout method. see the igraph documentation for details
     main='Krackhardt advice network',	#specifies the title
     edge.arrow.size=0.5,
     edge.color = "grey80",
     vertex.size=as.numeric(V(adgraph)$degree)+5,
     vertex.color = brewer_colors,
     vertex.frame.color=brewer_colors, 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(adgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary
)
dev.off()

# Remember to compare the saved graph to the plot in RStudio

###############################################################################
# TRY IT YOURSELF (2): FRIENDSHIP NETWORK

# Plot the friendship network, and color the nodes by degree.

# How might you compare the friendship and advice networks?

################################################################################
# EXTRA: MOSAIC PLOT 
g <- graph.data.frame(advice, directed=T, vertices=krack.attr)
indegree <- degree(g, mode="in")
indegree.bin <- cut(indegree, breaks=c(0,10,20), labels=c("Below 10", "Above 10"))
dat <- data.frame(cbind(krack.attr, indegree.bin))
mosaicplot(~indegree.bin + Dept, data=dat, color=TRUE, las=1, 
           main="Mosaic Plot", xlab="Indegree", ylab="Department")




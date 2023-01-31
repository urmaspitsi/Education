library(igraph)

g <- graph.tree(20, 2)
plot(g)
plot(g, layout=layout.circle)

plot(g, layout=layout.fruchterman.reingold)
plot(g, layout=layout.graphopt)
plot(g, layout=layout.kamada.kawai)

#set/get.graph/vertex/edge.attribute
#V(g).attribute <- "color"
#set_vertex_attr("name", value = letters[1:10])

V(g)$color <- "white"
V(g)[1]$color <- "white"
V(g)[6:10]$color <- "yellow"
plot(g)

V(g)[color=="white"] # gets white vertices

E(g)$color <- "grey"
E(g)[6:10]$color <- "red"
plot(g)

e <- E(g)
W <- get.adjacency(g) # , attr="weight"

# Example graph.
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T) # Edges.
links <- aggregate(links[,3], links[,-3], sum)
links_aggr <- aggregate(links[,3], links[,-3], sum) # Aggregate by weight column: +3 includes, -3 excludes the 3rd column.

links[,-3][1:3,]

links <- links[order(links$from, links$to),]
links_ordered <- links[order(links$from, links$to),] # Sort links by (from, to).

colnames(links)
colnames(links)[4] <- "weight"
rownames(links)
rownames(links) <- NULL

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)

#plot(net, edge.arrow.size=.4,vertex.label=NA)

net <- simplify(net, remove.multiple = F, remove.loops = T) 

#plot(net, edge.arrow.size=.2, edge.curved=0,
#     vertex.color="orange", vertex.frame.color="#555555",
#     vertex.label=V(net)$media, vertex.label.color="black",
#     vertex.label.cex=.7) 

net.sym <- as.undirected(net, mode= "collapse", edge.attr.comb=list(weight="sum", "ignore"))

vcol <- rep("grey80", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"
plot(as.undirected(net.sym), vertex.label=V(net.sym)$name, vertex.color=vcol)

ceb <- cluster_edge_betweenness(net) 
dendPlot(ceb, mode="hclust")
plot(ceb, net)

summ <- summary(net)

vcount(net)
ecount(net)


# Graph from edges.
g2 <- graph(edges=c(1,2,2,3,3,1,4,9), n=10 ,directed=F)
plot(g2)

# Small Graphs interface.
plot(graph_from_literal(a--+b, b--+c, c--+a))

# Tree graph.
tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size=10, vertex.label=NA) 



#-------------------------------------------------------------------------------------------
# PLOTTING. Source: https://kateto.net/networks-r-igraph
#-------------------------------------------------------------------------------------------

?par
plot(x=1:10, y=rep(5,10), pch=19, cex=3, col="dark red")
points(x=1:10, y=rep(6, 10), pch=19, cex=3, col="557799")
points(x=1:10, y=rep(4, 10), pch=19, cex=3, col=rgb(.25, .5, .3))
plot(x=1:5, y=rep(5,5), pch=19, cex=20, col=rgb(.25, .5, .3, alpha=.3), xlim=c(0,6))


pal1 <- heat.colors(5, alpha=1)   #  5 colors from the heat palette, opaque
pal2 <- rainbow(5, alpha=.5)      #  5 colors from the heat palette, transparent
plot(x=1:10, y=1:10, pch=19, cex=5, col=pal1)


palf <- colorRampPalette(c("gray80", "dark red"))
plot(x=10:1, y=1:10, pch=19, cex=5, col=palf(10)) 


palf <- colorRampPalette(c(rgb(1,1,1, .2),rgb(.8,0,0, .7)), alpha=TRUE)
plot(x=10:1, y=1:10, pch=19, cex=5, col=palf(10))


#-------------------------------------------------------------------------------------------
# IGRAPH. Source: https://kateto.net/networks-r-igraph
#-------------------------------------------------------------------------------------------

g2 <- graph(edges=c(1,2, 2,3, 3,1), n=10, directed = FALSE) # directed by default.
plot(g2)

g2 <- graph(edges=c(1,2, 2,3, 3,1), n=10) # directed by default.
plot(g2)

# In named graphs we can specify isolates by providing a list of their names.
g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jack", "Jim", "John", "John"),
             isolates=c("Jesse", "Janis", "Jennifer", "Justin") )  

plot(g4, edge.arrow.size=.5, vertex.color="gold", vertex.size=15,
     vertex.frame.color="gray", vertex.label.color="black",
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2) 

plot(graph_from_literal(a+-b, b-+c, c+a, a-d)) # + direction, - no edge.
plot(graph_from_literal(a:b:c-c:d:e))
plot(graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j))


#-------------------------------------------------------------------------------------------
# Explore sample: Media-Example-NODES.csv. Source: https://kateto.net/networks-r-igraph
# http://www.kateto.net/wordpress/wp-content/uploads/2016/01/netscix2016.zip
#-------------------------------------------------------------------------------------------
# Example graph.
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T) # Edges.

head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

# "Notice that there are more links than unique from-to combinations.
#  That means we have cases in the data where there are multiple links between the same two nodes.
#  We will collapse all links of the same type between the same two nodes by summing their weights,
#  using aggregate() by 'from', 'to', & 'type'. We donÿt use simplify() here so as not to collapse
#  different link types."

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)

net

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"

plot(net, edge.arrow.size=.4,vertex.label=NA)
plot(net, edge.arrow.size=.4,vertex.label=NA, vertex.size=20) # Default vertex.size=15.
plot(net, edge.arrow.size=.4,vertex.label=NA, vertex.size=(1:length(V(net))))
plot(net, edge.arrow.size=.4,vertex.label=V(net)) # Labels as vertices.
plot(net, edge.arrow.size=.4,vertex.label=(1:length(V(net)))) # Labels as vertice indices.
plot(net, edge.arrow.size=.4,vertex.label=NA, edge.curved=.3) # Curved edges.

net <- simplify(net, remove.multiple = F, remove.loops = T)
plot(net, edge.arrow.size=.4,vertex.label=NA)

edge_list <- as_edgelist(net, names=T)
adj_matrix <- as_adjacency_matrix(net, attr="weight")

edge_df <- as_data_frame(net, what="edges")
vertice_df <- as_data_frame(net, what="vertices")

?igraph.plotting

# Alternative 1. for graph settings. 
# Set edge color to gray, and the node color to orange. 
# Replace the vertex label with the node names stored in "media"

plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black", vertex.label.cex=.7) 


# Alternative 2. for graph settings. 
# The second way to set attributes is to add them to the igraph object.
# Letÿs say we want to color our network nodes based on type of media,
# and size them based on audience size (larger audience -> larger node).
# We will also change the width of the edges based on their weight.

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Set node size based on audience size:
V(net)$size <- V(net)$audience.size * 0.7

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label.color <- "black"
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight / 6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight / 12

plot(net)
plot(net, edge.color="orange", vertex.color="gray50")

# Add legend.
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40", vertex.label.cex=.7, edge.color="gray85")


# Color the edges based on their source node color.
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]
plot(net, edge.color=edge.col, edge.curved=.1)

#-------------------------------------------------------------------------------------------
# Graph Layouts. Source: https://kateto.net/networks-r-igraph
#-------------------------------------------------------------------------------------------

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(4,5), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }








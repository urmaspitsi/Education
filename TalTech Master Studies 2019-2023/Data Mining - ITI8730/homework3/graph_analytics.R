# Resources:
# https://kateto.net/networks-r-igraph
# https://kateto.net/polnet2018
# https://github.com/kateto/R-igraph-Network-Workshop-NetSciX/blob/master/NetSciX%202016%20Workshop.R
# http://statmath.wu.ac.at/research/friday/resources_WS0708_SS08/igraph.pdf

library(igraph)

# source: https://kateto.net/networks-r-igraph
nodes <- read.csv("C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment3/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment3/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T) # Edges

head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

# Notice that there are more links than unique from-to combinations.
# That means we have cases in the data where there are multiple links between the same two nodes.
# We will collapse all links of the same type between the same two nodes by summing their weights,
# using aggregate() by 'from', 'to', & 'type'. We donÿt use simplify() here so as not to collapse.
# different link types.
links <- aggregate(links[,3], links[,-3], sum) # Aggregate
links <- links[order(links$from, links$to),] # Sort

colnames(links)[4] <- "weight"
rownames(links) <- NULL

#---------------------------------------------------------------------------------
# Setup Graph g for further analysis.
#---------------------------------------------------------------------------------
g <- graph_from_data_frame(d=links, vertices=nodes, directed=T) # directed
g <- graph_from_data_frame(d=links, vertices=nodes, directed=F) # undirected
g <- simplify(g, remove.multiple=T, remove.loops=T)
par(mar=c(0.5,0.5,0.5,0.5))
plot(g, layout=layout_with_lgl)


##################################################################################
# Exercise 1: Subtasks 1-13.
##################################################################################

#---------------------------------------------------------------------------------
# 1. Local Clustering Coefficient.
#---------------------------------------------------------------------------------
# Definition from Aggrawal p.621, 628:
# The local clustering coefficient n(i) of node i is the fraction of
# these pairs that have an edge between them.
# Implementation for undirected networks.
# Loc Clust Coeff = Num edges betw connected nodes / num connected nodes-choose-2
#---------------------------------------------------------------------------------

local_clustering_coefficient <- function(g, node_id){
  # g: igraph, i: node id
  neibs <- neighbors(g, node_id, mode="all")
  num_neibs <- length(neibs)
  if (num_neibs > 1){
    adj_matrix <- as_adj(g, type="upper", attr=NULL, edges=FALSE, names=TRUE, sparse=FALSE) # igraph_opt("sparsematrices")
    edges_between_neighbors <- sum(adj_matrix[neibs,neibs])
    return(edges_between_neighbors / choose(num_neibs, 2))
  } else { return(0) }
}

#---------------------------------------------------------------------------------
# 2. Degree Centrality.
# Definition from Aggrawal p.624:
# The Degree Centrality Cd(i) of a node i of an undirected network is equal to the
# degree of the node, divided by the maximum possible degree of the nodes.
# Degree Centrality = Degree(i) / (Number of nodes - 1)
#---------------------------------------------------------------------------------

degree_centrality <- function(g, node_id){
  # g: igraph, i: node id
  num_nodes <- length(V(g)) - 1
  if (num_nodes > 0){ return(degree(g, v=node_id) / num_nodes) } else { return(0) }
}

#degree_centrality(g, 16)

#---------------------------------------------------------------------------------
# 3. Degree Prestige.
# Definition from Aggrawal p.624:
# Degree Prestige is defined for directed networks only: uses indegree of the node,
# rather than its degree.
# Degree Prestige = Indegree(i) / (Number of nodes - 1)
#---------------------------------------------------------------------------------

degree_prestige <- function(g, node_id){
  # g: igraph, i: node id
  num_nodes <- length(V(g)) - 1
  if (num_nodes > 0){ return(degree(g, v=node_id, mode="in") / num_nodes) } else { return(0) }
}

#degree_prestige(g, 7)

#---------------------------------------------------------------------------------
# 4. Gregariousness of a node.
# Definition from Aggrawal p.624:
# Degree Prestige is defined for directed networks only: uses outdegree of the node,
# rather than its degree.
# Node Gregariousness = Outdegree(i) / (Number of nodes - 1)
#---------------------------------------------------------------------------------

node_gregariousness <- function(g, node_id){
  # g: igraph, i: node id
  num_nodes <- length(V(g)) - 1
  if (num_nodes > 0){ return(degree(g, v=node_id, mode="out") / num_nodes) } else { return(0) }
}

#node_gregariousness(g, 16)

#---------------------------------------------------------------------------------
# 5. Closeness Centrality and Proximity Prestige.
#---------------------------------------------------------------------------------
# 5.1. Closeness Centrality.
# Definition from Aggrawal p.624:
# The notion of Closeness Centrality is meaningfully defined with respect to undirected
# and connected networks.
# Average Distance(i) = sum(Dist(i,j)), j->1..n / (n - 1)
# The Closeness Centrality is the inverse of the average distance of other nodes to i.
#---------------------------------------------------------------------------------

closeness_centrality <- function(g, node_id, use_weights=FALSE){
  # g: igraph, i: node id
  w <- if (use_weights) { NULL } else { NA }
  sp <- shortest_paths(g, node_id, weights=w)
  n <- length(sp$vpath)
  # Exclude node itself.
  if (n > 1){
    res <- 0.00001 # Some epsilon to avoid div/0.
    for (i in 1:n) { res <- res + (length(sp$vpath[[i]]) - 1) }
    return((n - 1) / res)
  } else { return(0) }
}

#closeness_centrality(g, 3)

#---------------------------------------------------------------------------------
# 5.2. Proximity Prestige.
# Definition from Aggrawal p.625:
# Proximity Prestige can be used to measure prestige in directed networks.
# Shotest path distance from all other nodes to node i. Path may not exist!
# Average Distance(i) = sum(Dist(i,j)), j in Influencers / num Influencers.
# Influencers are nodes that can reach i, there exists a path from node to i.
#---------------------------------------------------------------------------------

proximity_prestige <- function(g, node_id, use_weights=FALSE){
  # g: igraph, i: node id
  w <- if (use_weights) { NULL } else { NA }
  avg_dist <- 0.00001 # Epsilon to avoid Div/0.
  num_influencers <- 0
  for (i in V(g)){
    if (i != node_id && degree(g, v=i, mode="out") > 0) {
      s <- shortest_paths(g, i, to=node_id, mode="out", weights=w)[1]
      l = length(s$vpath[[1]]) - 1
      if (l > 0) {
        num_influencers <- num_influencers + 1
        avg_dist <- avg_dist + l
      }
    }
  }
  if (num_influencers > 0) {
    avg_dist <- avg_dist / num_influencers
    influence_frac <- num_influencers / (length(V(g)) - 1)
    return(influence_frac / avg_dist)
  } else { return(0) }
}

#proximity_prestige(g, 16)

#---------------------------------------------------------------------------------
# 6. Betweenness Centrality.
# Definition from Aggrawal p.626:
# Betweenness Centrality = Fraction of shortest paths that pass through node i.
# Betweenness Centrality = sum(num shortest j->i->k / num shortest j->k) / n-choose-2
#---------------------------------------------------------------------------------

# TODO!!! convert result to correct format.
betweenness_centrality_all <- function(g, use_weights=FALSE){
  # g: igraph, i: node id
  w <- if (use_weights) { NULL } else { NA }
  num_nodes <- length(V(g))
  res <- matrix(0, nrow=num_nodes, ncol=1) # result: shortest path counter for each vertice.
  #res <- rep(0, num_nodes) # result: shortest path counter for each vertice.
  for (i in 1:(num_nodes - 1)) {
    for (j in (i + 1):num_nodes) {
      nodes_to <- V(g)[V(g) >= j]
      all_shorts <- all_shortest_paths(g, i, to=nodes_to, mode="all", weights=w)[1]$res
      for (p in all_shorts) {
        if (length(p) > 2) {
          # Path must be at least 3 nodes. Then exclude first and last node.
          for (k in p[2:(length(p)-1)]) { res[k,1] <- res[k,1] + 1 }
        }
      }
    }
  }
  return(res)
}

#betweenness_centrality_all(g)

betweenness_centrality_one <- function(g, node_id, use_weights=FALSE){
  return(betweenness_centrality_all(g, use_weights=use_weights)[node_id])
}

#betweenness_centrality_one(g, 1)

#betweenness(g, v=V(g), directed=FALSE, weights=NA)

#---------------------------------------------------------------------------------
# 7. Common neighbor based measure.
# Definition from Aggrawal p.651:
# Common-Neighbor measure between nodes i and j = num common neighbors between i and j.
#---------------------------------------------------------------------------------

common_neighbor_measure <- function(g, i, j){
  # g: igraph, i, j: node id-s
  neibs_i <- neighbors(g, i, mode="total")
  neibs_j <- neighbors(g, j, mode="total")
  return(intersect(neibs_i, neibs_j))
}

#common_neighbor_measure(g, 6, 17)

#---------------------------------------------------------------------------------
# 8. Jaccard measure.
# Definition from Aggrawal p.651:
# Intersection over Union.
#---------------------------------------------------------------------------------

jaccard_measure <- function(g, i, j){
  # g: igraph, i, j: node id-s
  neibs_i <- neighbors(g, i, mode="total")
  neibs_j <- neighbors(g, j, mode="total")
  #print(intersect(neibs_i, neibs_j))
  #print(base::union(neibs_i, neibs_j))
  u <- length(base::union(neibs_i, neibs_j))
  if (u > 0) { return(length(intersect(neibs_i, neibs_j)) / u) } else { return(0) }
}

#jaccard_measure(g, 3, 4)

#---------------------------------------------------------------------------------
# 9. Morgan index.
# Definition from Aggrawal p.572:
# Node-specific index, equal to k-th order degree of a node:
# The number of nodes reachable from the node within a distance of k.
#---------------------------------------------------------------------------------
morgan_index <- function(g, node_id, k=2){
  # g: igraph, node_id: node id, k: max distance / k-neighborhood.
  neibs <- neighborhood(g, order=k, nodes=node_id, mode="all")
  return(length(neibs[[1]]))
}

#morgan_index(g, 16, 2)

#---------------------------------------------------------------------------------
# 10. Wiener index.
# Definition from Aggrawal p.572:
# Wiener Index is equal to the sum of the pairwise shortest path distances between
# all pairs of nodes.
# W(G) = sum(shortest_dist(i,j))
#---------------------------------------------------------------------------------

wiener_index <- function(g, use_weights=FALSE){
  w <- if (use_weights) { NULL } else { NA }
  d <- distances(g, mode="all", weights=w) #, algorithm="automatic")
  return(sum(d) / 2) # Divide by 2 because all distances are accounted twice.
}

#wiener_index(g, use_weights=FALSE)

#---------------------------------------------------------------------------------
# 11. Hosoya index (other name: Z-index).
# Definition from Aggrawal p.572:
# Hosoya Index = number of valid pairwise node matchings in the graph.
# Hosoya index is also referred to as Z-index.
#---------------------------------------------------------------------------------

# TODO!!!


#---------------------------------------------------------------------------------
# 12. Estrada index.
#---------------------------------------------------------------------------------
# Definition from Aggrawal p.572:
# Useful in chemical applications for measuring the degree of protein folding.
# E(G) = sum(e^eig(i)), where eig(i) = eigenvalues to the adjacency matrix.

estrada_index <- function(g){
  adj_matrix <- as_adj(g, type="both", attr=NULL, edges=FALSE, names=TRUE, sparse=FALSE)
  eigs <- eigen(adj_matrix, TRUE, only.values=TRUE)[1]
  res <- 0.0
  for (i in eigs$values) {
    res <- res + exp(i)
  }
  return(res)
}

#estr <- estrada_index(g)

#---------------------------------------------------------------------------------
# 13. Circuit rank.
# Definition from Aggrawal p.573:
# Circuit rank C(G) is equal to the minimum number of edges that need to be
# removed from a graph in order to remove all cycles.
# For a graph with m edges, n nodes and k connected components, this number is:
# C(G) = m - n + k
#---------------------------------------------------------------------------------

circuit_rank <- function(g, is_directed_graph=FALSE){
  num_edges <- length(E(g, directed=is_directed_graph))
  num_nodes <- length(V(g))
  num_components <- count_components(g, mode="weak")
  return(num_edges - num_nodes + num_components)
}

#circuit_rank(g)

#---------------------------------------------------------------------------------
# Helper functions to run over multiple nodes.
#---------------------------------------------------------------------------------

run_on_multiple_nodes <- function(g, func_to_run, node_ids=NULL){
  # Returns vector lenght = number of nodes in the graph g.
  # Applies func_to_run on all nodes or on node_ids that are provided.
  # node_ids: 1-d vector or list.
  selected_nodes <- if (is.null(node_ids)) { V(g) } else { node_ids }
  #res <- rep(0, length(selected_nodes))
  res <- matrix(0, nrow=length(selected_nodes), ncol=1)
  for (i in 1:length(selected_nodes)) {
    res[i,1] <- func_to_run(g, selected_nodes[i])
  }
  return(res)
}

node_analytics <- function(g, funcs, col_names, node_ids=NULL) {
  # Returns matrix (nrows=num_of_nodes, ncol=?).
  # In columns all graph analytics that can be calculated per node.
  num_rows <- if (is.null(node_ids)) { length(V(g)) } else { length(node_ids) }
  res <- matrix(0, nrow=num_rows, ncol=length(funcs))
  colnames(res) <- col_names
  i <- 1
  for (f in funcs) {
    print(col_names[i])
    if (col_names[i] == "betweenness centrality") {
      res[, i] <- betweenness_centrality_all(g)[,1]
    }
    else {
      res[, i] <- run_on_multiple_nodes(g, f, node_ids=node_ids)[,1]
    }
    i <- i + 1
  }
  return(res)
}

node_analytics_undirected <- function(g, node_ids=NULL) {
  # For undirected graphs.
  # Returns matrix (nrows=num_of_nodes, ncol=?).
  # In columns all graph analytics that can be calculated per node.
  funcs <- c(local_clustering_coefficient, degree_centrality, closeness_centrality, betweenness_centrality_all, morgan_index)
  col_names <- c("local clustering", "degree centrality", "closeness centrality", "betweenness centrality", "morgan index@2")
  return(node_analytics(g, funcs, col_names, node_ids))
}

node_analytics_directed <- function(g, node_ids=NULL) {
  # For directed graphs.
  # Returns matrix (nrows=num_of_nodes, ncol=?).
  # In columns all graph analytics that can be calculated per node.
  #funcs <- c(local_clustering_coefficient, degree_prestige, node_gregariousness, proximity_prestige, morgan_index)
  #col_names <- c("local clustering", "degree prestige", "gregariousness", "prox prestige", "morgan index@2")
  funcs <- c(degree_prestige, node_gregariousness, proximity_prestige)
  col_names <- c("degree prestige", "gregariousness", "prox prestige")
  return(node_analytics(g, funcs, col_names, node_ids))
}

graph_aggregate_analytics <- function(g) {
  # Returns matrix (nrows=1, ncol=?).
  # In columns all graph analytics.
  funcs <- c(wiener_index, estrada_index, circuit_rank)
  col_names <- c("wiener index", "estrada index", "circuit rank")
  res <- matrix(0, nrow=1, ncol=length(funcs))
  colnames(res) <- col_names
  i <- 1
  for (f in funcs) {
    print(col_names[i])
    res[1, i] <- f(g)
    i <- i + 1
  }
  return(res)
}

##################################################################################
# Exercise2: Maximal Common Subgraph.
##################################################################################
# Task description:
# Using igraph functions 'cliques' and 'isomorphism',
# develop your own code for finding maximal common subgraph. 
#
# Reference from Aggrawal p.86; ch.17.2, p.559., p.562 Ullman's algorithm.
# Graph isomorphism and Cliques.
#---------------------------------------------------------------------------------

# Create some sample graphs
# Graph from edges.
e1 <- c(1,3,2,3,3,4,3,6,3,7,5,8,5,9,5,10,5,11,6,8,8,9,8,10,8,11,9,10,9,11,10,11)
g1 <- graph(edges=e1, n=length(unique(e1)), directed=F)
plot(g1, layout=layout_with_lgl)
plot(g1, layout=layout_on_grid, edge.curved=0.3)

e1a <- c(1,3,2,3,3,4,3,6,3,7,5,8,5,9,5,10,5,11,6,8,8,9,8,10,8,11,9,10,9,11,10,11,11,12)
g1a <- graph(edges=e1a, n=length(unique(e1a)), directed=F)
plot(g1a, layout=layout_with_lgl)

btw_cent1 <- betweenness_centrality_all(g1); btw_cent1

e2 <- c(1,3,2,3,3,4,3,6,3,7,5,8,5,11,6,8,8,9,9,10,10,11)
g2 <- graph(edges=e2, n=length(unique(e2)), directed=F)
plot(g2, layout=layout_with_lgl)

btw_cent2 <- betweenness_centrality_all(g2); btw_cent2

e3 <- c(e1, (max(e1) + e2), c(6, 6 + max(e1)))
g3 <- graph(edges=e3, n=length(unique(e3)), directed=F)
plot(g3, layout=layout_with_lgl)

# Graph for Local Clustering coeff: copy of lecture 12, page 5.
e4 <- c(1,2,1,3,1,4,1,5,1,6,1,7,1,8,1,9,1,10,2,3,4,5,6,11,7,8,9,10,11,12,12,14,13,14,13,15,13,16,13,17,14,15,14,16,14,17,15,16,15,17,16,17)
g4 <- graph(edges=e4, n=length(unique(e4)), directed=F)
plot(g4, layout=layout_with_lgl)

# Degree, Closeness, Betweenness Centrality: copy of lecture 12, page 15.
e5 <- c(1,2,1,6,1,7,1,8,1,9,1,10,1,11,2,3,3,4,4,5,4,12,4,16,4,17,5,13,5,14,5,15)
g5 <- graph(edges=e5, n=length(unique(e5)), directed=F)
plot(g5, layout=layout_with_lgl)

# Directed graph: Prestige, Gregarious.
e6 <- c(1,2,1,3,1,4,1,5,1,6,2,6,3,4,3,6,4,5,4,6,5,6)
g6 <- graph(edges=e6, n=length(unique(e6)), directed=T)
plot(g6, layout=layout_with_lgl)

# Almost Complete graph.
e7 <- c(1,2,1,3,1,4,1,5,1,6,2,3,2,4,2,5,2,6,3,4,3,5,3,6,4,5,4,6,5,6)
g7 <- graph(edges=e7, n=length(unique(e7)), directed=F)
plot(g7, layout=layout_with_lgl)


analytics1_directed <- node_analytics_directed(g1)
analytics2_directed <- node_analytics_directed(g2)
analytics6_directed <- node_analytics_directed(g6)

analytics1_undirected <- node_analytics_undirected(g1)
analytics2_undirected <- node_analytics_undirected(g2)
analytics4_undirected <- node_analytics_undirected(g4)
analytics5_undirected <- node_analytics_undirected(g5)

analytics_aggregate <- graph_aggregate_analytics(g)
analytics5_aggregate <- graph_aggregate_analytics(g5)
analytics7_aggregate <- graph_aggregate_analytics(g7)

g1$name <- "Query graph"
g2$name <- "Graph 2"
g3$name <- "Data graph"

##################################################################################
# Maximum Common Subgraph.
##################################################################################

largest_subgraph_isomorph <- function(query_graph, data_graph) {
  sub_iso <- subgraph_isomorphisms(query_graph, data_graph)
  if (length(sub_iso) < 1) { return(list()) }
  return(sub_iso[[1]])
  res <- sub_iso[[1]]
  largest_so_far <- length(res)
    for (i in 1:length(sub_iso)) {
    l <- sub_iso[[i]]
    if (l > largest_so_far) {
      res <- sub_iso[[i]]
      largest_so_far <- l
    }
  }
  return(res)
}

max_common_sub_vertices <- largest_subgraph_isomorph(g1, g3)

plot(g1, layout=layout_with_lgl)
plot(g3, layout=layout_with_lgl)


full8 <- make_full_graph(8)
plot(full8, layout=layout_in_circle)

full5 <- make_full_graph(5)
plot(full5, layout=layout_in_circle)

full4 <- make_full_graph(4)
plot(full4, layout=layout_in_circle)

g1 <- full5
g3 <- full8

max_common_sub_vertices <- largest_subgraph_isomorph(g1, g3)

V(g1)$color <- "grey"
V(g3)$color <- "orange"
V(g3)[max_common_sub_vertices]$color <- "grey"

graphs <- list(g1, g3)
par(mfrow=c(1,2), mar=c(1,1,1,1))
i <- 1
for (gr in graphs) {
  title <- "" #gr$name #paste("Graph", 1, sep=" ")
  plot(gr, layout=layout_in_circle, main=title, edge.arrow.mode=0,
       vertex.size=20, vertex.label.color="black", vertex.label.cex=1)
  i <- i + 1
}

##################################################################################
# Maximum Common Subgraph continues.
##################################################################################
set.seed(17)

connect(g2, 3)
connect.neighborhood(g2, 3)
neighborhood(g2, 3, nodes=11)

# sample nodes from g1 where degree in degrees of g2
# find k-neighbourhood of a node i: degree < than max degree in g2
# take m-vertices
# try to find match in other graph
# sample from g2: degrees need to match with sample.

degrees_in_1 <- unique(degree(g1))
degrees_in_2 <- unique(degree(g2))
is_valid_degree_in_g1 <- function(v) { length(intersect(degree(g1,v), degrees_in_2)) }
is_valid_degree_in_g2 <- function(v) { length(intersect(degree(g2,v), degrees_in_1)) }
candidates_in_g1 <- Filter(is_valid_degree_in_g1, V(g1))
candidates_in_g2 <- Filter(is_valid_degree_in_g2, V(g2))
create_component <- function(g, node_id, num_steps, acc, loop_counter) {
  if (length(acc) >= num_steps || loop_counter > 100) { return(acc) }
  else {
    i <- sample(1:length(acc), 1)
    next_id <- sample(neighbors(g, acc[i]),1)
    res <- unique(append(acc, next_id))
    return(create_component(g, next_id, num_steps, res, loop_counter + 1))
  }
}
create_neibhoods <- function(g, k, node_ids, min_len, num_rounds=1) {
  res <- list()
  for (n in 1:num_rounds){
    for (i in node_ids) {
      nh <- create_component(g, i, k, c(i), 0)
      if (length(nh) >= min_len) { res <- base::union(list(nh), res) }
    }
  }
  return(res)
}
sample_from_g2 <- function(g2, comp_from_g1, num_rounds=1) {
  k <- length(comp_from_g1)
  comp_g2 <- create_neibhoods(g2, k, candidates_in_g2, k, num_rounds=num_rounds)
  return(comp_g2)
}

comps_from_g1 <- create_neibhoods(g1, k=8, node_ids=c(1,3,6,8), min_len=3, num_rounds=3)
comps_from_g2 <- sample_from_g2(g2, comps_from_g1[[1]], num_rounds=1)
is_possible_match <- function(c1, c2, g1, g2) {
  deg1 <- degree(g1, c1)
  deg2 <- degree(g2, c2)
  #if (sum(deg1) != sum(deg2)) { return(FALSE) }
  return(isomorphic(induced_subgraph(g1, c1), induced_subgraph(g2, c2)))
  
  if (!setequal(deg1, deg2)) { return(FALSE) }
  else { return(TRUE) }
}
comps_g2 <- function(g1, g2, comps_from_1, num_rounds=1) {
  res <- list()
  for (c1 in comps_from_1) {
    comps_g2 <- sample_from_g2(g2, c1, num_rounds=num_rounds)
    for (c2 in comps_g2) {
      if (is_possible_match(c1, c2, g1, g2)) {
        res <- append(res, list(c1))
        res <- append(res, list(c2))
        return(res)
      }
    }
  }
  return(res)
}

sub_graph_candidates <- comps_g2(g1, g3, comps_from_1=comps_from_g1, num_rounds=1)
sub_graph_candidates <- comps_g2(g1, g3, comps_from_1=list(candidates_in_g1), num_rounds=1)
sub_graph_g1 <- sub_graph_candidates[[1]]
sub_graph_g2<- sub_graph_candidates[[2]]

V(g1)$color <- "orange"
V(g3)$color <- "orange"
V(g1)[sub_graph_g2]$color <- "grey"
V(g3)[sub_graph_g2]$color <- "grey"

graphs <- list(g1, g3)
par(mfrow=c(1,2), mar=c(1,1,1,1))
i <- 1
for (gr in graphs) {
  title <- "" #gr$name #paste("Graph", 1, sep=" ")
  plot(gr, layout=layout_with_lgl, main=title, edge.arrow.mode=0,
       vertex.size=20, vertex.label.color="black", vertex.label.cex=1)
  i <- i + 1
}

induced_subgraph(g3, c(1:11))
isomorphic(g1, induced_subgraph(g3, c(1:11)))

##################################################################################
# ...
##################################################################################

coeff_name <- "local clustering" # undirected: "local clustering" "degree centrality" "closeness centrality" "morgan index@2"
coeff_name <- "local clustering" # directed: "degree prestige" "gregariousness" "morgan index@2"
graphs <- list(g1, g2)
anals <- list(analytics1_undirected, analytics2_undirected)
anals <- list(analytics1_directed, analytics2_directed)
par(mfrow=c(1,2), mar=c(1,1,1,1))
i <- 1
for (gr in graphs) {
  title <- gr$name #paste("Graph", 1, sep=" ")
  vertex_labels <- NULL # round(anals[[i]][, coeff_name],2)
  plot(gr, layout=layout_with_lgl, main=title,
       edge.arrow.mode=0,
       vertex.size=12, vertex.label=vertex_labels, vertex.label.dist=2.5,
       vertex.label.color="black", vertex.label.cex=1.2)
  i <- i + 1
}

coeff_name <- "gregariousness" # "betweenness centrality" "degree centrality" "local clustering" "degree prestige" "gregariousness" "morgan index@2"
gr <- g6
anals <- analytics6_directed
title <- "" #gr$name #paste("Graph", 1, sep=" ")
vertex_labels <- round(anals[, coeff_name],2)
par(mfrow=c(1,1), mar=c(1,1,1,1))
plot(gr, layout=layout_with_lgl, main=title,
     edge.arrow.mode=2,
     vertex.size=11, vertex.label=vertex_labels, vertex.label.dist=2.5,
     vertex.label.color="black", vertex.label.cex=1.2)

coeff_names <- c("degree prestige", "gregariousness")
graphs <- list(g6, g6)
anals <- analytics6_directed
par(mfrow=c(1,2), mar=c(1,1,1,1))
i <- 1
for (gr in graphs) {
  coeff_name = coeff_names[i]
  title <- "" #gr$name #paste("Graph", 1, sep=" ")
  vertex_labels <- round(anals[, coeff_name],2)
  plot(gr, layout=layout_with_lgl, main=title,
       edge.arrow.mode=2,
       vertex.size=12, vertex.label=vertex_labels, vertex.label.dist=2.5,
       vertex.label.color="black", vertex.label.cex=1.2)
  i <- i + 1
}

legend_text_from_aggregate_analytics <- function(anals) {
  nrs <- round(anals[1,], 1)
  res <- colnames(anals)
  for (i in 1:length(res)) {
    res[i] <- paste(res[i], nrs[i], sep=" ")
  }
  return(res)
}

par(mfrow=c(1,3), mar=c(1,3,1,1))
gr <- g
anals <- analytics_aggregate
legend_text <- legend_text_from_aggregate_analytics(anals)

plot(gr, edge.arrow.size=.2, vertex.color="orange", vertex.label="")
# edge.curved=0, vertex.label.color="black", vertex.frame.color="#555555", vertex.label=V(g)$media, , vertex.label.cex=0.7
colrs <- rep("grey", length(legend_text))
legend(x=-1.5, y=-1.1, legend_text, pch=21,
       col="#777777", pt.bg=colrs, pt.cex=1, cex=1.3, bty="n", ncol=1)

length(E(g7))
length(V(g7))


#---------------------------------------------------------------------------------
# Test Bed.
#---------------------------------------------------------------------------------
break()

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net)

g <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
g <- simplify(g, remove.multiple = T, remove.loops = T) 
plot(g, layout=layout_with_lgl)

plot(g, layout=layout_randomly)
plot(g, layout=layout_in_circle)
plot(g, layout=layout_with_fr) # Nice, default?
plot(g, layout=layout_with_kk)
plot(g, layout=layout_with_lgl) # Nice, default?
plot(g, layout=layout_with_gem) # Nice, default?
plot(g, layout=layout_with_dh) # Nice, default?
plot(g, layout=layout_on_grid) # Interesting, grid like structure.
plot(g, layout=layout_nicely) # Nicely! Pretty nice indeed.

all_layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
all_layouts

# Graph from edges.
g2 <- graph(edges=c(1,2,2,3,3,1,3,4,4,3,4,9), n=10, directed=T)
plot(g2)

g2 <- graph(edges=links, n=10 ,directed=F)
plot(g2)


adj_matrix <- as_adj(g, type="upper", attr=NULL,
                     edges=FALSE, names=TRUE, sparse=FALSE) # igraph_opt("sparsematrices")
# type = c("both", "upper", "lower")

sum(adj_matrix)
adj_matrix[1,]
neibs <- neighbors(g, 2, mode="total")
#neibs <- adjacent_vertices(g, 7, mode="total")
neibs
V(g)[neibs]$color <- "yellow"
plot(g, layout=layout_with_lgl)

a <- c("1","2","3")
b <- c("2","5","6", "6")
setdiff(a, b)
intersect(a, b)
setequal(a, b)
c <- base::union(a, b)
is.element("1", b)



#-------------------------------------------------------------------------------------------
# Graph Layouts. Source: https://kateto.net/networks-r-igraph
#-------------------------------------------------------------------------------------------

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(4,5), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(g))
  plot(g, edge.arrow.mode=0, layout=l, main=layout) }






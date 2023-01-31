library(coop)
library(igraph)
library("RColorBrewer")
#source("C:/Users/urmas/Documents/TalTech/Andmekaeve/final_project/cluster_analysis.R")

set.seed(0)

#---------------------------------------------------------------------------------------------
# Load Glove word vectors.
#---------------------------------------------------------------------------------------------
# source: https://jjallaire.github.io/deep-learning-with-r-notebooks/notebooks/6.1-using-word-embeddings.nb.html
glove_dir = "C:/Users/urmas/Documents/TalTech/Andmekaeve/final_project/"
lines <- readLines(file.path(glove_dir, "glove.6B.50d.txt"))
NUM_ROWS <- length(lines)
NUM_EMBEDDING_DIMS <- 50

words <- matrix("", nrow=NUM_ROWS, ncol=1)
embeddings <- matrix(0.0, nrow=NUM_ROWS, ncol=NUM_EMBEDDING_DIMS)

for (i in 1:length(lines)) {
  line <- lines[[i]]
  values <- strsplit(line, " ")[[1]]
  words[i,] <- values[[1]]
  embeddings[i,] <- as.double(values[-1])
}

words[1:3,]
embeddings[1,]

find_word_indexes <- function(all_words_vec, search_words_vec) {
  res <- matrix(NA, nrow=length(search_words_vec), ncol=1)
  for (i in 1:length(search_words_vec)) {
    res[i, 1] <- which(all_words_vec==search_words_vec[i])
  }
  return(res[,1])
}

#---------------------------------------------------------------------------------------------
# Load common nouns, verbs and adjectives in order to work on smaller subsets of the data.
#---------------------------------------------------------------------------------------------
load_words_from_csv <- function(file_name, skip_first_row=TRUE) {
  lines <- readLines(file_name)
  if (skip_first_row) { lines <- lines[2:length(lines)] }
  res <- matrix("", nrow = length(lines), ncol = 1)
  for (i in 1:length(lines)) {
    res[i,] <- (strsplit(lines[[i]], ",")[[1]])[1]
  }
  return(res)
}
# source1: https://gist.github.com/hugsy/8910dc78d208e40de42deb29e62df913
# source2: www.wordexample.com
nouns <- load_words_from_csv("C:/Users/urmas/Documents/TalTech/Andmekaeve/final_project/nouns.csv")
verbs <- load_words_from_csv("C:/Users/urmas/Documents/TalTech/Andmekaeve/final_project/verbs.csv")
adjectives <- load_words_from_csv("C:/Users/urmas/Documents/TalTech/Andmekaeve/final_project/adjectives.csv")
# source: https://github.com/first20hours/google-10000-english
# based on: https://norvig.com/ngrams/count_1w.txt
# https://norvig.com/ngrams/
words10k <- load_words_from_csv("C:/Users/urmas/Documents/TalTech/Andmekaeve/final_project/words10k.txt", skip_first_row=FALSE)

nouns_idxs <- which(words[,1] %in% nouns[,1])
verbs_idxs <- which(words[,1] %in% verbs[,1])
adjectives_idxs <- which(words[,1] %in% adjectives[,1])
words10k_idxs <- which(words[,1] %in% words10k[,1])

words_nouns <- words[nouns_idxs]
words_verbs <- words[verbs_idxs]
words_adjectives <- words[adjectives_idxs]
words_words10k <- words[words10k_idxs]

#---------------------------------------------------------------------------------------------
# Calculate top-k similar vectors
#---------------------------------------------------------------------------------------------

K <- 10
MIN_SIMILARITY <- 0.75

k_similar_idxs <- function(x, k=5, min_similarity=0.8) {
  # Returns matrix: nrows = nrows(x), ncols=k.
  # x is matrix: n items in rows, d dimensions in columns.
  # if no match then -1.
  similars <- coop::cosine(t(x))
  res <- matrix(-1.0, nrow=nrow(x), ncol=k)
  for (i in 1:nrow(x)) {
    ids <- order(similars[i,], decreasing=TRUE)[2:(k+1)]
    values <- similars[i, ids]
    q <- ids[values >= min_similarity]
    l <- length(q)
    if (l == 1) { res[i,1] <- q[1] }
    else if (l > 1) { res[i,1:l] <- q }
  }
  return(res)
}

k_similar_idxs2 <- function(x, k=5) {
  # Returns matrix: nrows = nrows(x), ncols=k.
  # x is matrix: n items in rows, d dimensions in columns.
  return(t(apply(coop::cosine(t(x)), 1, order, decreasing=TRUE)[2:(k+1),]))
}

#---------------------------------------------------------------------------------------------
# Construct Graph
#---------------------------------------------------------------------------------------------

create_edge_list_from_similars_idxs <- function(x, labels) {
  num_rows <- nrow(x) * ncol(x)
  res <- matrix(NA, nrow = num_rows, ncol = 2)
  r <- 1
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      if (x[i, j] >= 0) {
        res[r, 1] <- labels[i]
        res[r, 2] <- labels[x[i, j]]
        r <- r + 1
      }
    }
  }
  res <- res[!is.na(res[,1]),]
  return(res)
}

create_graph_from_embeddings <- function(embeddings, words, num_neighbours, min_similarity) {
  similar_idxs <- k_similar_idxs(embeddings, k=num_neighbours, min_similarity=min_similarity)
  edgelist <- create_edge_list_from_similars_idxs(similar_idxs, words)
  g <- graph_from_edgelist(edgelist, directed=FALSE)
  g <- simplify(g, remove.multiple=T, remove.loops = T)
  return(g)
}

extract_largest_cluster <- function(gr) {
  # Returns new graph: largest connected component of the input graph.
  clst <- clusters(gr)
  nodes <- names(clst$membership)[which(clst$membership==1)]
  return(induced_subgraph(gr, V(gr)[nodes]))
}

g_nouns <- create_graph_from_embeddings(embeddings[nouns_idxs,], words[nouns_idxs,], num_neighbours=K, min_similarity=MIN_SIMILARITY)
g_verbs <- create_graph_from_embeddings(embeddings[verbs_idxs,], words[verbs_idxs,], num_neighbours=K, min_similarity=MIN_SIMILARITY)
g_adjectives <- create_graph_from_embeddings(embeddings[adjectives_idxs,], words[adjectives_idxs,], num_neighbours=K, min_similarity=MIN_SIMILARITY)
g_words10k <- create_graph_from_embeddings(embeddings[words10k_idxs,], words[words10k_idxs,], num_neighbours=K, min_similarity=MIN_SIMILARITY)

g <- g_words10k
g <- extract_largest_cluster(g_words10k)
is_connected(g)
par(mar=c(1,1,1,1))
plot(g, main="Largest connected component", layout=layout_with_lgl, vertex.size=2.0, vertex.label.cex=0.01)
degree_counts <- degree(g)
degree_one <- which(degree_counts==1)
hist(degree_counts)

#---------------------------------------------------------------------------------------------
# Binning and discretizing
#---------------------------------------------------------------------------------------------

bin_breaks <- function(x, num_breaks) {
  x_min <- min(x)
  x_max <- max(x)
  step <- (x_max - x_min) / num_breaks
  return(seq(x_min, x_max, step))
}

bin_breaks2 <- function(x_min, x_max, num_breaks) {
  step <- (x_max - x_min) / num_breaks
  return(seq(x_min, x_max, step))
}

bin_vals <- function(x, num_breaks) {
  return(.bincode(x, bin_breaks(x, num_breaks), right=TRUE, include.lowest=TRUE))
}

bin_vals2 <- function(x, x_min, x_max, num_breaks) {
  return(.bincode(x, bin_breaks2(x_min, x_max, num_breaks), right=TRUE, include.lowest=TRUE))
}

bin_labels <- function(breaks) {
  res <- c()
  for (i in 1:(length(breaks) - 1)) {
    r <- if (breaks[i + 1] < 1) { 2 } else if(breaks[i + 1] < 10) { 1 } else { 0 }
    res <- c(res, paste(round(breaks[i], r), "..", round(breaks[i + 1], r), sep=""))
  }
  return(res)
}

#---------------------------------------------------------------------------------------------
# Centrality
#---------------------------------------------------------------------------------------------

degree_centralities <- centr_degree(g)
close_centralities <- centr_clo(g)
betw_centralities <- centr_betw(g)
eig_centralities <- centr_eigen(g)

num_colrs <- 5
bin_colrs <- brewer.pal(num_colrs, "Reds")

par(mfrow=c(1,3), mar=c(1,1,1,1))
for (i in 1:3) {
  if (i == 1) {
    a <- degree_centralities$res
    title <- "Degree centrality"
  }
  else if (i == 2) {
    a <- close_centralities$res
    title <- "Closeness centrality"
  }
  else if (i == 3) {
    a <- betw_centralities$res
    title <- "Betweenness centrality"
  }
  set.seed(10)
  plot(g, vertex.color=bin_colrs[bin_vals(a, num_colrs)], layout=layout_with_lgl, vertex.size=4.0, vertex.label.cex=0.001)
  legend("bottom", legend=bin_labels(bin_breaks(a, num_colrs)), col=bin_colrs, pch=15, cex=1.2)
}
# , main=title

# Summary
degree_centralities$centralization
close_centralities$centralization
betw_centralities$centralization

top_k_degree <- V(g)[order(degree_centralities$res, decreasing=TRUE)[1:5]]
top_k_close <- V(g)[order(close_centralities$res, decreasing=TRUE)[1:5]]
top_k_betw <- V(g)[order(betw_centralities$res, decreasing=TRUE)[1:5]]
degree(g, top_k_degree)
closeness(g, top_k_close)
betweenness(g, top_k_betw, directed=FALSE, weights=NA)

#---------------------------------------------------------------------------------------------
# Heatmaps and colour plots
#---------------------------------------------------------------------------------------------
display.brewer.pal(8, "Spectral")
display.brewer.pal(8, "Blues")
display.brewer.pal(8, "Reds")
display.brewer.pal(8, "Greens")

colrs <- brewer.pal(8, "Reds")

#---------------------------------------------------------------------------------------------
# Plot Neighbourhood
#---------------------------------------------------------------------------------------------
# nouns: food, car, queen, table #, child, king
# verbs: drive, smile, walk, dream
# adjectives: brown, sad
anchor_idx <- which(V(g)$name=="book")
neibs <- neighborhood(g, order=1, nodes=V(g)[anchor_idx])[[1]]
V(g)$name[neibs]
V(g)$color <- "grey"
V(g)[anchor_idx]$color <- "red"

par(mfrow=c(1,2), mar=c(1,1,1,2))
g1 <- induced_subgraph(g, neibs)
plot(g1, layout=layout_with_lgl, vertex.size=3.5, vertex.label.cex=1.5, vertex.label.dist=1.5, vertex.label.color="black")

#---------------------------------------------------------------------------------------------
# Plot Degree Distribution
#---------------------------------------------------------------------------------------------
degree(g, V(g)[anchor_idx])
plot(degree_distribution(g, cumulative=TRUE))

deg_dist_nouns <- 1 - degree_distribution(g_nouns, cumulative=TRUE, mode="all")
deg_dist_verbs <- 1 - degree_distribution(g_verbs, cumulative=TRUE, mode="all")
deg_dist_adjectives <- 1 - degree_distribution(g_adjectives, cumulative=TRUE, mode="all")
deg_dist_words10k <- 1 - degree_distribution(g_words10k, cumulative=TRUE, mode="all")

max_degree <- 32
deg_dist <- matrix(c(deg_dist_nouns[1:max_degree], deg_dist_verbs[1:max_degree],
                     deg_dist_adjectives[1:max_degree], deg_dist_words10k[1:max_degree]), 
                   nrow=max_degree, ncol=4)
colrs <- c("orange","green","blue","red")
matplot(deg_dist, type=c("b"), pch=19, cex=1.0, col=colrs, main="Cumulative degree distribution", xlab="Degree", ylab="Cumulative Frequency")
legend("bottomright", legend=c("nouns","verbs","adjectives","common 10k"), col=colrs, pch=19, cex=1)

#---------------------------------------------------------------------------------------------
# Clusters and Components
#---------------------------------------------------------------------------------------------
g_nouns_connected <- extract_largest_cluster(g_nouns)
g_verbs_connected <- extract_largest_cluster(g_verbs)
g_adjectives_connected <- extract_largest_cluster(g_adjectives)
g_words10k_connected <- extract_largest_cluster(g_words10k)

length(E(g_nouns_connected))
length(E(g_verbs_connected))
length(E(g_adjectives_connected))
length(E(g_words10k_connected))

clust_louvain_nouns <- cluster_louvain(g_nouns_connected)
clust_louvain_verbs <- cluster_louvain(g_verbs_connected)
clust_louvain_adjectives <- cluster_louvain(g_adjectives_connected)
clust_louvain_words10k <- cluster_louvain(g_words10k_connected)

clust_louvain_nouns$modularity
clust_louvain_verbs$modularity
clust_louvain_adjectives$modularity
clust_louvain_words10k$modularity

par(mfrow=c(1,1), mar=c(0,0,0,3))
set.seed(10)
vs <- V(g_nouns_connected)[which(clust_louvain_nouns$membership==6)]
plot(induced_subgraph(g_nouns_connected, vs), layout=layout_with_lgl, vertex.size=2.2, vertex.label.cex=1.2, vertex.label.dist=1.2, vertex.label.color="black")

set.seed(10)
vs <- V(g_verbs_connected)[which(clust_louvain_verbs$membership==18)]
plot(induced_subgraph(g_verbs_connected, vs), layout=layout_with_lgl, vertex.size=2.2, vertex.label.cex=1.2, vertex.label.dist=1.2, vertex.label.color="black")

set.seed(10)
vs <- V(g_adjectives_connected)[which(clust_louvain_adjectives$membership==18)]
plot(induced_subgraph(g_adjectives_connected, vs), layout=layout_with_lgl, vertex.size=2.2, vertex.label.cex=1.2, vertex.label.dist=1.2, vertex.label.color="black")

set.seed(10)
vs <- V(g_words10k_connected)[which(clust_louvain_words10k$membership==12)]
plot(induced_subgraph(g_words10k_connected, vs), layout=layout_with_lgl, vertex.size=2.2, vertex.label.cex=1.2, vertex.label.dist=1.2, vertex.label.color="black")


par(mfrow=c(1,4), mar=c(5,5,4,1))
ylab <- "Cluster size"
xlab <- "Cluster id"
hist(clust_louvain_nouns$membership, main="Nouns", xlab=xlab, ylab=ylab)
hist(clust_louvain_verbs$membership, main="Verbs", xlab=xlab, ylab=ylab)
hist(clust_louvain_adjectives$membership, main="Adjectives", xlab=xlab, ylab=ylab)
hist(clust_louvain_words10k$membership, main="Common 10k", xlab=xlab, ylab=ylab)

sum(clust_louvain_nouns$membership==1)
sum(clust_louvain_verbs$membership==1)
sum(clust_louvain_adjectives$membership==1)
sum(clust_louvain_words10k$membership==1)

set.seed(10)
plot(clust_louvain_nouns, g_nouns_connected)
plot(clust_louvain_verbs, g_verbs_connected)
plot(clust_louvain_adjectives, g_adjectives_connected)
#plot(clust_louvain_words10k, g_words10k)

comps_noun <- components(g_nouns)
comps_verb <- components(g_verbs)
comps_adj <- components(g_adjectives)
comps_words10k <- components(g_words10k)

length(comps_noun$csize)
length(comps_verb$csize)
length(comps_adj$csize)
length(comps_words10k$csize)

clust_noun <- clusters(g_nouns)
clust_noun[2]
clust_noun[3]
membs <- clust_noun[1]
membs$membership[1:10]
clust_noun$csize[1]

is_connected(g)
length(V(g)) #[953]

#---------------------------------------------------------------------------------------------
# Histograms of embeddings.
#---------------------------------------------------------------------------------------------
dimension_idx <- 40
hist(embeddings[nouns_idxs,][,dimension_idx])

num_colrs <- 10
bin_colrs <- brewer.pal(num_colrs, "Spectral")
display.brewer.pal(num_colrs, "Spectral")
min_emb <- min(embeddings)
max_emb <- max(embeddings)
#par(mfrow=c(1,3), mar=c(1,1,1,1))
set.seed(10)

ws <- c("man", "woman","child", "pretty", "nice", "good","book","story","novel")
ws <- c("pretty", "nice", "good")
ws <- c("man", "woman","child")

#a <- embeddings[which(words[,1] %in% ws),]
a <- embeddings[find_word_indexes(words[,1], ws),]

yval <- 0.1
for (i in 1:length(ws)) {
  plot(rep(yval, 50), col=bin_colrs[bin_vals2(a[i,], min_emb, max_emb, num_colrs)], type="p", pch=15, cex=2.0, ylim=c(0,5), ylab=NA)
  yval <- yval + 0.2
  par(new=TRUE)
}
legend("top", legend=bin_labels(bin_breaks2(min_emb, max_emb, num_colrs)), col=bin_colrs, pch=15, cex=1.0)


heatmap.2(acc[idx,], xlab="x", ylab="y", main="title", dendrogram="none", Rowv=FALSE, Colv=FALSE, trace="none")
binned_embs <- matrix(t(a), nrow=50, ncol=5)
colnames(binned_embs) <- ws
colrs <- bin_colrs[bin_vals(a, num_colrs)]
matplot(binned_embs, type=c("b"), pch=19, cex=1.0, col=colrs, main="Words", ylab=ws)
#legend("bottomright", legend=c("nouns","verbs","adjectives","common 10k"), col=colrs, pch=19, cex=1)

#---------------------------------------------------------------------------------------------
# PCA - Principal Compoent Analysis
#---------------------------------------------------------------------------------------------
pca <- prcomp(embeddings[adjectives_idxs,]) #, center=TRUE, scale.=TRUE) #, tol=0.5)
summary(pca)

which(words_adjectives=="female")
V(g)$color <- "grey"
V(g)$color <- "orange"






###################################################################################################
# Test bed
###################################################################################################
x_train <- embeddings[nouns_idxs,]
x_train <- embeddings[verbs_idxs,]
x_train <- embeddings[adjectives_idxs,]

similars <- coop::cosine(t(x_train))
similar_k <- t(apply(similars, 1, order, decreasing=TRUE)[2:6,])

words_nouns[7]
words_nouns[similar_k[7,]]

# For each datapoint calculate distances to other datapoints.
similars_eucl <- distances_2_clusters(x_train, NULL, "euclidean", knneighbors=5)
similar_k <- similars_eucl[, 6:10]

similar_nouns_idxs <- k_similar_idxs(embeddings[nouns_idxs,], k=K, min_similarity=MIN_SIMILARITY)
similar_verbs_idxs <- k_similar_idxs(embeddings[verbs_idxs,], k=K, min_similarity=MIN_SIMILARITY)
similar_adjectives_idxs <- k_similar_idxs(embeddings[adjectives_idxs,], k=K, min_similarity=MIN_SIMILARITY)

edgelist_nouns <- create_edge_list_from_similars_idxs(similar_nouns_idxs, words[nouns_idxs,])
g <- graph_from_edgelist(edgelist_nouns, directed=FALSE)
g <- simplify(g, remove.multiple = T, remove.loops = T)

edgelist_nouns[edgelist_nouns[,1]=='husband']
edgelist_nouns[edgelist_nouns[,2]=='husband']


# nouns_in_emb <- base::intersect(words[,1], nouns[,1])
# verbs_in_emb <- base::intersect(words[,1], verbs[,1])
# adjectives_in_emb <- base::intersect(words[,1], adjectives[,1])


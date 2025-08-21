rm(list = ls())
library(slam)
library(tm)
library(SnowballC)
library(proxy)
library(SentimentAnalysis)
library(dplyr)
library(igraph)

cname = file.path(".", "Corpus")
cname
dir(cname)
docs = Corpus(DirSource((cname)))
summary(docs)
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, removeWords, stopwords("en"))
docs = tm_map(docs, stemDocument, language = "en") 

# EDA
dtm = DocumentTermMatrix(docs)
inspect(dtm[1:21, 1:10])
mat_subset = as.matrix(dtm[1:21, 1:10])
dim(mat_subset)
print(mat_subset)

freq = colSums(as.matrix(dtm))
length(freq)
ord = order(freq)
freq[head(ord, 10)]
freq[tail(ord, 10)]
head(table(freq), 10)
tail(table(freq), 10)
dim(dtm)

# Find the best percentage to return approximately 25 tokens
s_vals = seq(0.50, 0.98, by = 0.01)
results = data.frame(s = s_vals, kept = NA_integer_)
for (i in seq_along(s_vals)) {
  trim = removeSparseTerms(dtm, s_vals[i])
  results$kept[i] = ncol(trim)
}
print(results)

dtm_trim = removeSparseTerms(dtm, 0.70) 
ncol(dtm_trim) # return 27 tokens

# Create matrix
dtms = as.matrix(dtm_trim)

# build the matrix into csv file
write.csv(dtms, "dtms.csv")

# Compute cosine‐distance matrix (1 -  cosine similarity)
dist_cos = proxy::dist(dtms, method = "cosine")

# Hierarchical clustering with Ward’s method
fit = hclust(dist_cos, method = "ward.D")

# Plot the dendrogram
plot(fit, hang = -1,labels = dtm_trim$dimnames$Docs, main = "Abstracts Cosine Distance")

# Cut the tree into k = 3 clusters (one per genre)
clusters = cutree(fit, k = 3)

# Assign genres to the docs
doc_names = dtm_trim$dimnames$Docs
genres = ifelse(grepl("^lyrics", doc_names), "lyrics",
                 ifelse(grepl("^philosophy", doc_names), "philosophy",
                        ifelse(grepl("^review", doc_names), "review", NA)))

# determine the accuracy of the classifier
ct = table(Cluster = clusters, Genre = genres)
ct
accuracy = sum(apply(ct, 1, max)) / length(genres)
cat("Clustering accuracy:", round(accuracy * 100, 2), "%\n")

dtm_data = read.csv("dtms.csv")
dtm_data

cname    = file.path(".", "Corpus")
ori_docs = VCorpus(DirSource(cname), readerControl = list(language = "en"))
# …apply your cleaning pipeline to ori_docs…

# Analyse sentiment 
SA_raw = analyzeSentiment(ori_docs)

# Turn into a data frame
SentimentA = as.data.frame(SA_raw, stringsAsFactors = FALSE)

# xtract the true filenames (document IDs)
doc_ids = sapply(ori_docs, function(d) meta(d, "id"))  

# Add those IDs as rownames 
rownames(SentimentA)= doc_ids
SentimentA$Document = doc_ids

# 6. Build your Group factor from the filenames
SentimentA$Group = factor(
  ifelse(grepl("^lyrics", SentimentA$Document), "lyrics",
         ifelse(grepl("^philosophy", SentimentA$Document), "philosophy",
                ifelse(grepl("^review", SentimentA$Document), "review", NA))),
  levels = c("lyrics","philosophy","review")
)

# Quick check: now rownames(SentimentA) == doc_ids
rownames(SentimentA)

# plot the box plot
pdf("Sentiments_by_Genre.pdf", height=5, width=10)
par(mfrow=c(2,2))

boxplot(WordCount ~ Group, data=SentimentA, main="Word Count by Genre", ylab="Word Count")

boxplot(SentimentQDAP ~ Group,data=SentimentA, main="Net QDAP Polarity", ylab="SentimentQDAP")

boxplot(PositivityQDAP ~ Group, data=SentimentA, main="Positivity (QDAP)", ylab="PositivityQDAP")

boxplot(RatioUncertaintyLM ~ Group, data=SentimentA, main="Uncertainty Ratio (LM)", ylab="RatioUncertaintyLM")

dev.off()

# group the measure of centrality for easy visualise
sentiment_stats = SentimentA %>%
  group_by(Group) %>%
  summarise(
    median_Word = median(WordCount),
    sd_WordC= sd(WordCount),
    iqr_Word = IQR(WordCount),
    median_PosQDAP = median(PositivityQDAP),
    sd_PosQDAP  = sd(PositivityQDAP),
    iqr_PosQDAP = IQR(PositivityQDAP),
    median_Ratio= median(RatioUncertaintyLM),
    sd_Ratio = sd(RatioUncertaintyLM),
    iqr_Ratio =IQR(RatioUncertaintyLM),
    median_NetQDAP = median(SentimentQDAP),
    sd_NetQDAP = sd(SentimentQDAP),
    iqr_NetQDAP = IQR(SentimentQDAP),
    .groups = "drop"
  )
print(sentiment_stats)

# kurskal test
kruskal.test(SentimentQDAP ~ Group, data=SentimentA)
kruskal.test(WordCount ~ Group, data=SentimentA)
kruskal.test(PositivityQDAP ~ Group, data=SentimentA)
kruskal.test(RatioUncertaintyLM ~ Group, data=SentimentA)

# 1. Build adjacency as before
m = as.matrix(dtm > 0)
ByAbsMatrix = m %*% t(m)
diag(ByAbsMatrix) = 0

ByAbs = graph_from_adjacency_matrix(ByAbsMatrix, mode = "undirected", weighted = TRUE)
windows()
plot(ByAbs)

# calculate stats from the graph object
format(closeness(ByAbs), digits = 2)
betweenness(ByAbs)

docs = V(ByAbs)$name
V(ByAbs)$color = ifelse(grepl("^lyrics", docs), "skyblue",
                         ifelse(grepl("^philosophy", docs), "tomato",
                                ifelse(grepl("^review", docs), "palegreen",
                                       "gray")))

# Compute closeness (treat weight as strength → cost = 1/weight)
clo = closeness(ByAbs)

# Scale closeness into a reasonable size range, e.g. [5, 15]
clo_norm = (clo - min(clo)) / diff(range(clo))
V(ByAbs)$size = clo_norm * 10 + 5

# Plot
windows()
plot(ByAbs,
     vertex.color = V(ByAbs)$color,
     vertex.size  = V(ByAbs)$size,
     vertex.label.cex = 0.7,
     edge.width = E(ByAbs)$weight / max(E(ByAbs)$weight) * 3,
     edge.color = "gray")

# Legend
legend("topleft",
       legend = c("Lyrics","Philosophy","Review"),
       pt.bg   = c("skyblue","tomato","palegreen"),
       pch     = 21,
       pt.cex  = 1.5,
       bty     = "n",
       title   = "Genre")

# degree
deg = strength(ByAbs)

# Closeness 
clo = closeness(ByAbs)

# Betweenness 
btw = betweenness(ByAbs)

# Eigenvector centrality
eig = eigen_centrality(ByAbs)$vector


# Assemble into a data.frame
result = data.frame(Degree  = round(deg, 2), Closeness = round(clo, 4), Betweenness = round(btw, 2),
                  Eigenvector = round(eig, 3), stringsAsFactors = FALSE)
result[order(-result$Closeness), ]

# we used the trim version
dtmsx = as.matrix(dtm_trim)
 # convert to binary matrix
dtmsx = as.matrix((dtmsx > 0) + 0)
 # mul]ply transpose binary matrix by binary matrix
ByTokenMatrix = t(dtmsx) %*% dtmsx
# make leading diagonal zero
diag(ByTokenMatrix) = 0

# create the network and plot
ByTokenMatrix = graph_from_adjacency_matrix(ByTokenMatrix, mode = "undirected", weighted = TRUE)
windows()
plot(ByTokenMatrix)

# Degree
deg = strength(ByTokenMatrix)

#Closeness 
clo = closeness(ByTokenMatrix)

# Betweenness 
btw = betweenness(ByTokenMatrix)

# Eigenvector centrality
eig = eigen_centrality(ByTokenMatrix)$vector

#Assemble into a data.frame
result = data.frame(Degree  = round(deg, 2), Closeness = round(clo, 4), Betweenness = round(btw, 2),
                  Eigenvector = round(eig, 3), stringsAsFactors = FALSE)

#   Here’s sorted by Degree:
result[order(-result$Degree), ]

# Node size: scale degree into [4, 12]
V(ByTokenMatrix)$size = (deg - min(deg)) / diff(range(deg)) * 8 + 4

# ode color: palette from light (low eig) to dark (high eig)
pal = colorRampPalette(c("lightblue","blue"))(100)
eig_norm = (eig - min(eig)) / diff(range(eig))
V(ByTokenMatrix)$color = pal[as.integer(eig_norm * 99) + 1]

# Edge width: scale weight into [1, 4]
w = E(ByTokenMatrix)$weight
E(ByTokenMatrix)$width  = (w - min(w)) / diff(range(w)) * 3 + 1

# Plot
set.seed(33520615)
lay = layout_with_fr(ByTokenMatrix)
windows()
plot(ByTokenMatrix, layout = lay,
     vertex.label.cex = 0.8,
     vertex.label.color = "black",
     edge.color  = "lightgray")

# Add a simple legend for node color (eigenvector)
legend("topright", legend = c("Low Influence","High Influence"), pch = 21, pt.bg = c("lightblue","blue"),
       pt.cex = 2, bty = "n", title = "Eigenvector\nCentrality")

# bipartite network
dtm = DocumentTermMatrix(docs)
dtmsa = as.data.frame(dtms)
dtmsa$ABS = rownames(dtmsa) 
dtmsb = data.frame()
dtmsb

for (i in 1:nrow(dtmsa)){
  for (j in 1:(ncol(dtmsa)-1)){
     touse = cbind(dtmsa[i,j], dtmsa[i,ncol(dtmsa)],
                    colnames(dtmsa[j]))
     dtmsb = rbind(dtmsb, touse ) } }
colnames(dtmsb) = c("weight", "abs", "token")
dtmsc = dtmsb[dtmsb$weight != 0,]
dtmsc = dtmsc[,c(2,3,1)]

# # create graph object and declare bipartite 
g = graph.data.frame(dtmsc, directed=FALSE)
bipartite.mapping(g)
V(g)$type = bipartite_mapping(g)$type
V(g)$color = ifelse(V(g)$type, "lightgreen", "pink")
V(g)$shape = ifelse(V(g)$type, "circle", "square")
E(g)$color = "lightgray"
windows()
plot(g) #original network

# improved network
V(g)$color = ifelse(
  V(g)$type,
  "lightgray",
  ifelse(grepl("^lyrics", V(g)$name), "skyblue",
         ifelse(grepl("^philosophy", V(g)$name), "tomato",
                ifelse(grepl("^review", V(g)$name), "seagreen","pink"))))

doc_idx   = which(!V(g)$type)   # vertices where type==FALSE
token_idx = which( V(g)$type)   # vertices where type==TRUE

# Create a two-column layout matrix
lay = matrix(NA, nrow=vcount(g), ncol=2)

# spacing for nodes to prevent cluttering
lay[doc_idx, 1] = 0
lay[doc_idx, 2] = seq(from=1, to=0, length.out=length(doc_idx))
lay[token_idx, 1] = 1
lay[token_idx, 2] = seq(from=1, to=0, length.out=length(token_idx))

# plot
windows()
plot(g,
     layout = lay,
     vertex.shape = V(g)$shape,
     vertex.color = V(g)$color,
     vertex.size = 8,
     vertex.label.cex = 0.7,
     edge.color = E(g)$color,
     edge.width = 1,
     asp  = 0)

# legend
legend("topleft",legend = c("Document","Token"), pch = c(21,22), pt.bg  = c("pink","lightgreen"),
       pt.cex = 1.5, bty = "n")

deg = strength(g)
clo = closeness(g)
btw = betweenness(g)     

#  Assemble into a data.frame
result = data.frame(
  Degree = deg,
  Closeness = clo,
  Betweenness = btw,
  stringsAsFactors = FALSE
)
result[order(-result$Degree), ]


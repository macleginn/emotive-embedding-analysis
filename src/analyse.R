library(uwot)
library(scales)

color.codes = c(
  Passion="darkgoldenrod1",
  Affection="cornsilk3",
  TemperatureCool="darkseagreen3",
  TemperatureHot="brown1",
  TemperatureWarm="coral",
  TemperatureCold="cadetblue2",
  Abstract="grey"
)

setwd("C:/Users/dniko/YandexDisk/WorkInProgress/emotive-embeddings/src")

data <- read.csv('../data/embeddings_df.csv', row.names = 1)

meta.data <- read.csv('../data/English_seed_words.csv')
rownames(meta.data) <- meta.data[,1]


# Convert all vectors to a pair of distances to 'good' and 'bad'.
# Scale by the distance between them.
# Repeat for 'positive' and 'negative'.

#
# Good-bad
#

good.bad.projection <- data.frame(
  X=apply(data, 1, function(r) { sum((r-data['good',])^2) }),
  Y=apply(data, 1, function(r) { sum((r-data['bad',])^2) })
)
good.bad.projection <- good.bad.projection / sum((data['good',]-data['bad',])^2)

pdf('../img/good_bad.pdf', width = 16, height = 10)
plot(
  x = good.bad.projection$X, 
  y = good.bad.projection$Y, 
  col = color.codes[meta.data[rownames(data),]$Category], 
  pch=16, 
  cex=2,
  xlab='',
  ylab=''
)
text(
  x = good.bad.projection$X,
  y = good.bad.projection$Y,
  labels = rownames(data),
  cex = .4
)
dev.off()


#
# Positive-negative
#

positive.negative.projection <- data.frame(
  X=apply(data, 1, function(r) { sum((r-data['positive',])^2) }),
  Y=apply(data, 1, function(r) { sum((r-data['negative',])^2) })
)
positive.negative.projection <- positive.negative.projection / 
  sum((data['positive',]-data['negative',])^2)

pdf('../img/positive_negative.pdf', width = 10, height = 10)
plot(
  x = positive.negative.projection$X, 
  y = positive.negative.projection$Y, 
  col = color.codes[meta.data[rownames(data),]$Category],
  pch=16 + as.numeric(positive.negative.projection$X < positive.negative.projection$Y), 
  cex=2,
  xlab='',
  ylab='',
  xlim=c(3,7),
  ylim=c(3,7)
)
abline(a=0, b=1, col='grey', lty='dashed')
text(
  x = positive.negative.projection$X,
  y = positive.negative.projection$Y,
  labels = rownames(data),
  cex = .4
)
dev.off()

#
# Combining positive/negative and hot/cold
#

quadrant <- data.frame(
  # How positive is this term?
  X=apply(data, 1, function(r) { sum((r-data['positive',])^2)-sum((r-data['negative',])^2) }),
  # How warm is this term?
  Y=apply(data, 1, function(r) { sum((r-data['warm',])^2)-sum((r-data['cold',])^2) })
)

pdf('../img/quadrants.pdf', width = 10, height = 10)
plot(
  x = quadrant$X, 
  y = quadrant$Y, 
  col = color.codes[meta.data[rownames(data),]$Category],
  pch=16, 
  cex=2,
  xlab='positive -> negative',
  ylab='warm -> cold'
)
abline(v=0, col='grey', lty='dashed')
abline(h=0, col='grey', lty='dashed')
text(
  x = quadrant$X,
  y = quadrant$Y,
  labels = rownames(data),
  cex = .5,
  col = alpha('black', 0.5)
)
dev.off()



embeddings.uwot <- umap(data, n_neighbors = 5)

pdf('../img/emotive_umap.pdf', width = 16, height = 10)
plot(
  embeddings.uwot, 
  col = color.codes[meta.data[rownames(data),]$Category],
  pch=16 + as.numeric(positive.negative.projection$X < positive.negative.projection$Y), 
  cex=2,
  xlab='',
  ylab=''
)
text(
  x = embeddings.uwot[,1],
  y = embeddings.uwot[,2],
  labels = rownames(data),
  cex = .4
)
dev.off()

temp.terms <- meta.data[meta.data$Category == 'Temperature',]$Word
temp.mean <- apply(data[temp.terms,], 2, mean)
other.mean <- apply(data[!(rownames(data) %in% temp.terms),], 2, mean)

# Shift temperature terms towards all other words
data2 <- data
data2[temp.terms,] <- data2[temp.terms,] - temp.mean + other.mean
  
pdf('../img/emotive_umap2.pdf', width = 16, height = 10)
embeddings2.uwot <- umap(data2, n_neighbors = 5)
plot(
  embeddings2.uwot, 
  col = as.numeric(as.factor(meta.data[rownames(data), 2]))+1, 
  pch=16, 
  cex=2,
  xlab='',
  ylab=''
)
text(
  x = embeddings2.uwot[,1],
  y = embeddings2.uwot[,2],
  labels = rownames(data),
  cex = .4
)
dev.off()

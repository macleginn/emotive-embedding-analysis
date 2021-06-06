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

data <- read.csv('../data/russian_embeddings_df.csv', row.names = 1, encoding = 'UTF-8')

meta.data <- read.csv('../data/Russian_seed_words.csv', encoding = 'UTF-8')
rownames(meta.data) <- meta.data[,1]


quadrant <- data.frame(
  # How positive is this term?
  X=apply(data, 1, function(r) { 
      dist_to_pos <- sum((r-data['положительный',])^2)
      dist_to_neg <- sum((r-data['отрицательный',])^2)
      dist_to_pos - dist_to_neg
    }),
  # How warm is this term?
  Y=apply(data, 1, function(r) { 
    dist_to_warm <- sum((r-data['тёплый',])^2)
    dist_to_cold <- sum((r-data['холодный',])^2)
    dist_to_warm - dist_to_cold
  })
)
rownames(quadrant) <- rownames(data)

# Remove the outlying points
quadrant <- quadrant[ !(rownames(quadrant) %in% c('положительный', 'отрицательный', 'тёплый', 'холодный')), ]

cairo_pdf('../img/quadrants_russian.pdf', width = 10, height = 10)
plot(
  x = quadrant$X, 
  y = quadrant$Y, 
  col = color.codes[meta.data[rownames(data),]$Category],
  pch=16, 
  cex=2,
  xlab='positive -> negative',
  ylab='warm -> cold'
)
abline(v=mean(quadrant$X), col='grey', lty='dashed')
abline(h=mean(quadrant$Y), col='grey', lty='dashed')
text(
  x = quadrant$X,
  y = quadrant$Y,
  labels = rownames(data),
  cex = .5,
  col = alpha('black', 0.5)
)
dev.off()

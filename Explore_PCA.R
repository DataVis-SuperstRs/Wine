## DataVis: Second Project
## Exploration and PCA

setwd('C:/Users/YOURPATH')
load("wine.RdA")
attach(wine)

## First explorations
head(wine[,1:15]) # show first 6 rows and first 15 columns
table(wine$year , wine$winetype) # which wine types are from which year
table(wineyard, winetype) # see which yard produces which wine type
sort(unique(wine$winetype)) # all wine types in alphabetical order
sort(unique(wine$year)) # wines are from 2002 - 2006
length(unique(wine$winetype)) # 6 different wine types
names(wine[,1:6]) # show all columns that are not metabolomics
# [1] "wineyard"  "winetype"  "year"      "posneg"    "bottlenum" "quality"   
unique(wine$bottlenum) # 1 - 6
unique(wine$quality) # premium (high), r...(middle), v...(low)

plot(wine$winetype , wine[,7]) # see exemplatory metabolomic distribution in col 6 for each wine type

## PCA ##
# library(factoextra)
# fviz_nbclust(wine[,7:5745], kmeans, method = "wss") 
# => optimal number of clusters : 4
km.out<-kmeans(wine[,7:5746],centers=3,nstart=1)
pr.out<-prcomp(wine[,7:5746],scale=TRUE)
plot(pr.out$x[,1:2],type="n", ylab="PC2")
text(pr.out$x[,1:2],col=km.out$cluster)

biplot(prcomp(wine[,7:5746]))

## More exploration ##
plot(wine$quality) # frequency of wine quality category

table(wine$quality, wine$bottlenum)

table(wine[,1:6])


sort(unique(wine$wineyard))
# Aresti    San Pedro Tarapaca  Tocornal  Undurraga VASCOS

# function that filters the dataframe by wine yard
byname <- function(name){
  return(wine[which(wineyard == name), ])
}

aresti <- byname("Aresti")
spedro <- byname("San Pedro")

## VASCOS ##
vascos <- byname("VASCOS")

plot(vascos$year, vascos$bottlenum) 
plot(vascos$year, vascos$quality)
plot(vascos$bottlenum, vascos$quality)

# How different are the metabolics for each vascos wine quality (P,R,V)
# Cluster the vascos wine and color them by quality to see if the metabolics have an influence on the quality
km.out<-kmeans(vascos[,7:5746],centers=3,nstart=1)
pr.out<-prcomp(vascos[,7:5746])#,scale=TRUE)
par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
plot(pr.out$x[,1:2],type="n", ylab="PC2", main="Bottle IDs colored by cluster", yaxt="n", xaxt="n")
text(pr.out$x[,1:2], labels=rownames(vascos), col=km.out$cluster) # bottles colored by cluster, nothing spectacular
plot(pr.out$x[,1:2], type="n", ylab="PC2", main="Bottle IDs colored by Quality",  yaxt="n", xaxt="n")
text(pr.out$x[,1:2], labels=rownames(vascos), col=c(vascos$quality)) # bottles colored by quality, wow!
mtext("PCA for VASCOS", outer = TRUE, cex = 1.5)
# pr.out$x[,1:2] # PC1 and PC2 coordinates for each bottle id

# find the top 15 metabolics with the most variances
var_vasc_meta <- apply(vascos[,7:5746], 2, var) # all metabolomic variances in a vector
tail(sort(var_vasc_meta),15) # 15 highest variances

sort(var_vasc_meta)[20] # 20th lowest variance
sort(var_vasc_meta)[1000]
sort(var_vasc_meta)[2500]
sort(var_vasc_meta)[5740] # highest variance in metabolomic "14"

par(mfrow=c(1,1))
plot(vascos$bottlenum, vascos[, "14"]) # the variance is equal for each bottle
plot(vascos$year, vascos[, "14"]) # the variance in metabolomic "14" differs a lot between the years

plot(vascos$year, vascos[,7])
vascos[,1:7]



# @Autor Sabrina
# Modify by Tata
## Second project: Wine data

setwd('C:/Users/YOURPATH')

data2 <- read.delim("DataOrganized.csv", sep = ",")
data2 <- data2[,2:5748]


##################
## Explorations ##
##################
wine <- as.data.frame(data2)

head(wine[,1:15]) # show first 6 rows and first 15 columns
#View(wine[0]) # this is basically the ID column
rownames(wine) <- seq(1,300) # OVERWRITE id column with sequence 1:300
table(wine$year , wine$wine) # which wine types are from which year
sort(unique(wine$wine)) # all wine types in alphabetical order
sort(unique(wine$year)) # wines are from 2002 - 2006
length(unique(wine$wine)) # 6 different wine types
names(wine[,1:7]) # show all columns that are not metabolomics
# [1] "X"         "Wineyard"  "wine"      "year"      "posneg"   
# [6] "bottlenum" "quality" unique(wine$bottlenum) # 1 - 6
unique(wine$quality) # premium (high), r...(middle), v...(low)

plot(wine$wine , wine[,8]) # see metabolomic distribution in col 6 for each wine type



## More exploration ##
plot(wine$quality) # frequency of wine quality category
barplot(table(wine$quality, wine$wine), col = heat.colors(3),
     main = "Wine Quality Distribution", las = 2)
legend("topright", legend = c("High", "Regular", "Low"), title="Quality",
       fill=heat.colors(3), horiz=FALSE)


# Another way to represented, I know colors are horrible
legend("center", title="Wine Quality", col= topo.colors(6),
       legend = levels(wine$wine), fill = topo.colors(6) )

plot(wine$quality,wine$wine, col = topo.colors(6), ylab = "",
     xlab = "") # how to remove the y axis?


# function that filters the dataframe by wine name
byname <- function(winename){
  return(wine[which(wine$wine == winename), ])
}

CabernetF <- byname("Cabernet Franc")     
Csauvignon <- byname("Cabernet-sauvignon")
Carmenere <- byname("Carmenere")          
Merlot <- byname("Merlot")            
Mix <- byname("Mix")            
SYRAH <- byname("SYRAH")

## Csauvignon ##

# HOw different are the metabolics for each Csauvignon wine quality (P,R,V)
# Cluster the Csauvignon wine and color them by quality to see 
# if the metabolics reflects the quality
km.out<-kmeans(Csauvignon[,8:5747],centers=3,nstart=1)
pr.out<-prcomp(Csauvignon[,8:5747])#,scale=TRUE)
par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
plot(pr.out$x[,1:2],type="n", ylab="PC2", main="Bottle IDs colored by cluster", yaxt="n", xaxt="n")
text(pr.out$x[,1:2], labels=rownames(Csauvignon), col=km.out$cluster) # bottles colored by cluster, nothing spectacular
plot(pr.out$x[,1:2], type="n", ylab="PC2", main="Bottle IDs colored by Quality",  yaxt="n", xaxt="n")
text(pr.out$x[,1:2], labels=rownames(Csauvignon), col=c(Csauvignon$quality)) # bottles colored by quality, wow!
mtext("PCA for Csauvignon", outer = TRUE, cex = 1.5)
# pr.out$x[,1:2] # PC1 and PC2 coordinates for each bottle id




# find the top 15 metabolics with the most variances

var_Csauvignon_meta <- apply(Csauvignon[,8:5747], 2, var) # all metabolomic varainces in a vector
tail(sort(var_Csauvignon_meta),15) # 15 highest variances

min(var_Csauvignon_meta) # 20th lowest variance
sort(var_Csauvignon_meta)[1000]
sort(var_Csauvignon_meta)[2500]
max(var_Csauvignon_meta) # highest variance

par(mfrow=c(1,1))
plot(Csauvignon$bottlenum, Csauvignon[, "493.13351"]) # the variance is equal for each bottle
plot(Csauvignon$year, Csauvignon[, "493.13351"]) # the variance in metabolomic 493.13351 differs a lot between the years

plot(Csauvignon$year, Csauvignon[,6])
Csauvignon[,1:6]



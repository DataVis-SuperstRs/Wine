## DataVis: Second Project
## Data Transformation

setwd("C:/Users/YOURPATH") # insert your working directory


## @Autor: Jannis Busch
data <- read.delim("../300_Wine_pos.txt")
wine_data <- data[,4:303]

# Split the name column into several columns and transpose dataframe 
quality <- matrix(ncol=1, nrow=dim(wine_data)[2])
bottlenum <- matrix(ncol=1, nrow=dim(wine_data)[2])
posneg <- matrix(ncol=1, nrow=dim(wine_data)[2])
year <- matrix(ncol=1, nrow=dim(wine_data)[2])
name <- matrix(ncol=1, nrow=dim(wine_data)[2])

for (i in 1:dim(wine_data)[2]) {
  quality[i] <- substr(colnames(wine_data)[i],4,4)
  letters <- nchar(colnames(wine_data)[i])
  bottlenum[i] <- substr(colnames(wine_data)[i],letters,letters)
  posneg[i] <- substr(colnames(wine_data)[i],letters-4,letters-2)
  year[i] <- substr(colnames(wine_data)[i],letters-7,letters-6)
  name[i] <- substr(colnames(wine_data)[i],6,letters-9)
}
t_wine_data <- as.data.frame(t(wine_data))
colnames(t_wine_data) = data$MZ

wine <- cbind(name, year, posneg, bottlenum, quality, t_wine_data)


## @Autor: Ana Maria Sandoval Jimenez

# change colnames to friendly ones just a number 
true_colnames <-colnames(wine)
colnames(wine)[6:5745] <- c(1:5740)

# change rownames to friendly ones, sequence 1:300
rownames(wine) <- seq(1,300) 

# change colum "name" to two columns one for wineyard the other for wine type
wineyard <- {}
winetype <- {}

for (i in 1:300) {
  if  (wine[i,1] == "A") {
    wineyard[i] <- "Aresti"
    winetype[i] <- "Cabernet-sauvignon"
  }
  else if   (wine[i,1] == "A_CARM") {
      wineyard[i] <- "Aresti"
      winetype[i] <- "Carmenere"
  }
  else if   (wine[i,1] == "A_CCM") {
      wineyard[i] <- "Aresti"
      winetype[i] <- "Mix"
  }
  else if   (wine[i,1] == "A_SYRAH") {
      wineyard[i] <- "Aresti"
      winetype[i] <- "SYRAH"
  }
  else if   (wine[i,2] == "CT") {
    wineyard[i] <- "Concha y Toro"
    winetype[i] <- "Cabernet-sauvignon"
  }
  else if   (wine[i,1] == "T_CCMS") {
    wineyard[i] <- "Tocornal"
    winetype[i] <- "Mix"
  }
  else if   (wine[i,1] == "T_CCMS") {
    wineyard[i] <- "Tocornal"
    winetype[i] <- "Mix"
  }
  else if   (wine[i,1] == "TA_CF") {
    wineyard[i] <- "Tarapaca"
    winetype[i] <- "Cabernet Franc"
  }
  else if   (wine[i,1] == "TA_M") {
    wineyard[i] <- "Tarapaca"
    winetype[i] <- "Merlot"
  }
  else if   (wine[i,1] == "TA_SYRAH") {
    wineyard[i] <- "Tarapaca"
    winetype[i] <- "SYRAH"
  }
  else if   (wine[i,1] == "U") {
    wineyard[i] <- "Undurraga"
    winetype[i] <- "Cabernet-sauvignon"
  }
  else if   (wine[i,1] == "U_CM") {
    wineyard[i] <- "Undurraga"
    winetype[i] <- "Carmenere"
  }
  else if   (wine[i,1] == "U_CM_") {
    wineyard[i] <- "Undurraga"
    winetype[i] <- "Carmenere"
  }
  else if   (wine[i,1] == "VASCOS") {
    wineyard[i] <- "VASCOS"
    winetype[i] <- "Cabernet-sauvignon"
  }
  else {
    wineyard[i] <- "San Pedro" 
    winetype[i] <- "Cabernet-sauvignon"
  }
}

# join the new columns
wine2 <- cbind(wine[,1],wineyard,winetype,wine[,2:5745])
colnames(wine2)[1] <- "name"
wine2[1:5,1:10]

# save new data as .RdA and .csv
save(wine2, file="wine.Rda")
write.csv(wine2, file = "wine.csv")

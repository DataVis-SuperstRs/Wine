# @Autor: Ana Maria Sandoval Jimenez

setwd("~/DS2")

# Load data
data <- read.delim("wine_transformed.csv", sep = ",")

# first look
head(data)
colnames(data)
rownames(data)
data$name
dim(data)
length(colnames(data)[7:5746])

# change colnames to friendly ones just a number 
true_colnames <-colnames(data)
colnames(data)[7:5746] <- c(1:5740) ; colnames(data)

# change colum "name" to two columns one for wineyard the other
# for wine type

Wineyard <- {}
wine <- {}

for (i in 1:300) {
  if  (data[i,2] == "A") {
    Wineyard[i] <- "Aresti"
    wine[i] <- "Cabernet-sauvignon"
  }
  else if   (data[i,2] == "A_CARM") {
      Wineyard[i] <- "Aresti"
      wine[i] <- "Carmenere"
  }
  else if   (data[i,2] == "A_CCM") {
      Wineyard[i] <- "Aresti"
      wine[i] <- "Mix"
  }
  else if   (data[i,2] == "A_SYRAH") {
      Wineyard[i] <- "Aresti"
      wine[i] <- "SYRAH"
  }
  else if   (data[i,2] == "CT") {
    Wineyard[i] <- "Concha y Toro"
    wine[i] <- "Cabernet-sauvignon"
  }
  else if   (data[i,2] == "T_CCMS") {
    Wineyard[i] <- "Tocornal"
    wine[i] <- "Mix"
  }
  else if   (data[i,2] == "T_CCMS") {
    Wineyard[i] <- "Tocornal"
    wine[i] <- "Mix"
  }
  else if   (data[i,2] == "TA_CF") {
    Wineyard[i] <- "Tarapaca"
    wine[i] <- "Cabernet Franc"
  }
  else if   (data[i,2] == "TA_M") {
    Wineyard[i] <- "Tarapaca"
    wine[i] <- "Merlot"
  }
  else if   (data[i,2] == "TA_SYRAH") {
    Wineyard[i] <- "Tarapaca"
    wine[i] <- "SYRAH"
  }
  else if   (data[i,2] == "U") {
    Wineyard[i] <- "Undurraga"
    wine[i] <- "Cabernet-sauvignon"
  }
  else if   (data[i,2] == "U_CM") {
    Wineyard[i] <- "Undurraga"
    wine[i] <- "Carmenere"
  }
  else if   (data[i,2] == "U_CM_") {
    Wineyard[i] <- "Undurraga"
    wine[i] <- "Carmenere"
  }
  else if   (data[i,2] == "VASCOS") {
    Wineyard[i] <- "VASCOS"
    wine[i] <- "Cabernet-sauvignon"
  }
  else {
    Wineyard[i] <- "San Pedro" 
    wine[i] <- "Cabernet-sauvignon"
  }
}

cbind(Wineyard,wine)

# join the new columns

data2 <- cbind(data[,1],Wineyard,wine,data[,3:5746])
data2[,1:3]
colnames(data2)
# save new data
write.csv(data2, file = "DataOrganized.csv", sep = ",")

# @Autor: Ana Maria Sandoval Jimenez

setwd("~/DS2")

# Load data
rawdata <- read.delim("300_wine_pos.txt")
data <- read.csv("wine_transformed.csv", sep = ",")
data2 <- read.csv("DataOrganized.csv", sep = ",")
data2 <- data2[,2:5748]
wine <- data2

# first look
head(data)
colnames(data)
rownames(data)
data$name
dim(data2)
length(colnames(data)[7:5746])

#### Relative abundance

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
### Add back the time column from the rawdata
CsauvignonTrans <- t(Csauvignon)
rownames(CsauvignonTrans)[8:5747] <- rawdata$Time


par(mfrow= c(3,1), mar=c(2,4,2,1))
plot(t(Csauvignon)[-c(1:7,301:5747),1], type = "l", main = "Low Quality",
     ylab= "MZ")
plot(t(Csauvignon)[-c(1:7,301:5747),24], type = "l", main = "Regular Quality",
      ylab= "MZ")
plot(t(Csauvignon)[-c(1:7,301:5747),148], type = "l", main = "High Quality",
      ylab= "MZ")



## Carmenere ##

par(mfrow= c(2,1), mar=c(2,4,2,1))
plot(t(Carmenere)[-c(1:7,301:5747),1], type = "l", main = "Regular Quality",
     ylab= "MZ", axes = FALSE)
Axis(side=1, labels=FALSE)
Axis(side=2, labels=FALSE)
plot(t(Carmenere)[-c(1:7,301:5747),20], type = "l", main = "Low Quality",
     ylab= "MZ", axes = FALSE)
Axis(side=1, labels=FALSE)
Axis(side=2, labels=FALSE)

## CabernetF Merlot SYRAH##

par(mfrow= c(3,1), mar=c(2,4,2,1))
plot(t(CabernetF)[-c(1:7,301:5747),1], type = "l", main = "Cabernet Franc",
     ylab= "MZ")
plot(t(Merlot)[-c(1:7,301:5747),1], type = "l", main = "Merlot",
     ylab= "MZ")
plot(t(SYRAH)[-c(1:7,301:5747),1], type = "l", main = " SYRAH",
     ylab= "MZ")


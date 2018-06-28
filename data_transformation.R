########WINE#############
#
# Description of Column names:
# MZ = mass to charge ratio (weight of metabolite)
# time = retention time from the mass spectometry (irrelevant)
# 
# Wine columns like II_R_VASCOS_06_POS_5:
# II = wine
# R = quality in P (premium), R (regular), V (Low)
# VASCOS = Name of the wine (VASCOS, VSP, CT, U, A_SYRAH, TA_SYRAH, T_MCSS, A_CCM, A_CARM, TA_CF, T_CCMS, A, U_CM)
# 06 = Year of the wine (04, 05, ..., 08)
# POS = mode of measurement (positive or negative) (irrelevant)
# 5 = bottle number (each wine has been measured several times)

setwd("C:/Users/Jannis/Desktop/DataViz/2project")
data <- read.delim("300_Wine_pos.txt")
wine_data <- data[,4:303]

# exploring column names and how to access them
#colnames(wine_data,1)
#cols <- colnames(wine_data)
#which(substr(cols, 4 ,4)=="V")

# build column for quality 
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
t_wine_data[1:5,1:5]

wine <- cbind(name, year, posneg, bottlenum, quality, t_wine_data)
# check final frame
wine[1:5,1:10]
save(wine, file="wine.Rda")

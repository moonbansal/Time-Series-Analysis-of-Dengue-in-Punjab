#packages
library(GGally)
library(stats)
library(dplyr)
library(tidyverse)
library(seasonal)

# Making a function to make a scatter plot of cases and factors
# The function also gives a 6x6 matrix of correlations between cases and the factors
corrf <- function(df) {
  plot(df[ ,3:8])
  correlations <- round(cor(df[,3:8]), digit=2)
  print(correlations)
  GGally::ggpairs(df[,3:8])
}

# SBS Nagar data from 2018 - 2021
sbsnagar <- read.csv("sbsnagar.csv")
summary(sbsnagar)
corrf(sbsnagar)

# Amritsar data
amritsar <- read.csv("amritsar.csv")
summary(amritsar)
corrf(amritsar)

# Gurdaspur data 
gurdaspur <- read.csv("gurdaspur.csv")
gurdaspur1 <- na.omit(gurdaspur) # 2 rows containing NA values removed
summary(gurdaspur1)
corrf(gurdaspur1)

# Jalandhar data
jalandhar <- read.csv("jalandhar.csv")
summary(jalandhar)
corrf(jalandhar)

# S.A.S. Nagar data
sasnagar <- read.csv("sasnagar.csv")
summary(sasnagar)
corrf(sasnagar)

# Shir Muktsar data
shirmuktsar <- read.csv("shirmuktsar.csv")
summary(shirmuktsar)
corrf(shirmuktsar)

# Lagged correlations of cases vs climate factors in all the regions

lagcorrf <- function(factor1, factor2, title) {
  lagcorr <- ccf(factor1, factor2, lag.max=10, plot=FALSE)
  plot(lagcorr, main=title)
  axis(side = 1, at = seq(-10, 10, by = 2))
  print(lagcorr)
}

# SBS Nagar

lagcorrf(sbsnagar$Cases, sbsnagar$BI, "SBS Cases vs BI")
lagcorrf(sbsnagar$Cases, sbsnagar$T.max, "SBS Cases vs T max")
lagcorrf(sbsnagar$BI, sbsnagar$T.max, "SBS BI vs T max")
lagcorrf(sbsnagar$Cases, sbsnagar$T.Min, "SBS Cases vs T min")
lagcorrf(sbsnagar$Cases, sbsnagar$RH, "SBS Cases vs RH")
lagcorrf(sbsnagar$Cases, sbsnagar$Rainfall, "SBS Cases vs Rainfall")

# Amritsar

lagcorrf(amritsar$Cases, amritsar$BI, "Amritsar Cases vs BI")
lagcorrf(amritsar$Cases, amritsar$T.max, "Amritsar Cases vs T max")
lagcorrf(amritsar$Cases, amritsar$T.Min, "Amritsar Cases vs T min")
lagcorrf(amritsar$Cases, amritsar$RH, "Amritsar Cases vs RH")
lagcorrf(amritsar$Cases, amritsar$Rainfall, "Amritsar Cases vs Rainfall")

# Gurdaspur

lagcorrf(gurdaspur1$Cases, gurdaspur1$BI, "Gurdaspur Cases vs BI")
lagcorrf(gurdaspur1$Cases, gurdaspur1$T.max, "Gurdaspur Cases vs T max")
lagcorrf(gurdaspur1$Cases, gurdaspur1$T.Min, "Gurdaspur Cases vs T min")
lagcorrf(gurdaspur1$Cases, gurdaspur1$RH, "Gurdaspur Cases vs RH")
lagcorrf(gurdaspur1$Cases, gurdaspur1$Rainfall, "Gurdaspur Cases vs Rainfall")

# Jalandhar

lagcorrf(jalandhar$Cases, jalandhar$BI, "Jalandhar Cases vs BI")
lagcorrf(jalandhar$Cases, jalandhar$T.max, "Jalandhar Cases vs T max")
lagcorrf(jalandhar$Cases, jalandhar$T.Min, "Jalandhar Cases vs T min")
lagcorrf(jalandhar$Cases, jalandhar$RH, "Jalandhar Cases vs RH")
lagcorrf(jalandhar$Cases, jalandhar$Rainfall, "Jalandhar Cases vs Rainfall")

# SAS Nagar
lagcorrf(sasnagar$Cases, sasnagar$BI, "SAS Cases vs BI")
lagcorrf(sasnagar$Cases, sasnagar$T.max, "SAS Cases vs T max")
lagcorrf(sasnagar$Cases, sasnagar$T.Min, "SAS Cases vs T min")
lagcorrf(sasnagar$Cases, sasnagar$RH, "SAS Cases vs RH")
lagcorrf(sasnagar$Cases, sasnagar$Rainfall, "SAS Cases vs Rainfall")

# Shir muktsar

lagcorrf(shirmuktsar$Cases, shirmuktsar$BI, "Shir muktsar Cases vs BI")
lagcorrf(shirmuktsar$Cases, shirmuktsar$T.max, "Shir muktsar Cases vs T max")
lagcorrf(shirmuktsar$Cases, shirmuktsar$T.Min, "Shir muktsar Cases vs T min")
lagcorrf(shirmuktsar$Cases, shirmuktsar$RH, "Shir muktsar Cases vs RH")
lagcorrf(shirmuktsar$Cases, shirmuktsar$Rainfall, "Shir muktsar Cases vs Rainfall")

# TIME SERIES PLOTS ==============

ts_sbsmain <- ts(sbsnagar[,3:5], frequency = 12, start=2018, end=2021)
ts_sbs2 <- ts(sbsnagar[,5:8], frequency = 12, start=2018, end=2021)

plot(ts_sbsmain, xlab ="Monthly Data (2018-2021)",
     main ="SBS Nagar",
     col.main ="darkgreen")

#axis(1, at = seq(as.Date("2018-01-01"), 
#                 as.Date("2021-12-01"), by = "month")
#     )

tsp <- tsp(ts_sbsmain)
months <- format(tsp[1:2], "%b")

sbsnagar$Months <- as.Date(sbsnagar$Months,
                               format = "%m")

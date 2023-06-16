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
lagcorrf <- function(data, cfactor) {
  lagcorr <- ccf(data$Cases, cfactor, lag.max=10, plot=TRUE, main="cases vs factor")
  print(lagcorr)
}

# SBS Nagar
lagcorrf(sbsnagar, sbsnagar$BI)
lagcorrf(sbsnagar, sbsnagar$T.max)

# Finding correlation bw BI and T.max to understand the effect
print(ccf(sbsnagar$BI, sbsnagar$T.max, lag.max=10, plot=TRUE, main="cases vs factor"))

lagcorrf(sbsnagar, sbsnagar$T.Min)
lagcorrf(sbsnagar, sbsnagar$RH)
lagcorrf(sbsnagar, sbsnagar$Rainfall)

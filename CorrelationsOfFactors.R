sbsnagar <- read.csv("sbsnagar.csv")
summary(sbsnagar)

plot(sbsnagar[, 3:8])
correlations <- round(cor(sbsnagar[,3:8]), digit=2)
correlations

GGally::ggpairs(sbsnagar[,3:8])

amritsar <- read.csv("amritsar.csv")

corrf <- 
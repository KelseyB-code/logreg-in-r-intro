## LOADING DATA

install.packages("ISLR")
library(ISLR)

# EXPLORING DATA

names(Smarket)
head(Smarket)
summary(Smarket)

# VISUALIZING DATA

#histogram to visualize distribution
par(mfrow=c(1,8), #par() combines multiple plots into one graph
    mar=c(1,1,1,1)) #reset margins with mar=c()
for(i in 1:8) {
  hist(Smarket[,i], main=names(Smarket)[i])
}


#box and whisker plots to visualize distribution + outliers
par(mfrow=c(1,8))
for(i in 1:8) {
  boxplot(Smarket[,i], main=names(Smarket)[i])
}

#look at missing data
library(Amelia)
library(mlbench)
missmap(Smarket, 
        col=c("blue", "red"), 
        legend=FALSE,
        mar=c(3,4))

#calculate pairwise correlation between numeric variables
library(corrplot)
correlations <- cor(Smarket[1:8])
corrplot(correlations, method="circle")

pairs(Smarket, col=Smarket$Direction)

library(caret)
x <- Smarket[,1:8]
y <- Smarket[,9]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
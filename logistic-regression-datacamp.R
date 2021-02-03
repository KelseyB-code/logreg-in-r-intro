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

## BUILDING LOGISTIC REGRESSION MODEL

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Smarket, 
               family = binomial)

summary(glm.fit)

glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:5]

glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred == Direction)

## CREATING TRAINING AND TEST SAMPLES

train = Year<2005 #true for years less than 2005

#refit model with glm.fit with subset=train
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, 
               family = binomial,
               subset = train)


glm.probs <- predict(glm.fit, 
                     newdata = Smarket[!train,],
                     type = "response")

glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

Direction.2005 = Smarket$Direction[!train]
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

## SOLVING OVERFITTING

#fit a smaller model - use only Lag1, Lag2, Lag3 as predictors + leave out others
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, newdata = Smarket[!train,], type = "response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

summary(glm.fit)

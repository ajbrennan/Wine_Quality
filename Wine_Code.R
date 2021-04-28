library(dplyr)
library(corrplot)
library(leaps)
library(caret)
library(randomForest)
library(rpart)

#### Change source for dataset below ####
wine <- read.csv("/Users/anthonybrennan/Documents/Trimedx/wine_data.csv", header = TRUE)

wine2 <- wine # Duplicate dataset for regression

head(wine)
str(wine) # Exploring the data
summary(wine) # Exploring the data
wine$quality <- as.factor(wine$quality) # Changed to factor for classification

# Random forest classification model

set.seed(100)
train.sample <- sample(1:nrow(wine), (nrow(wine)/4), replace=FALSE) # Creating training and testing data
training <- wine[train.sample,]
testing <- wine[-train.sample,]

rf <- randomForest(quality ~ ., data=training, ntree = 1000) # Random Forest model from random forest package
rf # Results on training
testing$prediction <- predict(rf, newdata=testing)

table(testing$quality, testing$prediction) # Confusion matrix
nrow(filter(testing, quality == prediction)) / nrow(testing) # Accuracy
#~62%
#~97% of predictions off by only 1 

importance <- importance(rf) # Variable importance
importance



# Regression model

training2 <- wine2[train.sample,] # Creating training and testing data
testing2 <- wine2[-train.sample,] # (same sample, just using numeric instead of factor for quality)

sub.sel <- regsubsets(quality ~ ., data = wine2, nbest = 5) # Subset selection to find most important variables
plot(sub.sel,scale = "Cp") # AIC
plot(sub.sel,scale = "bic") # BIC
plot(sub.sel,scale = "adjr2") # Adjusted R-squared

corwine <- cor(wine2) # Looking for co-linear relationships
corrplot(corwine[,c(12,1:11)])  # The 3 acids and pH are decently to strongly correlated (which makes sense), 
                                # The sulfer dioxides are also strongly correlated

lm1 <- lm(quality ~ ., training2) # Model with all variables
summary(lm1)

lm2 <- lm(quality ~ volatile.acidity+chlorides+free.sulfur.dioxide
          +total.sulfur.dioxide+pH+sulphates+alcohol, training2) # Model with statistically significant variables
summary(lm2)
plot(lm2) # Plot to make sure the model follows basic assumptions




testing2$prediction <- predict(lm2, testing2)
testing2$rounded <- round(testing2$prediction)

table(testing2$quality, testing2$rounded) # Confusion matrix of rounded predictions
nrow(filter(testing2, quality == rounded)) / nrow(testing2) # Accuracy
#60%



# Decision tree

training3 <- wine[train.sample,] # Creating training and testing data
testing3 <- wine[-train.sample,] # (same sample, just using factor for quality like the rf)

dt <- train(quality ~ ., data = training3, method = "rpart")
testing3$prediction <- predict(dt, testing3)

table(testing3$quality, testing3$prediction) # Confusion matrix of predictions
nrow(filter(testing3, quality == prediction)) / nrow(testing3) # Accuracy
#55.25%



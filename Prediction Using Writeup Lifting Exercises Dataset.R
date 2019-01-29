## work dir
path <- getwd()

## url training data file
url_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

file_train <- "pml-training.csv"

if(!file.exists(file_train)){
	print("train_file_downloaded")
	download.file(url_train, file_train, method = "curl")
}

## url test data file
url_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

file_test <- "pml-testing.csv"

if(!file.exists(file_test)){
	print("test_file_downloaded")
	download.file(url_test, file_test, method = "curl")
}

## setup
library(lattice)
library(ggplot2)
library(plyr)
library(randomForest)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(corrplot)
library(RColorBrewer)

## executive summary


## processing
training <- read.csv("pml-training.csv")
testing <-read.csv("pml-testing.csv")

## exploratory data analyses
#dim(training)  #if necessary

#head(training) #if necessary

str(training)

#summary(training) #if necessary

## create a partition with the training dataset
in_train <- createDataPartition(training$classe, p = 0.75, list = FALSE)
train_set <- training[in_train, ]
test_set <- training[-in_train, ]
#dim(train_set)

#dim(test_set)

## remove variables with NA variance
nvz <- nearZeroVar(train_set)
train_set <- train_set[, -nvz]
test_set <- test_set[, -nvz]
#dim(train_set)

#dim(test_set)

## remove variables mostly NA
all_na <- sapply(train_set, function(x) mean(is.na(x))) > 0.95
train_set <- train_set[, all_na==FALSE]
test_set <- test_set[, all_na==FALSE]
#dim(train_set)

#dim(test_set)

## remove identification only variables
train_set <- train_set[, -(1:5)]
test_set <- test_set[, -(1:5)]
#dim(train_set)

#dim(test_set)

## correlation analysis
library(ggcorrplot)
cor_matrix <- cor(train_set[, -54])
ggcorrplot(cor_matrix, type = "lower",
	colors = c("purple", "white", "blue"))

#############################################################################

library(Rmisc)
library(ggplot2)
library(RColorBrewer)

g1 <- ggplot(train_set, aes(classe, pitch_forearm)) +
	geom_boxplot(aes(fill = classe), col = "darkred", fill = "red") +
	scale_fill_brewer(palette="RColorBrewer") +
 	theme_dark()

g2 <- ggplot(train_set, aes(classe, magnet_arm_x)) +
	geom_boxplot(aes(fill = classe), col = "blue", fill = "lightblue") +
	scale_fill_brewer(palette="Blues") +
	theme_dark()

multiplot(g1, g2, cols = 2)

#############################################################################

## method random forest
set.seed(1)
controlRF <- trainControl(method = "cv", number = 3, verboseIter = FALSE)
modFitRandForest <- train(classe ~ ., data = train_set, method = "rf",
	trControl = controlRF)
modFitRandForest$finalModel

## prediction on test data
predictRandForest <- predict(modFitRandForest, newdata = test_set)
confMatRandForest <- confusionMatrix(predictRandForest, test_set$classe)
confMatRandForest

## plot matrix results
plot(confMatRandForest$table, col = confMatRandForest$byClass,
	color = c("red", "green", "yellow", "violet", "blue"),
	main = paste("Random Forest - Accuracy = ",
		round(confMatRandForest$overall['Accuracy'], 4)))

###############################################################################

## method decision trees
# modl fit
set.seed(1)
mod_fit_dec_tree <- train(classe ~., data = train_set, method = "rpart")
fancyRpartPlot(mod_fit_dec_tree$finalModel)

pv <- predict(mod_fit_dec_tree, newdata = test_set)
cm_ct <- confusionMatrix(pv, test_set$classe)
cm_ct$cm_ct$overall['Accuracy']

cm_ct

plot(cm_ct$table, col = cm_ct$byClass,
	color = c("red", "green", "yellow", "violet", "blue"),
	main = paste("Decision Trees - Accuracy = ",
		round(cm_ct$overall['Accuracy'], 4)))

########################################################################

## support vector machine

library(dplyr)
library(e1071)

mod_train_svm <- svm(classe ~., data = train_set)
mod_predict_svm <- predict(mod_train_svm, test_set)
cm_svm <- confusionMatrix(mod_predict_svm, test_set$classe)
cm_svm

plot(cm_svm$table, col = cm_svm$byClass,
	color = c("red", "green", "yellow", "violet", "blue"),
	main = paste("Support Vector Machine - Accuracy = ",
		round(cm_svm$overall['Accuracy'], 4)))
########################################################################

predict_test <- predict(modFitRandForest, newdata = testing)
predict_test
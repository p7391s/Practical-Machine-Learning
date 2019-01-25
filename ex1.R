##library(data.table)
##library(httr)


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

## executive summary


## processing
training_raw <- read.csv("pml-training.csv")
testing_raw <-read.csv("pml-testing.csv")

## exploratory data analyses
dim(training_raw)

head(training_raw)

str(training_raw)

summary(training_raw)

## a lot of data with NA
max_na_pers = 20
max_na_count <- nrow(training_raw) / 100 * max_na_pers
remove_columns <- which(colSums(is.na(training_raw) | training_raw=="") > max_na_count)
training_cleaned_01 <- training_raw[,-remove_columns]

##dim(training_cleaned_01)

testing_cleaned_01 <- testing_raw[,-remove_columns]

## remove all time related data
remove_columns <- grep("timestamp", names(training_cleaned_01))
training_cleaned_02 <- training_cleaned_01[,-c(1, remove_columns)]

testing_cleaned_02 <- testing_cleaned_01[,-c(1, remove_columns)]

## convert all factors to integers
classe_levels <- levels(training_cleaned_02$classe)
training_cleaned_03 <- data.frame(data.matrix(training_cleaned_02))
training_cleaned_03$classe <- factor(training_cleaned_03$classe, labels = classe_levels)

testing_cleaned_03 <- data.frame(data.matrix(testing_cleaned_02))

## dataset to be explored
training_cleaned <- training_cleaned_03
testing_cleaned <- testing_cleaned_03

## exploratory data analyses
set.seed(19791108)
library(caret)

classe_index <- which(names(training_cleaned) == "classe")

partition <- createDataPartition(y=training_cleaned$classe, p = 0.75, list = FALSE)

training_sub_set_train <- training_cleaned[partition, ]
training_sub_set_test <- training_cleaned[-partition, ]

## high correlations with the classe
correlations <- cor(training_sub_set_train[, -classe_index], as.numeric(training_sub_set_train$classe))
best_correlations <- subset(as.data.frame(as.table(correlations)), abs(Freq)>0.3)
best_correlations

## check visually
library(Rmisc)
library(ggplot2)

g1 <- ggplot(training_sub_set_train, aes(classe, pitch_forearm)) +
	geom_boxplot(aes(fill = classe), col = "darkred", fill = "red") +
	scale_fill_brewer(palette="RColorBrewer") +
 	theme_dark()

g2 <- ggplot(training_sub_set_train, aes(classe, magnet_arm_x)) +
	geom_boxplot(aes(fill = classe), col = "blue", fill = "lightblue") +
	scale_fill_brewer(palette="Blues") +
	theme_dark()

multiplot(g1, g2, cols = 2)

## model selection
library(corrplot)
correlation_matrix <- cor(training_sub_set_train[, -classe_index])
highly_correlated <- findCorrelation(correlation_matrix, cutoff = 0.9, exact = TRUE)
exclude_columns <- c(highly_correlated, classe_index)
corrplot(correlation_matrix, metod = "color", type = "lower", order = "hclust", tl.cex = 0.70, tl.col = "black", tl.str = 45, diag = FALSE)


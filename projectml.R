setwd("C:/Users/logo403/Documents/R/work/ml/project")

suppressWarnings(suppressMessages(library(caret)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(rattle)))


#Read files
train0 <- read.csv("pml-training.csv")
test0 <- read.csv("pml-testing.csv")

#Data cleaning
# Keep only non empty features in the testing file
allvar <- names(test0)
vBeg <- c("kurt","skew","max","min","ampl","var","avg","std","_x","_y","_z")
va <- vector()
for(i in 1:length(vBeg)){ va <- c(va,grep(vBeg[i],allvar,value = FALSE))}
train <- train0[,-va]
test <- test0[,-va]

# Remove all identifiers
train <- select(train,-c(1:7))
test <- select(test,-c(1:7))


# Data splitting/slicing
set.seed(123)
inTrain <- createDataPartition(y=train$classe,
                               p=0.7, list=FALSE)
training <- train[inTrain,]
testing <- train[-inTrain,]


# Cross validation
cvCtrl <- trainControl(method = "repeatedcv", repeats = 1,
                        classProbs = TRUE)

# Train three models: tree, forest, svm
set.seed(123)
Fitrpart <- train(classe ~ ., 
                    data = training, 
                    method = "rpart",
                    trControl = cvCtrl)
Predrpart <- predict(Fitrpart,testing)

set.seed(321)
Fitrf <- train(training$classe ~ ., 
                  data = training, 
                  method = "rf",
                  trControl = cvCtrl)
Predrf <- predict(Fitrf,testing)

# Models performance
Accuracyrpart <- confusionMatrix(Predrpart, testing$classe)$overall[1]
cMrpart <- confusionMatrix(Predrpart, testing$classe)$table

Accuracyrpart
cMrpart

Accuracyrf <- confusionMatrix(Predrf, testing$classe)$overall[1]
cMrf <- confusionMatrix(Predrf, testing$classe)$table


#Empirical match: method direct
predRight <- inner_join(unique(train0[,c("num_window","classe")]),
                        test0,by="num_window") %>% 
        select(problem_id,classe,num_window) %>% arrange(problem_id)

combPred <- predict(Fitrf,test0)
combPred
confusionMatrix(combPred, predRight$classe)





#Data exploration

fancyRpartPlot(Fitrpart$finalModel)


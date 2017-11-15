#Installing the required packages
install.packages("rpart")
install.packages("rpart.plot")
install.packages("Rweka")

#Loading libraries
library(rpart)
library(rpart.plot)
library(RWeka)
library(ggplot2)

#Setting the path to read pre processed data
setwd("C://Users//kamal//Desktop//ADS//Final_Midterm_Prudential_Insurance//Desicion tree regression")

#Verifying the directory path
getwd()

#Reading preprocessed data
Prud_dataset <- read.csv("train_Cleaned.csv")
Prud_dataset$Product_Info_2 <- NULL
Prud_dataset$X <- NULL
#Retrieving Coloumn details
str(Prud_dataset)

#viewing Prudential data summary
summary(Prud_dataset)
nrow(Prud_dataset)
ncol(Prud_dataset)

#Histogram plot to view response
Response <- Prud_dataset$Response
hist(Response, col="red", main="Histogram of Responses")

#Also trying to view responses and their frequencies using Piechart plot 
Response <- table(Prud_dataset$Response)
lbls <- paste(names(Response), "\n", Response, sep="")
pie(Response, labels = lbls,  col=rainbow(length(lbls)),
    main="Piechart of responses")

#spliting the dataset in 80:20 training and testing data sets
train_data <- Prud_dataset[1:47505, ]
test_data <- Prud_dataset[47506:59381, ]

#Building the model on training data set
#Here cp is complexity parameter we've choosen cp=0.01 to get decent tree size
model_1 <- rpart(train_data$Response~., data = train_data, 
               method = "anova", control=rpart.control(minsplit=35, cp=0.001))
summary(model_1)

model_2 <- rpart(train_data$Response~., data = train_data, 
                 method = "anova", control=rpart.control(minsplit=65, cp=0.01))
summary(model_2)

model_3 <- rpart(train_data$Response~., data = train_data, 
                 method = "anova", control=rpart.control(minsplit=100, cp=0.01))
summary(model_3)

# Predicting model_3 on testing data
prediction <- predict(model_3, test_data)
#View(round(prediction))
# Confusion matrix
pred_cm <- table(prediction,test_data$Response)
pred_cm

#Mainly we do this steps for classification rather than prediction
#Calculating accuracy 
acc <- sum(diag(pred_cm))/sum(pred_cm)
accuracy <- acc * 100
accuracy
# Accuracy is 36.09801%

#Calculating correlation
summary(round(prediction))
summary(test_data$Response)
cor(prediction,test_data$Response)
#correlation is found to be 0.4911

#Visualizing model_3
rpart.plot(model_3, digits=4, box.palette = "BuBn")


#measuring performance using mae (mean absolute error)
mae <- function(actual, prediction) {
  mean(abs(actual - prediction))
}
mae(test_data$Response, round(prediction))
# 1.733833
mean_res <- mean(train_data$Response)
#mean_res= 5.6348
mae(mean_res, test_data$Response)
# 2.04
#mean square error
mse <- mean((test_data$Response-prediction)^2)
#mean square error = 4.539


#Improving the model performance using rweka
model <- M5P(train_data$Response~., data = train_data)
model
summary(model)
#Relative absolute error                 72.9937 %
#Root relative squared error             79.5094 %
predict <- predict(model, test_data)
summary(predict)
cor(predict, test_data$Response)
# correlation changed to 0.60
mae(test_data$Response, round(predict))
# mae=1.5041

prediction_df <- data.frame(test_data[-122],prediction)
write.csv(prediction_df, file = "decision_tree_prediction.csv", row.names = FALSE)

#Plotting Predictions histogram
Prediction_hist <- prediction_df$prediction
hist(Prediction_hist, col="blue", main="Histogram of Predictions")

#Plotting Pie chart of Predictions and it's frequncies
#Prediction_pie <- table(prediction_df$prediction)
#lbls <- paste(names(Prediction_pie), "\n", Prediction_pie, sep="")
#pie(Prediction_pie, labels = lbls,  col=rainbow(length(lbls)),
#    main="Piechart of predictions")



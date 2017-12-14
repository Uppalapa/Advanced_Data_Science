## Gayathri Devi Maganti
## NU ID: 001239550


#### Midterm Assignment-ADS ###

#install.packages("forecast")
library("forecast")
#### Linear Regression ####

prudential_data <- read.csv("C://Users//kamal//Desktop//ADS//Final_Midterm_Prudential_Insurance//Desicion tree regression//train_Cleaned.csv", header=T)
#View(prudential_data)
as.data.frame(colnames(prudential_data))
train <- prudential_data[-c(1,3,4,8,13,26)]
train 
str(train)


#Check the new dataset
categ.var <- c(paste("Product_Info_", c(1,5:7), sep=""),
               paste("Employment_Info_", c(3,5), sep=""),
               paste("InsuredInfo_", c(1,2,4:7), sep=""),
               paste("Insurance_History_", c(1:4,7:9), sep=""), "Family_Hist_1",
               paste("Medical_History_", c(3:9, 11:14, 16:23, 25:31, 33:41), sep=""))

categ.var

#Categ variables
data.categ <- train[categ.var]

#All other variables
data.rest <- train[colnames(train)[colnames(train)!=categ.var]]
dim(train)
dim(data.categ)
dim(data.rest)


#Create a function to convert from categorical-numeric
categoricalfunc <- function(colNames, train){
  for(level in unique(train[[colNames]])){
    train[paste(colNames,sep = "_",level)]<- ifelse(train[[colNames]] == level,1,0)
  }
  return(subset(train,select = -get(colNames)))
}

#Converting all categorical variables to numeric
for(coln in colnames(data.categ)){
  data.categ <-categoricalfunc(coln, data.categ)
}
dim(data.categ)

train <- cbind(data.categ, data.rest)
dim(train)



#Linear Regression using all the variables together
fit <- lm(train$Response~. , data = train)
summary(fit)
tmp.summary <- summary(fit)$coefficients


#Dimension reduction with p-value < 0.05
#Binding only those columns with p-value<0.05
newrows <- rownames(tmp.summary)[tmp.summary[,4]<0.05][-1]
train <- cbind(train[newrows], train['Response'])
colnames(train)
dim(train)


#Split dataset into train and test data
ind <- sample(2, nrow(train), replace=TRUE, prob = c(0.8, 0.2))
tdata <- train[ind==1,]
vdata <- train[ind==2,]
head(tdata)
head(vdata)

colnames(vdata)
#Fitting the Linear Regression model with tdata after dimension reduction
fit2 <- lm(tdata$Response~. , data = tdata)
summary(fit2)


#Predicting the response with vdata or test data
prediction<-predict(fit2, vdata) 
                   
summary(prediction)

output<-round(prediction)
head(output)

actual<-vdata$Response
head(actual)
mean(abs(vdata$Response-output)) # 1.831
mean(vdata$Response-output)^2 #MSE= 0.0002

#calculating performance metrics using acccuracy function
performance <- accuracy(prediction, vdata$Response)
performance

#Finding transpose of rows and columns of performance matrix
t(performance)


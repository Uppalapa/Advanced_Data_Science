#install required packages
install.packages('neuralnet')
install.packages("xlsx")
install.packages("caTools")
install.packages("mice")
install.packages("Metrics")


#load libraries
library("Metrics")
library("mice")
library("neuralnet")
library("XLConnect")
library("caTools")
library('stats')

setwd("C:/Users/kamal/Desktop/ADS/Assignments/Neural Networks Assignment")


train <- read.csv('train.csv')
categ.var <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
               paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
               "Family_Hist_1", paste("Medical_History_", c(2:9, 11:14, 16:23, 25:31, 33:41), sep=""))
cont.var <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
              "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
              "Family_Hist_5")
disc.var <- c( paste("Medical_History_", c(1,10,15,24,32), sep=""), paste("Medical_Keyword_", 1:48, sep=""))

train.categ <- train[categ.var]
train.cont <- train[cont.var]
train.disc <- train[disc.var]

as.matrix(apply(sapply(train.cont, is.na), 2, sum)/nrow(train.cont)) # 5 columns are missing a lot values

as.matrix(apply(sapply(train.disc[,1:5], is.na), 2, sum)/nrow(train.disc)) # 4 columns are missing almost all values

#for Medical_History_10,15,24,32 and Family_Hist_5 just remove all five columns
train.disc <- train.disc[-(2:5)]
train.cont <- train.cont[-13]

#for Employment_Info_1,4,6 and Medical_History_1 which miss less than 20% data, we can use mean or median
#I've read from kaggle posts that median works best
fill.avg <- function(l){
  l[is.na(l)] <- mean(l, na.rm=T)
}
train.cont[,6:8] <- apply(train.cont[,6:8],2,fill.avg)
train.disc[,1] <- fill.avg(train.disc[,1])

#for Insurance_History_5, Family_Hist_2,3,4, we are missing 30-60% values
#let's try multiple imputation
train <- cbind(cbind(cbind(train.categ, train.cont), train.disc), train['Response'])

imp <- mice(train)
fit<-with(imp,expr = lm(train$Response~train$Insurance_History_5+train$Family_Hist_3+train$Family_Hist_4))
fit<-with(imp,expr = lm(train$Response~train$Insurance_History_5+train$Family_Hist_2+train$Family_Hist_4))
pooled<-pool(fit)
summary(pooled)
imp_2 = complete(imp, 2)

#Reassign train data and train.cont data
train <- imp_2
cont.var <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
              "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4")
train.cont <- train[cont.var]


#Convert categorical names
categoricalfunc <- function(colNames, data){
  for(level in unique(data[[colNames]])){
    data[paste(colNames,sep = "_",level)]<- ifelse(data[[colNames]] == level,1,0)
  }
  return(subset(data,select = -get(colNames)))
}

for(coln in colnames(train.categ)){
  train.categ <-categoricalfunc(coln, train.categ)
}

colnames(train)

#Dimension reduction using linear analysis
train.clean <- train
dim(train.clean)

fit <- lm(train.clean$Response~. , data = train.clean)
tmp <- summary(fit)$coefficients

newrows <- rownames(tmp)[tmp[,4]<0.05]

train.clean <- cbind(train.clean[newrows], train.clean['Response'])
write.csv(train.clean, 'cleaned_data.csv')


options(max.print = 100000)
setwd("C:/Users/lahar/Desktop/3 sem/ADS/Proj")
data<-read.csv('cleaned_data.csv')
View(data)
set.seed(200)
split = sample.split(data$Response,SplitRatio=2/3)

train= subset(data, split==TRUE)
test= subset(data, split==FALSE)
head(test)
head(train)
str(data)

#model-1
#n <- names(train)
f# <- as.formula(paste("Response ", paste(n[!n %in% "Response"], collapse = " + "), sep=" ~ "))
#nn <- neuralnet(f,data = head(train,100),hidden=c(8,5),linear.output=T)
#model-2
nn <- neuralnet(Response ~ BMI + Medical_History_22_2 + Medical_History_4_1 + Product_Info_6_1 + Employment_Info_2_1 , data = head(train,5),hidden=10,linear.output=TRUE)


names(test)
dev.off()
par(mfrow=c(5,5), mar=numeric(4),family='serif')
plot.nnet(nn)
plot(nn)
length(nn$model.list$variables)
length(test[1,1:5])
length(test[1:5])
test_data<-cbind(test$BMI,test$Medical_History_22_2 ,test$Medical_History_4_1,test$Product_Info_6_1,test$Employment_Info_2_1)

np <- compute(nn,test_data[,1:5],rep = 1)
head(np)
np$net.result
round(np$net.result)
table(round(np$net.result),test$Response)
compute <- compute(nn, test_data[,1:5])
pred.weights <- compute$net.result
idx <- apply(pred.weights, 1, which.max)
pred <- c('1','2','3','4','5','6','7','8')[idx]
table(pred, test$Response)


rmse.nn<-rmse(as.numeric(test[,"Response"]), as.numeric(pred))
rmse.nn




library(h2o)
library(caret)
library(mxnet)

full <- read.csv(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/semeion/semeion.data", header = F, sep = " ")
data <- full[, 1:256]
label_df <- full[, 257:266]
colnames(label_df) <- seq(0, 9, 1)
#convert last 10 columns to label
data$label <- factor(as.matrix(label_df) %*% 1:ncol(label_df), labels = seq(0,9,1))

#separate into train and test
set.seed(3000)
indxTrain <- createDataPartition(y = data$label,p = 0.65,list = FALSE)
train <- data[indxTrain,]
test <- data[-indxTrain,]


##mxnet
mxnet_train <- data.matrix(train)
train_x <- t(mxnet_train[,-257])
train_y <- mxnet_train[, 257]
dim(train_x) <- c(16, 16, 1, ncol(train_x))

mxnet_test <- data.matrix(test)
test_x <- t(mxnet_test[,-257])
test_y <- mxnet_test[, 257]
dim(test_x) <- c(16, 16, 1, ncol(test_x))

#input
data <- mx.symbol.Variable('data')
#first conv
conv_1 <- mx.symbol.Convolution(data = data, kernel = c(2, 2), num_filter = 20)
act_1 <- mx.symbol.Activation(data = conv_1, act_type = "Maxout")
pool_1 <- mx.symbol.Pooling(data = act_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
#second conv
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(2, 2), num_filter = 50)
act_2 <- mx.symbol.Activation(data = conv_2, act_type = "Maxout")
pool_2 <- mx.symbol.Pooling(data = act_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
#third conv
conv_3 <- mx.symbol.Convolution(data = pool_2, kernel =c(2, 2), num_filter = 80)
act_3 <- mx.symbol.Activation(data = conv_3, act_type = "Maxout")
pool_3 <- mx.symbol.Pooling(data = act_3, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

m1.fc1 <- mx.symbol.FullyConnected(data = pool_3, name = "fc1", num_hidden = 500)
m1.act1 <- mx.symbol.Activation(m1.fc1, name = "activation1", act_type = "tanh")

m1.fc2 <- mx.symbol.FullyConnected(m1.act1, name = "fc2", num_hidden = 10)
m1.softmax <- mx.symbol.SoftmaxOutput(m1.fc1, name ="softMax")
mx.set.seed(0)

mxnet <- mx.model.FeedForward.create(m1.softmax,
                                     X = train_x,
                                     y = train_y,
                                     num.round = 150,
                                     array.batch.size = 100,
                                     learning.rate = 0.07,
                                     momentum = 0.85,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))
mxnet_output <- predict(mxnet, test_x)
mxnet_pred <- max.col(t(mxnet_output))-1
mxnet_accuracy <- mean(test_y == mxnet_pred)


#h2o
x <- names(data)[-257]
y <- "label"


#splits data into training, validation and testing
h2o.init(nthreads = -1)
splits <- h2o.splitFrame(
  data = as.h2o(train),
  ratios = 0.7,   
  seed = 1234)
training <- splits[[1]]
validation <- splits[[2]]
testing <- as.h2o(test)

hyper_params <- list(
  hidden = list(c(25,25), 
                c(100, 100),
                c(128, 128), 
                c(200, 50, 50),
                c(50, 50)),
  epochs = c(50, 150, 200), 
  l1 = seq(0,1e-3,1e-6),
  l2 = seq(0,1e-3,1e-6),
  rate = seq(0, 0.05, 0.01),
  input_dropout_ratio = c(0, 0.1, 0.2),
  max_w2=c(10, 50, 100), 
  activation=c("Rectifier"))

search_criteria = list(
  strategy = "RandomDiscrete",     
  max_runtime_secs = 3600,        
  max_models = 100,                 
  seed = 1234,                       
  stopping_rounds = 5,               
  stopping_metric = "misclassification",
  stopping_tolerance = 1e-4)

grid <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  algorithm="deeplearning",
  grid_id = "dl23334",
  training_frame = training,
  validation_frame = validation, 
  x = x, 
  y = y,
  adaptive_rate = F,
  score_validation_samples = 10000, 
  score_duty_cycle = 0.025,
  seed = 1234)  

dl_grid <- h2o.getGrid("dl23334", sort_by="accuracy", decreasing=T)
dl_grid
dl <- h2o.getModel(dl_grid@model_ids[[1]])

dl_pred <- h2o.predict(dl, testing)
dl_accuracy <- mean(dl_pred$predict == testing$label)

#compare the outcome
confusionMatrix(mxnet_pred, test_y)
confusionMatrix(as.vector(dl_pred$predict), as.vector(testing$label))


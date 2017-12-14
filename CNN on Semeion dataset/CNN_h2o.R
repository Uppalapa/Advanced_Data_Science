#Run the following commands to install and load h2o
install.packages("h2o")
library(h2o)

#Import the semeion data directly from the website 
full <- read.csv(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/semeion/semeion.data", header = F, sep = " ")
data <- full[, 1:256]
label_df <- full[, 257:266]
colnames(label_df) <- seq(0, 9, 1)
data$label <- factor(as.matrix(label_df) %*% 1:ncol(label_df), labels = seq(0,9,1))

x <- names(data)[-257]
y <- "label"

#cross validation
nfolds <- 5
fold_assignment <- "Modulo" 

#splits data into training, validation and testing
h2o.init(nthreads = -1)
splits <- h2o.splitFrame(
  data = as.h2o(data),
  ratios = c(0.6, 0.2),   
  seed = 1234)
training <- splits[[1]]
validation <- splits[[2]]
testing <- splits[[3]]

hyper_params <- list(
  hidden = list(c(25,25), 
                c(30, 30, 30), 
                c(50, 50, 50),
                c(20, 20, 20, 20)),
  epochs = c(50, 100, 200), 
  l1 = seq(0,1e-3,1e-6),
  l2 = seq(0,1e-3,1e-6),
  rho = c(0.9, 0.95, 0.99),
  epsilon = c(1e-10, 1e-8, 1e-6, 1e-4),
  input_dropout_ratio = c(0, 0.05, 0.1, 0.2),
  max_w2=c(10, 100), 
  activation=c("Tanh"))

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
  grid_id = "dl_grid",
  training_frame = training,
  validation_frame = validation, 
  x=x, 
  y=y,
  score_validation_samples = 10000, 
  score_duty_cycle = 0.025,         
  nfolds = nfolds,                           
  fold_assignment = fold_assignment,
  keep_cross_validation_predictions = TRUE,
  seed = 1234)  

dl_grid <- h2o.getGrid("dl_grid", sort_by="accuracy", decreasing=T)
dl_grid
dl <- h2o.getModel(dl_grid@model_ids[[1]])

#To make the prediction
dl_pred <- h2o.predict(dl, testing)
# To get the accuracy
dl_accuracy <- mean(dl_pred$predict == testing$label)

number = matrix(unlist(data[61, 1:256]), nrow = 16, byrow = TRUE)
image(number,col=grey.colors(255))

library('ggplot2')
library("VIM")

train <- read.csv('train.csv')
dim(train)
str(train)

#Based on Kaggle data description, there are three types of variables: categorical, continous and discrete
#Let us first seperate them
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

#Take a look at how the data looks like in each category
head(train.categ)
str(train.categ)
summary(train.categ)

head(train.cont)
str(train.cont)
summary(train.cont)

head(train.disc)
str(train.disc)
summary(train.disc)

#Detect missing values
sum(is.na(train))/(ncol(train)*nrow(train)) #0.05171885
sum(is.na(train.categ))/(ncol(train.categ)*nrow(train.categ)) #0, no missing value in categorical/nominal variables
sum(is.na(train.cont))/(ncol(train.cont)*nrow(train.cont)) #0.2162567, this is a very large portion
sum(is.na(train.disc))/(ncol(train.disc)*nrow(train.disc)) #0.07186181

#Why are there so many missing values in continuous variables...
#What's missing values' distribution?
#Which label misses most values?
as.matrix(apply(sapply(train.cont, is.na), 2, sum)/nrow(train.cont)) # 5 columns are missing a lot values
as.matrix(apply(sapply(train.disc, is.na), 2, sum)/nrow(train.disc)) # 4 columns are missing most all values, dummy variables has no missing value 
#visualize missing value pattern in columns 6-13
aggr(train.cont[,6:13],prop=FALSE,numbers=TRUE) 
matrixplot(train.cont[,6:13])

#What's the relationship between missing values and responses?
train['Response'][is.na(train['Employment_Info_4'])]
resp.factor <- as.factor(train['Response'][is.na(train['Employment_Info_4'])])
lapply(split(train['Response'][is.na(train['Employment_Info_4'])], resp.factor), length) 

#What are some other questions about missing data?


#View EVERY column
# creating multiplot fucntion
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#for train.cont, check density and draw boxplot
#density
plot.train.cont <- function(emptyList, colNum){
  pt <- ggplot(train,aes(x=train.cont[,colNum])) + geom_line(stat="density") + labs(x=colnames(train.cont)[colNum])
  emptyList[[colNum]] <- pt
  return(emptyList)
}

density_plot_list <- list()
for(i in 1:13){
  density_plot_list <- plot.train.cont(density_plot_list, i)
}
multiplot(plotlist = density_plot_list, cols = 2)

#boxplot
plot.train.cont <- function(emptyList, colNum){
  pt <- ggplot(train,aes(x=as.factor(train$Response), y=train.cont[colNum])) + geom_boxplot() + labs(x='Response', y=colnames(train.cont)[colNum])
  emptyList[[colNum]] <- pt
  return(emptyList)
}

boxplot_list <- list()
for(i in 1:13){
  boxplot_list <- plot.train.cont(boxplot_list, i)
}
multiplot(plotlist = boxplot_list, cols = 2)







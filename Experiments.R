library(timetk) 
library(tidyquant)
library(corrplot)
library(Hmisc)
library(tidyr)
library(dplyr)

options(scipen=999)

############################# Preprocessing  ############################# 

#import data
dat <- read.csv("df.csv", header = TRUE, sep = ";", na.strings = "")

#replace NA's 
dat$italyarrivals <- ifelse(is.na(dat$italyarrivals), mean(dat$italyarrivals, na.rm=TRUE), dat$italyarrivals)

#Convert dates to as.Date format
dat$DateWeek = as.Date(dat$DateWeek, format = "%d/%m/%Y")
date <- dat$DateWeek

#Rescale the Arrival data to reduce variance.
dat$Greecearrivals <- rescale((dat$Greecearrivals), to = c(0, 100))
dat$italyarrivals <- rescale((dat$italyarrivals), to = c(0, 100))

#Make df numeric
dat <- data.frame(lapply(dat[,-1], function(x) as.numeric(as.character(x))))


############################# Variable elimination ############################# 

# We will keep only the searched terms that correlated with arrival dates in Greece.

Greece.cor <- round(cor(dat),3)[,1]
Italy.cor <- round(cor(dat),3)[,2] #Just to show that the keywords we are using do not correlate arrival dates in Italy.

hist(Greece.cor,main="Google Searches vs. Greek Arrivals", 
     xlab="Pearson Value")
hist(Italy.cor,main="Google Searches vs. Italy Arrivals", 
     xlab="Pearson Value")

y<-Greece.cor[Greece.cor> 0.5]
y.labels<- as.character(names(y))
data<- dat %>% dplyr::select(one_of(y.labels))


############################# Split the data ############################# 

#take a random sample of size 70% from a dataset dat 
set.seed(123)
index <- sample(1:nrow(data),size = 0.7*nrow(data))
train <- data[index,]
test <- data[-index,]
nrow(train)
nrow(test)

#plot the splited data
group <- rep(NA,111)
group <- ifelse(seq(1,111) %in% index,"Train","Test")
df <- data.frame(date=date,Arrivals=dat$Greecearrivals)



#Plot test and train data
ggplot(df,aes(x = date,y = Arrivals, color = group)) + geom_point() +
  scale_color_discrete(name="") + theme(legend.position="top")


############################# MODEL1: Baseline ############################# 

# Baseline models and calculate the RMSE and MAE for  continuous outcome variable
best.guess <- mean(train$Greecearrivals) 
RMSE.baseline <- sqrt(mean((best.guess-test$Greecearrivals)^2))
RMSE.baseline
MAE.baseline <- mean(abs(best.guess-test$Greecearrivals))
MAE.baseline

############################# MODEL2: Multiple (log)linear regression ############################# 

lin.reg <- lm(log(Greecearrivals+1) ~., data = train)
summary(lin.reg)
test.pred.lin <- exp(predict(lin.reg,test))-1
RMSE.lin.reg <- sqrt(mean((test.pred.lin-test$Greecearrivals)^2))
MAE.lin.reg <- mean(abs(test.pred.lin-test$Greecearrivals))
RMSE.lin.reg
MAE.lin.reg


############################# MODEL3: Decision trees model ############################# 

# Create Decision trees model

library(rpart)
library(rpart.plot)

rt <- rpart(Greecearrivals ~ ., data = train)
test.pred.rtree <- predict(rt,test)
RMSE.rtree <- sqrt(mean((test.pred.rtree-test$Greecearrivals)^2))
MAE.rtree <- mean(abs(test.pred.rtree-test$Greecearrivals))

printcp(rt)
rpart.plot(rt)

############################# MODEL4: Prune tree ############################# 
#Prune tree
min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
rt.pruned <- prune(rt,cp = min.xerror)
test.pred.rtree.p <- predict(rt.pruned,test)
RMSE.rtree.pruned <- sqrt(mean((test.pred.rtree.p-test$Greecearrivals)^2))
MAE.rtree.pruned <- mean(abs(test.pred.rtree.p-test$Greecearrivals))

rpart.plot(rt.pruned)

############################# MODEL5: Random Forest ############################# 
#Random Forest
#install.packages("randomForest")
library(randomForest)

set.seed(222)

rf <- randomForest(Greecearrivals ~ ., data = train, importance = TRUE, ntree=655)
#number of trees
which.min(rf$mse)

imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

#Variable importance Plot
varImpPlot(rf,type=1,cex=0.8)

test.pred.forest <- predict(rf,test)
RMSE.forest <- sqrt(mean((test.pred.forest-test$Greecearrivals)^2))
MAE.forest <- mean(abs(test.pred.forest-test$Greecearrivals))
RMSE.forest
MAE.forest

############## MODEL6: SVM ########
library(e1071)

svm <- svm(Greecearrivals ~., data = train)
summary(svm)
test.pred.svm <- predict(svm,test)


RMSE.svm <- sqrt(mean((test.pred.svm-test$Greecearrivals)^2))
MAE.svm <- mean(abs(test.pred.svm-test$Greecearrivals))
RMSE.svm
MAE.svm

############################# PLOT ALL ############################# 

accuracy <- data.frame(Method = c("Baseline","Linear Regression","Full tree","Pruned tree","Random forest","SVM"),
                       RMSE   = c(RMSE.baseline,RMSE.lin.reg,RMSE.rtree,RMSE.rtree.pruned,RMSE.forest,RMSE.svm),
                       MAE    = c(MAE.baseline,MAE.lin.reg,MAE.rtree,MAE.rtree.pruned,MAE.forest,MAE.svm))

accuracy$RMSE <- round(accuracy$RMSE,2)
accuracy$MAE <- round(accuracy$MAE,2) 
accuracy



all.predictions <- data.frame(actual = test$Greecearrivals,
                              baseline = best.guess,
                              linear.regression = test.pred.lin,
                              full.tree = test.pred.rtree,
                              pruned.tree = test.pred.rtree.p,
                              random.forest = test.pred.forest,
                              svm = test.pred.svm)


head(all.predictions) 


library(tidyr)
all.predictions <- gather(all.predictions,key = model,value = predictions, 2:7)
head(all.predictions)
tail (all.predictions)

ggplot(data = all.predictions,aes(x = actual, y = predictions)) + 
  geom_point(colour = "blue") + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = 3, colour = "green", linetype = "dashed") +
  facet_wrap(~ model,ncol = 2) + 
  coord_cartesian(xlim = c(0,70),ylim = c(0,70)) +
  ggtitle("Predicted vs. Actual, by model")


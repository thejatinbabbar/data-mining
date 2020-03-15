# Title     : Data Mining Project
# Objective : Perform Analysis of data in R
# Created by: jatin
# Created on: 03/02/2020

X_train <- read.csv('career-con-2019/X_train.csv')
X_test <- read.csv('career-con-2019/X_test.csv')
y_train <- read.csv('career-con-2019/y_train.csv')


num_of_series <- length(unique(X_train$series_id))
num_of_series_test <- length(unique(X_test$series_id))
num_of_measurements <- length(unique(X_train$measurement_number))
num_of_groups <- length(unique(y_train$group_id))
num_of_classes <- length(unique(y_train$surface))


class_freq <- table(y_train$surface)
class_freq <- data.frame(class_freq)
# class_freq <- class_freq[order("Freq")]
par(mfrow=c(1,1),mar=c(3,11,1,1))
barplot(class_freq$Freq, names.arg = class_freq$Var1, las=2, horiz =TRUE )


serie0 <- subset(x = X_train, subset = series_id==0)
serie1 <- subset(x = X_train, subset = series_id==1)
serie2 <- subset(x = X_train, subset = series_id==2)
serie3 <- subset(x = X_train, subset = series_id==3)
par(mfrow=c(2,2),mar=c(3,5,3,3))
plot(serie0$orientation_W,type = 'l')
plot(serie1$orientation_W, type = 'l')
plot(serie2$orientation_W, type = 'l')
plot(serie3$orientation_W, type = 'l')


mean_change_abs_change <- function (vector) {
  change <- diff(vector)
  abs_change <- abs(change)
  c_a_c <- diff(abs_change)
  m_c_a_c <- mean(c_a_c)
  return(m_c_a_c)
}


funcs <- list("sum","max","min","mean","m_c_a_c")
num_of_feat <- length(funcs) * (length(colnames(X_train)) - 3)
df <- data.frame(matrix(ncol = num_of_feat, nrow = num_of_series))
df_sub <- data.frame(matrix(ncol = num_of_feat, nrow = num_of_series_test))
train_cols <- colnames(X_train)
train_cols <- train_cols[4:length(colnames(X_train))]
df_cols <- list()
for (col in train_cols) {
  for (func in funcs) {
    name <- paste(col,func,sep = "_")
    df_cols <- append(df_cols,name)
  }
}
colnames(df) <- df_cols
colnames(df_sub) <- df_cols


i <- 1
j <- 4
funcs <- list(sum,max,min,mean,mean_change_abs_change)
while (i <= num_of_feat & j <= length(colnames(X_train))) {
  for (func in funcs) {
    temp <- aggregate(X_train[[j]] ~ series_id, X_train, func)
    temp <- temp$'X_train[[j]]'
    df[[i]] <- temp
    temp <- aggregate(X_test[[j]] ~ series_id, X_test, func)
    temp <- temp$'X_test[[j]]'
    df_sub[[i]] <- temp
    i <- i + 1
  }
  j <- j + 1
}


df$target <- y_train$surface
ind <- sample(2,nrow(df),replace = TRUE,prob = c(0.7,0.3))
train <- df[ind == 1,]
test <- df[ind == 2,]


library(randomForest)
rf <- randomForest(target~.,data=train)
importance(rf)
varImpPlot(rf)
pred <- predict(rf, newdata = test,type = "class")


library(caret)
confusionMatrix(table(pred,test$target))


rf2 <- randomForest(target~.,data = df)
importance(rf2)
varImpPlot(rf2)
pred <- predict(rf2, newdata = df_sub, type = "class")


sub <- data.frame(matrix(ncol = 2, nrow = num_of_series_test))
colnames(sub) <- list("series_id","surface")
num_of_series_test <- num_of_series_test - 1
sub$series_id <- 0:num_of_series_test
sub$surface <- pred
write.csv(sub,file="sub.csv",row.names=FALSE)


par(mfrow=c(1,3),mar=c(4,4,4,4))
hist(X_train$angular_velocity_X)
hist(X_train$angular_velocity_Y)
hist(X_train$angular_velocity_Z)


par(mfrow=c(1,3),mar=c(4,4,4,4))
hist(X_train$linear_acceleration_X,breaks = 100)
hist(X_train$linear_acceleration_Y,breaks = 100)
hist(X_train$linear_acceleration_Z,breaks = 100)
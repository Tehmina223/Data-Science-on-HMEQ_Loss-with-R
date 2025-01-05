
install.packages( "randomForest" )
install.packages("gbm")
install.packages("ROCR")
library( rpart )
library( rpart.plot )
library( randomForest )
library( gbm )
library(ROCR)

SEED = 1
set.seed( SEED )

PATH <- "C:/Users/tehmi/Downloads/HMEQ_Scrubbed"
setwd(PATH)
FILE_NAME = "HMEQ_Scrubbed.csv"
INFILE = paste( PATH, FILE_NAME, sep="/" )

CSV_FILE <- "HMEQ_Scrubbed.csv"
df <- read.csv(CSV_FILE)

#Step 1
str( df )
summary( df )
head(df)

#Step 2

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.7,0.3))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

dim( df_flag )
dim( df_train )
dim( df_test )

# TREE
tr_set = rpart.control( maxdepth = 10 )
tr_model = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )
rpart.plot( tr_model )
tr_model$variable.importance

pt = predict( tr_model, df_test, type="prob" )
head( pt )
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG )
pt3 = performance( pt2, "tpr", "fpr" )

# RF
rf_model = randomForest( data=df_train, TARGET_BAD_FLAG ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head( pr )
pr2 = prediction( pr, df_test$TARGET_BAD_FLAG )
pr3 = performance( pr2, "tpr", "fpr" )

# GRADIENT BOOSTING
gb_model = gbm( data=df_train, TARGET_BAD_FLAG ~ ., n.trees=500, distribution="bernoulli" )
summary.gbm( gb_model, cBars=10 )

pg = predict( gb_model, df_test, type="response" )
head( pg )
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG )
pg3 = performance( pg2, "tpr", "fpr" )

plot( pt3, col="green" )
abline(0,1,lty=2)
legend("bottomright",c("TREE"),col=c("green"), bty="y", lty=1 )

plot( pt3, col="green" )
plot( pr3, col="red", add=TRUE )
plot( pg3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING"),col=c("green","red","blue"), bty="y", lty=1 )

aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values

print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )

#Step 2.2
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.75,0.25))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

dim( df_flag )
dim( df_train )
dim( df_test )

# TREE
tr_set = rpart.control( maxdepth = 10 )
tr_model = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )
rpart.plot( tr_model )
tr_model$variable.importance

pt = predict( tr_model, df_test, type="prob" )
head( pt )
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG )
pt3 = performance( pt2, "tpr", "fpr" )

# RF
rf_model = randomForest( data=df_train, TARGET_BAD_FLAG ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head( pr )
pr2 = prediction( pr, df_test$TARGET_BAD_FLAG )
pr3 = performance( pr2, "tpr", "fpr" )

# GRADIENT BOOSTING
gb_model = gbm( data=df_train, TARGET_BAD_FLAG ~ ., n.trees=500, distribution="bernoulli" )
summary.gbm( gb_model, cBars=10 )

pg = predict( gb_model, df_test, type="response" )
head( pg )
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG )
pg3 = performance( pg2, "tpr", "fpr" )

plot( pt3, col="green" )
abline(0,1,lty=2)
legend("bottomright",c("TREE"),col=c("green"), bty="y", lty=1 )

plot( pt3, col="green" )
plot( pr3, col="red", add=TRUE )
plot( pg3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING"),col=c("green","red","blue"), bty="y", lty=1 )

aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values

print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )

#Step 2.3
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.8, 0.2))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

dim( df_flag )
dim( df_train )
dim( df_test )

# TREE
tr_set = rpart.control( maxdepth = 10 )
tr_model = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )
rpart.plot( tr_model )
tr_model$variable.importance

pt = predict( tr_model, df_test, type="prob" )
head( pt )
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG )
pt3 = performance( pt2, "tpr", "fpr" )

# RF
rf_model = randomForest( data=df_train, TARGET_BAD_FLAG ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head( pr )
pr2 = prediction( pr, df_test$TARGET_BAD_FLAG )
pr3 = performance( pr2, "tpr", "fpr" )

# GRADIENT BOOSTING
gb_model = gbm( data=df_train, TARGET_BAD_FLAG ~ ., n.trees=500, distribution="bernoulli" )
summary.gbm( gb_model, cBars=10 )

pg = predict( gb_model, df_test, type="response" )
head( pg )
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG )
pg3 = performance( pg2, "tpr", "fpr" )

plot( pt3, col="green" )
abline(0,1,lty=2)
legend("bottomright",c("TREE"),col=c("green"), bty="y", lty=1 )

plot( pt3, col="green" )
plot( pr3, col="red", add=TRUE )
plot( pg3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING"),col=c("green","red","blue"), bty="y", lty=1 )

aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values

print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )

#Step 3
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.7,0.3))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]

mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

# TREE
tr_set = rpart.control( maxdepth = 10 )

tr_model = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( tr_model )
rpart.plot( tr_model, digits=-3, extra=100 )
tr_model$variable.importance

pt = predict( tr_model, df_test )
head( pt )
RMSEt = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pt )^2 ) )

# RF
rf_model = randomForest( data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head( pr )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )

# GRADIENT BOOSTING
gb_model = gbm( data=df_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
summary.gbm( gb_model, cBars=10 )

pg = predict( gb_model, df_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )

print( paste("TREE RMSE=", RMSEt ))
print( paste("RF RMSE=", RMSEr ))
print( paste("GB RMSE=", RMSEg ))

#Step 3.2
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.75,0.25))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]

mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

# TREE
tr_set = rpart.control( maxdepth = 10 )

tr_model = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( tr_model )
rpart.plot( tr_model, digits=-3, extra=100 )
tr_model$variable.importance

pt = predict( tr_model, df_test )
head( pt )
RMSEt = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pt )^2 ) )

# RF
rf_model = randomForest( data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head( pr )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )

# GRADIENT BOOSTING
gb_model = gbm( data=df_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
summary.gbm( gb_model, cBars=10 )

pg = predict( gb_model, df_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )

print( paste("TREE RMSE=", RMSEt ))
print( paste("RF RMSE=", RMSEr ))
print( paste("GB RMSE=", RMSEg ))

#Step 3.3
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.8,0.2))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]

mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

# TREE
tr_set = rpart.control( maxdepth = 10 )

tr_model = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( tr_model )
rpart.plot( tr_model, digits=-3, extra=100 )
tr_model$variable.importance

pt = predict( tr_model, df_test )
head( pt )
RMSEt = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pt )^2 ) )

# RF
rf_model = randomForest( data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head( pr )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )

# GRADIENT BOOSTING
gb_model = gbm( data=df_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
summary.gbm( gb_model, cBars=10 )

pg = predict( gb_model, df_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )

print( paste("TREE RMSE=", RMSEt ))
print( paste("RF RMSE=", RMSEr ))
print( paste("GB RMSE=", RMSEg ))


#Step 4

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample(c(TRUE, FALSE), nrow(df_flag), replace=TRUE, prob=c(0.7,0.3))
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]

dim(df_flag)
dim(df_train)
dim(df_test)

tr_set = rpart.control(maxdepth = 10)
tr_model = rpart(data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information'))
rpart.plot(tr_model)
tr_model$variable.importance

pt = predict(tr_model, df_test, type="prob")
head(pt)
pt2 = prediction(pt[,2], df_test$TARGET_BAD_FLAG)
pt3 = performance(pt2, "tpr", "fpr")

plot(pt3)
abline(a=0, b=1, lty=2, col="gray")

auc = performance(pt2, "auc")
auc = auc@y.values[[1]]
print(paste("AUC:", round(auc, 4))
      
      
# Filter the data to include only records where TARGET_BAD_FLAG is 1
df_loss = df[df$TARGET_BAD_FLAG == 1, ]
df_loss$TARGET_BAD_FLAG = NULL  
print(df_loss)
      
FLAG = sample(c(TRUE, FALSE), nrow(df_flag), replace=TRUE, prob=c(0.7,0.3))
df_train = df_loss[FLAG, ]
df_test = df_loss[!FLAG, ]
      
mean( df_loss$TARGET_LOSS_AMT)
mean( df_train$TARGET_LOSS_AMT)
mean( df_test$TARGET_LOSS_AMT )
      
df_flag = df
df_loss <- df[df$TARGET_BAD_FLAG == 1, ]

SEED = 1
set.seed( SEED )

FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.8,0.2))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

dim( df_flag )
dim( df_train )
dim( df_test )

# Decision Tree
tr_set = rpart.control(maxdepth = 10)
tr_model = rpart(data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova")
rpart.plot(tr_model)
print(tr_model$variable.importance)

pt = predict( tr_model, df_test )
head( pt )
RMSEt = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pt )^2 ) )

# Predictions
tr_pred = predict(tr_model, df_test)
tr_mse = mean((df_test$TARGET_LOSS_AMT - tr_pred)^2)
tr_rmse = sqrt(tr_mse)
print(paste("Decision Tree RMSE:", round(tr_rmse, 2)))  

# RF

rf_model = randomForest(data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE, na.action=na.roughfix)
rf_model = randomForest(data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE, na.action=na.omit)
install.packages("missForest")
library(missForest)
df_train_imputed = missForest(df_train)$ximp
rf_model = randomForest(data=df_train_imputed, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE)

pr = predict( rf_model, df_test )
head( pr )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )

# Predictions
rf_pred = predict(rf_model, df_test)
rf_mse = mean((df_test$TARGET_LOSS_AMT - rf_pred)^2)
rf_rmse = sqrt(rf_mse)
print(paste("Random Forest RMSE:", round(rf_rmse, 2)))


# GRADIENT BOOSTING

gb_model = gbm(TARGET_LOSS_AMT ~ ., 
               data = df_train, 
               n.trees = 500, 
               distribution = "gaussian")
summary(gb_model, cBars = 10)

pg = predict(gb_model, df_test, type = "response")
head(pg)

rmse_gb = sqrt(mean((pg - df_test$TARGET_LOSS_AMT)^2))
print(paste("GB RMSE: ", rmse_gb))

rsq_gb = 1 - sum((pg - df_test$TARGET_LOSS_AMT)^2) / sum((df_test$TARGET_LOSS_AMT - mean(df_test$TARGET_LOSS_AMT))^2)
print(paste("GB R-squared: ", rsq_gb))

df_train_clean <- df_train[!is.na(df_train$TARGET_LOSS_AMT), ]
gb_model = gbm(data=df_train_clean, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="gaussian")

df_train$TARGET_LOSS_AMT[is.na(df_train$TARGET_LOSS_AMT)] <- mean(df_train$TARGET_LOSS_AMT, na.rm=TRUE)
gb_model = gbm(data=df_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="gaussian")


pg = predict( gb_model, df_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )

# Predictions
gb_pred = predict(gb_model, df_test, n.trees=500)
gb_mse = mean((df_test$TARGET_LOSS_AMT - gb_pred)^2)
gb_rmse = sqrt(gb_mse)
print(paste("Gradient Boosting RMSE:", round(gb_rmse, 2)))      

# Compare RMSE of all models
print(paste("Decision Tree RMSE:", round(tr_rmse, 2)))
print(paste("Random Forest RMSE:", round(rf_rmse, 2)))
print(paste("Gradient Boosting RMSE:", round(gb_rmse, 2)))

# Plot actual vs predicted values for each model
par(mfrow=c(2,2))
plot(df_test$TARGET_LOSS_AMT, tr_pred, main="Decision Tree", xlab="Actual", ylab="Predicted")
abline(0,1,col="red")
plot(df_test$TARGET_LOSS_AMT, rf_pred, main="Random Forest", xlab="Actual", ylab="Predicted")
abline(0,1,col="red")
plot(df_test$TARGET_LOSS_AMT, gb_pred, main="Gradient Boosting", xlab="Actual", ylab="Predicted")
abline(0,1,col="red")


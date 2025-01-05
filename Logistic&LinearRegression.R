install.packages( "randomForest" )
install.packages("gbm")
install.packages("ROCR")
library( rpart )
library( rpart.plot )
library( randomForest )
library( gbm )
library(ROCR)
library( MASS )

SEED = 1
set.seed( SEED )

PATH <- "C:/Users/tehmi/Downloads/HMEQ_Scrubbed"
setwd(PATH)
FILE_NAME = "HMEQ_Scrubbed.csv"
INFILE = paste( PATH, FILE_NAME, sep="/" )

CSV_FILE <- "HMEQ_Scrubbed.csv"
df <- read.csv(CSV_FILE)

#Step 1

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

#Step 1.2
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

# LOGISTIC FORWARD

theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data=df_train )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_train )

summary( theUpper_LR )
summary( theLower_LR )

lr_model = stepAIC(theLower_LR, direction="forward", scope=list(lower=theLower_LR, upper=theUpper_LR))
summary( lr_model )

plr = predict( lr_model, df_test, type="response" )
plr2 = prediction( plr, df_test$TARGET_BAD_FLAG )
plr3 = performance( plr2, "tpr", "fpr" )

plot( plr3, col="gold" )
abline(0,1,lty=2)
legend("bottomright",c("LOGISTIC REGRESSION FWD"),col=c("gold"), bty="y", lty=1 )

# LR STEP TREE
treeVars = tr_model$variable.importance
treeVars = names(treeVars)
treeVarsPlus = paste( treeVars, collapse="+")
F = as.formula( paste( "TARGET_BAD_FLAG ~", treeVarsPlus ))

tree_LR = glm( F, family = "binomial", data=df_train )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_train )

summary( tree_LR )
summary( theLower_LR )

lrt_model = stepAIC(theLower_LR, direction="both", scope=list(lower=theLower_LR, upper=tree_LR))
summary( lrt_model )

plrt = predict( lrt_model, df_test, type="response" )
plrt2 = prediction( plrt, df_test$TARGET_BAD_FLAG )
plrt3 = performance( plrt2, "tpr", "fpr" )

plot( plrt3, col="gray" )
abline(0,1,lty=2)
legend("bottomright",c("LOGISTIC REGRESSION TREE"),col=c("gray"), bty="y", lty=1 )

plot( pt3, col="green" )
plot( pr3, col="red", add=TRUE )
plot( pg3, col="blue", add=TRUE )
plot( plr3, col="gold", add=TRUE ) 
plot( plrt3, col="gray", add=TRUE ) 


abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING", "LOGIT REG FWD", "LOGIT REG TREE"),col=c("green","red","blue","gold","gray"), bty="y", lty=1 )

aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values
aucLR = performance( plr2, "auc")@y.values
aucLRT = performance( plrt2, "auc")@y.values


print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )
print( paste("LR AUC=", aucLR) )
print( paste("LRT AUC=", aucLRT) )

# Step 3
str( df )
summary( df )

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

# LINEAR BACKWARDS

theUpper_LR = lm( TARGET_LOSS_AMT ~ ., data=df_train )
theLower_LR = lm( TARGET_LOSS_AMT ~ 1, data=df_train )

summary( theUpper_LR )
summary( theLower_LR )

lr_model = stepAIC(theUpper_LR, direction="backward", scope=list(lower=theLower_LR, upper=theUpper_LR))
summary( lr_model )

plr = predict( lr_model, df_test )
head( plr )
RMSElr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - plr )^2 ) )

# LR STEP TREE
treeVars = tr_model$variable.importance
treeVars = names(treeVars)
treeVarsPlus = paste( treeVars, collapse="+")
F = as.formula( paste( "TARGET_LOSS_AMT ~", treeVarsPlus ))

tree_LR = lm( F, data=df_train )
theLower_LR = lm( TARGET_LOSS_AMT ~ 1, data=df_train )

summary( tree_LR )
summary( theLower_LR )

lrt_model = stepAIC(theLower_LR, direction="both", scope=list(lower=theLower_LR, upper=tree_LR))
summary( lrt_model )


plr_tree = predict( tree_LR, df_test )
head( plr_tree )
RMSElr_tree = sqrt( mean( ( df_test$TARGET_LOSS_AMT - plr_tree )^2 ) )

plr_tree_step = predict( lrt_model, df_test )
head( plr_tree_step )
RMSElr_tree_step = sqrt( mean( ( df_test$TARGET_LOSS_AMT - plr_tree_step )^2 ) )

print( paste("TREE RMSE=", RMSEt ))
print( paste("RF RMSE=", RMSEr ))
print( paste("GB RMSE=", RMSEg ))

print( paste("LR BACK RMSE=",  RMSElr ))
print( paste("LR TREE RMSE=",  RMSElr_tree ))
print( paste("LR TREE STEP RMSE=", RMSElr_tree_step ))


----STEP 4 

# Step 4: Probability / Severity Model
df <- na.omit(df)
set.seed(SEED)
FLAG <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7, 0.3))
df_train <- df[FLAG, ]
df_test <- df[!FLAG, ]
theLower_LR <- glm(TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_train)
tree_LR <- glm(TARGET_BAD_FLAG ~ ., family = "binomial", data=df_train)
lrt_model <- stepAIC(theLower_LR, direction="both", scope=list(lower=theLower_LR, upper=tree_LR))



# Use the tree-based logistic regression model from Step 2 to predict TARGET_BAD_FLAG
prob_default <- predict(lrt_model, df_test, type="response")

# Linear regression model to predict TARGET_LOSS_AMT for records where TARGET_BAD_FLAG is 1
df_train_loss <- df_train[df_train$TARGET_BAD_FLAG == 1, ]
df_test_loss <- df_test[df_test$TARGET_BAD_FLAG == 1, ]

lr_loss_model <- lm(TARGET_LOSS_AMT ~ ., data=df_train_loss)
lr_loss_model <- stepAIC(lr_loss_model, direction="both")

# List important variables for both models
cat("Important variables for Logistic model (TARGET_BAD_FLAG):\n")
print(summary(lrt_model)$coefficients[,4])

cat("\nImportant variables for Linear Regression model (TARGET_LOSS_AMT):\n")
print(summary(lr_loss_model)$coefficients[,4])

# Predict loss given default
loss_given_default <- predict(lr_loss_model, df_test)

# Multiply probability of default and loss given default
expected_loss <- prob_default * loss_given_default

# Calculate RMSE for Probability / Severity model
RMSE_prob_severity <- sqrt(mean((df_test$TARGET_LOSS_AMT - expected_loss)^2, na.rm=TRUE))

cat("\nRMSE for Probability / Severity model:", RMSE_prob_severity, "\n")

# Compare with Step 3 models
cat("RMSE comparison:\n")
cat("Probability / Severity model RMSE:", RMSE_prob_severity, "\n")
cat("Decision Tree RMSE:", RMSEt, "\n")
cat("Random Forest RMSE:", RMSEr, "\n")
cat("Gradient Boosting RMSE:", RMSEg, "\n")
cat("Linear Regression (Backward) RMSE:", RMSElr, "\n")
cat("Linear Regression (Tree-based) RMSE:", RMSElr_tree, "\n")
cat("Linear Regression (Tree-based with stepwise) RMSE:", RMSElr_tree_step, "\n")

# Recommendation
best_model <- which.min(c(RMSE_prob_severity, RMSEt, RMSEr, RMSEg, RMSElr, RMSElr_tree, RMSElr_tree_step))
model_names <- c("Probability / Severity", "Decision Tree", "Random Forest", "Gradient Boosting", 
                 "Linear Regression (Backward)", "Linear Regression (Tree-based)", 
                 "Linear Regression (Tree-based with stepwise)")

cat("\nRecommendation:\n")
cat("Based on the RMSE values, the recommended model is:", model_names[best_model], "\n")
cat("This model provides the lowest RMSE, indicating the best predictive performance for TARGET_LOSS_AMT.")


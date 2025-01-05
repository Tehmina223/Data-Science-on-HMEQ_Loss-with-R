install.packages( "randomForest" )
install.packages("gbm")
install.packages("ROCR")
install.packages("Rtsne")
library( rpart )
library( rpart.plot )
library( randomForest )
library( gbm )
library(ROCR)
library( MASS )
library( Rtsne )

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
df_pca = df
df_pca$TARGET_BAD_FLAG = NULL
df_pca$TARGET_LOSS_AMT = NULL

pca = prcomp(df_pca,center=TRUE, scale=TRUE)
summary(pca)
plot(pca, type = "l")
df_new = predict( pca, df_pca )

df_flags = df
df_flags$PC1 = df_new[,"PC1"]
df_flags$PC2 = df_new[,"PC2"]

# This code takes a random sample of the data so that we can visualize it easier.
df_flags$RAND1 = sample(100, size = nrow(df_flags), replace = TRUE)
df_flags$RAND2 = sample(100, size = nrow(df_flags), replace = TRUE)

df_flags0 = df_flags[ which(df_flags$TARGET_BAD_FLAG == 0), ]
df_flags1 = df_flags[ which(df_flags$TARGET_BAD_FLAG == 1), ]

df_flags0 = df_flags0[ df_flags0$RAND1 < 25, ]
df_flags1 = df_flags1[ df_flags1$RAND1 < 75, ]

df_flagsx = rbind( df_flags0, df_flags1 )
df_flagsx = df_flagsx[ df_flagsx$RAND2 < 15, ]

#df_flagsx = df_flags

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_flagsx$TARGET_BAD_FLAG + 1]
plot( df_flagsx$PC1, df_flagsx$PC2, col=colors, pch=16 )

pca2 = prcomp(df_pca[,c(1,2,3,4,5,6,7,8,9,10,12,14,16,18)] ,center=TRUE, scale=TRUE)
summary(pca2)
plot(pca2, type = "l")
df_new = predict( pca2, df_pca )

df_no_flags = df
df_no_flags$PC1 = df_new[,"PC1"]
df_no_flags$PC2 = df_new[,"PC2"]
df_no_flags$PC3 = df_new[,"PC3"]
df_no_flags$PC4 = df_new[,"PC4"]

pca2
head(df_new)
head(df_no_flags)

# This code takes a random sample of the data so that we can visualize it easier.
df_no_flags$RAND1 = sample(100, size = nrow(df_no_flags), replace = TRUE)
df_no_flags$RAND2 = sample(100, size = nrow(df_no_flags), replace = TRUE)

df_no_flags0 = df_no_flags[ which(df_no_flags$TARGET_BAD_FLAG == 0), ]
df_no_flags1 = df_no_flags[ which(df_no_flags$TARGET_BAD_FLAG == 1), ]

df_no_flags0 = df_no_flags0[ df_no_flags0$RAND1 < 25, ]
df_no_flags1 = df_no_flags1[ df_no_flags1$RAND1 < 75, ]

df_no_flagsx = rbind( df_no_flags0, df_no_flags1 )
df_no_flagsx = df_no_flagsx[ df_no_flagsx$RAND2 < 15, ]

#df_no_flagsx = df_no_flags

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_no_flagsx$TARGET_BAD_FLAG + 1]
plot( df_no_flagsx$PC1, df_no_flagsx$PC2, col=colors, pch=16 )

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_no_flagsx$TARGET_BAD_FLAG + 1]
plot( df_no_flagsx$PC1, df_no_flagsx$PC3, col=colors, pch=16 )

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_no_flagsx$TARGET_BAD_FLAG + 1]
plot( df_no_flagsx$PC1, df_no_flagsx$PC4, col=colors, pch=16 )

#Step 3

dfu = df
dfu$TARGET_LOSS_AMT = NULL
dfu = unique(dfu)
head( dfu )

theTSNE = Rtsne( dfu[,c(2,3,4,5,6,7,8,9,10,11,13,15,17,19)], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col=colors, pch=16 )


P = paste(colnames(dfu)[c(2,3,4,5,6,7,8,9,10,11,13,15,17,19)], collapse = "+")
F1 = as.formula( paste("TS1 ~", P ) )
F2 = as.formula( paste("TS2 ~", P ) )

print( F1 )
print( F2 )

ts1_model = lm( F1,data=dfu )
ts2_model = lm( F2,data=dfu )

ts1_model_rf = randomForest( data=dfu, F1, ntree=500, importance=TRUE )
ts2_model_rf = randomForest( data=dfu, F2, ntree=500, importance=TRUE )


df_tsne = df

df_tsne$TS1M = predict( ts1_model, df_tsne )
df_tsne$TS2M = predict( ts2_model, df_tsne )

df_tsne$TS1M_RF = predict( ts1_model_rf, df_tsne )
df_tsne$TS2M_RF = predict( ts2_model_rf, df_tsne )


# This code takes a random sample of the data so that we can visualize it easier.
df_tsne$RAND1 = sample(100, size = nrow(df_tsne), replace = TRUE)
df_tsne$RAND2 = sample(100, size = nrow(df_tsne), replace = TRUE)

df_tsne0 = df_tsne[ which(df_tsne$TARGET_BAD_FLAG == 0), ]
df_tsne1 = df_tsne[ which(df_tsne$TARGET_BAD_FLAG == 1), ]

df_tsne0 = df_tsne0[ df_tsne$RAND1 < 25, ]
df_tsne1 = df_tsne1[ df_tsne$RAND1 < 75, ]

df_tsnex = rbind( df_tsne0, df_tsne1 )
df_tsnex = df_tsnex[ df_tsnex$RAND2 < 20, ]

#df_tsnex = df_tsne

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_tsnex$TARGET_BAD_FLAG + 1]
plot( df_tsnex$TS1M, df_tsnex$TS2M, col=colors, pch=16 )

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_tsnex$TARGET_BAD_FLAG + 1]
plot( df_tsnex$TS1M_RF, df_tsnex$TS2M_RF, col=colors, pch=16 )

#Step 3.2

dfu = df
dfu$TARGET_LOSS_AMT = NULL
dfu = unique(dfu)
head( dfu )

theTSNE = Rtsne( dfu[,c(2,3,4,5,6,7,8,9,10,11,13,15,17,19)], dims = 2, perplexity=50, verbose=TRUE, max_iter = 500)

dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col=colors, pch=16 )


P = paste(colnames(dfu)[c(2,3,4,5,6,7,8,9,10,11,13,15,17,19)], collapse = "+")
F1 = as.formula( paste("TS1 ~", P ) )
F2 = as.formula( paste("TS2 ~", P ) )

print( F1 )
print( F2 )

ts1_model = lm( F1,data=dfu )
ts2_model = lm( F2,data=dfu )

ts1_model_rf = randomForest( data=dfu, F1, ntree=500, importance=TRUE )
ts2_model_rf = randomForest( data=dfu, F2, ntree=500, importance=TRUE )


df_tsne = df

df_tsne$TS1M = predict( ts1_model, df_tsne )
df_tsne$TS2M = predict( ts2_model, df_tsne )

df_tsne$TS1M_RF = predict( ts1_model_rf, df_tsne )
df_tsne$TS2M_RF = predict( ts2_model_rf, df_tsne )


# This code takes a random sample of the data so that we can visualize it easier.
df_tsne$RAND1 = sample(100, size = nrow(df_tsne), replace = TRUE)
df_tsne$RAND2 = sample(100, size = nrow(df_tsne), replace = TRUE)

df_tsne0 = df_tsne[ which(df_tsne$TARGET_BAD_FLAG == 0), ]
df_tsne1 = df_tsne[ which(df_tsne$TARGET_BAD_FLAG == 1), ]

df_tsne0 = df_tsne0[ df_tsne$RAND1 < 25, ]
df_tsne1 = df_tsne1[ df_tsne$RAND1 < 75, ]

df_tsnex = rbind( df_tsne0, df_tsne1 )
df_tsnex = df_tsnex[ df_tsnex$RAND2 < 20, ]

#df_tsnex = df_tsne

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_tsnex$TARGET_BAD_FLAG + 1]
plot( df_tsnex$TS1M, df_tsnex$TS2M, col=colors, pch=16 )

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_tsnex$TARGET_BAD_FLAG + 1]
plot( df_tsnex$TS1M_RF, df_tsnex$TS2M_RF, col=colors, pch=16 )

#Step 3.3

dfu = df
dfu$TARGET_LOSS_AMT = NULL
dfu = unique(dfu)
head( dfu )

theTSNE = Rtsne( dfu[,c(2,3,4,5,6,7,8,9,10,11,13,15,17,19)], dims = 2, perplexity=15, verbose=TRUE, max_iter = 500)

dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col=colors, pch=16 )


P = paste(colnames(dfu)[c(2,3,4,5,6,7,8,9,10,11,13,15,17,19)], collapse = "+")
F1 = as.formula( paste("TS1 ~", P ) )
F2 = as.formula( paste("TS2 ~", P ) )

print( F1 )
print( F2 )

ts1_model = lm( F1,data=dfu )
ts2_model = lm( F2,data=dfu )

ts1_model_rf = randomForest( data=dfu, F1, ntree=500, importance=TRUE )
ts2_model_rf = randomForest( data=dfu, F2, ntree=500, importance=TRUE )


df_tsne = df

df_tsne$TS1M = predict( ts1_model, df_tsne )
df_tsne$TS2M = predict( ts2_model, df_tsne )

df_tsne$TS1M_RF = predict( ts1_model_rf, df_tsne )
df_tsne$TS2M_RF = predict( ts2_model_rf, df_tsne )


# This code takes a random sample of the data so that we can visualize it easier.
df_tsne$RAND1 = sample(100, size = nrow(df_tsne), replace = TRUE)
df_tsne$RAND2 = sample(100, size = nrow(df_tsne), replace = TRUE)

df_tsne0 = df_tsne[ which(df_tsne$TARGET_BAD_FLAG == 0), ]
df_tsne1 = df_tsne[ which(df_tsne$TARGET_BAD_FLAG == 1), ]

df_tsne0 = df_tsne0[ df_tsne$RAND1 < 25, ]
df_tsne1 = df_tsne1[ df_tsne$RAND1 < 75, ]

df_tsnex = rbind( df_tsne0, df_tsne1 )
df_tsnex = df_tsnex[ df_tsnex$RAND2 < 20, ]

#df_tsnex = df_tsne

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_tsnex$TARGET_BAD_FLAG + 1]
plot( df_tsnex$TS1M, df_tsnex$TS2M, col=colors, pch=16 )

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_tsnex$TARGET_BAD_FLAG + 1]
plot( df_tsnex$TS1M_RF, df_tsnex$TS2M_RF, col=colors, pch=16 )

#Step 4 

df_pca = df
df_pca$TARGET_BAD_FLAG = NULL
df_pca$TARGET_LOSS_AMT = NULL

pca2 = prcomp(df_pca[,c(1,2,3,4,5,6,7,8,9,10,12,14,16,18)] ,center=TRUE, scale=TRUE)
summary(pca2)
plot(pca2, type = "l")
df_new = predict( pca2, df_pca )

dfu = df
dfu$TARGET_LOSS_AMT = NULL
dfu = unique(dfu)
head( dfu )

theTSNE = Rtsne( dfu[,c(2,3,4,5,6,7,8,9,10,11,13,15,17,19)], dims = 2, perplexity=100, verbose=TRUE, max_iter = 500)

dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]

P = paste(colnames(dfu)[c(2,3,4,5,6,7,8,9,10,11,13,15,17,19)], collapse = "+")
F1 = as.formula( paste("TS1 ~", P ) )
F2 = as.formula( paste("TS2 ~", P ) )

print( F1 )
print( F2 )

ts1_model_rf = randomForest( data=dfu, F1, ntree=500, importance=TRUE )
ts2_model_rf = randomForest( data=dfu, F2, ntree=500, importance=TRUE )

df_model = df
df_model$TARGET_LOSS_AMT = NULL

head( df_model )

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_model, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_model, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

rpart.plot( t1G )
rpart.plot( t1E )

t1G$variable.importance
t1E$variable.importance


theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data=df_model )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_model )

summary( theUpper_LR )
summary( theLower_LR )

lr_model = stepAIC(theLower_LR, direction="forward", scope=list(lower=theLower_LR, upper=theUpper_LR))
summary( lr_model )

pG = predict( t1G, df_model )
pG2 = prediction( pG[,2], df_model$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_model )
pE2 = prediction( pE[,2], df_model$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plr = predict( lr_model, df_model, type="response" )
plr2 = prediction( plr, df_model$TARGET_BAD_FLAG )
plr3 = performance( plr2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
plot( plr3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","ENTROPY","REGRESSION"),col=c("red","green","blue"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values
aucR = performance( plr2, "auc" )@y.values

print( aucG )
print( aucE )
print( aucR )

# Decision Tree
df_train = df
tr_set = rpart.control(maxdepth = 10)
tr_model = rpart(data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information'))
rpart.plot(tr_model)
print(tr_model$variable.importance)

# Logistic Regression with Stepwise Selection
full_model = glm(TARGET_BAD_FLAG ~ ., data=df_train, family=binomial)
step_model = step(full_model, direction="both")
summary(step_model)

# Predictions for Decision Tree
pt = predict(tr_model, df_test, type="prob")
pt2 = prediction(pt[,2], df_test$TARGET_BAD_FLAG)
pt3 = performance(pt2, "tpr", "fpr")

# Predictions for Logistic Regression
lr_pred = predict(step_model, df_test, type="response")
lr2 = prediction(lr_pred, df_test$TARGET_BAD_FLAG)
lr3 = performance(lr2, "tpr", "fpr")

# Plot ROC curves
plot(pt3, col="blue", main="ROC Curve")
plot(lr3, col="red", add=TRUE)
abline(a=0, b=1, lty=2, col="gray")
legend("bottomright", legend=c("Decision Tree", "Logistic Regression"), col=c("blue", "red"), lty=1)

# Calculate AUC
auc_tree = performance(pt2, "auc")@y.values[[1]]
auc_lr = performance(lr2, "auc")@y.values[[1]]
print(paste("Decision Tree AUC:", round(auc_tree, 4)))
print(paste("Logistic Regression AUC:", round(auc_lr, 4)))

# Step 5

df_model = df
df_model$TARGET_LOSS_AMT = NULL


df_model$PC1 = df_new[,"PC1"]
df_model$PC2 = df_new[,"PC2"]
df_model$PC3 = df_new[,"PC3"]
df_model$PC4 = df_new[,"PC4"]

df_model$TS1M_RF = predict( ts1_model_rf, df_model )
df_model$TS2M_RF = predict( ts2_model_rf, df_model )

df_model$KIDSDRIV = NULL
df_model$HOMEKIDS = NULL
df_model$TRAVTIME = NULL
df_model$BLUEBOOK = NULL
df_model$TIF = NULL
df_model$NPOLICY = NULL
df_model$OLDCLAIM = NULL
df_model$CLM_FREQ = NULL
df_model$MVR_PTS = NULL
df_model$IMP_AGE = NULL
df_model$IMP_YOJ = NULL
df_model$IMP_CAR_AGE = NULL
df_model$IMP_INCOME = NULL
df_model$IMP_HOME_VAL = NULL

head( df_model )

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_model, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_model, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

rpart.plot( t1G )

rpart.plot( t1E )

t1G$variable.importance
t1E$variable.importance


theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data=df_model )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_model )

summary( theUpper_LR )
summary( theLower_LR )

lr_model = stepAIC(theLower_LR, direction="forward", scope=list(lower=theLower_LR, upper=theUpper_LR))
summary( lr_model )

pG = predict( t1G, df_model )
pG2 = prediction( pG[,2], df_model$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_model )
pE2 = prediction( pE[,2], df_model$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plr = predict( lr_model, df_model, type="response" )
plr2 = prediction( plr, df_model$TARGET_BAD_FLAG )
plr3 = performance( plr2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
plot( plr3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","ENTROPY","REGRESSION"),col=c("red","green","blue"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values
aucR = performance( plr2, "auc" )@y.values

print( aucG )
print( aucE )
print( aucR )








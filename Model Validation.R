
library( rpart )
library( rpart.plot )
library( ROCR )

SEED = 1
set.seed( SEED )

PATH <- "C:/Users/tehmi/Downloads/HMEQ_Scrubbed"
setwd(PATH)

FILE_NAME = "HMEQ_Scrubbed.csv"
OUT_NAME 	= "HMEQ_Scrubbed1.csv"
OUTFILE = paste( PATH, OUT_NAME, sep="/" )

CSV_FILE <- "HMEQ_Scrubbed.csv"
df <- read.csv(CSV_FILE)

#Step 1
str( df )
summary( df )
head(df)

#Step 2
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.8,0.2))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

dim( df_flag )
dim( df_train )
dim( df_test )

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

rpart.plot( t1G )
t1G$variable.importance

rpart.plot( t1E )
t1E$variable.importance

# TRAIN DATA
pG = predict( t1G, df_train )
pG2 = prediction( pG[,2], df_train$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_train )
pE2 = prediction( pE[,2], df_train$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TRAIN GINI","TRAIN ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TRAIN AUC GINI=", aucG) )
print( paste("TRAIN AUC ENTROPY=", aucE) )


fG = predict( t1G, df_train, type="class" )
fE = predict( t1E, df_train, type="class" )

table( fG, df_train$TARGET_BAD_FLAG )
table( fE, df_train$TARGET_BAD_FLAG )

# TEST DATA
pG = predict( t1G, df_test )
pG2 = prediction( pG[,2], df_test$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TEST GINI","TEST ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TEST AUC GINI=", aucG) )
print( paste("TEST AUC ENTROPY=", aucE) )

fG = predict( t1G, df_test, type="class" )
fE = predict( t1E, df_test, type="class" )

table( fG, df_test$TARGET_BAD_FLAG )
table( fE, df_test$TARGET_BAD_FLAG )

# ALL DATA
tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

#rpart.plot( t1G )
rpart.plot( t1E )

t1G$variable.importance
t1E$variable.importance

pG = predict( t1G, df_flag )
pG2 = prediction( pG[,2], df_flag$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_flag )
pE2 = prediction( pE[,2], df_flag$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("ALL DATA GINI","ALL DATA ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("ALL DATA AUC GINI=", aucG) )
print( paste("ALL DATA AUC ENTROPY=", aucE) )

fG = predict( t1G, df_flag, type="class" )
fE = predict( t1E, df_flag, type="class" )

table( fG, df_flag$TARGET_BAD_FLAG )
table( fE, df_flag$TARGET_BAD_FLAG)


#Step 2.2
FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.7,0.3))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

dim( df_flag )
dim( df_train )
dim( df_test )

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

rpart.plot( t1G )
t1G$variable.importance

rpart.plot( t1E )
t1E$variable.importance

# TRAIN DATA 2.2
pG = predict( t1G, df_train )
pG2 = prediction( pG[,2], df_train$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_train )
pE2 = prediction( pE[,2], df_train$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TRAIN GINI","TRAIN ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TRAIN AUC GINI=", aucG) )
print( paste("TRAIN AUC ENTROPY=", aucE) )


fG = predict( t1G, df_train, type="class" )
fE = predict( t1E, df_train, type="class" )

table( fG, df_train$TARGET_BAD_FLAG )
table( fE, df_train$TARGET_BAD_FLAG )

# TEST DATA 2.2
pG = predict( t1G, df_test )
pG2 = prediction( pG[,2], df_test$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TEST GINI","TEST ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TEST AUC GINI=", aucG) )
print( paste("TEST AUC ENTROPY=", aucE) )

fG = predict( t1G, df_test, type="class" )
fE = predict( t1E, df_test, type="class" )

table( fG, df_test$TARGET_BAD_FLAG )
table( fE, df_test$TARGET_BAD_FLAG )

# ALL DATA 2.2
tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

#rpart.plot( t1G )
rpart.plot( t1E )

t1G$variable.importance
t1E$variable.importance

pG = predict( t1G, df_flag )
pG2 = prediction( pG[,2], df_flag$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_flag )
pE2 = prediction( pE[,2], df_flag$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("ALL DATA GINI","ALL DATA ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("ALL DATA AUC GINI=", aucG) )
print( paste("ALL DATA AUC ENTROPY=", aucE) )

fG = predict( t1G, df_flag, type="class" )
fE = predict( t1E, df_flag, type="class" )

table( fG, df_flag$TARGET_BAD_FLAG )
table( fE, df_flag$TARGET_BAD_FLAG)

#Step 2.3
FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.75,0.25))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

dim( df_flag )
dim( df_train )
dim( df_test )

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

rpart.plot( t1G )
t1G$variable.importance

rpart.plot( t1E )
t1E$variable.importance

# TRAIN DATA 2.3
pG = predict( t1G, df_train )
pG2 = prediction( pG[,2], df_train$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_train )
pE2 = prediction( pE[,2], df_train$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TRAIN GINI","TRAIN ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TRAIN AUC GINI=", aucG) )
print( paste("TRAIN AUC ENTROPY=", aucE) )


fG = predict( t1G, df_train, type="class" )
fE = predict( t1E, df_train, type="class" )

table( fG, df_train$TARGET_BAD_FLAG )
table( fE, df_train$TARGET_BAD_FLAG )

# TEST DATA 2.3
pG = predict( t1G, df_test )
pG2 = prediction( pG[,2], df_test$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TEST GINI","TEST ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TEST AUC GINI=", aucG) )
print( paste("TEST AUC ENTROPY=", aucE) )

fG = predict( t1G, df_test, type="class" )
fE = predict( t1E, df_test, type="class" )

table( fG, df_test$TARGET_BAD_FLAG )
table( fE, df_test$TARGET_BAD_FLAG )

# ALL DATA 2.3
tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

#rpart.plot( t1G )
rpart.plot( t1E )

t1G$variable.importance
t1E$variable.importance

pG = predict( t1G, df_flag )
pG2 = prediction( pG[,2], df_flag$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_flag )
pE2 = prediction( pE[,2], df_flag$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("ALL DATA GINI","ALL DATA ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("ALL DATA AUC GINI=", aucG) )
print( paste("ALL DATA AUC ENTROPY=", aucE) )

fG = predict( t1G, df_flag, type="class" )
fE = predict( t1E, df_flag, type="class" )

table( fG, df_flag$TARGET_BAD_FLAG )
table( fE, df_flag$TARGET_BAD_FLAG)

#Step 3
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.7,0.3))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]

mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

tr_set = rpart.control( maxdepth = 10 )

t1a = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( t1a )
t1a$variable.importance

t1p = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t1p )
t1p$variable.importance

# TRAIN
p1a = predict( t1a, df_train )
RMSE1a = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_train )
RMSE1p = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1p )^2 ) )

print( paste("TRAIN RMSE ANOVA =", RMSE1a) )
print( paste("TRAIN RMSE POISSON =", RMSE1p) )

# TEST
p1a = predict( t1a, df_test )
RMSE1a = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_test )
RMSE1p = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1p )^2 ) )

print( paste("TEST RMSE ANOVA =", RMSE1a) )
print( paste("TEST RMSE POISSON =", RMSE1p) )

# ALL DATA
t1a = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( t1a )
t1a$variable.importance

t1p = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t1p )
t1p$variable.importance


p1a = predict( t1a, df_amt )
RMSE1a = sqrt( mean( ( df_amt$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_amt )
RMSE1p = sqrt( mean( ( df_amt$TARGET_LOSS_AMT - p1p )^2 ) )

print( paste("ALL DATA RMSE ANOVA =", RMSE1a) )
print( paste("ALL DATA RMSE POISSON =", RMSE1p) )

#Step 3.2
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.75,0.25))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]

mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

tr_set = rpart.control( maxdepth = 10 )

t1a = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( t1a )
t1a$variable.importance

t1p = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t1p )
t1p$variable.importance

# TRAIN 3.2
p1a = predict( t1a, df_train )
RMSE1a = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_train )
RMSE1p = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1p )^2 ) )

print( paste("TRAIN RMSE ANOVA =", RMSE1a) )
print( paste("TRAIN RMSE POISSON =", RMSE1p) )

# TEST 3.2
p1a = predict( t1a, df_test )
RMSE1a = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_test )
RMSE1p = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1p )^2 ) )

print( paste("TEST RMSE ANOVA =", RMSE1a) )
print( paste("TEST RMSE POISSON =", RMSE1p) )

# ALL DATA 3.2
t1a = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( t1a )
t1a$variable.importance

t1p = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t1p )
t1p$variable.importance


p1a = predict( t1a, df_amt )
RMSE1a = sqrt( mean( ( df_amt$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_amt )
RMSE1p = sqrt( mean( ( df_amt$TARGET_LOSS_AMT - p1p )^2 ) )

print( paste("ALL DATA RMSE ANOVA =", RMSE1a) )
print( paste("ALL DATA RMSE POISSON =", RMSE1p) )

#Step 3.3
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.8,0.2))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]

mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

tr_set = rpart.control( maxdepth = 10 )

t1a = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( t1a )
t1a$variable.importance

t1p = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t1p )
t1p$variable.importance

# TRAIN 3.3
p1a = predict( t1a, df_train )
RMSE1a = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_train )
RMSE1p = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1p )^2 ) )

print( paste("TRAIN RMSE ANOVA =", RMSE1a) )
print( paste("TRAIN RMSE POISSON =", RMSE1p) )

# TEST 3.3
p1a = predict( t1a, df_test )
RMSE1a = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_test )
RMSE1p = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1p )^2 ) )

print( paste("TEST RMSE ANOVA =", RMSE1a) )
print( paste("TEST RMSE POISSON =", RMSE1p) )

# ALL DATA 3.3
t1a = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( t1a )
t1a$variable.importance

t1p = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t1p )
t1p$variable.importance


p1a = predict( t1a, df_amt )
RMSE1a = sqrt( mean( ( df_amt$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_amt )
RMSE1p = sqrt( mean( ( df_amt$TARGET_LOSS_AMT - p1p )^2 ) )

print( paste("ALL DATA RMSE ANOVA =", RMSE1a) )
print( paste("ALL DATA RMSE POISSON =", RMSE1p) )

# Step 4
df_probsev = df

FLAG = sample(c(TRUE, FALSE), nrow(df_probsev), replace=TRUE, prob=c(0.7, 0.3))
df_train = df_probsev[FLAG, ]
df_test = df_probsev[!FLAG, ]

# Model to predict TARGET_BAD_FLAG
tr_set = rpart.control(maxdepth = 10)

# Probability model for TARGET_BAD_FLAG
t_bad = rpart(data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class")
rpart.plot(t_bad)
t_bad$variable.importance

# Severity model for TARGET_LOSS_AMT where TARGET_BAD_FLAG is 1
df_train_loss = df_train[df_train$TARGET_BAD_FLAG == 1, ]
df_test_loss = df_test[df_test$TARGET_BAD_FLAG == 1, ]

t_loss = rpart(data=df_train_loss, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova")
rpart.plot(t_loss)
t_loss$variable.importance

# Predictions
# Predict Probability of Default (TARGET_BAD_FLAG)
p_bad_train = predict(t_bad, df_train, type="prob")[,2]  # Probability of TARGET_BAD_FLAG = 1
p_bad_test = predict(t_bad, df_test, type="prob")[,2]

# Predict Loss Given Default (TARGET_LOSS_AMT)
p_loss_train = predict(t_loss, df_train_loss)
p_loss_test = predict(t_loss, df_test_loss)

# Calculate Expected Loss = Probability of Default * Loss Given Default
# Train Set
expected_loss_train = p_bad_train * p_loss_train
actual_loss_train = ifelse(df_train$TARGET_BAD_FLAG == 1, df_train$TARGET_LOSS_AMT, 0)
RMSE_train = sqrt(mean((actual_loss_train - expected_loss_train)^2, na.rm=TRUE))

# Test Set
expected_loss_test = p_bad_test * p_loss_test
actual_loss_test = ifelse(df_test$TARGET_BAD_FLAG == 1, df_test$TARGET_LOSS_AMT, 0)
RMSE_test = sqrt(mean((actual_loss_test - expected_loss_test)^2, na.rm=TRUE))

print(paste("TRAIN RMSE Probability / Severity =", RMSE_train))
print(paste("TEST RMSE Probability / Severity =", RMSE_test))

# All Data
p_bad_all = predict(t_bad, df_probsev, type="prob")[,2]
p_loss_all = predict(t_loss, df_probsev[df_probsev$TARGET_BAD_FLAG == 1, ])
expected_loss_all = p_bad_all * p_loss_all
actual_loss_all = ifelse(df_probsev$TARGET_BAD_FLAG == 1, df_probsev$TARGET_LOSS_AMT, 0)
RMSE_all = sqrt(mean((actual_loss_all - expected_loss_all)^2, na.rm=TRUE))

print(paste("ALL DATA RMSE Probability / Severity =", RMSE_all))

#Step 4.2
FLAG = sample(c(TRUE, FALSE), nrow(df_probsev), replace=TRUE, prob=c(0.75, 0.25))
df_train = df_probsev[FLAG, ]
df_test = df_probsev[!FLAG, ]

# Model to predict TARGET_BAD_FLAG
tr_set = rpart.control(maxdepth = 10)

# Probability model for TARGET_BAD_FLAG
t_bad = rpart(data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class")
rpart.plot(t_bad)
t_bad$variable.importance

# Severity model for TARGET_LOSS_AMT where TARGET_BAD_FLAG is 1
df_train_loss = df_train[df_train$TARGET_BAD_FLAG == 1, ]
df_test_loss = df_test[df_test$TARGET_BAD_FLAG == 1, ]

t_loss = rpart(data=df_train_loss, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova")
rpart.plot(t_loss)
t_loss$variable.importance

# Predictions
# Predict Probability of Default (TARGET_BAD_FLAG)
p_bad_train = predict(t_bad, df_train, type="prob")[,2]  # Probability of TARGET_BAD_FLAG = 1
p_bad_test = predict(t_bad, df_test, type="prob")[,2]

# Predict Loss Given Default (TARGET_LOSS_AMT)
p_loss_train = predict(t_loss, df_train_loss)
p_loss_test = predict(t_loss, df_test_loss)

# Calculate Expected Loss = Probability of Default * Loss Given Default
# Train Set
expected_loss_train = p_bad_train * p_loss_train
actual_loss_train = ifelse(df_train$TARGET_BAD_FLAG == 1, df_train$TARGET_LOSS_AMT, 0)
RMSE_train = sqrt(mean((actual_loss_train - expected_loss_train)^2, na.rm=TRUE))

# Test Set
expected_loss_test = p_bad_test * p_loss_test
actual_loss_test = ifelse(df_test$TARGET_BAD_FLAG == 1, df_test$TARGET_LOSS_AMT, 0)
RMSE_test = sqrt(mean((actual_loss_test - expected_loss_test)^2, na.rm=TRUE))

print(paste("TRAIN RMSE Probability / Severity =", RMSE_train))
print(paste("TEST RMSE Probability / Severity =", RMSE_test))

# All Data
p_bad_all = predict(t_bad, df_probsev, type="prob")[,2]
p_loss_all = predict(t_loss, df_probsev[df_probsev$TARGET_BAD_FLAG == 1, ])
expected_loss_all = p_bad_all * p_loss_all
actual_loss_all = ifelse(df_probsev$TARGET_BAD_FLAG == 1, df_probsev$TARGET_LOSS_AMT, 0)
RMSE_all = sqrt(mean((actual_loss_all - expected_loss_all)^2, na.rm=TRUE))

print(paste("ALL DATA RMSE Probability / Severity =", RMSE_all))

# Step 4.3

FLAG = sample(c(TRUE, FALSE), nrow(df_probsev), replace=TRUE, prob=c(0.8, 0.2))
df_train = df_probsev[FLAG, ]
df_test = df_probsev[!FLAG, ]

# Model to predict TARGET_BAD_FLAG
tr_set = rpart.control(maxdepth = 10)

# Probability model for TARGET_BAD_FLAG
t_bad = rpart(data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class")
rpart.plot(t_bad)
t_bad$variable.importance

# Severity model for TARGET_LOSS_AMT where TARGET_BAD_FLAG is 1
df_train_loss = df_train[df_train$TARGET_BAD_FLAG == 1, ]
df_test_loss = df_test[df_test$TARGET_BAD_FLAG == 1, ]

t_loss = rpart(data=df_train_loss, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova")
rpart.plot(t_loss)
t_loss$variable.importance

# Predictions
# Predict Probability of Default (TARGET_BAD_FLAG)
p_bad_train = predict(t_bad, df_train, type="prob")[,2]  # Probability of TARGET_BAD_FLAG = 1
p_bad_test = predict(t_bad, df_test, type="prob")[,2]

# Predict Loss Given Default (TARGET_LOSS_AMT)
p_loss_train = predict(t_loss, df_train_loss)
p_loss_test = predict(t_loss, df_test_loss)

# Calculate Expected Loss = Probability of Default * Loss Given Default
# Train Set
expected_loss_train = p_bad_train * p_loss_train
actual_loss_train = ifelse(df_train$TARGET_BAD_FLAG == 1, df_train$TARGET_LOSS_AMT, 0)
RMSE_train = sqrt(mean((actual_loss_train - expected_loss_train)^2, na.rm=TRUE))

# Test Set
expected_loss_test = p_bad_test * p_loss_test
actual_loss_test = ifelse(df_test$TARGET_BAD_FLAG == 1, df_test$TARGET_LOSS_AMT, 0)
RMSE_test = sqrt(mean((actual_loss_test - expected_loss_test)^2, na.rm=TRUE))

print(paste("TRAIN RMSE Probability / Severity =", RMSE_train))
print(paste("TEST RMSE Probability / Severity =", RMSE_test))

# All Data
p_bad_all = predict(t_bad, df_probsev, type="prob")[,2]
p_loss_all = predict(t_loss, df_probsev[df_probsev$TARGET_BAD_FLAG == 1, ])
expected_loss_all = p_bad_all * p_loss_all
actual_loss_all = ifelse(df_probsev$TARGET_BAD_FLAG == 1, df_probsev$TARGET_LOSS_AMT, 0)
RMSE_all = sqrt(mean((actual_loss_all - expected_loss_all)^2, na.rm=TRUE))

print(paste("ALL DATA RMSE Probability / Severity =", RMSE_all))

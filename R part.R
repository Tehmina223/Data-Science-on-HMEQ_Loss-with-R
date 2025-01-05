install.packages("rpart.plot")
install.packages("ROCR")

library( rpart )
library( rpart.plot )
library( ROCR )

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

#STEP 2
df_flag = df
df_flag$TARGET_LOSS_AMT= NULL

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

rpart.plot( t1G )
rpart.plot( t1E )

t1G$variable.importance
t1E$variable.importance

pG = predict( t1G, df )
pG2 = prediction( pG[,2], df$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df )
pE2 = prediction( pE[,2], df$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( aucG )
print( aucE )

fG = predict( t1G, df, type="class" )
fE = predict( t1E, df, type="class" )

table( fG, df$TARGET_BAD_FLAG )
table( fE, df$TARGET_BAD_FLAG )

#Step 3
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL
mean( df_amt$TARGET_LOSS_AMT )

tr_set = rpart.control( maxdepth = 10 )

t1a = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( t1a )
t1a$variable.importance
p1a = predict( t1a, df )
RMSE1a = sqrt( mean( ( df$TARGET_LOSS_AMT - p1a )^2 ) )
print( RMSE1a )


t1p = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t1p )
t1p$variable.importance
p1p = predict( t1p, df )
RMSE1p = sqrt( mean( ( df$TARGET_LOSS_AMT - p1p )^2 ) )
print( RMSE1p )

print( RMSE1a )
print( RMSE1p )

#STEP 4
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

t2_f = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set )
rpart.plot( t2_f )
p2_f = predict( t2_f, df )

df_amt_2 = subset( df, TARGET_BAD_FLAG == 1 )
df_amt_2$TARGET_BAD_FLAG = NULL
head(df_amt_2)
t2_a = rpart( data=df_amt_2, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t2_a )
p2_a = predict( t2_a, df )

p2 = p2_f * p2_a
RMSE2 = sqrt( mean( ( df$TARGET_LOSS_AMT - p2 )^2 ) )

print( RMSE1p )
print( RMSE2 )


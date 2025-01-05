install.packages( "ggplot2" )
install.packages("flexclust")
library( ggplot2 )
library( flexclust )

SEED = 1
set.seed( SEED )

PATH <- "C:/Users/tehmi/Downloads/HMEQ_Scrubbed"
setwd(PATH)
FILE_NAME = "HMEQ_Scrubbed.csv"
INFILE = paste( PATH, FILE_NAME, sep="/" )

CSV_FILE <- "HMEQ_Scrubbed.csv"
df <- read.csv(CSV_FILE)

df_pca = df
df_pca$TARGET_BAD_FLAG = NULL
df_pca$TARGET_LOSS_AMT = NULL
df_pca = df_pca[ c(1,2,3,4,5,6,7,8,9,10,12,14,16,18) ]

pca = prcomp(df_pca,center=TRUE, scale=TRUE)
summary(pca)
plot(pca, type = "l")
df_new = data.frame( predict( pca, df_pca ) )

df_kmeans = df_new[1:2]
print( head( df_kmeans ) )
plot( df_kmeans$PC1, df_kmeans$PC2 )

# Maximum Clusters To Search
MAX_N = 10

# Set up an array to hold the Sum of Square Errors
WSS = numeric( MAX_N )

for ( N in 1:MAX_N ) 
{
  km = kmeans( df_kmeans, centers=N, nstart=20  )
  WSS[N] = km$tot.withinss
}

df_wss = as.data.frame( WSS )
df_wss$clusters = 1:MAX_N

scree_plot = ggplot( df_wss, aes( x=clusters, y=WSS, group=1 )) +
  geom_point( size=4 ) +
  geom_line() +
  scale_x_continuous( breaks=c(2,4,6,8,10)) +
  xlab("Number of Clusters")

scree_plot

BEST_N = 4
km = kmeans( df_kmeans, centers=BEST_N, nstart=20  )

print( km$size )
print( km$centers )


kf = as.kcca( object=km, data=df_kmeans, save.data=TRUE )
kfi = kcca2df( kf )
agg = aggregate( kfi$value, list( kfi$variable, kfi$group ), FUN=mean )

barplot(kf)

clus = predict( kf, df_kmeans )
plot( df_kmeans$PC1, df_kmeans$PC2, col=clus )
legend( x="topleft", legend=c(1:BEST_N), fill=c(1:BEST_N) )

df$CLUSTER = clus
agg = aggregate( df$TARGET_BAD_FLAG, list( df$CLUSTER ), FUN=mean )

library( rpart )
library( rpart.plot )

df_tree = df_pca
df_tree$CLUSTER = as.factor(clus)
dt = rpart( CLUSTER ~ . , data=df_tree )

rpart.plot( dt )

df_pca = df[ , c( "LOAN", "IMP_MORTDUE", "IMP_YOJ", "IMP_CLAGE", "IMP_CLNO", "IMP_DEBTINC" ) ]
pca = prcomp(df_pca, center=TRUE, scale=TRUE)
print( pca )
summary(pca)
plot(pca, type = "l")



df_new = data.frame( predict( pca, df_pca ) )
df_kmeans = df_new[1:2]
print( head( df_kmeans ) )
plot( df_kmeans$PC1, df_kmeans$PC2 )


# Maximum Clusters To Search
MAX_N = 10

# Set up an array to hold the Sum of Square Errors
WSS = numeric( MAX_N )

for ( N in 1:MAX_N ) 
	{
	km = kmeans( df_kmeans, centers=N, nstart=20  )
	WSS[N] = km$tot.withinss
	}

df_wss = as.data.frame( WSS )
df_wss$clusters = 1:MAX_N

scree_plot = ggplot( df_wss, aes( x=clusters, y=WSS, group=1 )) +
		geom_point( size=4 ) +
		geom_line() +
		scale_x_continuous( breaks=c(2,4,6,8,10)) +
		xlab("Number of Clusters")

scree_plot

BEST_N = 4
km = kmeans( df_kmeans, centers=BEST_N, nstart=20  )

print( km$size )
print( km$centers )

kf = as.kcca( object=km, data=df_kmeans, save.data=TRUE )
kfi = kcca2df( kf )
agg = aggregate( kfi$value, list( kfi$variable, kfi$group ), FUN=mean )

barplot(kf)

clus = predict( kf, df_kmeans )
plot( df_kmeans$PC1, df_kmeans$PC2, col=clus )
legend( x="topleft", legend=c(1:BEST_N), fill=c(1:BEST_N) )

df$CLUSTER = clus
agg = aggregate( df$TARGET_BAD_FLAG, list( df$CLUSTER ), FUN=mean )

library( rpart )
library( rpart.plot )

df_tree = df_pca
df_tree$CLUSTER = as.factor(clus)
dt = rpart( CLUSTER ~ . , data=df_tree )

rpart.plot( dt )

df_pca = df[ , c( "LOAN", "IMP_MORTDUE", "IMP_YOJ", "IMP_CLAGE", "IMP_CLNO", "IMP_DEBTINC" ) ]
pca = prcomp(df_pca, center=TRUE, scale=TRUE)
print( pca )
summary(pca)
plot(pca, type = "l")

df_new = data.frame( predict( pca, df_pca ) )
df_kmeans = df_new[1:2]
print( head( df_kmeans ) )
plot( df_kmeans$PC1, df_kmeans$PC2 )

# Maximum Clusters To Search
MAX_N = 10

# Set up an array to hold the Sum of Square Errors
WSS = numeric( MAX_N )

for ( N in 1:MAX_N ) 
{
  km = kmeans( df_kmeans, centers=N, nstart=20  )
  WSS[N] = km$tot.withinss
}

df_wss = as.data.frame( WSS )
df_wss$clusters = 1:MAX_N

scree_plot = ggplot( df_wss, aes( x=clusters, y=WSS, group=1 )) +
  geom_point( size=4 ) +
  geom_line() +
  scale_x_continuous( breaks=c(2,4,6,8,10)) +
  xlab("Number of Clusters")

scree_plot

BEST_N = 5
km = kmeans( df_kmeans, centers=BEST_N, nstart=20  )

print( km$size )
print( km$centers )

kf = as.kcca( object=km, data=df_kmeans, save.data=TRUE )
kfi = kcca2df( kf )
agg = aggregate( kfi$value, list( kfi$variable, kfi$group ), FUN=mean )

barplot(kf)

clus = predict( kf, df_kmeans )

plot( df_kmeans$PC1, df_kmeans$PC2 )
plot( df_kmeans$PC1, df_kmeans$PC2, col=clus )
legend( x="topright", legend=c(1:BEST_N), fill=c(1:BEST_N) )

df$CLUSTER = clus
agg = aggregate( df$TARGET_BAD_FLAG, list( df$CLUSTER ), FUN=mean )

df_tree = df_pca
df_tree$CLUSTER = as.factor(clus)

dt = rpart( CLUSTER ~ . , data=df_tree )
dt = rpart( CLUSTER ~ . , data=df_tree, maxdepth=3 )

rpart.plot( dt )


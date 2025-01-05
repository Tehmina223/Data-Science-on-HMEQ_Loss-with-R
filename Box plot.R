
PATH <- "C:/Users/tehmi/Downloads/HMEQ_WK02_10"
setwd(PATH)
FILE_NAME = "HMEQ_Loss.csv"
OUT_NAME 	= "HMEQ_Loss_Scrubbed.csv"
OUTFILE = paste( PATH, OUT_NAME, sep="/" )
CSV_FILE <- "HMEQ_Loss.csv"
df <- read.csv(CSV_FILE)

#STEP 1
str(df)
summary(df)
head(df)

#STEP 2
# Create boxplots for all numeric variables
numeric_cols <- sapply(df, is.numeric)
boxplot(df[, numeric_cols],
        main="Tehmina",
        col="lightblue",
        border="darkblue",
        las=2)  # Rotate x-axis labels

#STEP 3
hist(df$LOAN, 
     breaks = 10,
     prob = FALSE,  # Use counts instead of density
     main = "Histogram of HMEQ_Loss LOAN",
     xlab = "LOAN",
     ylab = "Count",
     col = "lightyellow",
     border = "black")

# Calculate the density
dens <- density(df$LOAN)

# Scale the density to match the count scale
scaling_factor <- max(hist_data$counts) / max(dens$y)
scaled_density <- dens$y * scaling_factor

# Add the scaled density line
lines(dens$x, scaled_density, col = "darkblue", lwd = 2)


#STEP 4
df$TARGET_LOSS_AMT[ is.na( df$TARGET_LOSS_AMT ) ] = 0
summary(df)

# MORTDUE
median( df$MORTDUE, na.rm=TRUE )
df$IMP_MORTDUE = df$MORTDUE
df$IMP_MORTDUE[ is.na( df$MORTDUE ) ] = 65019
df$M_MORTDUE = is.na( df$MORTDUE ) + 0
sum( df$M_MORTDUE )
summary( df )
df$MORTDUE = NULL
summary( df )

# VALUE
median( df$VALUE, na.rm=TRUE )
df$IMP_VALUE = df$VALUE
df$IMP_VALUE[ is.na( df$VALUE ) ] = 89235.5
df$M_VALUE = is.na( df$VALUE ) + 0
sum( df$M_VALUE )
summary( df )
#df$VALUE = NULL
summary( df )

# YOJ
median( df$YOJ, na.rm=TRUE )
df$IMP_YOJ = df$YOJ
df$IMP_YOJ[ is.na( df$YOJ ) ] = 7
df$M_YOJ = is.na( df$YOJ ) + 0
sum( df$M_YOJ )
summary( df )
df$YOJ = NULL
summary( df )

# DEROG
median( df$DEROG, na.rm=TRUE )
df$IMP_DEROG = df$DEROG
df$IMP_DEROG[ is.na( df$DEROG ) ] = 0
df$M_DEROG = is.na( df$DEROG ) + 0
sum( df$M_DEROG )
summary( df )
df$DEROG = NULL
summary( df )

# DELINQ
median( df$DELINQ, na.rm=TRUE )
df$IMP_DELINQ = df$DELINQ
df$IMP_DELINQ[ is.na( df$DELINQ ) ] = 0
df$M_DELINQ = is.na( df$DELINQ ) + 0
sum( df$M_DELINQ )
summary( df )
df$DELINQ = NULL
summary( df )

# CLAGE
median( df$CLAGE, na.rm=TRUE )
df$IMP_CLAGE = df$CLAGE
df$IMP_CLAGE[ is.na( df$CLAGE ) ] = 173.4667
df$M_CLAGE = is.na( df$CLAGE ) + 0
sum( df$M_CLAGE )
summary( df )
df$CLAGE = NULL
summary( df )

# NINQ
median( df$NINQ, na.rm=TRUE )
df$IMP_NINQ = df$NINQ
df$IMP_NINQ[ is.na( df$NINQ ) ] = 1
df$M_NINQ = is.na( df$NINQ ) + 0
sum( df$M_NINQ )
summary( df )
df$NINQ = NULL
summary( df )

# CLNO
median( df$CLNO, na.rm=TRUE )
df$IMP_CLNO = df$CLNO
df$IMP_CLNO[ is.na( df$CLNO ) ] = 20
df$M_CLNO = is.na( df$CLNO ) + 0
sum( df$M_CLNO )
summary( df )
df$CLNO = NULL
summary( df )

# DEBTINC
median( df$DEBTINC, na.rm=TRUE )
df$IMP_DEBTINC = df$DEBTINC
df$IMP_DEBTINC[ is.na( df$DEBTINC ) ] = 34.81826
df$M_DEBTINC = is.na( df$DEBTINC ) + 0
sum( df$M_DEBTINC )
summary( df )
df$DEBTINC = NULL
summary( df )

# REASON
table( df$REASON )
df$FLAG.R.HomeImp = ( df$REASON == "HomeImp"	) + 0 
df$FLAG.R.DebtCon	= ( df$REASON == "DebtCon"	) + 0
sum( df$FLAG.R.HomeImp  )
sum( df$FLAG.R.DebtCon  )

#df$REASON.HomeImp = NULL
#df$REASON.DebtCon = NULL
df$REASON = NULL
summary( df )


# JOB
table( df$JOB )
df$FLAG.Job.Mgr	= ( df$JOB == "Mgr"	) + 0 
df$FLAG.Job.Office	= ( df$JOB == "Office" 	) + 0 
df$FLAG.Job.Other = ( df$JOB == "Other" 		) + 0 
df$FLAG.Job.ProfExe	= ( df$JOB == "ProfExe" 	) + 0 
df$FLAG.Job.Sales	= ( df$JOB == "Sales" 		) + 0 
df$FLAG.Job.Self	= ( df$JOB == "Self" 	) + 0 
df$FLAG.Job.Salary		= ( df$JOB %in% c( "Mgr", "Office", "Sales") ) + 0

sum( df$FLAG.Job.Mgr )
sum( df$FLAG.Job.Office )
sum( df$FLAG.Job.Other )
sum( df$FLAG.Job.ProfExe )
sum( df$FLAG.Job.Sales )
sum( df$FLAG.Job.Self )
sum( df$FLAG.Job.Salary )

#df$JOB = NULL
summary(df)


#COMPLEX STEP 4
a = aggregate( x=df$VALUE, by=list( df$JOB ), na.rm=TRUE, FUN=median )
a = a[ order( a$x, decreasing=TRUE), ]
a
df$IMP_VALUE = df$VALUE
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "Mgr" 		) ] =  101258.0 
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "Office" 		) ] = 89094.5 
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "Other" 	) ] = 76599.5
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "ProfExe" 	) ] = 110007.0
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "Sales" 	) ] = 84473.5
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "Self" 	) ] =  130631.0
df$IMP_VALUE[ is.na(df$IMP_VALUE)  ] = 78227.0
df$M_VALUE = is.na( df$VALUE ) + 0
df$VALUE = NULL
df$JOB = NULL
summary( df )
df

write.csv( df, OUTFILE, row.names = FALSE )



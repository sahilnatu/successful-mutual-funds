rm(list = ls())

library(lubridate)
library(tree)
library(randomForest)

df <- read.csv(file = "Data/Equity Mutual Funds.csv")

############
### Keep relevant columns for Small Cap
############
small_cap <- df[sapply(strsplit(df$Morningstar.Category," "),"[",1) == "Small",c(1,2,4,5,6,7,9,10,16,17,18,19,20,21,22,23,25)]
#mid_cap <- df[sapply(strsplit(df$Morningstar.Category," "),"[",1) == "Mid-Cap",]
#large_cap <- df[sapply(strsplit(df$Morningstar.Category," "),"[",1) == "Large",]

for (i in 1:dim(small_cap)[1]) {
  #Remove 'Small' from Category to keep Growth/Value/Blended
  small_cap$Morningstar.Category[i] = strsplit(small_cap$Morningstar.Category," ")[[i]][2]
  
  #Returns - Remove date suffix and % sign, convert to numeric and show percentages in decimal terms
  if (length(strsplit(small_cap$X1.Yr,"%")[[i]][1]) > 0 ) {
    small_cap$X1.Yr[i] = as.numeric(strsplit(small_cap$X1.Yr,"%")[[i]][1])/100
  } else {
    small_cap$X1.Yr[i] = NA
  }
  if (length(strsplit(small_cap$X3.Yr,"%")[[i]][1]) > 0 ) {
    small_cap$X3.Yr[i] = as.numeric(strsplit(small_cap$X3.Yr,"%")[[i]][1])/100
  } else {
    small_cap$X3.Yr[i] = NA
  }
  if (length(strsplit(small_cap$X5.Yr,"%")[[i]][1]) > 0 ) {
    small_cap$X5.Yr[i] = as.numeric(strsplit(small_cap$X5.Yr,"%")[[i]][1])/100
  } else {
    small_cap$X5.Yr[i] = NA
  }
  if (length(strsplit(small_cap$X10.Yr,"%")[[i]][1]) > 0 ) {
    small_cap$X10.Yr[i] = as.numeric(strsplit(small_cap$X10.Yr,"%")[[i]][1])/100
  } else {
    small_cap$X10.Yr[i] = NA
  }
  
  #Expense Ratios - Remove % sign, convert to numeric and show percentages in decimal terms
  if (length(strsplit(small_cap$Expense.Ratio...Net,"%")[[i]][1]) > 0 ) {
    small_cap$Expense.Ratio...Net[i] = as.numeric(strsplit(small_cap$Expense.Ratio...Net,"%")[[i]][1])/100
  } else {
    small_cap$Expense.Ratio...Net[i] = NA
  }
  if (length(strsplit(small_cap$Expense.Ratio...Gross,"%")[[i]][1]) > 0 ) {
    small_cap$Expense.Ratio...Gross[i] = as.numeric(strsplit(small_cap$Expense.Ratio...Gross,"%")[[i]][1])/100
  } else {
    small_cap$Expense.Ratio...Gross[i] = NA
  }
  
  #SD, Beta, R^2 - Remove date suffix and convert to numeric
  if (length(strsplit(small_cap$Standard.Deviation," ")[[i]][1]) > 0 ) {
    small_cap$Standard.Deviation[i] = as.numeric(strsplit(small_cap$Standard.Deviation," ")[[i]][1])
  } else {
    small_cap$Standard.Deviation[i] = NA
  }
  if (length(strsplit(small_cap$X3.Year.Sharpe.Ratio," ")[[i]][1]) > 0 ) {
    small_cap$X3.Year.Sharpe.Ratio[i] = as.numeric(strsplit(small_cap$X3.Year.Sharpe.Ratio," ")[[i]][1])
  } else {
    small_cap$X3.Year.Sharpe.Ratio[i] = NA
  }
  if (length(strsplit(small_cap$Beta," ")[[i]][1]) > 0 ) {
    small_cap$Beta[i] = as.numeric(strsplit(small_cap$Beta," ")[[i]][1])
  } else {
    small_cap$Beta[i] = NA
  }
  if (length(strsplit(small_cap$R2," ")[[i]][1]) > 0 ) {
    small_cap$R2[i] = as.numeric(strsplit(small_cap$R2," ")[[i]][1])
  } else {
    small_cap$R2[i] = NA
  }
  
  #Manager Tenure - Extract year value from string and convert to numeric
  small_cap$Manager.Tenure[i] = as.numeric(strsplit(small_cap$Manager.Tenure," ")[[i]][1])
  
  #Years since Inception - Calculate years passed since inception date relative to today
  small_cap$Years.Since.Inception[i] = year(Sys.Date()) - year(as.Date(small_cap$Inception.Date[i],format="%m/%d/%Y"))
  
  #Assets - Remove $ sign, convert to numeric, multiply by 1 million
  small_cap$Assets[i] = 1000000 * as.numeric(gsub(",","",trimws(small_cap$Assets..Millions.[i],"left","[$]")))
  
  #Turnover Rate - Remove date suffix and % sign, convert to numeric and show percentages in decimal terms
  if (length(strsplit(small_cap$Turnover.Rates,"%")[[i]][1]) > 0 ) {
    small_cap$Turnover.Rates[i] = as.numeric(strsplit(small_cap$Turnover.Rates,"%")[[i]][1])/100
  } else {
    small_cap$Turnover.Rates[i] = NA
  }
  
  #Load/Non Load - Convert binary Y/N to 1/0
  if (small_cap$Load..Y.N.[i] == "Y") {
    small_cap$Load..Y.N.[i] = 1
  } else if (small_cap$Load..Y.N.[i] == "N") {
    small_cap$Load..Y.N.[i] = 0
  } else {
    small_cap$Load..Y.N.[i] = NA
  }
  
}

############
### Dropping Assets in Million USD and Inception Date columns
############
small_cap <- small_cap[,-c(14,15)]
small_cap[,c(3:17)] <- sapply(small_cap[,c(3:17)],as.numeric)

############
### Check column names and data types\
############
sapply(small_cap,class)








############
### Trees RF
############

#Remove rows if data in one of the following columns in NA
na_mask_rf <- ifelse(rowSums(!is.na(small_cap[,c(2,7,8,9,10,11,12,13,14,15,16,17)]))==ncol(small_cap[,c(2,7,8,9,10,11,12,13,14,15,16,17)]),TRUE,FALSE)
small_cap_rf_df <- small_cap[na_mask_rf,]

#1/4 data is for testing, rest for training
samp_rf <- sample(1:nrow(small_cap_rf_df),nrow(small_cap_rf_df)/4)
df_train_rf <- small_cap_rf_df[-samp_rf,-c(1,3,5,6)]
df_test_rf <- small_cap_rf_df[samp_rf,-c(1,3,5,6)]

#Set m as sq root of p
m_rf <- ceiling(sqrt(ncol(df_train_rf)-1))

rf.small_cap <- randomForest(X3.Yr~.,data=df_train_rf,mtry=m_rf,importance=TRUE,ntree=100)
rf.small_cap

#In sample error for different values of number of trees
plot(rf.small_cap)

#Calculating Yhat
yhat_rf <- predict(rf.small_cap,df_test_rf)

#Calculating Out of Sample MSE and RMSE
test_mse_rf <- mean((yhat_rf - df_test_rf[,"X3.Yr"])^2)
test_rmse_rf <- sqrt(test_mse_rf)
print(paste("Test RMSE in %:",test_rmse_rf*100))

#Calculating Residuals
residual_rf <- yhat_rf - df_test_rf[,"X3.Yr"]

#Variable Importance
importance(rf.small_cap)

############
### Plotting Actual vs Predicted 3-year Returns
############
y_rf <- sort(df_test_rf$X3.Yr,index.return=TRUE)
y_min_rf <- 100*min(yhat_rf,y_rf$x)
y_max_rf <- 100*max(yhat_rf,y_rf$x)
par(mar = c(5,5,3,5))
plot(c(1:nrow(df_test_rf)),100*yhat_rf[y_rf$ix],type="l",col="blue",ylab="% 3-year Returns",xlab="index",ylim=c(y_min_rf,y_max_rf))
par(new = TRUE)
plot(c(1:nrow(df_test_rf)),100*y_rf$x,type="l",xaxt="n",yaxt="n",col="red",xlab="",ylab="",ylim=c(y_min_rf,y_max_rf))
axis(side=4)
mtext("Actual vs Predicted 3-year Returns")
legend("topleft",c("Predicted","Actual"),col=c("blue","red"),lty=c(1,1))

#Residual Plot
par(mar = c(5,5,3,5))
plot(c(1:nrow(df_test_rf)),residual_rf[y_rf$ix]*100,col="black",ylab="Residual Error (%)", xlab="index")
abline(h=0,lty=2)
mtext("Residual Plot (Yhat - Y)")

############
### Trees RF for different 'm'
############
test_rmse_rf_matrix = matrix(0,ncol(df_train_rf)-1,2)
for (j in c(1:(ncol(df_train_rf)-1))) {
  m_rf_2 <- j
  print(m_rf_2)
  rf.small_cap_2 <- randomForest(X3.Yr~.,data=df_train_rf,mtry=m_rf_2,importance=TRUE,ntree=100)
  
  yhat_rf_2 <- predict(rf.small_cap_2,df_test_rf)
  test_mse_rf_2 <- mean((yhat_rf_2 - df_test_rf[,"X3.Yr"])^2)
  test_rmse_rf_2 <- sqrt(test_mse_rf_2)
  test_rmse_rf_matrix[j,1] <- j
  test_rmse_rf_matrix[j,2] <- test_rmse_rf_2*100
}
min_rmse_rf <- apply(test_rmse_rf_matrix,2,min)[2]
best_m_rf <- test_rmse_rf_matrix[which(test_rmse_rf_matrix == min_rmse_rf)-nrow(test_rmse_rf_matrix)]
print(paste("Best Test RMSE of",min_rmse_rf,"% is obtained for m =",best_m_rf))

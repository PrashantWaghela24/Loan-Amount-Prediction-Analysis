library(readxl)
library(car)
library(performance)
library(see)
library(patchwork)


loanData=read_excel('~/Documents/DMML Project Documents/LoanSanctionDataset/LoanPredictor_Dataset.xlsx')
summary(loanData)
names(loanData)
str(loanData)

#Data Pre-processing to remove NA and blank values from critical columns to optimize the linear model

df1=loanData[!(is.na(loanData$IncomeStability)),]
df2=df1[!(is.na(df1$Income)),]
df3=df2[!(is.na(df2$Dependents)),]
df4=df3[!(is.na(df3$CreditScore)),]
df5=df4[!(is.na(df4$PropertyLocation)),]
df5$LoanSanctionAmount[df5$LoanSanctionAmount== -999] = NA
df6=df5[!(is.na(df5$LoanSanctionAmount)),]
df6$CoApplicant[df6$CoApplicant== -999] = NA
df7=df6[!(is.na(df6$CoApplicant)),]
df7$IncomeStability[df7$IncomeStability== "Low"] = 0
df7$IncomeStability[df7$IncomeStability== "High"] = 1
df7$Location[df7$Location == "Rural"] = 1
df7$Location[df7$Location == "Semi-Urban"] = 2
df7$Location[df7$Location == "Urban"] = 3
df7$PropertyLocation[df7$PropertyLocation == "Rural"] = 1
df7$PropertyLocation[df7$PropertyLocation == "Semi-Urban"] = 2
df7$PropertyLocation[df7$PropertyLocation == "Urban"] = 3
df7$LoanSanctionAmount[df7$LoanSanctionAmount == 0] = 39225.23
df7$IncomeStability=as.numeric(as.character(df7$IncomeStability))
df7$PropertyLocation=as.numeric(as.character(df7$PropertyLocation))

boxplot(df7$LoanAmountRequest, df7$LoanSanctionAmount, ylab= "Amount", main="Normal BoxPlot",col="sky blue",names = c("LoanAmountRequest","LoanSanctionAmount"))

#Removal of Outliers from critical data fields
upperIncome=IQR(df7$Income)*1.5+(quantile(df7$Income,0.75))
upperLoanAmountRequest=IQR(df7$LoanAmountRequest)*1.5+(quantile(df7$LoanAmountRequest,0.75))
upperLoanSanction=IQR(df7$LoanSanctionAmount)*1.5+(quantile(df7$LoanSanctionAmount,0.75))

cleanData=subset(df7,(Income<=upperIncome & LoanAmountRequest<=upperLoanAmountRequest & LoanSanctionAmount<=upperLoanSanction))

loanData_subset=cleanData[c("Age","Income","IncomeStability","LoanAmountRequest","CreditScore","NoOfDefaults","Dependents","PropertyType","PropertyLocation","CoApplicant","LoanSanctionAmount")]
str(loanData_subset)
boxplot(loanData_subset$LoanAmountRequest, loanData_subset$LoanSanctionAmount, ylab= "Amount", main="Outlier removed BoxPlot",col="sky blue",names = c("LoanAmountRequest","LoanSanctionAmount"))

# library(reshape2)
# library(ggplot2)
# cormat <- round(cor(loanData_subset),2)
# get_lower_tri<-function(cormat){
#   cormat[upper.tri(cormat)] <- NA
#   return(cormat)
# }
# lower_tri <- get_lower_tri(cormat)
# lower_tri
# head(cormat)
# melted_cormat <- melt(lower_tri)
# head(melted_cormat)
# ggheatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + scale_fill_gradient2(low = "#8b0000", high = "#ffb302", mid = "#b7e2fc", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +theme_minimal()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+coord_fixed()
# ggheatmap + geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) +
#   theme(
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.border = element_blank(),
#     panel.background = element_blank(),
#     axis.ticks = element_blank(),
#     legend.justification = c(1, 0),
#     legend.position = c(0.5, 0.7),
#     legend.direction = "horizontal")+
#   guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
#                                title.position = "bottom", title.hjust = 0.5))

set.seed(1234)
loan_pd=sample(2,nrow(loanData_subset), replace=TRUE, prob=c(0.7,0.3))
loanData_train=loanData_subset[loan_pd==1,]
loanData_test=loanData_subset[loan_pd==2,]
str(loanData_train)

summary(loanData_train)
par(mfrow=c(2,2))
#Feature transformed data fields
hist(sqrt(loanData$CreditScore), xlab="Credit score", ylab="Value", main="Credit Score Histogram", col="#ffae42")
hist(log(loanData$LoanSanctionAmount), xlab="Loan Sanction Amount", ylab="Value", main="Loan Sanction Amount Histogram", col="#ffae42")
hist(log(loanData$LoanAmountRequest), xlab="Loan Amount Request", ylab="Value", main="Loan Amount Request", col="#ffae42")
hist(log(loanData$Dependents), xlab="Number of Dependents", ylab="Value", main="Dependents Histogram", col="#ffae42")

#Factor the categorical variables in train and test data
loanData_train$IncomeStability=as.factor(loanData_train$IncomeStability)
loanData_train$CoApplicant=as.factor(loanData_train$CoApplicant)
loanData_test$IncomeStability=as.factor(loanData_test$IncomeStability)
loanData_test$CoApplicant=as.factor(loanData_test$CoApplicant)

#Initial Multi-linear Regression Model
model_1=lm(log(LoanSanctionAmount)~log(LoanAmountRequest)+CreditScore+log(Dependents)+CoApplicant,data=loanData_train)

lm_predict=predict(model_1, loanData_test)
#Aggregate Root Mean Squared Error difference
rsme_lm<-sqrt(mean((loanData_test$LoanSanctionAmount-lm_predict)^2))

plot(model_1)
summary(model_1)
ncvTest(model_1)
vif(model_1)
durbinWatsonTest(model_1)
hist(model_1$residuals)


#----------------------------Decision Tree Regression Model----------------------------------------

library(MASS)
library(tree)
fit_m=tree(LoanSanctionAmount~.,loanData_train)
fit_m
summary(fit_m)

plot(fit_m)
text(fit_m, pretty=0)
cv_fit=cv.tree(fit_m)
plot(cv_fit$size, cv_fit$dev, type='b', xlab="best value", ylab="Error", main="Deviance vs Tree Size")
prune_fit=prune.tree(fit_m, best=10)

plot(prune_fit)
text(prune_fit, pretty=0)
pred3=predict(prune_fit, test=loanData_test)
pred3
summary(pred3)
summary(loanData_test$LoanSanctionAmount)
#Aggregate Root Mean Squared Error difference
rsme_dt=sqrt(mean((pred3-loanData_test$LoanSanctionAmount)^2))



#---------------------------Random Forest Regression--------------------------------------

library(randomForest)
library(caret)

loan_rf<-randomForest(LoanSanctionAmount~.,data=loanData_train, mtry = 4,importance = TRUE, na.action = na.omit)
loan_rf
varImp(loan_rf)
summary(predict(loan_rf,loanData_test))
summary(loanData_test$LoanSanctionAmount)
plot(predict(loan_rf,loanData_test),loanData_test$LoanSanctionAmount)
#Aggregate Root Mean Squared Error difference
rsme_rf<-sqrt(mean((loanData_test$LoanSanctionAmount-predict(loan_rf,loanData_test))^2))



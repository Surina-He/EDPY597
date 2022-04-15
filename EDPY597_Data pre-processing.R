install.packages("tidyverse")
install.packages("reader")
install.packages("zoo")
install.packages("lubridate")

library(tidyverse)
library(dplyr)

getwd()


# --------------Loading dataset and selected interets variables-----------------
# The dataframe has 23503 rows and 9614 columns
library(reader)
hsls <- read_csv("hsls.csv",na = c("-7", "-8", "-9"))
View(hsls) # view the data in the script
head(hsls) # check the variable names
str(hsls) # check the structure of the dataframe

# Import data containing interest variable names
hsls_interest_variables <- read_csv("hsls_interest variables.csv")
View(hsls_interest_variables)



# ---------------------------Subsetting columns and rows------------------------
# Subsetting columns from raw dataset based on interested variable names
# The new dataframe has 23503 rows and 98 columns now
# There are 1 character data, 97 numeric data
hsls_interest <-hsls %>%
  subset(select = hsls_interest_variables$`Variable Name`) 
skimr::skim(hsls_interest)

# Subsetting rows based on two conditions
# Condition1: select participants participated all four waves
print(hsls_interest$X4UNIV1) 
hsls_con1<-subset(hsls_interest,X4UNIV1 == "11111") #subset condition1
# Condition2: select parent respondent is bio/adoptive/step-mother or father
# The result is 8142 bio/adoptive/step-mother respondent and 2408 father
hsls_con2<-subset(hsls_con1,X1MOMRESP == 1| X1DADRESP == 1)
hsls_con2<-subset (hsls_con2, select = -c(X4UNIV1,X1MOMRESP,X1DADRESP, STU_ID)) # removing filtering variables
skimr::skim(hsls_con2)
summary(hsls_con2)
table(hsls_con2$X1SEX)
# The new dataframe has 10550 rows and 98 columns now
# There are 94 numeric data now



# ----------------------------Pre-processing variable---------------------------
# Changing the format of current date from %y%m to %y%m%d
print(hsls_con2$X1STDOB) 
hsls_con2$X1STDOB <-as.factor(hsls_con2$X1STDOB)
skimr::skim(hsls_con2$X1STDOB)
library(zoo)
yrmo <- as.yearmon(hsls_con2$X1STDOB, "%Y%m") #read the current date data
yrmo
hsls_con2$X1STDOB
as.Date(yrmo) #change %y%m to %y%m%d format
hsls_con2$X1STDOB <- as.Date(yrmo) # replace original date value with new one
hsls_con2$X1STDOB
# Changing date to age
library(lubridate)
test_date <- as.Date("2009-12-01") #create test_date
age<-trunc((hsls_con2$X1STDOB %--% test_date) / years(1)) # calculate age
summary(age) 
hsls_con2$new_col <- age #create new column using the value of age
print(hsls_con2$new_col)
hsls_con2<-subset (hsls_con2, select = -c(X1STDOB)) # removing date
skimr::skim(hsls_con2)
summary(hsls_con2)


# Checking frequency of two possible outcome variables
# Outcome1: enrolled in post-secondary or not
table(hsls_con2$X4ATNDCLG16FB) 
# Outcome2: enrolled in post-secondary with level
table(hsls_con2$X4PSENRSTLV)
# Recoding X4PSENRSTLV from 5 levels to 3 levels
hsls_con2$X4PSENRSTLV<-recode(hsls_con2$X4PSENRSTLV,
                              '0'='0', '1' = '2','2' = '1', '3' = '1', '4' = '1')
table(hsls_con2$X4PSENRSTLV)
# Removing unused outcome variable
hsls_outcome<-subset (hsls_con2, select = -c(X4ATNDCLG16FB))
skimr::skim(hsls_outcome)
summary(hsls_outcome)
 


# ---------------------Checking and imputing missing values---------------------
# Count missing values
# Having 12.9% percent of missing value
sum(is.na(hsls_outcome))
mean(is.na(hsls_outcome))
# Computing missing percentage of each variable and case
library(mice)
pMiss <-function(hsls_outcome){round(sum(is.na(hsls_outcome))/length(hsls_outcome),3)}
apply(hsls_outcome, 1, pMiss)
apply(hsls_outcome, 2, pMiss)
# Removing cases whose NA>30% and filtering variables
hsls_missing_case<-subset(hsls_outcome,apply(hsls_outcome, 1, pMiss) < 0.30)
# Removing variables have extreme missing percentage
hsls_missing_var<-subset(hsls_missing_case, select=-c(P1TUITION,S1TUITION))
mean(is.na(hsls_missing_var))
skimr::skim(hsls_missing_var)
# The overall missing percent is 10.8%
# Imputing NA using multiple imputation
library(mice)
hsls_impute<-mice(hsls_missing_var,m=5,method = "pmm",maxit = 50,seed=1)
hsls_completed <- complete(hsls_impute,1) # getting back the completed dataset  
sum(is.na(hsls_completed))
mean(is.na(hsls_completed))
skimr::skim(hsls_completed)
summary(hsls_completed)



# --------------------------Feature selection (Lasso)---------------------------
install.packages("glmnet")
install.packages("ISLR")
install.packages("leaps")
library(glmnet)
library(ISLR)
library(leaps)

# Removing all missing values
hsls_lasso = na.omit(hsls_completed)

# Using model.matrix() function to create x
# predict hsls_impute on the hsls_impute data as outcome
which(colnames(hsls_lasso)=="X4PSENRSTLV" )
x = model.matrix(X4PSENRSTLV~., hsls_lasso)[,-90]
y = hsls_lasso %>%
  select(X4PSENRSTLV) %>%
  unlist() %>%
  as.numeric()

# Creating a sequence of λ
grid=10^seq (10,-2, length=100)

# Splitting the samples into a training set and a test set
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test =(-train )
y.test =y[test]

# Fitting lasso using glmnet() with alpha = 1
# we can see from the plot that some coefficients can be 0
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

# Using cross-validation and compute the associated test error
set.seed (1)
cv.out=cv.glmnet(x[train,],y[train],alpha =1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred= predict (lasso.mod ,s=bestlam , newx=x[test ,])
mean ((lasso.pred -y.test)^2)

# Refit the lasso on the full data set, using λ chosen by C-V
out=glmnet(x,y,alpha =1,lambda=grid)
out
lasso.coef=predict(out,type ="coefficients",s= bestlam) 
lasso.coef # printing coefficients of all predictors
lasso.coef[lasso.coef!=0] # printing coefficients not equal to 0
inds<-which(lasso.coef!=0) 
variables<-as.data.frame(row.names(lasso.coef)[inds]) # saving the name of predictors
print(variables)
# Only having 46 predictors finally



# ---------------------------Finalizing dataframe-------------------------------
# Only select outcome variable and three predictors
hsls_final3<-subset (hsls_lasso, select = c(X4PSENRSTLV,X3TGPATOT,X1SES, S1FRNDCLG))
skimr::skim(hsls_final3)w
# Exporting data for doing nested cross-validation in th python
write.csv(hsls_final3, "hsls_final3.csv")











#Both files contain a performance tag which represents whether the applicant has gone 90 days past 
#due or worse in the past 12-months (i.e. defaulted) after getting a credit card

#In some cases, you will find that all the variables in the credit bureau data are zero and 
#credit card utilisation is missing. These represent cases in which there is a no-hit in the credit bureau. 
#You will also find cases with credit card utilisation missing. 
#These are the cases in which the applicant does not have any other credit card.

#Some customers with missing outstanding balance and presence of home loan do have other credit bureau data missing

#In credit risk analytics, the weight of evidence (WOE) (and, equivalently, 
#information value analysis) is often used to identify the important variables. 

#Model evaluation
#Evaluate the models using relevant metrics and report the results. 
#As a part of model validation, predict the likelihood of default for the rejected candidates 
#and assess whether the results correspond to your expectations.


#Assessing the financial benefit of your project
#You need to assess and explain the potential financial benefit of your project 
#to the management of the bank. From a P&L perspective, identify the metrics you are trying to optimise, explain (in simple terms) how the analysis and the model works, and share the results of the model. Finally, assess the financial benefit of the model and report the following:
  
#The implications of using the model for auto approval or rejection, i.e. 
#how many applicants on an average would the model automatically approve or reject

#The potential credit loss avoided with the help of the model

#Assumptions based on which the model has been built 


#Make appropriate assumptions about the numbers wherever needed 
#(e.g. the potential average credit loss per default, etc.). 
#Present your analysis and recommendations in a PowerPoint presentation.  


#reading demographic data
Demo_Data <- read.csv("Demographic data.csv",header = T,
                      na.strings = c(""," ","NA","na","-"))

#Reading Credit Bureau data
Credit_Data <- read.csv("Credit Bureau data.csv",header = T,
                        na.strings = c(""," ","NA","na","-"))


library(dplyr)
library(mice)
library(VIM)
library(missMDA)
library(Information)
library(ggplot2)
library(ggthemes)
library(scales)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(boot)
library(pROC)
library(DMwR)



#------------------------------------Checking whether both the datasets have same customers------------------------------------

sum(Demo_Data[,"Application.ID"] == Credit_Data[,"Application.ID"])#71295 True results
#-----------------------------------------------Exploratory Univariate analysis--------------------------------------------------------
summary(Demo_Data)

#Plotting histgram to see the distribution across variables-Demographic Data

List_Demo <- c("Age","Income","No.of.months.in.current.company","No.of.months.in.current.residence")

par(mfrow=c(2,2))

for (i in 1:length(List_Demo)){
  hist(Demo_Data[,List_Demo[i]],main = List_Demo[i],col="Red")
  next
}

dev.off()

#No of months in current residence is right skewed
#The other variables have flat distributions


#Plotting histgram to see the distribution across variables-Credit Data
summary(Credit_Data)
List_Credit <- names(Credit_Data[,-c(1,15,18,19)])

par(mfrow=c(2,4))

for (i in 1:length(List)){
  hist(Credit_Data[,List_Credit[i]],main = List_Credit[i],col="Red")
next
}
dev.off()

#most of the variables from credit data are right skewed
#----------------------------------------------Checking Outliers----------------------------------------------------------------
#Creating a list of numerical variables
List <- c("Age","Income","No.of.months.in.current.residence","No.of.months.in.current.company")
Credit_Data_numericalvariables <- Credit_Data[,c(2:14,16,17)]
List1 <- names(Credit_Data_numericalvariables)

#identifying the outliers for Demographic data
par(mfrow=c(2,4))

for (i in 1:length(List)){
  boxplot(Demo_Data[,List[i]],main= List[i],col="Red")
}

dev.off()
#No of months in Current Company from Demographic data has outliers
#To eliminate the outlier impact, need to bin No of months in Current Company based 
#on it's relation with Customer performance

#identifying the outliers for Credit data
par(mfrow=c(4,5))

for (i in 1:length(List1)){
  boxplot(Credit_Data[,List1[i]],main= List1[i],col="Red")
}

dev.off()


#In Credit data,the Variables with outliers are the important indicators of cutomers behaviour 
#whether he is good or bad customer.
#For e.g. No.of.times.90.DPD.or.worse.in.last.12.months is an important indicator of customer behaviour 
#and outliers present in this variable are the indicators of customer's bad behaviour.
#Hence to eliminate the outliers impact and also to retain the important piece of data from the perspective of identifying Bad customers
#most of the variables in creadit data need to be binned based on it's observed relation with Customer performance.

#---------------------------------------------------#Data sanity checks----------------------------------------------------------
#---------------------------------------------------Age-------------------------------------------------------------------------
length(Demo_Data$Age[Demo_Data$Age <= 0])#20 observations with less than 0 age

min(Demo_Data$Age[Demo_Data$Age > 0])# minimum age 15 years if age <= 0 is ignored


#Creating a subset of data where age is less than 18

Agelessthan18 <- Demo_Data[Demo_Data$Age < 18,]#65 records
Index_Age <- which(Demo_Data$Age < 18)


#we have 65 records where Customer's age is less than 18. 
#Customer with Age < 18 are omitted from both the datasets based on the assumption that 
#banks will not considers minors for credit card business 
#Some of the records even show these customers below 18 are married which is inconsistent with the society norms.

#removing the customers less than 18 from both the datasets
Demo_Data <- Demo_Data[-Index_Age, ]
Credit_Data <- Credit_Data[-Index_Age,]

#------------------------------------------------------------No.of.dependents---------------------------------------------------

#Creating a suset of data where age is above 18 and marital status single
Ageabove18Single <- Demo_Data[Demo_Data$Marital.Status..at.the.time.of.application.== "Single",]

#Assumptions
#As per the data dictionary, "No of dependents" variable in Demographic Dataset represents "No of Children". 
#We have observed that the customers with Single marital status have children which cannot be the case. 
#However, we have considered this variable for model building assuming 
#that it represents no of dependents(not "no of children").


#---------------------------------------------------Verifying Income----------------------------------------------------------------
#Income has negative or zero values
length(Demo_Data$Income[Demo_Data$Income <= 0])/nrow(Demo_Data)#106 observations(0.15%)

#Replacing negative and zero values as NA for Income
Demo_Data$Income[Demo_Data$Income <= 0] <- NA

#-----------------------------------------------------Avgas.CC.Utilization.in.last.12.months------------------------------------

#no of records where credit utilization is missing
sum(is.na(Credit_Data$Avgas.CC.Utilization.in.last.12.months))/nrow(Credit_Data)
#Avgas.CC.Utilization.in.last.12.months has 1058 missing values(1.5% missing values)

#Creating a subset of Credit Bureau data where Credit utilization is missing to derive the information on subset
missingcreditutilization <- Credit_Data[is.na(Credit_Data$Avgas.CC.Utilization.in.last.12.months),]
write.csv(missingcreditutilization,"missingcreditutilization.csv")
#Looking at the data, there are 560 cases where all the variables in the credit bureau data are zero and 
#credit card utilisation is missing. These represent the cases in which there is a no-hit in the credit bureau.
#remaining  498 cases where applicants do not have any other credit card (Credit utilization is missing)

#identifying index for the customers who have missing CC utilization
Index_CC <- which(is.na(Credit_Data$Avgas.CC.Utilization.in.last.12.months))

#It has been observed that there are customers with missing credit card utilization.  
#In some of these cases, the other variables in the Credit Bureau data are zero. 
#As per the available information on record, these are the cases where either there is no hit in the credit bureau 
#or the applicant does not have any other credit card.  
#Hence, the missing "Credit Card Utilization "in the Credit Card data is replaced with 0 and retained for analysis
#considering the fact that these customers have the relevant demographic data. 
Credit_Data[is.na(Credit_Data$Avgas.CC.Utilization.in.last.12.months),"Avgas.CC.Utilization.in.last.12.months"] <- 0


#--------------------------------function to find missing values/missing value percentage on demo data-------------------------

missing_value <- function(x){
  
  missing <- sum(is.na(x))
  
}

missing_perc <- function(x){
 
  Perc <-sum(is.na(x))/nrow(Demo_Data)*100
  Perc <- paste(round(Perc,2),"%")
}

apply(Demo_Data,2,missing_value)
apply(Demo_Data,2,missing_perc)

#Income(106),Education(119),Profession(13),Type of residence(8) has missing values

#Performance Tag from Demographic data has 1425 missing values(2%)

index_income <- which(is.na(Demo_Data$Income))#random missing values-0.15%
index_Education <- which(is.na(Demo_Data$Education))#random missing values-0.17%
index_Profession <- which(is.na(Demo_Data$Profession))#random missing values-0.02%
index_Typeofresidence <- which(is.na(Demo_Data$Type.of.residence))#random missing values-0.01%

#function to find missing values/missing value percentage on credit bureau data

missing_value <- function(x){
  
  missing <- sum(is.na(x))
  
}

missing_perc <- function(x){
  
  Perc <-sum(is.na(x))/nrow(Credit_Data)*100
  Perc <- paste(round(Perc,2),"%")
}


apply(Credit_Data,2,missing_value)#Presence of Open home loan and Outstanding balance has missing values
apply(Credit_Data,2,missing_perc)

#Performance Tag from Credit Bureau data has 1425 missing values(2%)

index_homeloan <- which(is.na(Credit_Data$Presence.of.open.home.loan))#random missing values
index_Outstandingbalance <- which(is.na(Credit_Data$Outstanding.Balance))#random missing values
length(index_homeloan)#272 missing values
length(index_Outstandingbalance)#272 missing values
sum(index_homeloan  == index_Outstandingbalance)
#missing values for Presence.of.open.home.loan and Outstanding Balance has commmon observations


#---------------------------------------------------Performance Tag--------------------------------------------------
#-----------------------Dependent Variable with Missing values for Demographic data and Credit bireau data----------------------

#identifying the index of missing values for Performance tag
Index_Demo_Performance <- which(is.na(Demo_Data$Performance.Tag))#No random missing values
Index_Credit_Performance <- which(is.na(Credit_Data$Performance.Tag))#No Random missing values

length(Index_Demo_Performance)#1425 observations
length(Index_Credit_Performance)#1425 observations

#These Customers with missing Performance tag in both the datasets are rejected customers 
#on which we need to evaluate the model

#--------------------------------------------Missing Values Imputation-Mice Package----------------------------------------------

#It has been observed that all the other variables apart from Credit utilization and Performance tag has
#less than 1%
#Using Mice package to impute the missing values from Demographic and Credit Bureau Data

#Precisely, the methods used by mice package are:
#PMM (Predictive Mean Matching)  - For numeric variables
#logreg(Logistic Regression) - For Binary Variables( with 2 levels)
#polyreg(Bayesian polytomous regression) - For Factor Variables (>= 2 levels)
#Proportional odds model (ordered, >= 2 levels)

#-------------------------------Imputing missing values for continuous variables-Demographic Data(Method-PMM)----------------------------------
#Creating a subset of numerical/continuous variables variables
Demo_Continuous <- Demo_Data[,c("Age","Income","No.of.months.in.current.residence","No.of.months.in.current.company")]

#Identifying the pattern of missing values
md.pattern(Demo_Continuous)
mice_plot <- aggr(Demo_Continuous, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Demo_Continuous), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#imputing missing values for Continuous variables from Demographic data
imputed_Demo_Continuous <- mice(Demo_Continuous, m=1, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Demo_Continuous)

#Checking imputed values
imputed_Demo_Continuous$imp$Age
imputed_Demo_Continuous$imp$Income
imputed_Demo_Continuous$imp$No.of.months.in.current.residence
imputed_Demo_Continuous$imp$No.of.months.in.current.company

#get the complete data
completeDemo_Continuous <- complete(imputed_Demo_Continuous)

#Rounding up Income numbers
completeDemo_Continuous$Income <- round(completeDemo_Continuous$Income)

range(completeDemo_Continuous$Income)

#------------------------------Imputing missing values for categorical variable(Demographic Data)(Method-polyreg)-------------

#Converting No of dependents to categorical variable
Demo_Data$No.of.dependents <- as.factor(Demo_Data$No.of.dependents)

#Creating a subset of categorical variables with 2 levels
Demo_Categorical1 <- Demo_Data[,c("Gender","Marital.Status..at.the.time.of.application.")]
#Creating a subset of categorical variables with more than 2 levels
Demo_Categorical2 <- Demo_Data[,c("No.of.dependents","Education","Profession","Type.of.residence")]

#Identifying the pattern of missing values
md.pattern(Demo_Categorical1)
mice_plot <- aggr(Demo_Categorical1, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Demo_Categorical1), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

md.pattern(Demo_Categorical2)
mice_plot <- aggr(Demo_Categorical2, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Demo_Categorical2), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#imputing missing values for Categorical variables from Demographic data
# imputed_Demo_Categorical1 <- mice(Demo_Categorical1, m=1, maxit = 50, method = 'logreg', seed = 500)
# summary(imputed_Demo_Categorical1)
# 
# imputed_Demo_Categorical2 <- mice(Demo_Categorical2, m=1, maxit = 50, method = 'polyreg', seed = 500)
# summary(imputed_Demo_Categorical2)

#Saving imputed_Demo_Categorical1 and imputed_Demo_Categorical2 as RDS object
#saveRDS(imputed_Demo_Categorical1, file = "imputed_Demo_Categorical1.rds") 
# saveRDS(imputed_Demo_Categorical2, file = "imputed_Demo_Categorical2.rds") 

# Restore the object 
imputed_Demo_Categorical1 <- readRDS(file = "imputed_Demo_Categorical1.rds")
imputed_Demo_Categorical2 <- readRDS(file = "imputed_Demo_Categorical2.rds")

#Checking imputed values
imputed_Demo_Categorical1$imp$Gender
imputed_Demo_Categorical1$imp$Marital.Status..at.the.time.of.application.
imputed_Demo_Categorical2$imp$No.of.dependents
imputed_Demo_Categorical2$imp$Education
imputed_Demo_Categorical2$imp$Profession
imputed_Demo_Categorical2$imp$Type.of.residence

#get the complete data
completeDemo_Categorical1 <- complete(imputed_Demo_Categorical1)
completeDemo_Categorical2 <- complete(imputed_Demo_Categorical2)

#Combining the Cateogorical,Continuous variables after imputation with Preformance Tag and Application ID

Final_Demo_Data <- cbind(completeDemo_Categorical1,completeDemo_Categorical2,
                         completeDemo_Continuous,Demo_Data$Application.ID,Demo_Data$Performance.Tag)
colnames(Final_Demo_Data)[11:12] <- c("Application.ID","Performance.Tag")
str(Final_Demo_Data)
summary(Final_Demo_Data)


#------------------------------Imputing missing values for Credit Data----------------------------------------------------------

#----------------------------------------------------------No.of.trades.opened.in.last.6.months-------------------------------

Index_nooftrades <- which(is.na(Credit_Data$No.of.trades.opened.in.last.6.months))
#1 customer with missing nof of trades in last 6 months

#hence replacing No of Trades missing value with mean
Credit_Data[Index_nooftrades,"No.of.trades.opened.in.last.6.months"] <- mean(Credit_Data$No.of.PL.trades.opened.in.last.6.months)

#--------------------------------------Presence.of.open.home.loan and Outstanding Balance-------------------------------------
#There is one categorical variable "Presence.of.open.home.loan" 
#and one continuous variable "Outstanding Balance" with missing values
#the missing values on "Presence.of.open.home.loan" and "Outstanding.Balance" are 272.
#hence as identified before the missing values for "Presence.of.open.home.loan" and "Outstanding Balance" have
#common records

length(index_homeloan )
length(index_Outstandingbalance)

sum(index_homeloan == index_Outstandingbalance)#272 true results

#Hence, Customers with missing data for presence of home loan do have missing data on outstanding balance

#subsetting the data where presence of home loans and outstanding balance has mssing values 
#to check the status on other variables

data <- Credit_Data[index_homeloan,]
summary(data)#No credit data available for the customers 
#with missing data for "Presence of Homeloan" and "Outstanding Balance"

#Checking the demographic data for these customers
Demo_withmissingOBandHL <- Demo_Data[index_homeloan,]
#customers with missing "Presence of Homeloan" and "Outstanding Balance" has demographic data

#It has been observed that the customer who have missing data for both "Presence of Homeloan" and "Outstanding Balance" 
#(272 customers) also do not have any other credit data on record. However these customers have demographic data
#hence replacing the missing values for  "Presence of Homeloan" and "Outstanding Balance"  as zero

Credit_Data[index_homeloan,"Presence.of.open.home.loan" ] <- 0
Credit_Data[index_Outstandingbalance,"Outstanding.Balance"] <- 0

str(Credit_Data)

summary(Credit_Data)

#------------------------------------------------------------Performance Tag with missing data------------------------------------

#Seperate the data where Performance tag is missing for both the datasets
Index_Demo_Performance <- which(is.na(Final_Demo_Data$Performance.Tag))
Index_Credit_Performance <- which(is.na(Credit_Data$Performance.Tag))

#Are both the datasets have same customers where performance tag is missing

sum(Final_Demo_Data[Index_Demo_Performance,"Application.ID"] == Credit_Data[Index_Credit_Performance,"Application.ID"])


#Creating a different subset for Rejected population(Customers with missing Performance tag

#Rejected_Demo_Data <- Final_Demo_Data[Index_Demo_Performance,]
#Final_Demo_Data <- Final_Demo_Data[-Index_Demo_Performance,]
#Rejected_Credit_Data <- Credit_Data[Index_Credit_Performance,]
#Credit_Data <- Credit_Data[-Index_Credit_Performance,]

#Customers with missing perforamnce tag on both the datasets are rejected customers

#creating a tag for approved and rejected customers based on corresponding index on missing values
Final_Demo_Data$Status <- Final_Demo_Data$Performance.Tag
Final_Demo_Data[Index_Demo_Performance,"Status"] <- "Rejected"
Final_Demo_Data[-Index_Demo_Performance,"Status"] <- "Approved"


Credit_Data$Status <- Credit_Data$Performance.Tag
Credit_Data[Index_Credit_Performance,"Status"] <- "Rejected"
Credit_Data[-Index_Credit_Performance,"Status"] <- "Approved"

#replacing missing performance tag for rejected customers with 1
Final_Demo_Data[Index_Demo_Performance,"Performance.Tag"] <- 1
Credit_Data[Index_Credit_Performance,"Performance.Tag"] <- 1
#-------------------------------------------------------Exploratory Bivariate analysis-------------------------------------------

#Perform the WOE transformation after binning. Next, we run logistic regression with 1 independent variable 
#having WOE values. If the slope is not 1 or the intercept is not ln(% of non-events / % of events) 
#then the binning algorithm is not good. [Source : Article]

#-------------------------------------------------------Demographic data--------------------------------------------------------
#using WOE and information value analysis
IV_Demo <- create_infotables(Final_Demo_Data,y="Performance.Tag")
IV_Demo$Summary
IV_Demo$Tables


IV_Demo$Summary
#Variable           IV

#8                                       Income 1.438040e-01
#12                                      Status 1.285606e-01
#9            No.of.months.in.current.residence 1.086684e-01
#10             No.of.months.in.current.company 9.392109e-02
#3                             No.of.dependents 3.933087e-02
#7                                          Age 1.285959e-02
#5                                   Profession 7.523716e-03
#6                            Type.of.residence 1.944644e-03
#11                              Application.ID 1.657657e-03
#4                                    Education 1.583289e-03
#2  Marital.Status..at.the.time.of.application. 7.140854e-04
#1                                       Gender 8.970843e-05


#Information Value	Variable Predictiveness
#Less than 0.02	Not useful for prediction
#0.02 to 0.1	Weak predictive Power
#0.1 to 0.3	Medium predictive Power
#0.3 to 0.5	Strong predictive Power
#>0.5	Suspicious Predictive Power


#Creating  dummy dataframe for WOE variable of Demographic dataset

Demo_WOE <- as.data.frame(cbind(Final_Demo_Data$Application.ID,Final_Demo_Data$Status))
names(Demo_WOE) <- c("Application.ID","Status")

#----------------------------------------------------Gender----------------------------------------------------------------------

Gender <- Final_Demo_Data%>%group_by(Gender)%>%
  summarise(NoofCustomers=n(),Bad_Customers=sum(Performance.Tag))%>%
  mutate(Bad_CustomersRate=round(Bad_Customers/NoofCustomers,2))

ggplot(Gender, aes(Gender, NoofCustomers,label = Bad_CustomersRate)) + 
  geom_bar(stat = 'identity',aes(fill=Gender)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)


IV_Demo$Tables$Gender

#Gender     N   Percent          WOE           IV
#1      F 16821 0.2361505  0.016951510 6.836540e-05
#2      M 54409 0.7638495 -0.005292101 8.970843e-05

#Creating new variable representing WOE for each level of Gender 
Demo_WOE$Gender_WOE=Final_Demo_Data$Gender

Demo_WOE$Gender_WOE <- ifelse(Demo_WOE$Gender_WOE == "F",0.016951510,-0.005292101)

#----------------------------------------*Marital.Status..at.the.time.of.application.-----------------------------------------------

MaritalStatus <- Final_Demo_Data%>%group_by(Marital.Status..at.the.time.of.application.)%>%
  summarise(NoofCustomers=n(),Bad_Customers=sum(Performance.Tag))%>%
  mutate(Bad_CustomersRate=round(Bad_Customers/NoofCustomers,2))

ggplot(MaritalStatus, aes(Marital.Status..at.the.time.of.application., NoofCustomers,label = Bad_CustomersRate)) + 
  geom_bar(stat = 'identity',aes(fill=Marital.Status..at.the.time.of.application.)) + 
  theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#Bad customers rate is higher for Single (7%) compared to married customer(6%)


IV_Demo$Tables$Marital.Status..at.the.time.of.application.

#Marital.Status..at.the.time.of.application.     N   Percent         WOE           IV
#1                                     Married 60695 0.8520988 -0.01131689 0.0001085899
#2                                      Single 10535 0.1479012  0.06310282 0.0007140854

#Creating new variable representing WOE for each level of Marital Status 
Demo_WOE$MaritalStatus_WOE=Final_Demo_Data$Marital.Status..at.the.time.of.application.

Demo_WOE$MaritalStatus_WOE <- ifelse(Demo_WOE$MaritalStatus_WOE == "Married",-0.01131689,0.06310282)
                                            
#-----------------------------------------------------*Income--------------------------------------------------------------------

#Using dplyr package
Income <- Final_Demo_Data%>%mutate(IncomeGroups=ntile(Income,4))%>%group_by(IncomeGroups)%>%
  summarize(min=min(Income),max=max(Income),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

Income$IncomeGroups <- paste(Income$min,"-",Income$max)

ggplot(Income, aes(IncomeGroups, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=IncomeGroups)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is decrease in Bad Rate with the increase in Income


IV_Demo$Tables$Income


#Income    N    Percent         WOE         IV
#1    [1,5] 6624 0.09299452  0.66007939 0.05427585
#2   [6,10] 6804 0.09552155  0.47093101 0.08036972
#3  [11,15] 6752 0.09479152  0.11852010 0.08177254
#4  [16,20] 6920 0.09715008  0.07113464 0.08227975
#5  [21,26] 8327 0.11690299 -0.07340192 0.08288969
#6  [27,31] 6891 0.09674295 -0.10817011 0.08396938
#7  [32,36] 6903 0.09691141 -0.29985624 0.09161963
#8  [37,41] 6781 0.09519865 -0.41385239 0.10525537
#9  [42,48] 7860 0.11034676 -0.35073405 0.11691639
#10 [49,60] 7368 0.10343956 -0.57696469 0.14380401

#Based on EDA, we can create three income groups below
#Income[1,14] 
#income[14,27]
#income[27,40]
#income[40,60]

Final_Demo_Data$IncomeGroup <- Final_Demo_Data$Income
Final_Demo_Data$IncomeGroup <- ifelse(Final_Demo_Data$IncomeGroup < 14,"1_14",
                      ifelse((Final_Demo_Data$IncomeGroup>= 14 & Final_Demo_Data$IncomeGroup <27),"14_27", 
                            ifelse((Final_Demo_Data$IncomeGroup>= 27 & Final_Demo_Data$IncomeGroup <40),"27_40","40_60")))

#recalculating WOE and information value analysis for income
IV_Demo <- create_infotables(Final_Demo_Data,y="Performance.Tag")
IV_Demo$Summary
IV_Demo$Tables

IV_Demo$Tables$IncomeGroup


#  IncomeGroup     N   Percent          WOE         IV
#1        1-14 17455 0.2450512  0.489034341 0.07276979
#2       14-27 17972 0.2523094 -0.002026917 0.07277083
#3       27-40 17879 0.2510038 -0.242035549 0.08600538
#4       40-60 17924 0.2516355 -0.452099107 0.12832723


#Creating new variable representing WOE for each level of Income
Demo_WOE$Income_WOE=Final_Demo_Data$IncomeGroup

Demo_WOE$Income_WOE <- ifelse(Demo_WOE$Income_WOE < 14 ,0.489034341,
                                            ifelse(Demo_WOE$Income_WOE >= 14 & Demo_WOE$Income_WOE < 27 ,-0.002026917,
                                                   ifelse(Demo_WOE$Income_WOE >= 27 & Demo_WOE$Income_WOE < 40 ,-0.242035549,-0.452099107)))
                                                   
#----------------------------------------------------Education---------------------------------------------------------------------
#Using dplyr package
Education <- Final_Demo_Data%>%group_by(Education)%>%
  summarise(NoofCustomers=n(),Bad_Customers=sum(Performance.Tag))%>%
  mutate(Bad_CustomersRate=round(Bad_Customers/NoofCustomers,2))%>%mutate(TotalCustomers=NoofCustomers/nrow(Final_Demo_Data))

ggplot(Education, aes(Education, NoofCustomers,label = Bad_CustomersRate)) + 
  geom_bar(stat = 'identity',aes(fill=Education)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#Bad Customers rate for Others is significantly high 
#but the No of Customers for this category is only 0.02% of overall Customers

IV_Demo$Tables$Education

#Education     N     Percent         WOE           IV
#1     Bachelor 17704 0.248546961  0.04739059 0.0005699442
#2      Masters 23992 0.336824372  0.01395259 0.0006359181
#3       Others   121 0.001698722  0.32040529 0.0008368275
#4          Phd  4548 0.063849502 -0.03941284 0.0009343117
#5 Professional 24865 0.349080444 -0.04353038 0.0015832886


#Creating new variable representing WOE for each level of Education
Demo_WOE$Education_WOE=Final_Demo_Data$Education

Demo_WOE$Education_WOE <- ifelse(Demo_WOE$Education_WOE == "Bachelor",0.04739059,
                                        ifelse(Demo_WOE$Education_WOE == "Masters",0.01395259,
                                               ifelse(Demo_WOE$Education_WOE == "Others",0.32040529,
                                                      ifelse(Demo_WOE$Education_WOE == "Phd",-0.03941284, -0.04353038))))

#-----------------------------------------------------------No of Dependents----------------------------------------------
#Using dplyr package
Noofdependents <- Final_Demo_Data%>%group_by(No.of.dependents)%>%
  summarise(NoofCustomers=n(),Bad_Customers=sum(Performance.Tag))%>%
  mutate(Bad_CustomersRate=round(Bad_Customers/NoofCustomers,2))%>%mutate(TotalCustomers=NoofCustomers/nrow(Final_Demo_Data))

ggplot(Noofdependents, aes(No.of.dependents, NoofCustomers,label = Bad_CustomersRate)) + 
  geom_bar(stat = 'identity',aes(fill=No.of.dependents)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)



IV_Demo$Tables$No.of.dependents

#No.of.dependents     N   Percent          WOE          IV
#1                1 15370 0.2157799 -0.128267001 0.003356653
#2                2 15266 0.2143198 -0.236986275 0.014214027
#3                3 16262 0.2283027  0.307383955 0.038922230
#4                4 12219 0.1715429 -0.049332586 0.039330791
#5                5 12113 0.1700548 -0.000687145 0.039330871

#It is observed that bad customers rate for less than 3 dependents is 5%, for 3 dependents it is 8% and
#for more than 3 dependents it is 6%


#Creating new variable representing WOE for each level of No of Dependents
Demo_WOE$Noofdependents_WOE=Final_Demo_Data$No.of.dependents

Demo_WOE$Noofdependents_WOE <- ifelse(Demo_WOE$Noofdependents_WOE == "1",-0.128267001,
                                        ifelse(Demo_WOE$Noofdependents_WOE == "2",-0.236986275,
                                               ifelse(Demo_WOE$Noofdependents_WOE == "3",0.307383955,
                                                      ifelse(Demo_WOE$Noofdependents_WOE == "4",-0.049332586, -0.000687145))))

#---------------------------------------------------*Profession-----------------------------------------------------------------
#Using dplyr package
Profession <- Final_Demo_Data%>%group_by(Profession)%>%
  summarise(NoofCustomers=n(),Bad_Customers=sum(Performance.Tag))%>%
  mutate(Bad_CustomersRate=round(Bad_Customers/NoofCustomers,2))%>%mutate(TotalCustomers=NoofCustomers/nrow(Final_Demo_Data))

ggplot(Profession, aes(Profession, NoofCustomers,label = Bad_CustomersRate)) + 
  geom_bar(stat = 'identity',aes(fill=Profession)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#Self Employed Customers do have relatively high bad cutomers(7%) compared to Salaries and Self Employed Professionals(6%)

IV_Demo$Tables$Profession

#Profession     N   Percent         WOE           IV
#1        SAL 40410 0.5673171 -0.03769112 0.0007927428
#2         SE 14299 0.2007441  0.16413199 0.0066061202
#3    SE_PROF 16521 0.2319388 -0.06378259 0.0075237158


#Creating new variable representing WOE for each level of Profession
Demo_WOE$Profession_WOE=Final_Demo_Data$Profession

Demo_WOE$Profession_WOE <- ifelse(Demo_WOE$Profession_WOE == "SAL",-0.03769112,
                                             ifelse(Demo_WOE$Profession_WOE == "SE",0.16413199,-0.06378259))


#---------------------------------------------------------Type.of.residence----------------------------------------------------------
#Using dplyr package
Typeofresidence <- Final_Demo_Data%>%group_by(Type.of.residence)%>%
  summarise(NoofCustomers=n(),Bad_Customers=sum(Performance.Tag))%>%
  mutate(Bad_CustomersRate=round(Bad_Customers/NoofCustomers,2))%>%mutate(TotalCustomers=NoofCustomers/nrow(Final_Demo_Data))

ggplot(Typeofresidence, aes(Type.of.residence, NoofCustomers,label = Bad_CustomersRate)) + 
  geom_bar(stat = 'identity',aes(fill=Type.of.residence)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#60% of the customers have rented homes where bad customers rate is 6%
#Company Provided have 6% bad customers rate and Living with parents have 7% bad customers rate.
#remaining others have 3% bad customers rate

IV_Demo$Tables$Type.of.residence

#Type.of.residence     N     Percent           WOE           IV
#1    Company provided  1628 0.022855538  0.0008056132 1.483878e-08
#2 Living with Parents  1808 0.025382564  0.0835424646 1.837960e-04
#3              Others   199 0.002793767 -0.7435803227 1.309833e-03
#4               Owned 14228 0.199747297 -0.0515285961 1.828367e-03
#5              Rented 53367 0.749220834  0.0124238720 1.944644e-03

Final_Demo_Data$Type.of.residence <- ifelse(Final_Demo_Data$Type.of.residence == "Company provided","Companyprovided",
                                            ifelse(Final_Demo_Data$Type.of.residence == "Living with Parents","LivingwithParents",
                                                   ifelse(Final_Demo_Data$Type.of.residence == "Others","Others",
                                                          ifelse(Final_Demo_Data$Type.of.residence == "Owned","Owned", "Rented"))))


#Creating new variable representing WOE for each level of Type.of.residence
Demo_WOE$TypeofResidence_WOE=Final_Demo_Data$Type.of.residence

Demo_WOE$TypeofResidence_WOE <- ifelse(Demo_WOE$TypeofResidence_WOE == "Companyprovided",0.0008056132,
                                             ifelse(Demo_WOE$TypeofResidence_WOE == "LivingwithParents",0.0835424646,
                                                    ifelse(Demo_WOE$TypeofResidence_WOE == "Others",-0.7435803227,
                                                           ifelse(Demo_WOE$TypeofResidence_WOE == "Owned",-0.0515285961, 0.0124238720))))

#-------------------------------------------------------Age------------------------------------------------------------------

#Using dplyr package
Age <- Final_Demo_Data%>%mutate(AgeGroups=ntile(Age,4))%>%group_by(AgeGroups)%>%
  summarize(min=min(Age),max=max(Age),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
            Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))%>%mutate(WOE=log(GoodCustomers_Rate/BadCustomers_Rate))

Age$AgeGroups <- paste(Age$min,"-",Age$max)

ggplot(Age, aes(AgeGroups, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=AgeGroups)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)



IV_Demo$Tables$Age

#Age    N    Percent         WOE          IV
#1  [18,30] 6067 0.08517479  0.13891273 0.001747248
#2  [31,35] 7159 0.10050541  0.20914059 0.006567998
#3  [36,38] 7086 0.09948056  0.08965310 0.007399741
#4  [39,41] 7274 0.10211989  0.04568236 0.007617172
#5  [42,44] 7114 0.09987365 -0.11452877 0.008863240
#6  [45,47] 6959 0.09769760 -0.02811540 0.008939522
#7  [48,50] 6868 0.09642005 -0.03999059 0.009091044
#8  [51,53] 6959 0.09769760 -0.14945263 0.011135408
#9  [54,57] 7731 0.10853573 -0.06590337 0.011593401
#10 [58,65] 8013 0.11249474 -0.10864128 0.012859587


#Based on EDA, we can create groups below
#Income[18,54] 
#income[54,65]

Final_Demo_Data$AgeGroup <- Final_Demo_Data$Age
Final_Demo_Data$AgeGroup <- ifelse(Final_Demo_Data$AgeGroup <= 30,"18_30",
                                      ifelse((Final_Demo_Data$Income> 30 & Final_Demo_Data$Income <=40),"31_40", 
                                           ifelse((Final_Demo_Data$Income> 40 & Final_Demo_Data$Income <=50),"41_50","51_65")))

#Using dplyr package
Age <- Final_Demo_Data%>%group_by(AgeGroup)%>%
  summarize(NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
            Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))%>%mutate(WOE=log(GoodCustomers_Rate/BadCustomers_Rate))


ggplot(Age, aes(AgeGroup, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=AgeGroup)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#Age Group 18-30 and 51-65 are the one where bad customers rate is high

#recalculating WOE and information value analysis for age
IV_Demo <- create_infotables(Final_Demo_Data,y="Performance.Tag")
IV_Demo$Summary
IV_Demo$Tables

IV_Demo$Tables$AgeGroup

#AgeGroup     N    Percent        WOE          IV
#1    18-30  6067 0.08517479  0.1389127 0.001747248
#2    31-40 12301 0.17269409 -0.3025362 0.015608659
#3    41-50 10262 0.14406851 -0.3877946 0.033929112
#4    51-65 42600 0.59806261  0.1272422 0.044169819

#Creating new variable representing WOE for each level of age
Demo_WOE$Age_WOE=Final_Demo_Data$AgeGroup

Demo_WOE$Age_WOE <- ifelse(Demo_WOE$Age_WOE == "18-30",0.1389127,
                           ifelse(Demo_WOE$Age_WOE == "31-40",-0.3025362,
                                  ifelse(Demo_WOE$Age_WOE == "41-50",-0.3877946,0.1272422)))
#---------------------------------------------------------"No.of.months.in.current.residence"-----------------------------------------------------------

#Using dplyr package
CurrentresidencePeriod <- Final_Demo_Data%>%mutate(CurrentresidencePeriod=ntile(No.of.months.in.current.residence,2))%>%
  group_by(CurrentresidencePeriod)%>%summarize(min=min(No.of.months.in.current.residence),
  max=max(No.of.months.in.current.residence),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

CurrentresidencePeriod$CurrentresidencePeriod <- paste(CurrentresidencePeriod$min,"-",CurrentresidencePeriod$max)

ggplot(CurrentresidencePeriod, aes(CurrentresidencePeriod, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=CurrentresidencePeriod)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#Customers  with less than 10 months in Current Residence have lesser(5%) Bad Customers 
#compared to Customers with more than 10 months in current residence (8%)

IV_Demo$Tables$No.of.months.in.current.residence

#No.of.months.in.current.residence     N    Percent         WOE         IV
#1                            [6,10] 35598 0.49976134 -0.27405968 0.03332248
#2                           [11,28]  6877 0.09654640  0.66819682 0.09127345
#3                           [29,48]  7097 0.09963499  0.33053064 0.10387043
#4                           [49,71]  7140 0.10023866  0.11058556 0.10515738
#5                           [72,97]  7330 0.10290608  0.02828707 0.10524075
#6                          [98,126]  7188 0.10091254 -0.19218142 0.10866835


#Creating a group for no of months in current residence as "6-10" and "10-126" months
Final_Demo_Data$CurrentResidencePeriod <- Final_Demo_Data$No.of.months.in.current.residence
Final_Demo_Data$CurrentResidencePeriod <- ifelse(Final_Demo_Data$CurrentResidencePeriod <= 6,"Lessthanorequalto6months",
                                                 ifelse(Final_Demo_Data$CurrentResidencePeriod <= 24,"Lessthanorequalto24months","Greaterthan24"))
                                              
#recalculating WOE and information value analysis for CurrentResidencePeriod
IV_Demo <- create_infotables(Final_Demo_Data,y="Performance.Tag")
IV_Demo$Summary
IV_Demo$Tables

IV_Demo$Tables$CurrentResidencePeriod

#CurrentResidencePeriod     N    Percent        WOE         IV
#1                 Greater than 24 30174 0.42361365  0.1127462 0.00565870
#2 Less than or equal to 24 months  7098 0.09964902  0.7785389 0.09090577
#3  Less than or equal to 6 months 33958 0.47673733 -0.3978067 0.15442990

#Creating new variable representing WOE for each level of CurrentResidencePeriod
Demo_WOE$CurrentResidencePeriod_WOE=Final_Demo_Data$CurrentResidencePeriod

Demo_WOE$CurrentResidencePeriod_WOE <- ifelse(Demo_WOE$CurrentResidencePeriod_WOE == "Lessthanorequalto6months",-0.3978067,
                                              ifelse(Demo_WOE$CurrentResidencePeriod_WOE == "Lessthanorequalto24months",0.7785389,0.1127462))

#---------------------------------------------------------*No.of.months.in.current.company------------------------------------------                                  

#Using dplyr package
CurrentcompanyPeriod <- Final_Demo_Data%>%mutate(CurrentcompanyPeriod=ntile(No.of.months.in.current.company,2))%>%
  group_by(CurrentcompanyPeriod)%>%summarize(min=min(No.of.months.in.current.company),
  max=max(No.of.months.in.current.company),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))
CurrentcompanyPeriod$CurrentcompanyPeriod <- paste(CurrentcompanyPeriod$min,"-",CurrentcompanyPeriod$max)

ggplot(CurrentcompanyPeriod, aes(CurrentcompanyPeriod, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=CurrentcompanyPeriod)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#Customers  with less than 34 month in Current company have 7 % Bad Customers 
#whereas Customers with more than 34 months in current company have 5% bad Customers rate

IV_Demo$Tables$No.of.months.in.current.company


#No.of.months.in.current.company    N    Percent         WOE         IV
#1                            [3,5] 7010 0.09841359  0.42083603 0.02099598
#2                           [6,12] 7125 0.10002808  0.45871848 0.04678176
#3                          [13,19] 7104 0.09973326  0.21545051 0.05187270
#4                          [20,26] 7055 0.09904535  0.01945138 0.05191049
#5                          [27,33] 7197 0.10103889 -0.17644834 0.05482325
#6                          [34,39] 6193 0.08694370 -0.10911610 0.05581022
#7                          [40,46] 7370 0.10346764 -0.33609734 0.06591369
#8                          [47,53] 7177 0.10075811 -0.37055991 0.07769915
#9                          [54,61] 7870 0.11048715 -0.41426005 0.09355319
#10                        [62,133] 7129 0.10008423 -0.06145003 0.09392109


#Creating a group for no of months in current residence as "3-36" and "36-133" months based on EDA
Final_Demo_Data$CurrentcompanyPeriod <- Final_Demo_Data$No.of.months.in.current.company
Final_Demo_Data$CurrentcompanyPeriod <- ifelse(Final_Demo_Data$CurrentcompanyPeriod <= 36,"3_36","37_133")

#recalculating WOE and information value analysis for No.of.months.in.current.company
IV_Demo <- create_infotables(Final_Demo_Data,y="Performance.Tag")
IV_Demo$Summary
IV_Demo$Tables

IV_Demo$Tables$CurrentcompanyPeriod


#CurrentcompanyPeriod     N   Percent        WOE         IV
#1                 3-36 38546 0.5411484  0.1893558 0.02109204
#2               37-133 32684 0.4588516 -0.2733726 0.05154257

#Creating new variable representing WOE for each level of CurrentcompanyPeriod
Demo_WOE$CurrentcompanyPeriod_WOE=Final_Demo_Data$CurrentcompanyPeriod

Demo_WOE$CurrentcompanyPeriod_WOE <- ifelse(Demo_WOE$CurrentcompanyPeriod_WOE == "3_36",0.1893558,-0.2733726)


#recalculating WOE and information value analysis
set.seed(100)
IV_Demo <- create_infotables(Final_Demo_Data,y="Performance.Tag")
IV_Demo$Summary


# Variable           IV
# 15                      CurrentResidencePeriod 1.544299e-01
# 8                                       Income 1.438040e-01
# 12                                      Status 1.285606e-01
# 13                                 IncomeGroup 1.283272e-01
# 9            No.of.months.in.current.residence 1.086684e-01
# 10             No.of.months.in.current.company 9.392109e-02
# 16                        CurrentcompanyPeriod 5.154257e-02
# 14                                    AgeGroup 4.416982e-02
# 3                             No.of.dependents 3.933087e-02
# 7                                          Age 1.285959e-02
# 5                                   Profession 7.523716e-03
# 6                            Type.of.residence 1.944644e-03
# 11                              Application.ID 1.657657e-03
# 4                                    Education 1.583289e-03
# 2  Marital.Status..at.the.time.of.application. 7.140854e-04
# 1                                       Gender 8.970843e-05

#--------------------------------------------Demographic Dataset created with WOE-Logitic Regression------------------------------------------
#Removing column 1 from Demo_WOE dataset
#names(Demo_WOE)[1] <- "Application.ID"

#Adding Performance tag to WOE data set

Demo_WOE$PerformanceTag=Final_Demo_Data$Performance.Tag

str(Demo_WOE)

#Converting Performance Tag to factor
Demo_WOE$PerformanceTag <- as.factor(Demo_WOE$PerformanceTag)

Approved_Demo_WOE <- Demo_WOE[Demo_WOE$Status == "Approved",-c(1,2)]
Rejected_Demo_WOE <- Demo_WOE[Demo_WOE$Status == "Rejected",-c(1,2)]

#--------------------------------------------Demographic Dataset -created to build Other Model like Random Forest, SVM------------------------------------------

# a new data subset for the other variables inlcuding Performance tag.
#There are outliers in the variable No of months with Current Company. 
#Hence binning the variable No of months with Current company based on Observed replation of the variable 
#with Customer performance. using this binned variable for model building instead of original variable for
#No of months in current company

Demo_Data1 <- Final_Demo_Data[,c(1:9,17)]
Demo_Data2 <- Final_Demo_Data[,c(11:13)]
str(Demo_Data1)
#Converting Current Company Period and Type.of.residence to factor
Demo_Data1$Type.of.residence <- as.factor(Demo_Data1$Type.of.residence)
Demo_Data1$CurrentcompanyPeriod <- as.factor(Demo_Data1$CurrentcompanyPeriod)

#creating dummy variables

k1 <- Demo_Data1

Demo_Data1 <- dummy.data.frame(Demo_Data1)

Demo_Data3 <- cbind(Demo_Data1,Demo_Data2)

Demo_Data3$Performance.Tag <- as.factor(Demo_Data3$Performance.Tag)

#-------------------------------------------------------Credit data--------------------------------------------------------
#using WOE and information value analysis
set.seed(100)
IV_Credit <- create_infotables(Credit_Data,y="Performance.Tag")
IV_Credit$Summary#Important Variables*)


# 7                    No.of.times.30.DPD.or.worse.in.last.12.months 0.792977173
# 4                     No.of.times.30.DPD.or.worse.in.last.6.months 0.763554613
# 3                     No.of.times.60.DPD.or.worse.in.last.6.months 0.716110845
# 5                    No.of.times.90.DPD.or.worse.in.last.12.months 0.679616409
# 6                    No.of.times.60.DPD.or.worse.in.last.12.months 0.674900932
# 10                           No.of.trades.opened.in.last.12.months 0.538827825
# 8                           Avgas.CC.Utilization.in.last.12.months 0.522019922
# 12                        No.of.PL.trades.opened.in.last.12.months 0.513241482
# 2                     No.of.times.90.DPD.or.worse.in.last.6.months 0.499824817*
# 14 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.491739207*
# 17                                              Total.No.of.Trades 0.441690407*
# 16                                             Outstanding.Balance 0.421097399*
# 11                         No.of.PL.trades.opened.in.last.6.months 0.363644002*
# 13  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0.334321001*
# 9                             No.of.trades.opened.in.last.6.months 0.331031811*
# 19                                                          Status 0.128560575
# 15                                      Presence.of.open.home.loan 0.048369912
# 18                                      Presence.of.open.auto.loan 0.001867389
# 1                                                   Application.ID 0.001657657


#Creating a dummy Credit_WOE data
Credit_WOE <- as.data.frame(cbind(Credit_Data$Application.ID,Credit_Data$Status))
names(Credit_WOE) <- c("Application.ID","Status")

#--------------------------------------------------------*Avgas.CC.Utilization.in.last.12.months-------------------------------------------
boxplot(Credit_Data$Avgas.CC.Utilization.in.last.12.months,col="red",main="Avgas.CC.Utilization.in.last.12.months")

#Using dplyr package
CCUtilization <- Credit_Data%>%mutate(AvgCCUtilization=ntile(Avgas.CC.Utilization.in.last.12.months,3))%>%
  group_by(AvgCCUtilization)%>%summarize(min=min(Avgas.CC.Utilization.in.last.12.months),
  max=max(Avgas.CC.Utilization.in.last.12.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

CCUtilization$AvgCCUtilization <- paste(CCUtilization$min,"-",CCUtilization$max)

ggplot(CCUtilization, aes(AvgCCUtilization, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=AvgCCUtilization)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate with the increase in Credit card utilization.

IV_Credit$Tables$Avgas.CC.Utilization.in.last.12.months


#Avgas.CC.Utilization.in.last.12.months    N    Percent        WOE         IV
#1                                   [0,4] 6578 0.09234873 -0.7663416 0.03916977
#2                                   [5,6] 5467 0.07675137 -1.1583286 0.10283722
#3                                   [7,8] 6870 0.09644813 -1.0885783 0.17543414
#4                                  [9,10] 6704 0.09411765 -1.1058733 0.24805562
#5                                 [11,14] 9511 0.13352520 -0.7427314 0.30176924
#6                                 [15,22] 7515 0.10550330 -0.2108032 0.30604630
#7                                 [23,37] 6771 0.09505826  0.5348756 0.34050924
#8                                 [38,52] 7498 0.10526464  0.6784524 0.40594361
#9                                 [53,71] 7005 0.09834339  0.7962049 0.49462061
#10                               [72,113] 7311 0.10263934  0.4660403 0.52201992


#Based on EDA Creating a group for Avgas.CC.Utilization.in.last.12.months
Credit_Data$AvgCCUtilizationGroups <- Credit_Data$Avgas.CC.Utilization.in.last.12.months
Credit_Data$AvgCCUtilizationGroups <- ifelse(Credit_Data$AvgCCUtilizationGroups <= 10,"Lessthan11",
                                             ifelse(Credit_Data$AvgCCUtilizationGroups > 10 & Credit_Data$AvgCCUtilizationGroups <= 32,"Lessthan33","MorethanorEqualto33"))

#recalculating WOE and information value analysis for Avgas.CC.Utilization.in.last.12.months
IV_Credit <- create_infotables(Credit_Data,y="Performance.Tag")
IV_Credit$Summary
IV_Credit$Tables

IV_Credit$Tables$AvgCCUtilizationGroups

#AvgCCUtilizationGroups     N   Percent        WOE        IV
#1           Less than 11 25619 0.3596659 -1.0134009 0.2416314
#2           Less than 33 21647 0.3039028 -0.1867258 0.2513991
#3           More than 33 23964 0.3364313  0.6484721 0.4399401

#it is observed that there is increase in bad rate with the increase in CC utilization

#Creating new variable representing WOE for each level of Avg CC Utilization
Credit_WOE$AvgCCUtilization_WOE <- Credit_Data$AvgCCUtilizationGroups
Credit_WOE$AvgCCUtilization_WOE <- ifelse(Credit_WOE$AvgCCUtilization_WOE == "Lessthan11",-1.0134009,
                               ifelse(Credit_WOE$AvgCCUtilization_WOE == "Lessthan33",-0.1867258,0.6484721))

#-------------------------------------------No.of.times.90.DPD.or.worse.in.last.6.months---------------------------------------------------------------

boxplot(Credit_Data$No.of.times.90.DPD.or.worse.in.last.6.months,col="red",main="No.of.times.90.DPD.or.worse.in.last.6.months")


#Using dplyr package
Times90DPDlast6months <- Credit_Data%>%mutate(Times90DPDlast6months=ntile(No.of.times.90.DPD.or.worse.in.last.6.months,2))%>%
  group_by(Times90DPDlast6months)%>%summarize(min=min(No.of.times.90.DPD.or.worse.in.last.6.months),
  max=max(No.of.times.90.DPD.or.worse.in.last.6.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

Times90DPDlast6months$Times90DPDlast6months <- paste(Times90DPDlast6months$min,"-",Times90DPDlast6months$max)


ggplot(Times90DPDlast6months, aes(Times90DPDlast6months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=Times90DPDlast6months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)


#It is observed that there is increase in bad customers rate 
#with the increase in number of times 90 days past due in last 6 months.

IV_Credit$Tables$No.of.times.90.DPD.or.worse.in.last.6.months
#No.of.times.90.DPD.or.worse.in.last.6.months     N   Percent        WOE        IV
#1                                        [0,0] 54812 0.7695072 -0.5483186 0.1828265
#2                                        [1,3] 16418 0.2304928  0.9507160 0.4998248

#Based on EDA Creating a group for No.of.times.90.DPD.or.worse.in.last.6.months
Credit_Data$Times90DPDlast6months <- Credit_Data$No.of.times.90.DPD.or.worse.in.last.6.months
Credit_Data$Times90DPDlast6months <- ifelse(Credit_Data$Times90DPDlast6months == 0,"No90DPDLast6mths","90DPDLast6mths")

#Creating new variable representing WOE for each level of No.of.times.90.DPD.or.worse.in.last.6.months
Credit_WOE$Times90DPD6months_WOE <- Credit_Data$No.of.times.90.DPD.or.worse.in.last.6.months
Credit_WOE$Times90DPD6months_WOE <- ifelse(Credit_WOE$Times90DPD6months_WOE == 0 ,-0.5483186,0.9507160)


#-----------------------------------------------No.of.times.60.DPD.or.worse.in.last.6.months--------------------------------------------



#Using dplyr package
Times60DPDlast6months <- Credit_Data%>%mutate(Times60DPDlast6months=ntile(No.of.times.60.DPD.or.worse.in.last.6.months,2))%>%
  group_by(Times60DPDlast6months)%>%summarize(min=min(No.of.times.60.DPD.or.worse.in.last.6.months),
  max=max(No.of.times.60.DPD.or.worse.in.last.6.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

Times60DPDlast6months$Times60DPDlast6months <- paste(Times60DPDlast6months$min,"-",Times60DPDlast6months$max)


ggplot(Times60DPDlast6months, aes(Times60DPDlast6months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=Times60DPDlast6months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of times 60 days past due in last 6 months.

IV_Credit$Tables$No.of.times.60.DPD.or.worse.in.last.6.months
#No.of.times.60.DPD.or.worse.in.last.6.months     N   Percent        WOE        IV
#1                                        [0,0] 51868 0.7281763 -0.6989428 0.2640992
#2                                        [1,1] 11390 0.1599045  0.4369694 0.3011444
#3                                        [2,5]  7972 0.1119191  1.4158917 0.7161108

#Based on EDA Creating a group for No.of.times.60.DPD.or.worse.in.last.6.months
Credit_Data$Time60DPDlast6months <- Credit_Data$No.of.times.60.DPD.or.worse.in.last.6.months
Credit_Data$Time60DPDlast6months <- ifelse(Credit_Data$Time60DPDlast6months == 0,"No60DPDLast6mths",
                                           ifelse(Credit_Data$Time60DPDlast6months == 1,"1time60DPDLast6mths","Morethan1time60DPDLast6mths"))


#Creating new variable representing WOE for each level of No.of.times.60.DPD.or.worse.in.last.6.months
Credit_WOE$Times60DPD6months_WOE <- Credit_Data$No.of.times.60.DPD.or.worse.in.last.6.months
Credit_WOE$Times60DPD6months_WOE <- ifelse(Credit_WOE$Times60DPD6months_WOE == 0 ,-0.6989428,
                                           ifelse(Credit_WOE$Times60DPD6months_WOE == 1 ,0.4369694,1.4158917))


#-------------------------------------------No.of.times.30.DPD.or.worse.in.last.6.months--------------------------------------------------------------------

#Using dplyr package
Times30DPDlast6months <- Credit_Data%>%mutate(Times30DPDlast6months=ntile(No.of.times.30.DPD.or.worse.in.last.6.months,4))%>%
  group_by(Times30DPDlast6months)%>%summarize(min=min(No.of.times.30.DPD.or.worse.in.last.6.months),
  max=max(No.of.times.30.DPD.or.worse.in.last.6.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

Times30DPDlast6months$Times30DPDlast6months <- paste(Times30DPDlast6months$min,"-",Times30DPDlast6months$max)


ggplot(Times30DPDlast6months, aes(Times30DPDlast6months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=Times30DPDlast6months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of times 30 days past due in last 6 months.

IV_Credit$Tables$No.of.times.30.DPD.or.worse.in.last.6.months
#No.of.times.30.DPD.or.worse.in.last.6.months     N   Percent        WOE        IV
#1                                        [0,0] 50055 0.7027236 -0.7749155 0.3037056
#2                                        [1,1]  9605 0.1348449  0.2323627 0.3117721
#3                                        [2,7] 11570 0.1624316  1.2647675 0.7635546

#Based on EDA Creating a group for No.of.times.30.DPD.or.worse.in.last.6.months
Credit_Data$Time30DPDlast6months <- Credit_Data$No.of.times.30.DPD.or.worse.in.last.6.months
Credit_Data$Time30DPDlast6months <- ifelse(Credit_Data$Time30DPDlast6months == 0,"No30DPDLast6mths",
                                           ifelse(Credit_Data$Time30DPDlast6months == 1,"One30DPDLast6mths","MorethanOne30DPDLast6mths"))

#Creating new variable representing WOE for each level of No.of.times.30.DPD.or.worse.in.last.6.months
Credit_WOE$Times30DPDlast6months_WOE <- Credit_Data$No.of.times.30.DPD.or.worse.in.last.6.months
Credit_WOE$Times30DPDlast6months_WOE <- ifelse(Credit_WOE$Times30DPDlast6months_WOE == 0 ,-0.7749155,
                                    ifelse(Credit_WOE$Times30DPDlast6months_WOE == 1,0.2323627,1.2647675))

#----------------------------------------No.of.times.90.DPD.or.worse.in.last.12.months----------------------------------------------------------------

#Using dplyr package
Times90DPDlast12months <- Credit_Data%>%mutate(Times90DPDlast12months=ntile(No.of.times.90.DPD.or.worse.in.last.12.months,4))%>%
  group_by(Times90DPDlast12months)%>%summarize(min=min(No.of.times.90.DPD.or.worse.in.last.12.months),
  max=max(No.of.times.90.DPD.or.worse.in.last.12.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

Times90DPDlast12months$Times90DPDlast12months <- paste(Times90DPDlast12months$min,"-",Times90DPDlast12months$max)


ggplot(Times90DPDlast12months, aes(Times90DPDlast12months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=Times90DPDlast12months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of times 90 days past due in last 12 months.

IV_Credit$Tables$No.of.times.90.DPD.or.worse.in.last.12.months
#No.of.times.90.DPD.or.worse.in.last.12.months     N   Percent        WOE        IV
#1                                         [0,0] 50485 0.7087604 -0.7209014 0.2710110
#2                                         [1,1] 11988 0.1682999  0.4587896 0.3144111
#3                                         [2,5]  8757 0.1229398  1.2981086 0.6796164

#Based on EDA Creating a group for No.of.times.90.DPD.or.worse.in.last.12.months
Credit_Data$Time90DPDlast12months <- Credit_Data$No.of.times.90.DPD.or.worse.in.last.12.months
Credit_Data$Time90DPDlast12months <- ifelse(Credit_Data$Time90DPDlast12months == 0,"No90DPDLast12mths",
                                           ifelse(Credit_Data$Time90DPDlast12months == 1,"One90DPDLast12mths","MorethanOne90DPDLast12mths"))


#Creating new variable representing WOE for each level of No.of.times.90.DPD.or.worse.in.last.12.months
Credit_WOE$Times90DPDlast12months_WOE <- Credit_Data$No.of.times.90.DPD.or.worse.in.last.12.months
Credit_WOE$Times90DPDlast12months_WOE <- ifelse(Credit_WOE$Times90DPDlast12months_WOE == 0 ,-0.7209014,
                                    ifelse(Credit_WOE$Times90DPDlast12months_WOE == 1,0.4587896,1.2981086))

#------------------------------------------No.of.times.60.DPD.or.worse.in.last.12.months-----------------------------------------------------------------------------

#Using dplyr package
Times60DPDlast12months <- Credit_Data%>%mutate(Times60DPDlast12months=ntile(No.of.times.60.DPD.or.worse.in.last.12.months,3))%>%
  group_by(Times60DPDlast12months)%>%summarize(min=min(No.of.times.60.DPD.or.worse.in.last.12.months),
  max=max(No.of.times.60.DPD.or.worse.in.last.12.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

Times60DPDlast12months$Times60DPDlast12months <- paste(Times60DPDlast12months$min,"-",Times60DPDlast12months$max)


ggplot(Times60DPDlast12months, aes(Times60DPDlast12months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=Times60DPDlast12months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of times 60 days past due in last 12 months.

IV_Credit$Tables$No.of.times.60.DPD.or.worse.in.last.12.months
#No.of.times.60.DPD.or.worse.in.last.12.months     N   Percent         WOE        IV
#1                                         [0,0] 45834 0.6434648 -0.73831801 0.2562433
#2                                         [1,1] 12914 0.1813000 -0.02533879 0.2563584
#3                                         [2,7] 12482 0.1752352  1.19037123 0.6749009

#Based on EDA Creating a group for No.of.times.60.DPD.or.worse.in.last.12.months
Credit_Data$Time60DPDlast12months <- Credit_Data$No.of.times.60.DPD.or.worse.in.last.12.months
Credit_Data$Time60DPDlast12months <- ifelse(Credit_Data$Time60DPDlast12months == 0,"No60DPDLast12mths",
                                            ifelse(Credit_Data$Time60DPDlast12months == 1,"One60DPDLast12mths","MorethanOne60DPDLast12mths"))


#Creating new variable representing WOE for each level of No.of.times.60.DPD.or.worse.in.last.12.months
Credit_WOE$Times60DPDlast12months_WOE <- Credit_Data$No.of.times.60.DPD.or.worse.in.last.12.months
Credit_WOE$Times60DPDlast12months_WOE <- ifelse(Credit_WOE$Times60DPDlast12months_WOE == 0 ,-0.73831801,
                                     ifelse(Credit_WOE$Times60DPDlast12months_WOE == 1,-0.02533879,1.19037123))


#------------------------------------------No.of.times.30.DPD.or.worse.in.last.12.months-----------------------------------------------------------------------------

#Using dplyr package
Times30DPDlast12months <- Credit_Data%>%mutate(Times30DPDlast12months=ntile(No.of.times.30.DPD.or.worse.in.last.12.months,3))%>%
  group_by(Times30DPDlast12months)%>%summarize(min=min(No.of.times.30.DPD.or.worse.in.last.12.months),
  max=max(No.of.times.30.DPD.or.worse.in.last.12.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

Times30DPDlast12months$Times30DPDlast12months <- paste(Times30DPDlast12months$min,"-",Times30DPDlast12months$max)


ggplot(Times30DPDlast12months, aes(Times30DPDlast12months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=Times30DPDlast12months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of times 30 days past due in last 12 months.

IV_Credit$Tables$No.of.times.30.DPD.or.worse.in.last.12.months
#No.of.times.30.DPD.or.worse.in.last.12.months     N    Percent        WOE        IV
#1                                         [0,0] 44818 0.62920118 -0.7677328 0.2676942
#2                                         [1,1] 11489 0.16129440 -0.2704859 0.2781862
#3                                         [2,2]  6264 0.08794047  0.4842363 0.3037364
#4                                         [3,9]  8659 0.12156395  1.4616975 0.7929772

#Based on EDA Creating a group for No.of.times.30.DPD.or.worse.in.last.12.months
Credit_Data$Time30DPDlast12months <- Credit_Data$No.of.times.30.DPD.or.worse.in.last.12.months
Credit_Data$Time30DPDlast12months <- ifelse(Credit_Data$Time30DPDlast12months == 0,"No30DPDLast12mths",
                                            ifelse(Credit_Data$Time30DPDlast12months == 1,"1time30DPDLast12mths",
                                                   ifelse(Credit_Data$Time30DPDlast12months == 2,"2times30DPDLast12mths","Morethan2times30DPDLast12mths")))


#Creating new variable representing WOE for each level of No.of.times.30.DPD.or.worse.in.last.12.months
Credit_WOE$Times30DPDlast12months_WOE <- Credit_Data$No.of.times.30.DPD.or.worse.in.last.12.months
Credit_WOE$Times30DPDlast12months_WOE <- ifelse(Credit_WOE$Times30DPDlast12months_WOE == 0 ,-0.7677328,
                                     ifelse(Credit_WOE$Times30DPDlast12months_WOE== 1 ,-0.2704859,
                                            ifelse(Credit_WOE$Times30DPDlast12months_WOE == 2,0.4842363,1.4616975)))

#-----------------------------------------------No.of.trades.opened.in.last.6.months--------------------------------------------------------

#Using dplyr package
TradesinLast6months <- Credit_Data%>%mutate(TradesinLast6months=ntile(No.of.trades.opened.in.last.6.months,4))%>%
  group_by(TradesinLast6months)%>%summarize(min=min(No.of.trades.opened.in.last.6.months),
  max=max(No.of.trades.opened.in.last.6.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

TradesinLast6months$TradesinLast6months <- paste(TradesinLast6months$min,"-",TradesinLast6months$max)


ggplot(TradesinLast6months, aes(TradesinLast6months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=TradesinLast6months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of trades in last 6 months.

IV_Credit$Tables$No.of.trades.opened.in.last.6.months
#No.of.trades.opened.in.last.6.months     N    Percent          WOE         IV
#1                                [0,0] 12207 0.17137442 -0.923433924 0.09906804
#2                             [1,1.21] 20226 0.28395339 -0.658830469 0.19209627
#3                                [2,2] 12457 0.17488418  0.271224566 0.20659759
#4                                [3,3]  9847 0.13824231  0.598527607 0.27115254
#5                                [4,4]  6621 0.09295241  0.689044901 0.33103132
#6                               [5,12]  9872 0.13859329  0.001880049 0.33103181

#Creating groups for No.of.trades.opened.in.last.6.months based on EDA results
Credit_Data$TradesinLast6months <- Credit_Data$No.of.trades.opened.in.last.6.months
Credit_Data$TradesinLast6months <- ifelse(Credit_Data$TradesinLast6months < 1,"NoTrades",
                                             ifelse(Credit_Data$TradesinLast6months >= 1 & Credit_Data$TradesinLast6months < 3,"Lessthan3Trades","Morethanorequalto3Trades"))

#recalculating WOE and information value analysis for No.of.trades.opened.in.last.6.months
IV_Credit <- create_infotables(Credit_Data,y="Performance.Tag")
IV_Credit$Summary
IV_Credit$Tables

IV_Credit$Tables$TradesinLast6months
#TradesinLast6months     N   Percent        WOE         IV
#1             Less than 3 Trades 32683 0.4588376 -0.2094815 0.01837906
#2 More than or equal to 3 Trades 26340 0.3697880  0.4350372 0.10321921
#3                      No Trades 12207 0.1713744 -0.9234339 0.20228725

#Creating new variable representing WOE for each level of No.of.trades.opened.in.last.6.months
Credit_WOE$TradesinLast6months_WOE <- Credit_Data$TradesinLast6months
Credit_WOE$TradesinLast6months_WOE <- ifelse(Credit_WOE$TradesinLast6months_WOE == "NoTrades" ,-0.9234339,
                                     ifelse(Credit_WOE$TradesinLast6months_WOE == "Lessthan3Trades",-0.2094815,0.4350372))

#-----------------------------------------No.of.trades.opened.in.last.12.months-------------------------------------------------

#Using dplyr package
TradesinLast12months <- Credit_Data%>%mutate(TradesinLast12months=ntile(No.of.trades.opened.in.last.12.months,3))%>%
  group_by(TradesinLast12months)%>%summarize(min=min(No.of.trades.opened.in.last.12.months),
  max=max(No.of.trades.opened.in.last.12.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

TradesinLast12months$TradesinLast12months <- paste(TradesinLast12months$min,"-",TradesinLast12months$max)


ggplot(TradesinLast12months, aes(TradesinLast12months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=TradesinLast12months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of trades in last 12 months.

IV_Credit$Tables$No.of.trades.opened.in.last.12.months
#No.of.trades.opened.in.last.12.months     N    Percent        WOE         IV
#1                                 [0,0]  4947 0.06945107 -1.0377871 0.04846454
#2                                 [1,1] 11355 0.15941317 -1.4011272 0.22497042
#3                                 [2,2]  9328 0.13095606 -1.1088764 0.32644765
#4                                 [3,4]  9636 0.13528008 -0.1468174 0.32918262
#5                                 [5,5]  4657 0.06537976  0.1954600 0.33190518
#6                                 [6,7]  8688 0.12197108  0.6072587 0.39076313
#7                                 [8,9]  7566 0.10621929  0.7400169 0.47148108
#8                               [10,12]  7045 0.09890496  0.6724257 0.53171408
#9                               [13,28]  8008 0.11242454 -0.2665477 0.53882783

#Creating groups for No.of.trades.opened.in.last.12.months based on EDA results
Credit_Data$TradesinLast12months <- Credit_Data$No.of.trades.opened.in.last.12.months
Credit_Data$TradesinLast12months <- ifelse(Credit_Data$TradesinLast12months < 2,"Lessthan2",
                                          ifelse(Credit_Data$TradesinLast12months >= 2 & Credit_Data$TradesinLast12months < 7,"Lessthan7","Morethanorequalto7"))

#recalculating WOE and information value analysis for No.of.trades.opened.in.last.6.months
IV_Credit <- create_infotables(Credit_Data,y="Performance.Tag")
IV_Credit$Summary
IV_Credit$Tables

IV_Credit$Tables$TradesinLast12months
#TradesinLast12months     N   Percent        WOE        IV
#1             Less than 2 16302 0.2288642 -1.2768646 0.2204814
#2             Less than 7 28012 0.3932613 -0.1405636 0.2277890
#3 More than or equal to 7 26916 0.3778745  0.4806083 0.3357631

#Creating new variable representing WOE for each level of No.of.trades.opened.in.last.12.months
Credit_WOE$TradesinLast12months_WOE <- Credit_Data$TradesinLast12months
Credit_WOE$TradesinLast12months_WOE <- ifelse(Credit_WOE$TradesinLast12months_WOE == "Lessthan2" ,-1.2768646,
                                  ifelse(Credit_WOE$TradesinLast12months_WOE == "Lessthan7",-0.1405636,0.4806083))

#-------------------------------------------------No.of.PL.trades.opened.in.last.6.months--------------------------------------------------
#Using dplyr package
PLTradesinLast6months <- Credit_Data%>%mutate(PLTradesinLast6months=ntile(No.of.PL.trades.opened.in.last.6.months,3))%>%
  group_by(PLTradesinLast6months)%>%summarize(min=min(No.of.PL.trades.opened.in.last.6.months),
  max=max(No.of.PL.trades.opened.in.last.6.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

PLTradesinLast6months$PLTradesinLast6months <- paste(PLTradesinLast6months$min,"-",PLTradesinLast6months$max)


ggplot(PLTradesinLast6months, aes(PLTradesinLast6months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=PLTradesinLast6months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of PL trades in last 6 months.

IV_Credit$Tables$No.of.PL.trades.opened.in.last.6.months
#No.of.PL.trades.opened.in.last.6.months     N   Percent        WOE        IV
#1                                   [0,0] 31143 0.4372175 -0.8943401 0.2398435
#2                                   [1,1] 13851 0.1944546  0.1717711 0.2460320
#3                                   [2,2] 13072 0.1835182  0.5384281 0.3135588
#4                                   [3,6] 13164 0.1848098  0.4692390 0.3636440

#Creating groups for No.of.PL.trades.opened.in.last.6.months based on EDA results
Credit_Data$PLTradesinLast6months <- Credit_Data$No.of.PL.trades.opened.in.last.6.months
Credit_Data$PLTradesinLast6months <- ifelse(Credit_Data$PLTradesinLast6months == 0,"NoTrades","PLTradesLast6months")


#recalculating WOE and information value analysis for No.of.PL.trades.opened.in.last.6.months
IV_Credit <- create_infotables(Credit_Data,y="Performance.Tag")
IV_Credit$Summary
IV_Credit$Tables

IV_Credit$Tables$PLTradesinLast6months
#PLTradesinLast6months     N   Percent        WOE        IV
#1                  No Trades 31143 0.4372175 -0.8943401 0.2398435
#2 PL Trades in Last 6 months 40087 0.5627825  0.3993675 0.3469456

#Creating new variable representing WOE for each level of No.of.PL.trades.opened.in.last.6.months
Credit_WOE$PLTradesinLast6months_WOE <- Credit_Data$PLTradesinLast6months
Credit_WOE$PLTradesinLast6months_WOE <- ifelse(Credit_WOE$PLTradesinLast6months_WOE == "NoTrades" ,-0.8943401,0.3993675)
                                   

#-------------------------------------------------No.of.PL.trades.opened.in.last.12.months--------------------------------------------------


#Using dplyr package
PLTradesinLast12months <- Credit_Data%>%mutate(PLTradesinLast12months=ntile(No.of.PL.trades.opened.in.last.12.months,3))%>%
  group_by(PLTradesinLast12months)%>%summarize(min=min(No.of.PL.trades.opened.in.last.12.months),
  max=max(No.of.PL.trades.opened.in.last.12.months),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

PLTradesinLast12months$PLTradesinLast12months <- paste(PLTradesinLast12months$min,"-",PLTradesinLast12months$max)


ggplot(PLTradesinLast12months, aes(PLTradesinLast12months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=PLTradesinLast12months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of PL trades in last 12 months.

IV_Credit$Tables$No.of.PL.trades.opened.in.last.12.months
#No.of.PL.trades.opened.in.last.12.months     N    Percent        WOE        IV
#1                                    [0,0] 25781 0.36194019 -1.2808125 0.3503189
#2                                    [1,1]  6691 0.09393514 -0.3350085 0.3594365
#3                                    [2,2]  6981 0.09800646  0.2034768 0.3638750
#4                                    [3,3]  8432 0.11837709  0.4855906 0.3984817
#5                                    [4,4]  8266 0.11604661  0.6300928 0.4593842
#6                                    [5,5]  6483 0.09101502  0.5947614 0.5012822
#7                                   [6,12]  8596 0.12067949  0.2949512 0.5132415

#Creating groups for No.of.PL.trades.opened.in.last.12.months based on EDA results
Credit_Data$PLTradesinLast12months <- Credit_Data$No.of.PL.trades.opened.in.last.12.months
Credit_Data$PLTradesinLast12months <- ifelse(Credit_Data$PLTradesinLast12months <= 0,"NoPLTrades",
                                            ifelse(Credit_Data$PLTradesinLast12months > 0 & Credit_Data$PLTradesinLast12months < 3,"Lessthan3PLTrades","Morethanorequalto3PLTrades"))

#recalculating WOE and information value analysis for No.of.PL.trades.opened.in.last.12.months
IV_Credit <- create_infotables(Credit_Data,y="Performance.Tag")
IV_Credit$Summary
IV_Credit$Tables

IV_Credit$Tables$PLTradesinLast12months
#PLTradesinLast12months     N   Percent         WOE           IV
#1             Less than 3 PL Trades 13672 0.1919416 -0.02849863 0.0001539547
#2 More than or equal to 3 PL Trades 31777 0.4461182  0.50086855 0.1398538202
#3                      No PL Trades 25781 0.3619402 -1.28081248 0.4901727438

#Creating new variable representing WOE for each level of No.of.trades.opened.in.last.12.months
Credit_WOE$PLTradesinLast12months_WOE <- Credit_Data$PLTradesinLast12months
Credit_WOE$PLTradesinLast12months_WOE <- ifelse(Credit_WOE$PLTradesinLast12months_WOE == "NoPLTrades" ,-1.28081248,
                                    ifelse(Credit_WOE$PLTradesinLast12months_WOE == "Lessthan3PLTrades",-0.02849863,0.50086855))

#------------------------------------------No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.---------------------------------

#Using dplyr package
InquiriesinLast6months <- Credit_Data%>%mutate(InquiriesinLast6months=ntile(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,2))%>%
  group_by(InquiriesinLast6months)%>%summarize(min=min(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.),
  max=max(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

InquiriesinLast6months$InquiriesinLast6months <- paste(InquiriesinLast6months$min,"-",InquiriesinLast6months$max)


ggplot(InquiriesinLast6months, aes(InquiriesinLast6months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=InquiriesinLast6months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of no of enquiries in last 6 months.

IV_Credit$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.     N   Percent        WOE        IV
#1                                                          [0,0] 25128 0.3527727 -0.9284035 0.2057235
#2                                                          [1,1] 13510 0.1896673  0.1954099 0.2136175
#3                                                          [2,2] 13345 0.1873508  0.3970876 0.2488303
#4                                                          [3,4] 11962 0.1679349  0.5791293 0.3216211
#5                                                         [5,10]  7285 0.1022743 -0.3828009 0.3343210

#Creating groups for No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. based on EDA results
Credit_Data$InquiriesinLast6months <- Credit_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
Credit_Data$InquiriesinLast6months <- ifelse(Credit_Data$InquiriesinLast6months <= 0,"NoEnquiry",
                                             ifelse(Credit_Data$InquiriesinLast6months > 0 & Credit_Data$InquiriesinLast6months <= 1,"1Enquiry","Morethan1Enquiry"))

#recalculating WOE and information value analysis for No.of.trades.opened.in.last.6.months
IV_Credit <- create_infotables(Credit_Data,y="Performance.Tag")
IV_Credit$Summary
IV_Credit$Tables

IV_Credit$Tables$InquiriesinLast6months
#InquiriesinLast6months     N   Percent        WOE          IV
#1              1 Enquiry 13510 0.1896673  0.1954099 0.007893947
#2    More than 1 Enquiry 32592 0.4575600  0.3406493 0.069616752
#3             No Enquiry 25128 0.3527727 -0.9284035 0.275340297

#Creating new variable representing WOE for each level of No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
Credit_WOE$InquiriesinLast6months_WOE <- Credit_Data$InquiriesinLast6months
Credit_WOE$InquiriesinLast6months_WOE <- ifelse(Credit_WOE$InquiriesinLast6months_WOE == "NoEnquiry" ,-0.9284035,
                                     ifelse(Credit_WOE$InquiriesinLast6months_WOE == "1Enquiry",0.1954099,0.3406493))


#-----------------------------------------No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.-------------------------------------------


#Using dplyr package
InquiriesinLast12months <- Credit_Data%>%mutate(InquiriesinLast12months=ntile(No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,2))%>%
  group_by(InquiriesinLast12months)%>%summarize(min=min(No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.),
  max=max(No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

InquiriesinLast12months$InquiriesinLast12months <- paste(InquiriesinLast12months$min,"-",InquiriesinLast12months$max)


ggplot(InquiriesinLast12months, aes(InquiriesinLast12months, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=InquiriesinLast12months)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of no of enquiries in last 12 months.

IV_Credit$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.     N    Percent        WOE        IV
#1                                                           [0,0] 20546 0.28844588 -1.4254213 0.3275847
#2                                                           [1,1]  3931 0.05518742 -0.2638503 0.3310104
#3                                                           [2,2]  8074 0.11335112  0.1114155 0.3324882
#4                                                           [3,3]  9277 0.13024007  0.2920709 0.3451279
#5                                                           [4,4]  7488 0.10512425  0.5437034 0.3846632
#6                                                           [5,5]  5233 0.07346624  0.8071797 0.4530759
#7                                                           [6,8]  9171 0.12875193  0.4078567 0.4787278
#8                                                          [9,20]  7510 0.10543310 -0.3815158 0.4917392

#Creating groups for No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. based on EDA results
Credit_Data$InquiriesinLast12months <- Credit_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
Credit_Data$InquiriesinLast12months <- ifelse(Credit_Data$InquiriesinLast12months <= 0,"NoEnquiry",
                                             ifelse(Credit_Data$InquiriesinLast12months > 0 & Credit_Data$InquiriesinLast12months < 4,"Lessthan4Enquiries","MorethanorEqualto4Enquiries"))

#recalculating WOE and information value analysis for No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
IV_Credit <- create_infotables(Credit_Data,y="Performance.Tag")
IV_Credit$Summary
IV_Credit$Tables

IV_Credit$Tables$InquiriesinLast12months
#InquiriesinLast12months     N   Percent        WOE          IV
#1             Less than 4 Enquiries 21282 0.2987786  0.1372348 0.005977439
#2 More than or Equal to 4 Enquiries 29402 0.4127755  0.3812346 0.076986120
#3                        No Enquiry 20546 0.2884459 -1.4254213 0.404570861

#Creating new variable representing WOE for each level of No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
Credit_WOE$InquiriesinLast12months_WOE <- Credit_Data$InquiriesinLast12months
Credit_WOE$InquiriesinLast12months_WOE <- ifelse(Credit_WOE$InquiriesinLast12months_WOE == "NoEnquiry" ,-1.4254213,
                                     ifelse(Credit_WOE$InquiriesinLast12months_WOE == "Lessthan4Enquiries",0.1372348,0.3812346))

#------------------------------------------Presence.of.open.auto.loan----------------------------------------------------------


AutoLoan <- Credit_Data%>%group_by(Presence.of.open.auto.loan)%>%
  summarise(NoofCustomers=n(),Bad_Customers=sum(Performance.Tag))%>%
  mutate(Bad_CustomersRate=round(Bad_Customers/NoofCustomers,2))

ggplot(AutoLoan, aes(as.factor(Presence.of.open.auto.loan), NoofCustomers,label = Bad_CustomersRate)) + 
  geom_bar(stat = 'identity',aes(fill=as.factor(Presence.of.open.auto.loan))) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that the Customers with presence of auto loan have relatively low bad ratings

IV_Credit$Tables$Presence.of.open.auto.loan

#Presence.of.open.auto.loan     N    Percent         WOE           IV
#1                      [0,0] 65203 0.91538678  0.01268836 0.0001481949
#2                      [1,1]  6027 0.08461322 -0.14719632 0.0018673890

#Creating new variable representing WOE for each level of Presence.of.open.auto.loan 
Credit_WOE$Autoloan_WOE=Credit_Data$Presence.of.open.auto.loan

Credit_WOE$Autoloan_WOE <- ifelse(Credit_WOE$Autoloan_WOE == 0,0.01268836,-0.14719632)

#------------------------------------------Presence.of.open.home.loan----------------------------------------------------------


HomeLoan <- Credit_Data%>%group_by(Presence.of.open.home.loan)%>%
  summarise(NoofCustomers=n(),Bad_Customers=sum(Performance.Tag))%>%
  mutate(Bad_CustomersRate=round(Bad_Customers/NoofCustomers,2))

ggplot(HomeLoan, aes(as.factor(Presence.of.open.home.loan), NoofCustomers,label = Bad_CustomersRate)) + 
  geom_bar(stat = 'identity',aes(fill=as.factor(Presence.of.open.home.loan))) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that the Customers with presence of home loan have relatively low bad ratings


IV_Credit$Tables$Presence.of.open.home.loan

#Presence.of.open.home.loan     N   Percent        WOE         IV
#1                      [0,0] 53046 0.7447143  0.1148293 0.01032849
#2                      [1,1] 18184 0.2552857 -0.4229341 0.04836991

#Creating new variable representing WOE for each level of Presence.of.open.home.loan 
Credit_WOE$Homeloan_WOE=Credit_Data$Presence.of.open.home.loan

Credit_WOE$Homeloan_WOE <- ifelse(Credit_WOE$Homeloan_WOE == 0,0.1148293,-0.4229341)
                                          

#------------------------------------------------------Outstanding.Balance-------------------------------------------------------------
#Using dplyr package
OutstandingBalance <- Credit_Data%>%mutate(OutstandingBalance=ntile(Outstanding.Balance,2))%>%
  group_by(OutstandingBalance)%>%summarize(min=min(Outstanding.Balance),
  max=max(Outstanding.Balance),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

OutstandingBalance$OutstandingBalance <- paste(OutstandingBalance$min,"-",OutstandingBalance$max)


ggplot(OutstandingBalance, aes(OutstandingBalance, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=OutstandingBalance)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in outstanding balance.

IV_Credit$Tables$Outstanding.Balance
#Outstanding.Balance    N    Percent        WOE         IV
#1             [0,6714] 7122 0.09998596 -1.1462902 0.08160428
#2         [6715,26283] 7123 0.10000000 -1.2815564 0.17847866
#3       [26286,388857] 7123 0.10000000 -0.1970249 0.18204115
#4      [388866,586441] 7123 0.10000000  0.3191497 0.19376919
#5      [586455,773442] 7123 0.10000000  0.6112207 0.24274248
#6      [773457,969226] 7123 0.10000000  0.6170585 0.29278499
#7     [969233,1349026] 7123 0.10000000  0.5358841 0.32919267
#8    [1349027,2959319] 7123 0.10000000 -0.6245173 0.35905151
#9    [2959321,3274024] 7123 0.10000000 -0.9540728 0.42001132
#10   [3274039,5218801] 7124 0.10001404  0.1018987 0.42109740

#Creating groups for Outstanding.Balance based on EDA results
Credit_Data$OutstandingBalance <- Credit_Data$Outstanding.Balance
Credit_Data$OutstandingBalance <- ifelse(Credit_Data$OutstandingBalance <= 773457,"LowerOutstandingBalance","HigherOutstandingBalance")

#It is observed that the Bad Customers rate is high with the Higher Outstanding balance group compared to
#Highest Outstanding balance group

#recalculating WOE and information value analysis for Outstanding.Balance
IV_Credit <- create_infotables(Credit_Data,y="Performance.Tag")
IV_Credit$Summary
IV_Credit$Tables

IV_Credit$Tables$OutstandingBalance
#OutstandingBalance     N Percent         WOE          IV
#1 Higher Outstanding Balance 35615     0.5  0.09219733 0.004426026
#2  Lower Outstanding Balance 35615     0.5 -0.10031718 0.009241854

#Creating new variable representing WOE for each level of Outstanding Balance
Credit_WOE$OutstandingBalance_WOE <- Credit_Data$OutstandingBalance
Credit_WOE$OutstandingBalance_WOE <- ifelse(Credit_WOE$OutstandingBalance_WOE == "LowerOutstandingBalance" ,-0.10031718,0.09219733)

#----------------------------------------------------------Total.No.of.Trades---------------------------------------------
#Using dplyr package
TotalTrades <- Credit_Data%>%mutate(TotalTrades=ntile(Total.No.of.Trades,2))%>%
  group_by(TotalTrades)%>%summarize(min=min(Total.No.of.Trades),
  max=max(Total.No.of.Trades),NoofCustomers=n(),Bad_Customers=sum(Performance.Tag),
  Good_Customers=NoofCustomers-Bad_Customers)%>%mutate(BadCustomers_Rate=round(Bad_Customers/NoofCustomers,2))%>%
  mutate(GoodCustomers_Rate=round(Good_Customers/NoofCustomers,2))

TotalTrades$TotalTrades <- paste(TotalTrades$min,"-",TotalTrades$max)


ggplot(TotalTrades, aes(TotalTrades, NoofCustomers,label = BadCustomers_Rate)) + 
  geom_bar(stat = 'identity',aes(fill=TotalTrades)) + theme(axis.text.x = element_text( hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#It is observed that there is increase in bad customers rate 
#with the increase in number of total trades.

IV_Credit$Tables$Total.No.of.Trades
#Total.No.of.Trades     N    Percent        WOE         IV
#1               [0,1]  3908 0.05486452 -1.0317571 0.03793177
#2               [2,2]  6756 0.09484768 -1.3832226 0.14096565
#3               [3,3]  8628 0.12112874 -0.9784147 0.21787566
#4               [4,4]  7528 0.10568581 -0.6233102 0.24932608
#5               [5,5]  5778 0.08111751 -0.1808839 0.25177887
#6               [6,7] 10100 0.14179419  0.2917107 0.26550378
#7               [8,8]  4731 0.06641864  0.5872842 0.29521648
#8              [9,10]  7532 0.10574196  0.7376331 0.37497103
#9             [11,19]  8837 0.12406289  0.5529263 0.42342315
#10            [20,44]  7432 0.10433806 -0.4622577 0.44169041

#Creating groups for Total.No.of.Trades based on EDA results
Credit_Data$TotalTrades <- Credit_Data$Total.No.of.Trades
Credit_Data$TotalTrades <- ifelse(Credit_Data$TotalTrades < 6,"Lessthan6Trades","Morethanorequalto6Trades")
                                             
#recalculating WOE and information value analysis for No.of.trades.opened.in.last.6.months
IV_Credit <- create_infotables(Credit_Data,y="Performance.Tag")
IV_Credit$Summary
IV_Credit$Tables

IV_Credit$Tables$TotalTrades
#TotalTrades     N   Percent        WOE        IV
#1             Less than 6 Trades 32598 0.4576443 -0.7689043 0.1952067
#2 More than or equal to 6 Trades 38632 0.5423557  0.3933564 0.2950706

#Creating new variable representing WOE for each level of Total.No.of.Trades
Credit_WOE$TotalTrades_WOE <- Credit_Data$TotalTrades
Credit_WOE$TotalTrades_WOE <- ifelse(Credit_WOE$TotalTrades_WOE == "Lessthan6Trades" ,-0.7689043,0.3933564)

Credit_Data_IMP_Factors <- Credit_Data[,c(19,21:33,35,16)]
set.seed(100)
IV_Credit <- create_infotables(Credit_Data_IMP_Factors,y="Performance.Tag")
IV_Credit$Summary

# Variable        IV
# 7    Time30DPDlast12months 0.7929772
# 4     Time30DPDlast6months 0.7635546
# 3     Time60DPDlast6months 0.7161108
# 5    Time90DPDlast12months 0.6796164
# 6    Time60DPDlast12months 0.6749009
# 2    Times90DPDlast6months 0.4998248*strong predictor
# 11  PLTradesinLast12months 0.4901727*strong predictor
# 1   AvgCCUtilizationGroups 0.4399401*strong predictor
# 15     Outstanding.Balance 0.4210974*strong predictor
# 13 InquiriesinLast12months 0.4045709*strong predictor
# 10   PLTradesinLast6months 0.3469456*strong predictor
# 9     TradesinLast12months 0.3357631*strong predictor
# 14             TotalTrades 0.2950706*medium Predictor
# 12  InquiriesinLast6months 0.2753403*medium Predictor
# 8      TradesinLast6months 0.2022873*medium Predictor



#---------------------------------------------Merging Demographic and credit data with WOE-Logistic Model---------------------------------------------------

#Adding Performance tag to WOE data set

Credit_WOE$PerformanceTag=as.factor(Credit_Data$Performance.Tag ) 

#merging Train_Demo_WOE with Credit_WOE data for logistic regression model
Data_WOE <- cbind(Demo_WOE,Credit_WOE)

#removing duplicate variables
Data_WOE <- Data_WOE[,-c(1,2,13)]

#creating data based on the Approved and rejected status
ApprovedData_WOE <- Data_WOE[Data_WOE$Status == "Approved",-c(11,12)]
RejectedData_WOE <- Data_WOE[Data_WOE$Status == "Rejected",-c(11,12)]

#---------------------------------Merging Demographic and credit data for other models like SVM and Random Forest---------------------------------------------------

#Creating a subset on Credit data for Randome Forest and SVM Modeling
Credit_Data1 <- Credit_Data[,c(21:33,35,16)]
Credit_Data2 <- Credit_Data[,c(19,20)]

# #Writing a function to convert all the variables to factor except Performance tag
List_CreditData1 <- names(Credit_Data1)

for ( i in 1:(length(List_CreditData1)-1)){
      {
    Credit_Data1[,List_CreditData1[i]] <- as.factor(Credit_Data1[,List_CreditData1[i]])
  }
}

#Creating Dummy variables for Credit Data1
k1 <- Credit_Data1

Credit_Data1 <- dummy.data.frame(Credit_Data1)
# 

#binding Credit Data1 and Credit Data2 
Data <- cbind(Credit_Data2,Credit_Data1)

#merging demo data and credit data
Data <- cbind(Data,Demo_Data3)

#Removing duplicate variables like Application ID and Performance tag
Data= Data[,-c(1,2)]

#Creating  data based on approved and rejected status
ApprovedData <- Data[Data$Status == "Approved",-c(69,71)]
RejectedData <- Data[Data$Status == "Rejected",-c(69,71)]

#############################################################Logistic Regression-Demographic Data############################################

#------------------------------------------------------Model Building on Train Demographic (WOE) data---------------------------------------
set.seed(100)
Index <- sample(nrow(Approved_Demo_WOE),0.70*nrow(Approved_Demo_WOE),replace = F)

Train_Demo_WOE <- Approved_Demo_WOE[Index,]
Test_Demo_WOE <- Approved_Demo_WOE[-Index,]


#Building the logistic regression
set.seed(100)
logist_Demo_WOE <- glm(PerformanceTag~.,data= Train_Demo_WOE,
                       family = "binomial")
summary(logist_Demo_WOE)


# Using stepwise algorithm for removing insignificant variables 
logist_Demo_WOE_StepAIC <- stepAIC(logist_Demo_WOE, direction = "both")
#Step:  AIC=16929.34
#PerformanceTag ~ Gender_WOE + Income_WOE + Noofdependents_WOE + 
#  Profession_WOE + CurrentResidencePeriod_WOE + CurrentcompanyPeriod_WOE

#Iteration1

logist_Demo_WOE1 <- glm(PerformanceTag ~ Gender_WOE + Income_WOE + Noofdependents_WOE + 
                        Profession_WOE + CurrentResidencePeriod_WOE + CurrentcompanyPeriod_WOE,
                        data=Train_Demo_WOE,family = "binomial")

vif(logist_Demo_WOE1)#no multicolinearity)
summary(logist_Demo_WOE1)

#Null deviance: 17158  on 48862  degrees of freedom
#Residual deviance: 16915  on 48856  degrees of freedom
#AIC: 16929

#Iteration2
#removing Profession_WOE

logist_Demo_WOE2 <-  glm(PerformanceTag ~ Gender_WOE + Income_WOE + Noofdependents_WOE + 
                       CurrentResidencePeriod_WOE + CurrentcompanyPeriod_WOE,
                         data=Train_Demo_WOE,family = "binomial")


vif(logist_Demo_WOE2)#no multicolinearity)
summary(logist_Demo_WOE2)

#Null deviance: 17158  on 48862  degrees of freedom
#Residual deviance: 16918  on 48857  degrees of freedom
#AIC: 16930

#logist_Demo_WOE2 is final model

prop.table(table(Train_Demo_WOE$PerformanceTag))

#0          1 
#0.95778239 0.04221761 

prop.table(table(Test_Demo_WOE$PerformanceTag))

#0          1 
#0.95826569 0.04173431

#---------------------------------------------------Model Evaluation on Train Demographic data-------------------------------------
#Predicting the response on test data 

Predict_logit_Train_Demo_WOE <- predict(logist_Demo_WOE2,Train_Demo_WOE[,-11],type = "response")
summary(Predict_logit_Train_Demo_WOE)


# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(Predict_logit_Train_Demo_WOE >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, Train_Demo_WOE$PerformanceTag,positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    


s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#0.04 is the cut off where we have the trade off between accuracy, sensitivity and specificity

# Let's use the probability cutoff of 4%.

predicted_response <- factor(ifelse(Predict_logit_Train_Demo_WOE >= 0.04, "1", "0"))

table(predicted_response)

# Creating confusion matrix for identifying the model evaluation.
conf_Train_Demo_WOE  <- confusionMatrix(predicted_response, Train_Demo_WOE$PerformanceTag, positive = "1")

#Reference
#Prediction     0     1
#0 24370   760
#1 22420  1313

#Accuracy : 0.5256        
#95% CI : (0.5212, 0.53)
#No Information Rate : 0.9576        
#P-Value [Acc > NIR] : 1             

#Kappa : 0.0257        
#Mcnemar's Test P-Value : <2e-16        

#Sensitivity : 0.63338       
#Specificity : 0.52084       
#Pos Pred Value : 0.05532       
#Neg Pred Value : 0.96976       
#Prevalence : 0.04242       
#Detection Rate : 0.02687       
# Detection Prevalence : 0.48570       
# Balanced Accuracy : 0.57711       
# 
# 'Positive' Class : 1         


#---------------------------------------------------Model Evaluation on Test Demographic data-------------------------------------
#Predicting the response on test data 

Predict_logit_Test_Demo_WOE <- predict(logist_Demo_WOE2,Test_Demo_WOE[,-11],type = "response")
summary(Predict_logit_Test_Demo_WOE)


# Let's use the probability cutoff of 4%.

predicted_response1 <- factor(ifelse(Predict_logit_Test_Demo_WOE >= 0.04, "1", "0"))

table(predicted_response1)

# Creating confusion matrix for identifying the model evaluation.
conf_Test_Demo_WOE <- confusionMatrix(predicted_response1, Test_Demo_WOE$PerformanceTag, positive = "1")

#Confusion Matrix and Statistics

#Reference
#Prediction     0     1
#0 10338   316
#1  9730   558

#Accuracy : 0.5203          
#95% CI : (0.5135, 0.5271)
#No Information Rate : 0.9583          
#P-Value [Acc > NIR] : 1               

#Kappa : 0.025           
#Mcnemar's Test P-Value : <2e-16          
                                          
#            Sensitivity : 0.63844         
#            Specificity : 0.51515         
#         Pos Pred Value : 0.05424         
#         Neg Pred Value : 0.97034         
#             Prevalence : 0.04173         
#         Detection Rate : 0.02665         
#   Detection Prevalence : 0.49126         
#      Balanced Accuracy : 0.57680         
                                          
#       'Positive' Class : 1  
accuracy <- conf$overall[1]#52%

sensitivity <- conf$byClass[1]#63.8%

Specificity <- conf$byClass[2]#51.5%


#Accuracy on demographic data-~52%
#Sensitivity-~63%
#Specificisty-~52%

#--------------------------------------------------------Model Evaluation on Rejected Customers-----------------------------------------
#Predicting the response on rejected sample

Predict_logit_Rejected_Demo_WOE <- predict(logist_Demo_WOE2,Rejected_Demo_WOE[,-11],type = "response")
summary(Predict_logit_Rejected_Demo_WOE)

predicted_response3 <- factor(ifelse(Predict_logit_Rejected_Demo_WOE >= 0.04, "1", "0"))

table(predicted_response3)

# Creating confusion matrix for identifying the model evaluation.
conf_Rejected_Demo_WOE <- confusionMatrix(predicted_response3, Rejected_Demo_WOE$PerformanceTag, positive = "1")

#Confusion Matrix and Statistics

#Reference
#Prediction    0    1
#0    0  244
#1    0 1181

#Accuracy : 0.8288         
#95% CI : (0.8082, 0.848)
#No Information Rate : 1              
#P-Value [Acc > NIR] : 1              

#Kappa : 0              
#Mcnemar's Test P-Value : <2e-16         
                                         
#            Sensitivity : 0.8288         
#            Specificity :     NA         
#         Pos Pred Value :     NA         
#         Neg Pred Value :     NA         
#             Prevalence : 1.0000         
#         Detection Rate : 0.8288         
#   Detection Prevalence : 0.8288         
#      Balanced Accuracy :     NA         
                                         
#       'Positive' Class : 1     

#Model built on Demographic dataset has accuracy and Specificity of 52% and sensitivity of 63%  on test data 
#However, 82.88% on Rejected customers are classified as Bad customers. 

#The dataset on which the model is built has 4% positive rates. This is imbalanced data.
#If the data set is imbalanced the model will be Biased. 
#Think of it like this if you are feeding the model only 0 for every possible combination,
#it will give you a 0 for every set of input.

##############################################Using SMOTE to balance the demographic data########################################################
# SMOTE more positive cases(creating balanced data by adding more positice cases)
library(DMwR)
set.seed(400)
Train_Demo_WOE1 <- SMOTE(PerformanceTag ~ ., Train_Demo_WOE, perc.over = 100, perc.under=200)
#TrainData_WOE1$PerformanceTag <- as.numeric(TrainData_WOE1$PerformanceTag)

prop.table(table(Train_Demo_WOE1$PerformanceTag))

#0   1 
#0.5 0.5
#50% positive classes


#------------------------Building Model using K Fold cross validation on Balanced Demo Data-Logistic Regression-------------------------------------------

library(doParallel)
library(foreach)
### Register parallel backend
cl <- makeCluster(detectCores())
registerDoParallel(cl)
getDoParWorkers()
#insert code here


ctrl <- trainControl(method="cv", number=10)

# evaluate the SMOTE performance
set.seed(100)
tbmodel_Demo_Logistic <- train(PerformanceTag ~ ., data = Train_Demo_WOE1, method = "glm",
                 trControl = ctrl)

 
# Generalized Linear Model 

# 8292 samples
# 10 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 7463, 7463, 7462, 7463, 7463, 7464, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.6268656  0.2537402

### Stop cluster
stopCluster(cl)

#-------------------------------------------------Model Buildinging on Train Demo balanced data----------------------------------------------
#Building the logistic regression
set.seed(100)
logist_Demo_WOE_Bal <- glm(PerformanceTag~.,data= Train_Demo_WOE1,
                       family = "binomial")
summary(logist_Demo_WOE_Bal)


# Using stepwise algorithm for removing insignificant variables 
logist_Demo_WOE_Bal_StepAIC <- stepAIC(logist_Demo_WOE_Bal, direction = "both")
# Step:  AIC=10837.15
# PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
#   Education_WOE + Profession_WOE + TypeofResidence_WOE + CurrentResidencePeriod_WOE

#Iteration1

logist_Demo_WOE_Bal1 <- glm(PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
                        Education_WOE + Profession_WOE + TypeofResidence_WOE + CurrentResidencePeriod_WOE,
                        data=Train_Demo_WOE1,family = "binomial")

vif(logist_Demo_WOE_Bal1)#no multicolinearity)
summary(logist_Demo_WOE_Bal1)




#----------------------------------------------------Model evaluation on Train Demo Balanced data--------------------------------------------
pred_logit_Train_Demo_WOE_bal <- predict(logist_Demo_WOE_Bal1, Train_Demo_WOE[,-11],type = "response")


#deciding on probability cut off

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(pred_logit_Train_Demo_WOE_bal >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, Train_Demo_WOE$PerformanceTag,positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    


s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    
dev.off()
# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#Deciding cut off @49%
pred_logit_Train_Demo_WOE_bal1 <- factor(ifelse(pred_logit_Train_Demo_WOE_bal >= 0.49,1,0))

conf_Train_Demo_WOE_Balanced <- confusionMatrix(pred_logit_Train_Demo_WOE_bal1,Train_Demo_WOE$PerformanceTag,positive = "1")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 29434  1061
# 1 17356  1012
# 
# Accuracy : 0.6231          
# 95% CI : (0.6188, 0.6274)
# No Information Rate : 0.9576          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0247          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.48818         
# Specificity : 0.62907         
# Pos Pred Value : 0.05510         
# Neg Pred Value : 0.96521         
# Prevalence : 0.04242         
# Detection Rate : 0.02071         
# Detection Prevalence : 0.37591         
# Balanced Accuracy : 0.55862         
# 
# 'Positive' Class : 1                             
#----------------------------------------------------Model evaluation on Test data--------------------------------------------
pred_logit_Test_Demo_WOE_bal <- predict(logist_Demo_WOE_Bal1, Test_Demo_WOE[,-11],type = "response")


#using cut off @49%
pred_logit_Test_Demo_WOE_bal1 <- factor(ifelse(pred_logit_Test_Demo_WOE_bal >= 0.49,1,0))

conf_Test_Demo_WOE_Balanced <- confusionMatrix(pred_logit_Test_Demo_WOE_bal1,Test_Demo_WOE$PerformanceTag,positive = "1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 12472   421
# 1  7596   453
# 
# Accuracy : 0.6172          
# 95% CI : (0.6106, 0.6238)
# No Information Rate : 0.9583          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0284          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.51831         
# Specificity : 0.62149         
# Pos Pred Value : 0.05628         
# Neg Pred Value : 0.96735         
# Prevalence : 0.04173         
# Detection Rate : 0.02163         
# Detection Prevalence : 0.38435         
# Balanced Accuracy : 0.56990         
# 
# 'Positive' Class : 1               


#Model on imbalanced demographic data
#sensitivity-63.8%
#specificity-51.5%
#Accuracy-52.03%

#Model on balanced data
#sensitivity-51.8%
#specificity-62.1%(Improved when compared to imbalanced data)
#Accuracy-61.72%(Improved when compared to imbalanced data)

#Model built on Demographic data has low prediction power on sensitivity, accuracy and Specificity

#----------------------------------------------------Evaluation on rejected customers----------------------------------------------------
pred_logit_Rejected_Demo_WOE_bal <- predict(logist_Demo_WOE_Bal1, Rejected_Demo_WOE[,-11],type = "response")

#identifying response variable based on probability cut off
pred_logit_Rejected_Demo_WOE_bal1 <- factor(ifelse(pred_logit_Rejected_Demo_WOE_bal >= 0.49,1,0))

table(pred_logit_Rejected_Demo_WOE_bal1)

conf_Rejected_Demo_WOE_Balanced <- confusionMatrix(pred_logit_Rejected_Demo_WOE_bal1,Rejected_Demo_WOE$PerformanceTag,positive = "1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0   0 501
# 1   0 924
# 
# Accuracy : 0.6484         
# 95% CI : (0.623, 0.6732)
# No Information Rate : 1              
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0              
# Mcnemar's Test P-Value : <2e-16         
# 
# Sensitivity : 0.6484         
# Specificity :     NA         
# Pos Pred Value :     NA         
# Neg Pred Value :     NA         
# Prevalence : 1.0000         
# Detection Rate : 0.6484         
# Detection Prevalence : 0.6484         
# Balanced Accuracy :     NA         
# 
# 'Positive' Class : 1    
#Model classifies 64.8% rejected customers as bad customers. 


#when it comes to the prediction of good customers over the real good customers , Model on demographic data
#shows 62% accuracy,  62% Specificity and 52% sensitivity

#However, model on demographic data can classify only 64.8% of rejected customers as bad customers


#################################################Logistic Regression-Demographic and Credit data##########################################

#--------------------------------------------------Model Building on Demographic and Credit data----------------------------------------
#Splitting Approved Customers data into train and test dataset
set.seed(200)
Index1 <- sample(nrow(ApprovedData_WOE),0.70*nrow(ApprovedData_WOE),replace = F)

TrainData_WOE <- ApprovedData_WOE[Index1,]
TestData_WOE <- ApprovedData_WOE[-Index1,]

#Building the logistic regression
set.seed(100)
logist_WOE <- glm(PerformanceTag~.,data= TrainData_WOE,
                       family = "binomial")
summary(logist_WOE)


# Using stepwise algorithm for removing insignificant variables 
logist_WOE_StepAIC <- stepAIC(logist_WOE, direction = "both")
#Step:  AIC=16269.17
#PerformanceTag ~ Profession_WOE + AvgCCUtilization_WOE + Times90DPDlast12months_WOE + 
#  Times30DPDlast12months_WOE + PLTradesinLast12months_WOE + 
#  InquiriesinLast12months_WOE

#Iteration1


#important variables identified-Income,Profession,CurrentResidencePeriod,
#CurrentcompanyPeriod,Times30DPDlast6months,Times90DPDlast6months,Times60DPDlast6months,
#PLTradesinLast12months,Total Trades,InquiriesinLast12months,AvgCCUtilization,Outstanding Balance

logist_WOE1 <- glm(PerformanceTag ~ Profession_WOE + AvgCCUtilization_WOE + Times90DPDlast12months_WOE + 
                     Times30DPDlast12months_WOE + PLTradesinLast12months_WOE + 
                     InquiriesinLast12months_WOE,data=TrainData_WOE,family = "binomial")

vif(logist_WOE1)
summary(logist_WOE1)#Profession_WOE is insignificant

#Null deviance: 16977  on 48862  degrees of freedom
#Residual deviance: 16255  on 48856  degrees of freedom
#AIC: 16269

#Iteration2
#removing insignificant variables Profession_WOE from model

logist_WOE2 <- glm(PerformanceTag ~ AvgCCUtilization_WOE + Times90DPDlast12months_WOE + 
                     Times30DPDlast12months_WOE + PLTradesinLast12months_WOE + 
                     InquiriesinLast12months_WOE,data=TrainData_WOE,family = "binomial")

vif(logist_WOE2)
summary(logist_WOE2)


#Null deviance: 16977  on 48862  degrees of freedom
#Residual deviance: 16257  on 48857  degrees of freedom
#AIC: 16269

#Using the important variables from IV method
logist_WOE_IMPV <- glm(PerformanceTag ~ Times90DPD6months_WOE + AvgCCUtilization_WOE + OutstandingBalance_WOE + 
                     PLTradesinLast12months_WOE+
                     InquiriesinLast12months_WOE
                      ,data=TrainData_WOE,family = "binomial")

vif(logist_WOE_IMPV)
summary(logist_WOE_IMPV)


#final Variables-AvgCCUtilization_WOE,Times30DPDlast12months_WOE,Times90DPDlast12months_WOE,
#PLTradesinLast12months_WOE,InquiriesinLast12months_WOE

## Model Evaluation: Logistic Regression
prop.table(table(TrainData_WOE$PerformanceTag))
#0          1 
#0.95816876 0.04183124 

prop.table(table(TestData_WOE$PerformanceTag))
#0          1 
#0.95688091 0.04311909 

#---------------------------------------------------------Model Evaluation on Train data-------------------------------------------
#Predicting the response on test data

Predict_logit_TrainData_WOE <- predict(logist_WOE2,TrainData_WOE[,-28],type = "response")
summary(Predict_logit_TrainData_WOE)

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(Predict_logit_TrainData_WOE >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, TrainData_WOE$PerformanceTag,positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    


s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#0.04 is the cut off where we have the trade off between accuracy, sensitivity and specificity

# Let's use the probability cutoff of 4%.

predicted_response_TrainData_WOE1 <- factor(ifelse(Predict_logit_TrainData_WOE >= 0.04, "1", "0"))

table(predicted_response_TrainData_WOE1)

# Creating confusion matrix for identifying the model evaluation.
conf_TrainData_WOE <- confusionMatrix(predicted_response_TrainData_WOE1, TrainData_WOE$PerformanceTag, positive = "1")
# 
# 
# Reference
# Prediction     0     1
# 0 23590   489
# 1 23229  1555
# 
# Accuracy : 0.5146         
# 95% CI : (0.5102, 0.519)
# No Information Rate : 0.9582         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.0419         
# Mcnemar's Test P-Value : <2e-16         
# 
# Sensitivity : 0.76076        
# Specificity : 0.50386        
# Pos Pred Value : 0.06274        
# Neg Pred Value : 0.97969        
# Prevalence : 0.04183        
# Detection Rate : 0.03182        
# Detection Prevalence : 0.50721        
# Balanced Accuracy : 0.63231        
# 
# 'Positive' Class : 1  
#---------------------------------------------------------Model Evaluation on Test data-------------------------------------------
#Predicting the response on test data

Predict_logit_TestData_WOE <- predict(logist_WOE2,TestData_WOE[,-28],type = "response")
summary(Predict_logit_TestData_WOE)


# Let's use the probability cutoff of 4%.

predicted_response_TestData_WOE1 <- factor(ifelse(Predict_logit_TestData_WOE >= 0.04, "1", "0"))

table(predicted_response_TestData_WOE1)

# Creating confusion matrix for identifying the model evaluation.
conf_TestData_WOE <- confusionMatrix(predicted_response_TestData_WOE1, TestData_WOE$PerformanceTag, positive = "1")

#Confusion Matrix and Statistics
# Reference
# Prediction     0     1
# 0 10095   212
# 1  9944   691
# 
# Accuracy : 0.515           
# 95% CI : (0.5082, 0.5218)
# No Information Rate : 0.9569          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0438          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.76523         
# Specificity : 0.50377         
# Pos Pred Value : 0.06497         
# Neg Pred Value : 0.97943         
# Prevalence : 0.04312         
# Detection Rate : 0.03300         
# Detection Prevalence : 0.50783         
# Balanced Accuracy : 0.63450         
# 
# 'Positive' Class : 1               

accuracy <- conf$overall[1]#51.5%

sensitivity <- conf$byClass[1]#76.5%

Specificity <- conf$byClass[2]#50.4% 


#Area under the curve
Predict_logit_TestData_WOE <- predict(logist_WOE2,TestData_WOE[,-28])

auc <- roc(TestData_WOE$PerformanceTag, Predict_logit_TestData_WOE)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

#--------------------------------------------------------Model Evaluation on Rejected Customers----------------------------------------------------------------

#Predicting the response on rejected sample

Predict_logit_RejectedData_WOE <- predict(logist_WOE2,RejectedData_WOE[,-28],type = "response")
summary(Predict_logit_RejectedData_WOE)

predicted_response_RejectedData_WOE1 <- factor(ifelse(Predict_logit_RejectedData_WOE >= 0.04, "1", "0"))

table(predicted_response_RejectedData_WOE1)

# Creating confusion matrix for identifying the model evaluation.
conf_RejectedData_WOE <- confusionMatrix(predicted_response_RejectedData_WOE1, RejectedData_WOE$PerformanceTag, positive = "1")

#Confusion Matrix and Statistics

#Reference
#Prediction    0    1
#0    0    2
#1    0 1423

#Accuracy : 0.9986          
#95% CI : (0.9949, 0.9998)
#No Information Rate : 1               
#P-Value [Acc > NIR] : 1.0000          

#Kappa : 0               
#Mcnemar's Test P-Value : 0.4795          
                                          
#            Sensitivity : 0.9986          
#            Specificity :     NA          
#         Pos Pred Value :     NA          
#         Neg Pred Value :     NA          
#             Prevalence : 1.0000          
#         Detection Rate : 0.9986          
#   Detection Prevalence : 0.9986          
#      Balanced Accuracy :     NA          
                                          
#       'Positive' Class : 1

                                 
#Logistic Model built on Demographic and Credit dataset has accuracy of 51.5%  and sensitivity of 76%
#Logistic Model is classifying 99.86% of the rejected customers as bad customers

#The dataset on which the model is built has 4% positive rates. This is imbalanced data.
#If the data set is imbalanced the model will be Biased. 
#Think of it like this if you are feeding the model only 0 for every possible combination,
#it will give you a 0 for every set of input.

##############################################Using SMOTE to balance the data########################################################
# SMOTE more positive cases(creating balanced data by adding more positice cases)
# library(DMwR)
set.seed(400)
TrainData_WOE1 <- SMOTE(PerformanceTag ~ ., TrainData_WOE, perc.over = 100, perc.under=200)
#TrainData_WOE1$PerformanceTag <- as.numeric(TrainData_WOE1$PerformanceTag)

prop.table(table(TrainData_WOE1$PerformanceTag))

#0   1 
#0.5 0.5
#50% positive classes
#---------------------------Building Model on Balanced data using K Fold cross validation-Logistic Regression--------------------------------------------
# 
# library(doParallel)
# library(foreach)
### Register parallel backend
cl <- makeCluster(detectCores())
registerDoParallel(cl)
getDoParWorkers()
#insert code here


ctrl <- trainControl(method="cv", number=10)

# evaluate the SMOTE performance
set.seed(100)
tbmodel_Logistic_demo_credit <- train(PerformanceTag ~ ., data = TrainData_WOE1, method = "glm",
                 trControl = ctrl)

# Generalized Linear Model 
# 
# 8176 samples
# 27 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 7358, 7358, 7358, 7358, 7359, 7359, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.6846852  0.3693776

### Stop cluster
stopCluster(cl)

#Building Logistic Model on Balanced data(Demo+Credit)

set.seed(100)
logist_WOE_Balanced <- glm(PerformanceTag~.,data= TrainData_WOE1,
                  family = "binomial")
summary(logist_WOE)


# Using stepwise algorithm for removing insignificant variables 
logist_WOE_Balanced_StepAIC <- stepAIC(logist_WOE_Balanced, direction = "both")

#Step:  AIC=9653.12
# PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
#   Education_WOE + Noofdependents_WOE + TypeofResidence_WOE + 
#   CurrentResidencePeriod_WOE + CurrentcompanyPeriod_WOE + AvgCCUtilization_WOE + 
#   Times90DPD6months_WOE + Times60DPD6months_WOE + Times30DPDlast6months_WOE + 
#   Times90DPDlast12months_WOE + Times30DPDlast12months_WOE + 
#   TradesinLast6months_WOE + TradesinLast12months_WOE + PLTradesinLast6months_WOE + 
#   InquiriesinLast6months_WOE + InquiriesinLast12months_WOE + 
#   Autoloan_WOE + Homeloan_WOE + OutstandingBalance_WOE + TotalTrades_WOE

logist_WOE_Balanced1 <- glm(PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
                              Education_WOE + Noofdependents_WOE + TypeofResidence_WOE + 
                              CurrentResidencePeriod_WOE + CurrentcompanyPeriod_WOE + AvgCCUtilization_WOE + 
                              Times90DPD6months_WOE + Times60DPD6months_WOE + Times30DPDlast6months_WOE + 
                              Times90DPDlast12months_WOE + Times30DPDlast12months_WOE + 
                              TradesinLast6months_WOE + TradesinLast12months_WOE + PLTradesinLast6months_WOE + 
                              InquiriesinLast6months_WOE + InquiriesinLast12months_WOE + 
                              Autoloan_WOE + Homeloan_WOE + OutstandingBalance_WOE + TotalTrades_WOE,
                              data= TrainData_WOE1,family = "binomial")

vif(logist_WOE_Balanced1)
summary(logist_WOE_Balanced1)


#REmoving variable Times30DPDlast6months_WOE with VIF =13.9

logist_WOE_Balanced2 <- glm(PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
                              Education_WOE + Noofdependents_WOE + TypeofResidence_WOE + 
                              CurrentResidencePeriod_WOE + CurrentcompanyPeriod_WOE + AvgCCUtilization_WOE + 
                              Times90DPD6months_WOE + Times60DPD6months_WOE  + 
                              Times90DPDlast12months_WOE + Times30DPDlast12months_WOE + 
                              TradesinLast6months_WOE + TradesinLast12months_WOE + PLTradesinLast6months_WOE + 
                              InquiriesinLast6months_WOE + InquiriesinLast12months_WOE + 
                              Autoloan_WOE + Homeloan_WOE + OutstandingBalance_WOE + TotalTrades_WOE,
                            data= TrainData_WOE1,family = "binomial")

vif(logist_WOE_Balanced2)
summary(logist_WOE_Balanced2)

#REmoving variable Times60DPD6months_WOE with VIF =10.11

logist_WOE_Balanced3 <- glm(PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
                              Education_WOE + Noofdependents_WOE + TypeofResidence_WOE + 
                              CurrentResidencePeriod_WOE + CurrentcompanyPeriod_WOE + AvgCCUtilization_WOE + 
                              Times90DPD6months_WOE + Times90DPDlast12months_WOE + Times30DPDlast12months_WOE + 
                              TradesinLast6months_WOE + TradesinLast12months_WOE + PLTradesinLast6months_WOE + 
                              InquiriesinLast6months_WOE + InquiriesinLast12months_WOE + 
                              Autoloan_WOE + Homeloan_WOE + OutstandingBalance_WOE + TotalTrades_WOE,
                            data= TrainData_WOE1,family = "binomial")

vif(logist_WOE_Balanced3)
summary(logist_WOE_Balanced3)

#REmoving variable CurrentResidencePeriod_WOE

logist_WOE_Balanced4 <- glm(PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
                              Education_WOE + Noofdependents_WOE + TypeofResidence_WOE + 
                              CurrentcompanyPeriod_WOE + AvgCCUtilization_WOE + 
                              Times90DPD6months_WOE + Times90DPDlast12months_WOE + Times30DPDlast12months_WOE + 
                              TradesinLast6months_WOE + TradesinLast12months_WOE + PLTradesinLast6months_WOE + 
                              InquiriesinLast6months_WOE + InquiriesinLast12months_WOE + 
                              Autoloan_WOE + Homeloan_WOE + OutstandingBalance_WOE + TotalTrades_WOE,
                            data= TrainData_WOE1,family = "binomial")

vif(logist_WOE_Balanced4)
summary(logist_WOE_Balanced4)

#REmoving insignificant variables CurrentcompanyPeriod_WOE

logist_WOE_Balanced5 <- glm(PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
                              Education_WOE + Noofdependents_WOE + TypeofResidence_WOE + 
                              AvgCCUtilization_WOE + Times90DPD6months_WOE + Times90DPDlast12months_WOE + 
                              Times30DPDlast12months_WOE + TradesinLast6months_WOE + TradesinLast12months_WOE + 
                              PLTradesinLast6months_WOE + InquiriesinLast6months_WOE + InquiriesinLast12months_WOE + 
                              Autoloan_WOE + Homeloan_WOE + OutstandingBalance_WOE + TotalTrades_WOE,
                            data= TrainData_WOE1,family = "binomial")


vif(logist_WOE_Balanced5)
summary(logist_WOE_Balanced5)

#REmoving variables TradesinLast12months_WOE with vif  4.743502

logist_WOE_Balanced6 <- glm(PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
                              Education_WOE + Noofdependents_WOE + TypeofResidence_WOE + 
                              AvgCCUtilization_WOE + Times90DPD6months_WOE + Times90DPDlast12months_WOE + 
                              Times30DPDlast12months_WOE + TradesinLast6months_WOE +  
                              PLTradesinLast6months_WOE + InquiriesinLast6months_WOE + InquiriesinLast12months_WOE + 
                              Autoloan_WOE + Homeloan_WOE + OutstandingBalance_WOE + TotalTrades_WOE,
                            data= TrainData_WOE1,family = "binomial")


vif(logist_WOE_Balanced6)
summary(logist_WOE_Balanced6)


#REmoving variables Times90DPDlast12months_WOE with vif  4.901930

logist_WOE_Balanced7 <- glm(PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
                              Education_WOE + Noofdependents_WOE + TypeofResidence_WOE + 
                              AvgCCUtilization_WOE + Times90DPD6months_WOE +  
                              Times30DPDlast12months_WOE + TradesinLast6months_WOE +  
                              PLTradesinLast6months_WOE + InquiriesinLast6months_WOE + InquiriesinLast12months_WOE + 
                              Autoloan_WOE + Homeloan_WOE + OutstandingBalance_WOE + TotalTrades_WOE,
                            data= TrainData_WOE1,family = "binomial")


vif(logist_WOE_Balanced7)
summary(logist_WOE_Balanced7)

#REmoving variables InquiriesinLast12months_WOE with vif  3.983250


logist_WOE_Balanced8 <- glm(PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
                              Education_WOE + Noofdependents_WOE + TypeofResidence_WOE + 
                              AvgCCUtilization_WOE + Times90DPD6months_WOE +  
                              Times30DPDlast12months_WOE + TradesinLast6months_WOE +  
                              PLTradesinLast6months_WOE + InquiriesinLast6months_WOE +  
                              Autoloan_WOE + Homeloan_WOE + OutstandingBalance_WOE + TotalTrades_WOE,
                            data= TrainData_WOE1,family = "binomial")


vif(logist_WOE_Balanced8)
summary(logist_WOE_Balanced8)

logist_WOE_Balanced9 <- glm(PerformanceTag ~ Gender_WOE + MaritalStatus_WOE + Income_WOE + 
                               Education_WOE + Noofdependents_WOE + TypeofResidence_WOE + 
                               AvgCCUtilization_WOE + Times90DPD6months_WOE +  
                               Times30DPDlast12months_WOE + TradesinLast6months_WOE +  
                               PLTradesinLast6months_WOE + InquiriesinLast12months_WOE + 
                               Autoloan_WOE + Homeloan_WOE + OutstandingBalance_WOE + TotalTrades_WOE,
                             data= TrainData_WOE1,family = "binomial")


vif(logist_WOE_Balanced9)
summary(logist_WOE_Balanced9)

#----------------------------------------------------Model evaluation on Train data--------------------------------------------
pred_logit_TrainData_WOE_Bal <- predict(logist_WOE_Balanced9, TrainData_WOE[,-28],type = "response")


#deciding on probability cut off

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(pred_logit_TrainData_WOE_Bal >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, TrainData_WOE$PerformanceTag,positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    


s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#Deciding cut off @47%
pred_logit_TrainData_WOE_Bal <- factor(ifelse(pred_logit_TrainData_WOE_Bal >= 0.49,1,0))

conf_TrainData_WOE_Balanced <- confusionMatrix(pred_logit_TrainData_WOE_Bal,TrainData_WOE$PerformanceTag,positive = "1")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 29557   825
# 1 17262  1219
# 
# Accuracy : 0.6298          
# 95% CI : (0.6255, 0.6341)
# No Information Rate : 0.9582          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.047           
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.59638         
# Specificity : 0.63130         
# Pos Pred Value : 0.06596         
# Neg Pred Value : 0.97285         
# Prevalence : 0.04183         
# Detection Rate : 0.02495         
# Detection Prevalence : 0.37822         
# Balanced Accuracy : 0.61384         
# 
# 'Positive' Class : 1          


#Area under the curve
pred_logit_TrainData_WOE_Bal <- predict(logist_WOE_Balanced9, TrainData_WOE[,-28])

auc <- roc(TrainData_WOE$PerformanceTag, pred_logit_TrainData_WOE_Bal)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

#----------------------------------------------------Model evaluation on Test data--------------------------------------------
pred_logit_TestData_WOE_Bal <- predict(logist_WOE_Balanced9, TestData_WOE[,-28],type = "response")



#using cut off @47%
pred_logit_TestData_WOE_Bal <- factor(ifelse(pred_logit_TestData_WOE_Bal >= 0.49,1,0))

conf_TestData_WOE_Balanced <- confusionMatrix(pred_logit_TestData_WOE_Bal,TestData_WOE$PerformanceTag,positive = "1")
# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 12684   370
# 1  7355   533
# 
# Accuracy : 0.6311          
# 95% CI : (0.6245, 0.6377)
# No Information Rate : 0.9569          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0476          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.59025         
# Specificity : 0.63297         
# Pos Pred Value : 0.06757         
# Neg Pred Value : 0.97166         
# Prevalence : 0.04312         
# Detection Rate : 0.02545         
# Detection Prevalence : 0.37666         
# Balanced Accuracy : 0.61161         
# 
# 'Positive' Class : 1    


#Area under the curve
pred_logit_TestData_WOE_Bal <- predict(logist_WOE_Balanced9, TestData_WOE[,-28])

auc <- roc(TestData_WOE$PerformanceTag, pred_logit_TestData_WOE_Bal)#65% AUC
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

#Performance of the model on demographic balanced data vs Performance of the model on balanced data(Demographic + Credit Bureau)

#Logistic Model preformance on Demographic balanced data
#sensitivity-52%
#specificity-62%
#kappa-0.0284
#Accuracy-62%

#Logistic Model performance on Demographic + Credit Bureau balanced data
#sensitivity-59%
#specificity-63%
#kappa-0.0476
#Accuracy-63%

#----------------------------------------------------Evaluation on rejected customers----------------------------------------------------
pred_logit_RejectedData_WOE_Bal <- predict(logist_WOE_Balanced9, RejectedData_WOE[,-28],type = "response")

#identifying response variable based on probability cut off
pred_logit_RejectedData_WOE_Bal <- factor(ifelse(pred_logit_RejectedData_WOE_Bal >= 0.49,1,0))

table(pred_logit_RejectedData_WOE_Bal)

conf_RejectedData_WOE_Balanced <- confusionMatrix(pred_logit_RejectedData_WOE_Bal,RejectedData_WOE$PerformanceTag,positive = "1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0    0   68
# 1    0 1357
# 
# Accuracy : 0.9523          
# 95% CI : (0.9399, 0.9628)
# No Information Rate : 1               
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0               
# Mcnemar's Test P-Value : 4.476e-16       
#                                           
#             Sensitivity : 0.9523          
#             Specificity :     NA          
#          Pos Pred Value :     NA          
#          Neg Pred Value :     NA          
#              Prevalence : 1.0000          
#          Detection Rate : 0.9523          
#    Detection Prevalence : 0.9523          
#       Balanced Accuracy :     NA          
                                   
# 'Positive' Class : 1           

#Model classifies 94% rejected customers as bad customers. 


#----------------------------------------------------------Summary-Logistic Model------------------------------------------------------------
#Performance of the Logistic model built on Demographic data
#a.performance on train and test data-62% accuracy ,62% specificity and 52% sensitivity (@49% probability cut off)
#b.performance on rejected data- Model is able to classify 65% of the rejected customers as bad customers

#Performance of the Logistic model built on Demographic  and Credit data
#a.performance on train and test data-63% accuracy and specificity,59% sensitivity
#b.performance on rejected data-95% of rejected customers are classified as bad

#Model on Demographic and Credit data has better prediction power compared to model on demographic data when it come to sensitivity.
#Model built on both the data sets is able to classify 95% of the rejected customers as bad whereas 
#model built on demographic data is able to classify only 65% of the rejected customers correctly.

#------------------------------------------------------------------------------------------------------------------------------
#Function to Calculate Accuracy,Specificity, Sensitivity and Precision,Recall


Metrix <- function(x,y){
  
  TruePositives <- sum(x == 1 & y == 1)
  TrueNegatives <- sum(x == 0 & y == 0)
  FalsePositives <- sum(x == 0 & y == 1)
  FalseNegatives <- sum(x == 1 & y == 0)
  
  
  Accuracy <-  (TruePositives + TrueNegatives)/length(x)
  Sensitivity <-  TruePositives/sum(x == 1)
  Specificity <- TrueNegatives/sum(x == 0)
  Precision <- TruePositives/sum(y == 1)
  FalsePositiverate <- 1-Specificity
  FalseNegativerate <- 1-Sensitivity
  
  return(c(FalsePositiverate=FalsePositiverate,FalseNegativerate=FalseNegativerate,Accuracy=Accuracy,Sensitivity =Sensitivity,Specificity = Specificity,Precision = Precision))
  
}


#---------------------------------------------------Random Forest--------------------------------------------------------------
#--------------------------------------------------Building model on Demographic data--------------------------------------------------
#Creating Approved and Rejected dataset(Without WOE)
Approved_Demo_Data <- Demo_Data3[Demo_Data3$Status == "Approved",-c(28,30)]
Rejected_Demo_Data <- Demo_Data3[Demo_Data3$Status == "Rejected",-c(28,30)]

#Create Samples
set.seed(500)
Index_Demo_Data <- sample(nrow(Approved_Demo_Data),0.70*nrow(Approved_Demo_Data),replace=F)

Train_Demo_Data <-Approved_Demo_Data[Index_Demo_Data,]
Test_Demo_Data <- Approved_Demo_Data[-Index_Demo_Data,]

prop.table(table(Train_Demo_Data$Performance.Tag))
#0          1 
#0.95806643 0.04193357

prop.table(table(Test_Demo_Data$Performance.Tag))
#0          1 
#0.95711966 0.04288034 

#------------------------------------------------- SMOTE more positive cases--------------------------------------------------------

set.seed(500)
Train_Demo_Data1 <- SMOTE(Performance.Tag ~ ., Train_Demo_Data, perc.over = 100, perc.under=200)

prop.table(table(Train_Demo_Data1$Performance.Tag))
#0   1 
#0.5 0.5 


library(randomForest)


# set.seed(100)
# Model_RanF_DemoData <- randomForest(Performance.Tag~.,data = Train_Demo_Data1,importance = TRUE)
# Model_RanF_DemoData



# Call:
#   randomForest(formula = Performance.Tag ~ ., data = Train_Demo_Data1,      importance = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 5
# 
# OOB estimate of  error rate: 26.49%
# Confusion matrix:
#   0    1 class.error
# 0 3053 1045   0.2550024
# 1 1126 2972   0.2747682
# 
# #Saving Model_RanF_DemoData as RDS object
# saveRDS(Model_RanF_DemoData, file = "Model_RanF_DemoData.rds")
 

# Restore the object 
Model_RanF_DemoData <- readRDS(file = "Model_RanF_DemoData.rds")

# Predicting on train set
predTrainRF_Demo <- predict(Model_RanF_DemoData, Train_Demo_Data[,-28], type = "class")
# Checking classification accuracy
table(predTrainRF_Demo, Train_Demo_Data$Performance.Tag)  
mean(predTrainRF_Demo == Train_Demo_Data$Performance.Tag)#73.59% 
Metrix(Train_Demo_Data$Performance.Tag,predTrainRF_Demo)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# 0.2710087         0.1039531         0.7359966         0.8960469         0.7289913         0.1264202 

# Predicting on test data
PredTestRF_Demo <- predict(Model_RanF_DemoData,Test_Demo_Data[,-28],type = "class")
table(PredTestRF_Demo, Test_Demo_Data$Performance.Tag)  
mean(PredTestRF_Demo == Test_Demo_Data$Performance.Tag)#69.66%
Metrix(Test_Demo_Data$Performance.Tag,PredTestRF_Demo)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# 0.28866494        0.63251670        0.69659058        0.36748330        0.71133506        0.05395683 

# Predicting on rejected data
PredRejectRF_Demo <- predict(Model_RanF_DemoData,Rejected_Demo_Data[,-28],type = "class")
table(PredRejectRF_Demo, Rejected_Demo_Data$Performance.Tag)  
mean(PredRejectRF_Demo == Rejected_Demo_Data$Performance.Tag)#54.59%
Metrix(Rejected_Demo_Data$Performance.Tag,PredRejectRF_Demo)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# NaN         0.4540351         0.5459649         0.5459649               NaN         1.0000000 

#RF Model built on Demographic Balanced data gives accuracy of 73.59% on Train data (89.6% Sensitivity,72.9 % Specificity)
#RF Model built on Demographic Balanced data gives accuracy of 69.66% on test data (36.74% Sensitivity,71.13% Specificity)
#RF Model built on Demographic Balanced data has classified 54.59% of Customers as Bad customers out of 1425 rejected customers

#-------------------------------------------------------Tuning the parameters-Demographic data model---------------------------------------------

# # Using For loop to identify the right mtry for model
# set.seed(500)
# a=c()
# i=5
# for (i in 3:10) {
#   model2 <- randomForest(Performance.Tag ~ ., data = Train_Demo_Data1, ntree = 500, mtry = i, importance = TRUE)
#   predValid <- predict(model2, Test_Demo_Data, type = "class")
#   a[i-2] = mean(predValid == Test_Demo_Data$Performance.Tag)
# }
# 
# a
# b <- c(3:10)
# c <- cbind(a,b)
# a  b
# [1,] 0.6826951  3
# [2,] 0.6923408  4
# [3,] 0.6921497  5
# [4,] 0.6908605  6
# [5,] 0.6883774  7
# [6,] 0.6839366  8
# [7,] 0.6805463  9
# [8,] 0.6762009 10

plot(b,a)

# #Saving c as RDS object
# saveRDS(c, file = "c1.rds")
# saveRDS(a, file = "a1.rds")
# saveRDS(b, file = "b1.rds")


# Restore the object 
c1 <- readRDS(file = "c1.rds")
a1 <- readRDS(file = "a1.rds")
b1 <- readRDS(file = "b1.rds")

#Model shows the highest accuracy of 69.23% for 4 variables at each split. 
#Accuracy has dropped for more than 4 variables at each split


# set.seed(100)
# Model_RanF_DemoData_Tuned <- randomForest(Performance.Tag~.,data = Train_Demo_Data1,importance = TRUE,mtry=4,ntree=500)
# Model_RanF_DemoData_Tuned

# Call:
#   randomForest(formula = Performance.Tag ~ ., data = Train_Demo_Data1,      importance = TRUE, mtry = 4, ntree = 500) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 4
# 
# OOB estimate of  error rate: 28.21%
# Confusion matrix:
#   0    1 class.error
# 0 2997 1101   0.2686676
# 1 1211 2887   0.2955100
# 
# #Saving Model_RanF_DemoData_Tuned as RDS object
# saveRDS(Model_RanF_DemoData_Tuned, file = "Model_RanF_DemoData_Tuned.rds")


# Restore the object 
Model_RanF_DemoData_Tuned <- readRDS(file = "Model_RanF_DemoData_Tuned.rds")

# Predicting on train set
predTrainRF_Demo_Tuned <- predict(Model_RanF_DemoData_Tuned, Train_Demo_Data[,-28], type = "class")
# Checking classification accuracy
table(predTrainRF_Demo_Tuned, Train_Demo_Data$Performance.Tag)  
mean(predTrainRF_Demo_Tuned == Train_Demo_Data$Performance.Tag)#72.25% 
Metrix(Train_Demo_Data$Performance.Tag,predTrainRF_Demo_Tuned)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# 0.2815611         0.1849683         0.7224894         0.8150317         0.7184389         0.1124503 


# Predicting on test data
PredTestRF_Demo_Tuned <- predict(Model_RanF_DemoData_Tuned,Test_Demo_Data[,-28],type = "class")
table(PredTestRF_Demo_Tuned, Test_Demo_Data$Performance.Tag)  
mean(PredTestRF_Demo_Tuned == Test_Demo_Data$Performance.Tag)#69.16% 
Metrix(Test_Demo_Data$Performance.Tag,PredTestRF_Demo_Tuned)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# 0.29490122        0.60913140        0.69162449        0.39086860        0.70509878        0.05605238 


# Predicting on rejected data
PredRejectRF_Demo_Tuned <- predict(Model_RanF_DemoData_Tuned,Rejected_Demo_Data[,-28],type = "class")
table(PredRejectRF_Demo_Tuned, Rejected_Demo_Data$Performance.Tag)  
mean(PredRejectRF_Demo_Tuned == Rejected_Demo_Data$Performance.Tag)#57.05%
Metrix(Rejected_Demo_Data$Performance.Tag,PredRejectRF_Demo_Tuned)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# NaN         0.4294737         0.5705263         0.5705263               NaN         1.0000000 


#RF Model built on Demographic Balanced Train data gives accuracy of 72.25% on Train data(81.5% Sensitivity,71.84% Specificity) 
#RF Model built on Demographic Balanced Test data gives accuracy of 69.16% on test data(39.08% Sensitivity, 70.50% Specificity)
#RF Model built on Demographic Balanced Rejected data has classified 612 (57.05%) Customers as Bad customers 
#out of 1425 rejected customers



#--------------------------------------------Model Building on Demographic and Credit Data--------------------------------------------------------

#creating Train and Test Data

set.seed(300)
Index3 <- sample(nrow(ApprovedData),0.70*nrow(ApprovedData),replace = F)

TrainData <- ApprovedData[Index3,]
TestData <- ApprovedData[-Index3,]

summary(TrainData)
summary(TestData)

#------------------------------------------------------SMOTE more positive cases-------------------------------------------------

set.seed(500)
TrainData1 <- SMOTE(Performance.Tag ~ ., TrainData, perc.over = 100, perc.under=200)


prop.table(table(TrainData1$Performance.Tag))
#0   1 
#0.5 0.5 

# set.seed(500)
# Model_RanF_Data <- randomForest(Performance.Tag~.,data = TrainData1,importance = TRUE)
# Model_RanF_Data

# Call:
#   randomForest(formula = Performance.Tag ~ ., data = TrainData1,      importance = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 8
# 
# OOB estimate of  error rate: 22.57%
# Confusion matrix:
#   0    1 class.error
# 0 3276  836   0.2033074
# 1 1020 3092   0.2480545

# #Saving Model_RanF_Data as RDS object
# saveRDS(Model_RanF_Data, file = "Model_RanF_Data.rds")


# Restore the object 
Model_RanF_Data <- readRDS(file = "Model_RanF_Data.rds")

# Predicting on train set

predTrainRFData <- predict(Model_RanF_Data, TrainData[,-69], type = "class")
# Checking classification accuracy
table(predTrainRFData, TrainData$Performance.Tag) 
prop.table(table(predTrainRFData, TrainData$Performance.Tag)) 
mean(predTrainRFData == TrainData$Performance.Tag)#82% accuracy
Metrix(TrainData$Performance.Tag,predTrainRFData)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# 0.19048433        0.01750973        0.81679389        0.98249027        0.80951567        0.18471105 

# Predicting on test data
PredTestRFData <- predict(Model_RanF_Data,TestData[,-69],type = "class")
table(PredTestRFData, TestData$Performance.Tag) 
prop.table(table(PredTestRFData, TestData$Performance.Tag)) 
mean(PredTestRFData == TestData$Performance.Tag)#77.3% accuracy with 588 bad customers misclassified out of 891
Metrix(TestData$Performance.Tag,PredTestRFData)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# 0.20772031        0.65993266        0.77303982        0.34006734        0.79227969        0.06781558 

# Predicting on rejected data
PredRejectRFData <- predict(Model_RanF_Data,RejectedData[,-69],type = "class")
table(PredRejectRFData, RejectedData$Performance.Tag)
prop.table(table(PredRejectRFData, RejectedData$Performance.Tag))  
mean(PredRejectRFData == RejectedData$Performance.Tag)#69.26% accuracy 
Metrix(RejectedData$Performance.Tag,PredRejectRFData)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# NaN         0.3073684         0.6926316         0.6926316               NaN         1.0000000 

# To check important variables
importance(Model_RanF_Data)        
varImpPlot(Model_RanF_Data)

#RF Model built on Demographic and Credit Train Balanced data gives accuracy of 82% on Train data (98% Sensitivity,80% Specificity)
#RF Model built on Demographic and Credit Test Balanced data gives accuracy of 77.30% on test data (34% sensitivity,79.22% Specificity)
#RF Model built on Demographic and Credit Rejected Balanced data has classified 
#987 (69%) Customers as Bad customers out of 1425 rejected customers


#----------------------------------------------tuning the parameters-Demographic and Credit data model--------------------------------------------------------

# # # # Using For loop to identify the right mtry for model
# a=c()
# set.seed(200)
# i=5
# for (i in 3:10) {
#   model3 <- randomForest(Performance.Tag ~ ., data = TrainData1, ntree = 500, mtry = i, importance = TRUE)
#   predValid <- predict(model3, TestData, type = "class")
#   a[i-2] = mean(predValid == TestData$Performance.Tag)
# }

a
b <- c(3:10)
c <- cbind(a,b)
# a  b
# [1,] 0.7405214  3
# [2,] 0.7648267  4
# [3,] 0.7738038  5
# [4,] 0.7759049  6
# [5,] 0.7740903  7
# [6,] 0.7737561  8
# [7,] 0.7730876  9
# [8,] 0.7703180 10
dev.off()
plot(b,a)
# 
# #Saving c as RDS object
# saveRDS(c, file = "c.rds")
# saveRDS(a, file = "a.rds")
# saveRDS(b, file = "b.rds")


# Restore the object 
c <- readRDS(file = "c.rds")
a <- readRDS(file = "a.rds")
b <- readRDS(file = "b.rds")

#Model shows the highest accuracy of 77.6% for 6 variables at each split. 
#Accuracy has dropped for more than 6 variables at each split
# 
# set.seed(500)
# Model_RanF_Data_Tuned <- randomForest(Performance.Tag~.,data = TrainData1,importance = TRUE,ntree = 500,mtry=6)
# Model_RanF_Data_Tuned
# Call:
#   randomForest(formula = Performance.Tag ~ ., data = TrainData1,      importance = TRUE, ntree = 500, mtry = 6) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 6
# 
# OOB estimate of  error rate: 22.96%
# Confusion matrix:
#   0    1 class.error
# 0 3285  827   0.2011187
# 1 1061 3051   0.2580253
# 
# #Saving Model_RanF_Data_Tuned as RDS object
# saveRDS(Model_RanF_Data_Tuned, file = "Model_RanF_Data_Tuned.rds")


# Restore the object 
Model_RanF_Data_Tuned <- readRDS(file = "Model_RanF_Data_Tuned.rds")

# Predicting on train set
predTrainRFDataTuned <- predict(Model_RanF_Data_Tuned, TrainData[,-69], type = "class")
# Checking classification accuracy
table(predTrainRFDataTuned, TrainData$Performance.Tag) 
prop.table(table(predTrainRFDataTuned, TrainData$Performance.Tag))
mean(predTrainRFDataTuned == TrainData$Performance.Tag)#82% accuracy
Metrix(TrainData$Performance.Tag,predTrainRFDataTuned)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# 0.18896746        0.06712062        0.81615947        0.93287938        0.81103254        0.17820310 


# Predicting on test data
PredTestRFDataTuned <- predict(Model_RanF_Data_Tuned,TestData[,-69],type = "class")
table(PredTestRFDataTuned, TestData$Performance.Tag)
prop.table(table(PredTestRFDataTuned, TestData$Performance.Tag)) 
mean(PredTestRFDataTuned == TestData$Performance.Tag)#77% accuracy with 592 misclassification out of 891 Bad customers
Metrix(TestData$Performance.Tag,PredTestRFDataTuned)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# 0.20637375        0.66442200        0.77413810        0.33557800        0.79362625        0.06738787  

# Predicting on rejected data
PredRejectRFDataTuned <- predict(Model_RanF_Data_Tuned,RejectedData[,-69],type = "class")
table(PredRejectRFDataTuned, RejectedData$Performance.Tag) 
prop.table(table(PredRejectRFDataTuned, RejectedData$Performance.Tag))
mean(PredRejectRFDataTuned == RejectedData$Performance.Tag)#70% accuracy with 1003 classified as bad customers out of 1425 rejected customers
Metrix(RejectedData$Performance.Tag,PredRejectRFDataTuned)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# NaN         0.2961404         0.7038596         0.7038596               NaN         1.0000000 

# To check important variables
importance(Model_RanF_Data_Tuned)        
varImpPlot(Model_RanF_Data_Tuned)


#RF Model built on Demographic and Credit Balanced data gives accuracy of 82% on Train data(93% Sensitivity,81% Specificity) 
#RF Model built on Demographic Balanced data gives accuracy of 77% on test data (33.56% Sensitivity,79% Specificity)
#with 592 misclassification on positives out of 898 cases.
#RF Model built on Demographic Balanced data has classified 1003 Customers as Bad customers out of 1425 rejected customers


#Summary- though random forest model accuracy is improved when compared with logistic regression model,
#the random forest model is bias when the sensitivity of the model is compared between Train and test data


#------------------------------------------------Summary on Logistic and Random forest model--------------------------------------
#--------------------------------Logistic Model on Balanced Demo Data @49% probability cut off--------------------------------
#sensitivity-52%
#specificity-62%(Improved when compared to imbalanced data)
#Accuracy-62%(Improved when compared to imbalanced data)
#model on demographic data can classify only 70% of rejected customers as bad customers


#--------------------------------Logistic Model on Balanced Demo+ Credit data-------------------------------------------------
#sensitivity-59%
#specificity-63%
#Accuracy-63%
#Model classifies 95% rejected customers as bad customers. 

#----------------------------------------Random Forest Model on Demo Data-----------------------------------------------------
# Accuracy-69%
# Sensitivity-39%(Model is bias on sensitivity)
# Specificity-71%
# Model has classified 57% of Rejected customers correctly

#-------------------------------------Random Forest Model on Demo and Credit data---------------------------------------------
#Accuracy-77%
#Sensitivity-34%
#Specificity-79%

#Model has classified 70% of Customers as Bad customers out of 1425 rejected customers

#Model Performance is better when built on Demographic and Credit data together compared to Demographic data
#Random Forest model performance is improved significantly in terms of accuracy and Specificity but 
#model is  bias when it comes to sensitivity

# To check important variables
importance(Model_RanF_Data_Tuned)        
varImpPlot(Model_RanF_Data_Tuned)

#Important variable.names
#Age
#Income
#No.of.months.in.current.residence
#Outstanding.Balance 
# EducationMasters
# EducationBachelor
# EducationProfessional
# ProfessionSAL
# ProfessionSE
# ProfessionSE_PROF
# CurrentcompanyPeriod3_36
# CurrentcompanyPeriod37_133
# Type.of.residenceOwned
# Type.of.residenceRented 
# No.of.dependents1 
# No.of.dependents2
# No.of.dependents3
# No.of.dependents4
# No.of.dependents5
# GenderF
# GenderM
# InquiriesinLast6monthsNoEnquiry
# InquiriesinLast6months1Enquiry
# InquiriesinLast6monthsMorethan1Enquiry
# AvgCCUtilizationGroupsLessthan11
# AvgCCUtilizationGroupsLessthan33
# AvgCCUtilizationGroupsMorethanorEqualto33
# Marital.Status..at.the.time.of.application.Married
# Marital.Status..at.the.time.of.application.Single
# InquiriesinLast12monthsLessthan4Enquiries
# InquiriesinLast12monthsMorethanorEqualto4Enquiries
#InquiriesinLast12monthsNoEnquiry
# TradesinLast6monthsLessthan3Trades
# TradesinLast6monthsNoTrades


#Random Forest Cross Validation

# #Creating a subset of important variables

TrainData2 <- TrainData1[,c("Age","Income","No.of.months.in.current.residence","Outstanding.Balance","EducationMasters",
                            "EducationBachelor","EducationProfessional","ProfessionSAL","ProfessionSE","ProfessionSE_PROF",
                            "CurrentcompanyPeriod3_36","CurrentcompanyPeriod37_133","Type.of.residenceOwned","Type.of.residenceRented",
                            "No.of.dependents1","No.of.dependents2","No.of.dependents3","No.of.dependents4","No.of.dependents5",
                            "GenderF","GenderM","InquiriesinLast6monthsNoEnquiry","InquiriesinLast6months1Enquiry",
                            "InquiriesinLast6monthsMorethan1Enquiry","AvgCCUtilizationGroupsLessthan11","AvgCCUtilizationGroupsLessthan33",
                            "AvgCCUtilizationGroupsMorethanorEqualto33","Marital.Status..at.the.time.of.application.Married",
                            "Marital.Status..at.the.time.of.application.Single","InquiriesinLast12monthsLessthan4Enquiries",
                            "InquiriesinLast12monthsMorethanorEqualto4Enquiries","InquiriesinLast12monthsNoEnquiry",
                            "TradesinLast6monthsLessthan3Trades","TradesinLast6monthsNoTrades","Performance.Tag")]


### Register parallel backend
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# getDoParWorkers()
# #insert code here
# 
# 
# ctrl <- trainControl(method="cv", number=10)
# 
# # evaluate the SMOTE performance
# set.seed(100)
# Model_RanF_Data_Tuned_IMPVar <- train(Performance.Tag ~ ., data = TrainData2, method = "rf",
#                  trControl = ctrl)

# Random Forest 
# 
# 8224 samples
# 34 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 7402, 7401, 7401, 7402, 7402, 7402, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.7227628  0.4455367
# 18    0.7548591  0.5097113
# 34    0.7509700  0.5019326
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 18.

### Stop cluster
# stopCluster(cl)
# 
# set.seed(500)
# Model_RanF_Data_Tuned_IMPVar <- randomForest(Performance.Tag~.,data = TrainData2,importance = TRUE,ntree = 500,mtry=18)
# Model_RanF_Data_Tuned_IMPVar


# Call:
#   randomForest(formula = Performance.Tag ~ ., data = TrainData2,      importance = TRUE, ntree = 500, mtry = 18) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 18
# 
# OOB estimate of  error rate: 23.98%
# Confusion matrix:
#   0    1 class.error
# 0 3241  871   0.2118191
# 1 1101 3011   0.2677529

# #Saving Model_RanF_Data_Tuned as RDS object
# saveRDS(Model_RanF_Data_Tuned_IMPVar, file = "Model_RanF_Data_Tuned_IMPVar.rds")

#Restore the object
Model_RanF_Data_Tuned_IMPVar <- readRDS(file = "Model_RanF_Data_Tuned_IMPVar.rds")

# Predicting on train set
predTrainRFDataTuned_IMPVar<- predict(Model_RanF_Data_Tuned_IMPVar, TrainData[,-69], type = "prob")
predTrainRFDataTuned_IMPVar1 <- ifelse(predTrainRFDataTuned_IMPVar[,2]>=0.50,1,0)
# Checking classification accuracy
table(predTrainRFDataTuned_IMPVar1, TrainData$Performance.Tag) 
prop.table(table(predTrainRFDataTuned_IMPVar1, TrainData$Performance.Tag))
mean(predTrainRFDataTuned_IMPVar1 == TrainData$Performance.Tag)#80.56 accuracy
Metrix(TrainData$Performance.Tag,predTrainRFDataTuned_IMPVar1)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# 0.2028543         0.0000000         0.8056812         1.0000000         0.7971457         0.1779932 


# Predicting on test data
PredTestRFDataTuned_IMPVar <- predict(Model_RanF_Data_Tuned_IMPVar,TestData[,-69],type = "prob")
PredTestRFDataTuned_IMPVar1 <- ifelse(PredTestRFDataTuned_IMPVar[,2]>=0.50,1,0)
table(PredTestRFDataTuned_IMPVar1, TestData$Performance.Tag)
prop.table(table(PredTestRFDataTuned_IMPVar1, TestData$Performance.Tag)) 
mean(PredTestRFDataTuned_IMPVar1 == TestData$Performance.Tag)#76% accuracy with 584 misclassification out of 891 Bad customers
Metrix(TestData$Performance.Tag,PredTestRFDataTuned_IMPVar1)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# 0.22357987        0.65544332        0.75804603        0.34455668        0.77642013        0.06409186

# Predicting on rejected data
PredRejectRFDataTuned_IMPVar <- predict(Model_RanF_Data_Tuned_IMPVar,RejectedData[,-69],type = "prob")
PredRejectRFDataTuned_IMPVar1 <- ifelse(PredRejectRFDataTuned_IMPVar[,2]>=0.50,1,0)
table(PredRejectRFDataTuned_IMPVar1, RejectedData$Performance.Tag) 
prop.table(table(PredRejectRFDataTuned_IMPVar1, RejectedData$Performance.Tag))
mean(PredRejectRFDataTuned_IMPVar1 == RejectedData$Performance.Tag)#55% accuracy with 784 classified as bad customers out of 1425 rejected customers
Metrix(RejectedData$Performance.Tag,PredRejectRFDataTuned_IMPVar1)
# FalsePositiverate FalseNegativerate          Accuracy       Sensitivity       Specificity         Precision 
# NaN         0.4484211         0.5515789         0.5515789               NaN         1.0000000 

	
#Logistic model is low on accuracy when compared with Randome Forest. 
#However, RandomForest is bias in terms of sensitivity. The objective of building model for CredX is to reduce the credit loss.
#Hence, Sensitivity and False Negative rates determines the model's capability to identify the bad customers correctly.
#Randome forest model does not show satisfactory results on the parameters Sensitivity and False Positive Rate.
#------------------------------------------------SVM Model-----------------------------------------------------------------

#SVM Model on balanced data

# ### Register parallel backend
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# getDoParWorkers()
# 
# trctrl <-  trainControl(method= "CV",number = 10)
# 
# set.seed(1001)
# svm_linear <- train(Performance.Tag~.,data=TrainData2,method="svmLinear",
#                       trControl= trctrl,preProcess=c("center", "scale"),
#                       tuneLength = 10)
# 
# ### Stop cluster
# stopCluster(cl)


svm_linear
# Support Vector Machines with Linear Kernel 
# 
# 8224 samples
# 34 predictor
# 2 classes: '0', '1' 
# 
# Pre-processing: centered (34), scaled (34) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 7402, 7401, 7401, 7402, 7402, 7402, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.6049352  0.2098665
# 
# Tuning parameter 'C' was held constant at a value of 1

#prediction on train data



# library(e1071)
# set.seed(1001)
# svm_linear <- svm(Performance.Tag~.,data=TrainData2,probability = TRUE,cost=1)

#Saving svm_linear as RDS object
#saveRDS(svm_linear, file = "svm_linear.rds")


# Restore the object 
svm_linear <- readRDS(file = "svm_linear.rds")

train_pred_svmLinear <- predict(svm_linear,TrainData,probability = TRUE)
head(attr(train_pred_svmLinear,"probabilities"))
confusionMatrix(train_pred_svmLinear,TrainData$Performance.Tag,positive = "1")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 34460   746
# 1 12347  1310
# 
# Accuracy : 0.732          
# 95% CI : (0.7281, 0.736)
# No Information Rate : 0.9579         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.101          
# Mcnemar's Test P-Value : <2e-16         
# 
# Sensitivity : 0.63716        
# Specificity : 0.73621        
# Pos Pred Value : 0.09592        
# Neg Pred Value : 0.97881        
# Prevalence : 0.04208        
# Detection Rate : 0.02681        
# Detection Prevalence : 0.27950        
# Balanced Accuracy : 0.68669        
# 
# 'Positive' Class : 1       



#Prediction on test data

test_pred_svmLinear <- predict(svm_linear,TestData,probability = TRUE)
head(attr(test_pred_svmLinear,"probabilities" ))

confusionMatrix(test_pred_svmLinear ,TestData$Performance.Tag,positive = "1")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 14729   525
# 1  5322   366
# 
# Accuracy : 0.7208          
# 95% CI : (0.7147, 0.7269)
# No Information Rate : 0.9575          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0407          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.41077         
# Specificity : 0.73458         
# Pos Pred Value : 0.06435         
# Neg Pred Value : 0.96558         
# Prevalence : 0.04255         
# Detection Rate : 0.01748         
# Detection Prevalence : 0.27161         
# Balanced Accuracy : 0.57268         
# 
# 'Positive' Class : 1  
#prediction on rejected data

rejected_pred_svmLinear <- predict(svm_linear,RejectedData,probability = TRUE)
head(attr(rejected_pred_svmLinear,"probabilities"))


confusionMatrix(rejected_pred_svmLinear,RejectedData$Performance.Tag,positive = "1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0   0 603
# 1   0 822
# 
# Accuracy : 0.5768          
# 95% CI : (0.5507, 0.6027)
# No Information Rate : 1               
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0               
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.5768          
# Specificity :     NA          
# Pos Pred Value :     NA          
# Neg Pred Value :     NA          
# Prevalence : 1.0000          
# Detection Rate : 0.5768          
# Detection Prevalence : 0.5768          
# Balanced Accuracy :     NA          
# 
# 'Positive' Class : 1               



#customizing C value(Cost) in Linear classifier by inputting values in grid search.
# 
# grid <- expand.grid(C=c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
# 
# 
# ### Register parallel backend
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# getDoParWorkers()
# 
# 
# trctrl <-  trainControl(method= "CV",number = 10)
# 
# set.seed(1001)
# svm_linear_Grid <- train(Performance.Tag~.,data=TrainData2,method="svmLinear",
#                     trControl= trctrl,preProcess=c("center", "scale"),
#                           tuneLength = 10,tuneGrid=grid)
# 
# ### Stop cluster
# stopCluster(cl)

svm_linear_Grid

# Support Vector Machines with Linear Kernel 
# 
# 8224 samples
# 34 predictor
# 2 classes: '0', '1' 
# 
# Pre-processing: centered (34), scaled (34) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 7402, 7401, 7401, 7402, 7402, 7402, ... 
# Resampling results across tuning parameters:
#   
#   C     Accuracy   Kappa    
# 0.00        NaN        NaN
# 0.01  0.6022582  0.2045135
# 0.05  0.6049352  0.2098665
# 0.10  0.6049352  0.2098665
# 0.25  0.6049352  0.2098665
# 0.50  0.6049352  0.2098665
# 0.75  0.6049352  0.2098665
# 1.00  0.6049352  0.2098665
# 1.25  0.6049352  0.2098665
# 1.50  0.6049352  0.2098665
# 1.75  0.6049352  0.2098665
# 2.00  0.6049352  0.2098665
# 5.00  0.6049352  0.2098665
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was C = 0.05.

 

plot(svm_linear_Grid)#Accuracy is highest at C=0.5


# set.seed(1001)
# svm_linear_Grid <- svm(Performance.Tag~.,data=TrainData2,probability = TRUE,cost=0.05)
#Saving svm_linear_Grid as RDS object
# saveRDS(svm_linear_Grid, file = "svm_linear_Grid.rds")


#Restore the object
svm_linear_Grid <- readRDS(file = "svm_linear_Grid.rds")

#predictions for train data
train_pred_svmLinearGrid <- predict(svm_linear_Grid,TrainData,probability = TRUE)
head(attr(train_pred_svmLinearGrid,"probabilities"))

confusionMatrix(train_pred_svmLinearGrid,TrainData$Performance.Tag,positive = "1")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 26711   654
# 1 20096  1402
# 
# Accuracy : 0.5753          
# 95% CI : (0.5709, 0.5797)
# No Information Rate : 0.9579          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0458          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.68191         
# Specificity : 0.57066         
# Pos Pred Value : 0.06522         
# Neg Pred Value : 0.97610         
# Prevalence : 0.04208         
# Detection Rate : 0.02869         
# Detection Prevalence : 0.43996         
# Balanced Accuracy : 0.62628         
# 
# 'Positive' Class : 1  


#predictions for test data
predict_Test_SVMlinearGrid <- predict(svm_linear_Grid,TestData,probability = TRUE)

head(attr(predict_Test_SVMlinearGrid,"probabilities"))


confusionMatrix(predict_Test_SVMlinearGrid,TestData$Performance.Tag,positive = "1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 11489   329
# 1  8562   562
# 
# Accuracy : 0.5754          
# 95% CI : (0.5687, 0.5822)
# No Information Rate : 0.9575          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0376          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.63075         
# Specificity : 0.57299         
# Pos Pred Value : 0.06160         
# Neg Pred Value : 0.97216         
# Prevalence : 0.04255         
# Detection Rate : 0.02684         
# Detection Prevalence : 0.43568         
# Balanced Accuracy : 0.60187         
# 
# 'Positive' Class : 1  

predict_Rejected_SVMlinearGrid <- predict(svm_linear_Grid,RejectedData,probability = TRUE)

head(attr(predict_Rejected_SVMlinearGrid,"probabilities"))


confusionMatrix(predict_Rejected_SVMlinearGrid,RejectedData$Performance.Tag,positive = "1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0    0  230
# 1    0 1195
# 
# Accuracy : 0.8386          
# 95% CI : (0.8184, 0.8573)
# No Information Rate : 1               
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0               
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.8386          
# Specificity :     NA          
# Pos Pred Value :     NA          
# Neg Pred Value :     NA          
# Prevalence : 1.0000          
# Detection Rate : 0.8386          
# Detection Prevalence : 0.8386          
# Balanced Accuracy :     NA          
# 
# 'Positive' Class : 1               


#SVM Classifier using Non-Linear Kernel

# ### Register parallel backend
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# getDoParWorkers()
# 
# 
# trctrl <-  trainControl(method= "CV",number = 10)
# 
# set.seed(1001)
# svm_Radial <- train(Performance.Tag~.,TrainData2,method="svmRadial",
#                     trControl=trctrl,preProcess=c("center","scale"),
#                     tuneLength=10)
# 
# ### Stop cluster
# stopCluster(cl)


svm_Radial

# Support Vector Machines with Radial Basis Function Kernel 
# 
# 8224 samples
# 34 predictor
# 2 classes: '0', '1' 
# 
# Pre-processing: centered (34), scaled (34) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 7402, 7401, 7401, 7402, 7402, 7402, ... 
# Resampling results across tuning parameters:
#   
#   C       Accuracy   Kappa    
# 0.25  0.6489549  0.2979195
# 0.50  0.6718138  0.3436335
# 1.00  0.6871327  0.3742701
# 2.00  0.6960107  0.3920200
# 4.00  0.7073206  0.4146393
# 8.00  0.7144927  0.4289859
# 16.00  0.7203305  0.4406600
# 32.00  0.7256802  0.4513600
# 64.00  0.7271397  0.4542762
# 128.00  0.7248289  0.4496560
# 
# Tuning parameter 'sigma' was held constant at a value of 0.01725058
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 0.01725058 and C = 64.


plot(svm_Radial)

# set.seed(1001)
# svm_Radial <- svm(Performance.Tag~.,data=TrainData2,probability = TRUE,kernel="radial",cost=64,gamma=0.01725058)

#Saving svm_linear_Grid as RDS object
# saveRDS(svm_Radial, file = "svm_Radial.rds")


# Restore the object 
svm_Radial <- readRDS(file = "svm_Radial.rds")

#prediction on train data
predict_train_svmRadial <- predict(svm_Radial,TrainData,probability=TRUE)
head(attr(predict_train_svmRadial,"probabilities"))
confusionMatrix(predict_train_svmRadial,TrainData$Performance.Tag,positive = "1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 35594   337
# 1 11213  1719
# 
# Accuracy : 0.7636          
# 95% CI : (0.7598, 0.7674)
# No Information Rate : 0.9579          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.169           
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.83609         
# Specificity : 0.76044         
# Pos Pred Value : 0.13293         
# Neg Pred Value : 0.99062         
# Prevalence : 0.04208         
# Detection Rate : 0.03518         
# Detection Prevalence : 0.26466         
# Balanced Accuracy : 0.79827         
# 
# 'Positive' Class : 1  

#prediction on test data
predict_test_svmRadial <- predict(svm_Radial,TestData,probability = TRUE)
head(attr(predict_test_svmRadial,"probabilities"))
confusionMatrix(predict_test_svmRadial,TestData$Performance.Tag,positive = "1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 14976   598
# 1  5075   293
# 
# Accuracy : 0.7291         
# 95% CI : (0.723, 0.7351)
# No Information Rate : 0.9575         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.0223         
# Mcnemar's Test P-Value : <2e-16         
# 
# Sensitivity : 0.32884        
# Specificity : 0.74690        
# Pos Pred Value : 0.05458        
# Neg Pred Value : 0.96160        
# Prevalence : 0.04255        
# Detection Rate : 0.01399        
# Detection Prevalence : 0.25633        
# Balanced Accuracy : 0.53787        
# 
# 'Positive' Class : 1   


#prediction on rejected data
predict_Rejected_svmRadial <- predict(svm_Radial,RejectedData,probability = TRUE)
head(attr(predict_Rejected_svmRadial,"probabilities"))
confusionMatrix(predict_Rejected_svmRadial,RejectedData$Performance.Tag,positive = "1")
# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0   0 780
# 1   0 645
# 
# Accuracy : 0.4526          
# 95% CI : (0.4266, 0.4789)
# No Information Rate : 1               
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0               
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.4526          
# Specificity :     NA          
# Pos Pred Value :     NA          
# Neg Pred Value :     NA          
# Prevalence : 1.0000          
# Detection Rate : 0.4526          
# Detection Prevalence : 0.4526          
# Balanced Accuracy :     NA          
# 
# 'Positive' Class : 1       

#SVM Radial Model is performing well on Train data compared to SVM Linear Model but not stable


#Based on results of Logistic, Random Forest and SVM Model
#summary-

# Discriminatory power
#Logistic Regression Model is able to classify Good and Bad customers with 63% accuracy,59% sensitivity 
#and 63% specificity. Area under curve is 66%
#Random Forest Model is able to classify Good and Bad Customers with 82% Accuracy and 81% specificity. 
#But RF Modelis not able to discriminate positives correctly(Sensitivity is inconsistent across training and validation datasets)
#SVM Model is able to classify Good and Bad Customers with 58% accuracy and specificity. 
#Sensitivity of this model is 63%

#stability of the model
#Performance of Logistic and SVM model is consistence during K fold validation, Train and Test data. 
#However, RF model is not consistent across train and validation dataset when it comes to sensitivity.

#Rejected population.
#Logistic Model is able to classify 95% of the Rejected Customers as bad customers 
#where SVM is able to classify 84% of the rejected customers as bad customers

# So the Logistic Model is chosen  based on the below parameters 
# 1)Discriminatory power,
# 2)Accuracy 
# 3)stability
# 4)high level accuracy on rejected population


#--------------------------------------------------------Application Score Card
Prediction <- predict(logist_WOE_Balanced9,ApprovedData_WOE[,-28],type = "response")
ApprovedData$Prediction <- Prediction
ApprovedData$prob_good <- 1-ApprovedData$Prediction 
ApprovedData$odds_good <- ApprovedData$prob_good/ApprovedData$Prediction
ApprovedData$log_odds_good <- log(ApprovedData$odds_good)
ApprovedData <- ApprovedData%>%arrange(desc(odds_good))

library(scorecard)

# filter variable via missing rate, iv, identical value rate
dt_f = var_filter(ApprovedData, y="Performance.Tag")

#woe binning
bins=woebin(dt_f$Performance.Tag,y="Performance.Tag")

# binning adjustment
## adjust breaks interactively
breaks_adj = woebin_adj(dt_f, "Performance.Tag", bins) 

# score ------
## scorecard
card = scorecard(bins, logist_WOE_Balanced9)
## credit score
score_list = lapply(dt_f0, function(x) scorecard_ply(x, card))
## psi
perf_psi(score = score_list, label = label_list)


#We can use scorecard::woebin() to automatically create the bins and calculate the WOE values

bins <- bins = woebin(ApprovedData, y = 'Performance.Tag')

#Classically we would use predict() with type = 'response' to directly get the probability. 
#However here we will do the calculations manually as described above.

pred = predict(logist_WOE_Balanced9)
resp = predict(logist_WOE_Balanced9, type = 'response')

res = tibble( logit = pred
              , odds = exp(pred)
              , prob = odds / (odds + 1)
              , prob_ctrl = resp )


res %>%
  f_datatable_universal( page_length =  10, round_other_nums = 5 )

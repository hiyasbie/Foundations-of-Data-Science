#Table1
getwd()
setwd("~/Documents/JCU/SP1_2020/FoundationsOfDataScience")
read.table("crx.data")
Data = read.table("crx.data", header = FALSE,sep = ",",na.strings = "?")
#Table2
names(Data) <- c("Gender", "Age", "MonthlyExpenses", "MaritalStatus", "HomeStatus", "Occupation", "BankingInstitution", "YearsEmployed", "NoPriorDefault", "Employed", "CreditScore", "DriversLicense", "AccountType", "MonthlyIncome", "AccountBalance", "Approved")
#Table3
Data$Gender <- as.factor(Data$Gender) 
Data$Age <- as.numeric(Data$Age)
Data$MonthlyExpenses <- as.integer(Data$MonthlyExpenses) 
Data$MaritalStatus <- as.factor(Data$MaritalStatus) 
Data$HomeStatus <- as.factor(Data$HomeStatus) 
Data$Occupation <- as.factor(Data$Occupation) 
Data$BankingInstitution <- as.factor(Data$BankingInstitution) 
Data$YearsEmployed <- as.numeric(Data$YearsEmployed) 
Data$NoPriorDefault <- as.factor(Data$NoPriorDefault) 
Data$Employed <- as.factor(Data$Employed) 
Data$CreditScore <- as.numeric(Data$CreditScore) 
Data$DriversLicense <- as.factor(Data$DriversLicense)
Data$AccountType <- as.factor(Data$AccountType) 
Data$MonthlyIncome <- as.integer(Data$MonthlyIncome) 
Data$AccountBalance <- as.numeric(Data$AccountBalance) 
Data$Approved <- as.factor(Data$Approved)


summary(Data)
#Table5
Data <- na.omit(Data)
#Table6
summary(Data)


#Table7
install.packages("cluster")
library("cluster")
Dist = daisy(Data,metric ='gower')    
Dist

#Table8
Dist<-as.matrix(Dist)
#Table9
Dist[10,60]

#Table10
dim <- ncol(Dist)  # used to define axis in image
image(1:dim, 1:dim, Dist, axes = FALSE, xlab="", ylab="", col = rainbow(100))
#Optional
heatmap(Dist, Rowv=TRUE, Colv="Rowv", symm = TRUE)

#Table12
DataRequired = c("Age","MonthlyExpenses", "YearsEmployed","CreditScore","MonthlyIncome","AccountBalance")
Data2 = Data[ ,DataRequired]
cor(Data2, method ="pearson")
cor(Data2, method ="spearman")


#Table13
library(ggplot2)
ggplot(Data,map=aes(x=Approved,y=Age))+ geom_boxplot(outlier.color = "red",fill ="Yellow") + geom_jitter(width=0.05, alpha=0.05,color = "Blue")
ggplot(Data,map=aes(x=Approved,y=MonthlyExpenses))+ geom_boxplot(outlier.color = "red",fill ="Orange") + geom_jitter(width=0.05, alpha=0.05,color = "Blue")
ggplot(Data,map=aes(x=Approved,y=CreditScore))+ geom_boxplot(outlier.color = "red",fill ="YellowGreen") + geom_jitter(width=0.05, alpha=0.05,color = "Blue")
ggplot(Data,map=aes(x=Approved,y=AccountBalance))+geom_boxplot(outlier.shape = NA,fill ="Purple") + geom_jitter(width=0.05, alpha=0.05,color="Blue")+ coord_cartesian(ylim=quantile(y=AccountBalance,c(0,5000)))

#Table 15
ggplot(Data,map=aes(Employed))+geom_bar(fill="Red")+facet_wrap(Data$Approved,)+ylab('Approved Count')
ggplot(Data,map=aes(MaritalStatus))+geom_bar(fill="Blue")+facet_wrap(Data$Approved)+ylab('Approved Count')
ggplot(Data,map=aes(BankingInstitution))+geom_bar(fill="Green")+ facet_wrap(Data$Approved)+ylab('Approved Count')
ggplot(Data,map=aes(NoPriorDefault))+geom_bar(fill="Brown")+facet_wrap(Data$Approved)+ylab('Approved Count')
  

table(Data$NoPriorDefault,Data$Approved)
#using SMC formula
SMC = (286+278)/(286+18+71+278)
SMC
#using Jaccard formula
Jaccard = (278)/(18+71+278)
Jaccard

library(ggplot2)
ggplot(Data, aes (MonthlyIncome,))+
  geom_histogram()+facet_wrap("Approved")

ggplot(Data) + 
  geom_point(aes(x=Approved, y=MonthlyIncome, color='red'))


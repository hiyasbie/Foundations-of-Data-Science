library(dplyr)
library(ggplot2)
library(tidyr)

#Read Data
Data = read.table("Live.csv", header = T, sep = ",",na.strings = "")
summary(Data)

#Select Necessary variables and data / Delete Columns1,2,3,4
Data = Data %>% select(status_id,status_type,status_published,
                       num_reactions,num_comments,num_shares, num_likes,num_loves,num_wows,
                       num_hahas,num_sads,num_angrys)
str(Data)

#Assign Variable Types manually
Data$status_published = as.POSIXct(Data$status_published , format = "%m/%d/%Y %H:%M")
Data$num_reactions = as.numeric(Data$num_reactions)
Data$num_comments = as.numeric(Data$num_comments)
Data$num_shares = as.numeric(Data$num_shares)
Data$num_likes = as.numeric(Data$num_likes)
Data$num_loves = as.numeric(Data$num_loves)
Data$num_wows = as.numeric(Data$num_wows)
Data$num_hahas = as.numeric(Data$num_hahas)
Data$num_sads = as.numeric(Data$num_sads)
Data$num_angrys = as.numeric(Data$num_angrys)
str(Data)

#Added new Variables Quarters,Months and Years
Data = Data %>% mutate(Quarters = quarters.POSIXt(status_published)) %>% 
  mutate(Months = months.POSIXt(status_published)) %>% 
  mutate(Years = as.numeric(format(Data$status_published,'%Y')))

#Assign type for new Variables 
Data$Years = as.numeric(Data$Years)
Data$Quarters = as.factor(Data$Quarters)
Data$Months = factor(Data$Months, levels = c("January","February","March","April",
                                             "May","June","July","August","September",
                                             "October","November", "December"))
#Eliminate rows with Year 2012 and 2018
Data = Data %>% filter(Years > 2012 ) %>% filter(Years < 2018)
summary(Data)

#Data Exploration - Pearson Correlation
DataRequired = c( "num_reactions","num_comments","num_shares","num_likes","num_loves","num_wows","num_hahas","num_sads","num_angrys")
Data2 = Data[ ,DataRequired]
cor(Data2, method ="pearson")

#Data Visualization - scatterplot and Linear Regression
ggplot(Data2, aes(num_reactions,num_likes)) + geom_point(color="Red") + geom_smooth(method = "lm") + 
  labs(title = "Scatterplot between Number of Reaction and Likes")
ggplot(Data2, aes(num_comments,num_shares)) + geom_point(color="Blue") + geom_smooth(method = "lm") +
labs(title = "Scatterplot between Number of Comments and Shares")
ggplot(Data2, aes(num_loves,num_shares)) + geom_point(color="Green") + geom_smooth(method = "lm") +
labs(title = "Scatterplot between Number of Loves and Shares")

#Data Manipulation - Group data set by Years,Quarters, Months and Means of the Number of Reactions, comments and shares
Data_Years = Data %>% group_by(Years, Quarters, Months, status_type) %>%
  summarise(num_reactions=mean(num_reactions),
            num_comments = mean(num_comments),
            num_shares = mean(num_shares))

#Bargraph of Number of Reactions per Years and Quarters
ggplot(Data_Years,aes(Years, num_reactions)) + geom_col(fill = "Red") 
ggplot(Data_Years,aes(Years, num_comments)) + geom_col(fill = "Blue") 
ggplot(Data_Years,aes(Years, num_shares)) + geom_col(fill = "Green") 

# Quarters Data
Data_Quarters = Data_Years %>% group_by( Quarters,Months, status_type) %>%
  summarise(num_reactions=mean(num_reactions),
            num_comments = mean(num_comments),
            num_shares = mean(num_shares))

#Bar graph for Quarters Data
ggplot(Data_Quarters,aes(Quarters, num_reactions)) + geom_col(fill = "Red") 
ggplot(Data_Quarters,aes(Quarters, num_comments)) + geom_col(fill = "Blue") 
ggplot(Data_Quarters,aes(Quarters, num_shares)) + geom_col(fill = "Green") 

#Boxplot for status_type
ggplot(Data_Type, aes(status_type,num_reactions)) + geom_boxplot(fill="Purple")
ggplot(Data_Type, aes(status_type,num_comments)) + geom_boxplot(fill="Yellow") +
  coord_cartesian(ylim = quantile(y=num_comments,c(0,700)))
ggplot(Data_Type, aes(status_type,num_shares)) + geom_boxplot(fill="Green")

#Months Data
Data_Months = Data_Years %>% group_by(Months, status_type) %>%
  summarise(num_reactions=mean(num_reactions),
            num_comments = mean(num_comments),
            num_shares = mean(num_shares))

#Bar graph for Months Data
ggplot(Data_Months,aes(Months, num_reactions)) + geom_col(fill = "Red") + facet_wrap("status_type")
ggplot(Data_Months,aes(Months, num_comments)) + geom_col(fill = "Blue") + facet_wrap("status_type")
ggplot(Data_Months,aes(Months, num_shares)) + geom_col(fill = "Green") + facet_wrap("status_type")





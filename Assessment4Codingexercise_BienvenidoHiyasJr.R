library(dplyr)
library(ggplot2)

setwd("~/Documents/JCU/SP1_2020/FoundationsOfDataScience")
data = read.csv("Internet.csv")
str(data)
summary(data)

#table2
ggplot(data, aes(databytes)) + 
  geom_histogram(bins = 20)

#table3 = plot

#table4
data2 = filter(data, 
            databytes < 32)

#table5
data2[1:4,]

#table6
data = mutate(data,
              kbytes =databytes/100 )
#table7
data[1:4,]

#table8
data = read.csv("Internet.csv")
data_summary = data %>%
  filter(databytes < 32) %>%
  mutate(Mbytes = databytes/1000) %>%
  group_by(source) %>%
  summarise(databytes = median(databytes))

#table8
data_summary[1:4,]

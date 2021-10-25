#~~~~~~~~~~~
# Libraries
#~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(tidyr)

#~~~~~~~~~~~
# Functions
#~~~~~~~~~~
rescale_01 <-function(x) (x-min(x))/(max(x)-min(x)) -1/2
z_stand<-function(x) (x-mean(x))/sd(x)
ExpectedBrix <- function(x) (x*0.21084778699754 + 4.28455310831511)

#~~~~~~~~~~~~
# Thresholds
#~~~~~~~~~~~
Thresh.Brix.min <- 15
Thresh.Brix.max <- 30

Thresh.Pol.min <- 50
Thresh.Pol.max <- 105
ExpectedBrix.delta <- 1

Thresh.Fibre.min <- 4
Thresh.Fibre.max <- 25
Thresh.Fibre.delta <- .25

Thresh.Ash.min <- 0
Thresh.Ash.max <- 8

#Table2
setwd("~/Documents/JCU/SP1_2020/FoundationsOfDataScience/Assessment3")
Lab_Fibre_Data =read.csv("Lab_Fibre_Weights.csv")
View(data)
#Table3
Fibre1 = 100 * (Lab_Fibre_Data$InitialSampleCanWeight_1 - Lab_Fibre_Data$FinalSampleCanWeight_1) / Lab_Fibre_Data$SampleWeight_1
Lab_Fibre_Data = cbind(Lab_Fibre_Data,Fibre1)
#Table4
Lab_Fibre_Data = mutate(Lab_Fibre_Data,
                        Fibre2 = 100 * (Lab_Fibre_Data$InitialSampleCanWeight_2- Lab_Fibre_Data$FinalSampleCanWeight_2) / Lab_Fibre_Data$SampleWeight_2)
#Table5
Lab_Fibre_Filtered = Lab_Fibre_Data %>%
  filter(SampleWeight_1 >0)%>%
  filter(InitialSampleCanWeight_1 >0)%>%
  filter(FinalSampleCanWeight_1>0)%>%
  filter(SampleWeight_2>0)%>%
  filter(InitialSampleCanWeight_2 >0)%>%
  filter(InitialSampleCanWeight_2 >0)
  
#Tabale6
Lab_Fibre_Filtered = Lab_Fibre_Data %>%
  filter(SampleWeight_1 >0)%>%
  filter(InitialSampleCanWeight_1 >0)%>%
  filter(FinalSampleCanWeight_1>0)%>%
  filter(SampleWeight_2>0)%>%
  filter(InitialSampleCanWeight_2 >0)%>%
  filter(InitialSampleCanWeight_2 >0)%>%
  filter(abs(Fibre1-Fibre2)<0.25)
#Table7
Lab_Fibre_Filtered = mutate(Lab_Fibre_Filtered,
                             Fibre = (Fibre1+Fibre2)/2)
#Table8
Lab_Fibre_Filtered = Lab_Fibre_Filtered %>%
  filter(Fibre>Thresh.Fibre.min) %>%
  filter(Fibre<Thresh.Fibre.max)
  
#Table9
Lab_Fibre = transmute(Lab_Fibre_Filtered,
                      LabID,Fibre)


#Table10
Lab_Ash_Data = read.csv("Lab_Ash_Weights.csv")
summary(Lab_Ash_Data)
#Table11
#a
#b
#c
#d
Lab_Ash_Calculated = Lab_Ash_Data %>%
  filter(TinWeight >0) %>%
  filter(InitialSampleInTinWeight >0) %>%
  filter(FinalSampleInTinWeight>0) %>% 
  mutate(Ash = 100 * ((FinalSampleInTinWeight - TinWeight)/(InitialSampleInTinWeight  - TinWeight)))

#Table12
Lab_Ash_Calculated = Lab_Ash_Data %>%
  filter(TinWeight >0) %>%
  filter(InitialSampleInTinWeight >0) %>%
  filter(FinalSampleInTinWeight>0) %>%
  mutate(Ash = 100 * ((FinalSampleInTinWeight - TinWeight)/(InitialSampleInTinWeight  - TinWeight))) %>%
  filter(Ash > Thresh.Ash.min) %>%
  filter(Ash < Thresh.Ash.max)
summary(Lab_Ash_Calculated)
  
#Table13
Lab_Ash = Lab_Ash_Calculated %>%
  group_by(LabID) %>%
  summarise_at(vars(Ash), funs(mean))

#Table14
Lab_PB_Data = read.csv("Lab_Pol_Brix.csv")
#Table15
Lab_PB_Data = mutate(Lab_PB_Data,
                    PredBrix = ExpectedBrix(x=Pol))
str(Lab_PB_Data)
#Table16
ggplot(Lab_PB_Data, aes(Brix, Pol, color=abs(PredBrix-Brix)>1)) + geom_point(alpha=0.2) +
  geom_smooth(method = "lm") + labs(title = "Scatterplot of Pol and Brix")

#Table17
Lab_PB = Lab_PB_Data %>%
  filter(abs(Brix-PredBrix)<1) %>%
  filter(Pol >= Thresh.Pol.min) %>%
  filter(Pol <= Thresh.Pol.max) %>%
  filter(Brix >= Thresh.Brix.min) %>% 
  filter(Brix <= Thresh.Brix.max) %>%
  transmute(LabID,Pol, Brix)
summary(Lab_PB)

#Table18
Lab <- full_join(Lab_Ash, Lab_Fibre, by=c("LabID" = "LabID"))
#Table19
Lab = full_join(Lab,Lab_PB, by=c("LabID" = "LabID"))
Lab[1:11,]
summary(Lab)
#Table20
write.table(Lab, file = "Lab_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

#Table21
Lab_Fibre = transform(Lab_Fibre,Fibre= z_stand(x=Fibre))
summary(Lab_Fibre)
write.table(Lab_Fibre, file = "Lab_Fibre_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

#Table22
Lab_Ash = Lab_Ash %>%
  mutate(Ash = log10(Ash)) %>%
  mutate(Ash = z_stand(Ash))
write.table(Lab_Ash, file = "Lab_Ash_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")
#Table23
Lab_PB$Bbin <- cut(Lab_PB$Brix, 40, labels = FALSE)
str(Lab_PB)
#Table24
Lab_PB$Bbin <- as.factor(Lab_PB$Bbin)
#Table25
Lab_B_Stratified_Balanced = Lab_PB %>%
    group_by(Bbin) %>%
    sample_n(50, replace = TRUE)
#Table26
Lab_B_Stratified_Balanced = transform(Lab_B_Stratified_Balanced, Brix = rescale_01(x=Brix))
Lab_B_Stratified_Balanced = transmute(Lab_B_Stratified_Balanced,LabID,Brix)
write.table(Lab_B_Stratified_Balanced, file = "Lab_Brix_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

#Table27
#re enter the codes of table#17for Lab_PB
Lab_PB$Bbin <- cut(Lab_PB$Pol, 40, labels = FALSE)
Lab_PB$Bbin <- as.factor(Lab_PB$Bbin)
Lab_P_Stratified_Balanced = Lab_PB %>%
  group_by(Bbin) %>%
  sample_n(50, replace = TRUE)

Lab_P_Stratified_Balanced$Pol=rescale_01(Lab_P_Stratified_Balanced$Pol) #Rescaling variable Pol with rescale_o1
Lab_P_Stratified_Balanced = Lab_P_Stratified_Balanced %>%
  ungroup() %>%   #ungroup to eliminate Bbin during transmute
  transmute(LabID, Pol) #drop other vars except LabID and Pol
write.table(Lab_P_Stratified_Balanced, file = "Lab_Pol_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

#Table28
#~~~~~~~~~~~
# Libraries
#~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(tidyr)

#~~~~~~~~~~~~
# Thresholds
#~~~~~~~~~~~
Thresh.Brix.min <- 15
Thresh.Brix.max <- 30

Thresh.Pol.min <- 50
Thresh.Pol.max <- 105

Thresh.Fibre.min <- 4
Thresh.Fibre.max <- 25

Thresh.Ash.min <- 0
Thresh.Ash.max <- 8

#Table29
setwd("~/Documents/JCU/SP1_2020/FoundationsOfDataScience/Assessment3")
NIRData =read.csv("NIRPred.csv")
summary(NIRData)
NIRData[1:15,]

#Table30
NIRData$DateTime <- as.POSIXct(NIRData$DateTime, format = "%Y-%m-%d %H:%M:%S")
#Table31
LabID = floor(NIRData$ScanID)
NIRData = cbind(NIRData,LabID)
NIRData[1:15,]
#Table32
NIRData_Filtered = NIRData %>%
  filter(GH < 3.5) %>%
  filter(NH < 2) %>%
  filter(NIR_Pol > Thresh.Pol.min) %>%
  filter(NIR_Pol < Thresh.Pol.max) %>%
  filter(NIR_Brix > Thresh.Brix.min) %>%
  filter(NIR_Brix < Thresh.Brix.max) %>%
  filter(NIR_Fibre > Thresh.Fibre.min) %>%
  filter(NIR_Fibre < Thresh.Fibre.max ) %>%
  filter(NIR_Ash >Thresh.Ash.min) %>%
  filter(NIR_Ash < Thresh.Ash.max ) %>%
  filter( ScanID > -1)
summary(NIRData_Filtered)

#Table33
NIR_Final = NIRData_Filtered %>%
  group_by(LabID) %>%
  summarise(DateTime=min(DateTime), 
            NIR_Pol=mean(NIR_Pol),
            NIR_Brix=mean(NIR_Brix),
            NIR_Fibre=mean(NIR_Fibre),
            NIR_Ash=mean(NIR_Ash))
NIR_Final[1:15,]
#Table34
write.table(NIR_Final, file = "Lab_NIR_Final.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

#Table35
NIR_Final = read.csv("NIR_Final.csv")
summary(NIR_Final)

#Table36
par(mfrow = c(2,2))

ggplot(NIR_Final, aes(x=LabID,y=NIR_Pol)) + geom_boxplot()
  
#str(NIR_Final)

ggplot(NIR_Final, aes(NIR_Brix,NIR_Pol)) + geom_point(color="Blue") +
  geom_smooth(method = "lm")
ggplot(NIR_Final, aes(NIR_Brix,NIR_Fibre)) + geom_point(color="Red") +
  geom_smooth(method = "lm")
ggplot(NIR_Final, aes(NIR_Brix,NIR_Ash)) + geom_point(color="Green") +
  geom_smooth(method = "lm")
ggplot(NIR_Final, aes(NIR_Fibre,NIR_Ash)) + geom_point(color="Green") +
  geom_smooth(method = "lm")
ggplot(NIR_Final, aes(NIR_Fibre,NIR_Brix)) + geom_point(color="Purple") +
  geom_smooth(method = "lm")
ggplot(NIR_Final, aes(NIR_Fibre,NIR_Pol)) + geom_point(color="Red") +
  geom_smooth(method = "lm")



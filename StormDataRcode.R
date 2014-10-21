# Peer Assessmen 2
## Reproducable Research

## Data Processing
#Downloading
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="StormData.csv.bz2",method="curl")
StormData<-read.csv("./StormData.csv.bz2", header=T, na.strings = "NA",
            colClasses=c(rep("NULL",6),"character","character",
                         rep("NULL",14),"numeric","numeric","numeric",
                         "character","numeric","character",
                         rep("NULL",9)))

#Fill in missing values with NA
StormData[StormData==""]=NA
StormData[StormData=="?"]=NA

#Only use the complete cases with no missing values
temp1<-complete.cases(StormData)
StormData<-StormData[temp1,]
rm(temp1)

#because many events list a few causes I will attempt to simplify based on what I believe t be the most severe in the list
StormData$myevent<-StormData$EVTYPE
  
StormData$myevent[grep("HURRICANE|TYPHOON|TROPICAL|SPOUT|SURGE",StormData$EVTYPE)]<-"Hurricane"
StormData$myevent[grep("BLIZZARD|WINTER|SLEET|SNOW",StormData$EVTYPE)]<-"Winter Storm"
StormData$myevent[grep("AVALANCHE",StormData$EVTYPE)]<-"Avalanche"
StormData$myevent[grep("TORNADO|FUNNEL|GUSTNADO",StormData$EVTYPE)]<-"Tornado"
StormData$myevent[grep("TSUNAMI|SEICHE",StormData$EVTYPE)]<-"Tsunami"
StormData$myevent[grep("FLOOD|FLD|Flooding",StormData$EVTYPE)]<-"Flood"
StormData$myevent[grep("VOLCANIC",StormData$EVTYPE)]<-"Volcano"
StormData$myevent[grep("HAIL",StormData$EVTYPE)]<-"Hail"
StormData$myevent[grep("THUNDERSTORM|LIGHTNING",StormData$EVTYPE)]<-"Thunderstorm"
StormData$myevent[grep("FIRE|SMOKE",StormData$EVTYPE)]<-"Fire"
StormData$myevent[grep("DUST",StormData$EVTYPE)]<-"Dust Storm"
StormData$myevent[grep("RIP",StormData$EVTYPE)]<-"Rip current"
StormData$myevent[grep("LANDSLIDE",StormData$EVTYPE)]<-"Landslide"
StormData$myevent[grep("WIND|MICROBURST",StormData$EVTYPE)]<-"Wind"
StormData$myevent[grep("SURF|Surf",StormData$EVTYPE)]<-"High Surf"
StormData$myevent[grep("RAIN",StormData$EVTYPE)]<-"Rain"
StormData$myevent[grep("DROUGHT",StormData$EVTYPE)]<-"Drought"
StormData$myevent[grep("ICE|ICY",StormData$EVTYPE)]<-"Ice"
StormData$myevent[grep("TIDE",StormData$EVTYPE)]<-"Tide"
StormData$myevent[grep("FOG",StormData$EVTYPE)]<-"Fog"
StormData$myevent[grep("HEAT",StormData$EVTYPE)]<-"Heat"
StormData$myevent[grep("FREEZE|COLD|Frost",StormData$EVTYPE)]<-"Cold"


#change the event types into factor variables
StormData$myevent<-as.factor(StormData$myevent)
#split the data set based on the event types
temp<-split(StormData,StormData$myevent)

#find the event that creates the maximum fatalities and injuries
booboos_mean<-sapply(temp,function(x) colMeans(x[c("INJURIES")]))
death_mean<-sapply(temp,function(x) colMeans(x[c("FATALITIES")]))

booboos_sum<-sapply(temp,function(x) colSums(x[c("INJURIES")]))
death_sum<-sapply(temp,function(x) colSums(x[c("FATALITIES")]))

which.max(booboos_mean)
which.max(booboos_sum)
which.max(death_mean)
which.max(death_sum)
#############################
#reformat for plotting sum data
#make a data frame for injuries and fatalities. 
dfIN<-data.frame(booboos_sum,rep("injuries",22))
dfFT<-data.frame(death_sum,rep("fatalities",22))
row.names(dfIN)<-gsub(".INJURIES","",row.names(dfIN))
row.names(dfFT)<-gsub(".FATALITIES","",row.names(dfFT))
dfIN<-cbind(dfIN,row.names(dfIN))
dfFT<-cbind(dfFT,row.names(dfFT))
#label the columns
names(dfIN)<-c("Total","Type","Event")
names(dfFT)<-c("Total","Type","Event")

#combine the two data frames
GrandTotal<-rbind(dfIN,dfFT)

#take out the top 7 weather events
top7_sum<-GrandTotal[c("Heat","Heat1","Flood","Flood1","Wind","Wind1","Hurricane","Hurricane1","Tornado","Tornado1","Ice","Ice1","Tsunami","Tsunami1"),]

#plot it
library(ggplot2)
qplot(Event,Total,data=top7_sum,fill=Type,xlab="Weather Event",ylab="Total number of people",main="Total number of people injured or killed by top 7 weather events")+geom_histogram(stat = "identity")
#################################
#reformat for plotting mean data
#make a data frame for injuries and fatalities. 
dfINJ<-data.frame(booboos_mean,rep("injuries",22))
dfFAT<-data.frame(death_mean,rep("fatalities",22))
row.names(dfINJ)<-gsub(".INJURIES","",row.names(dfINJ))
row.names(dfFAT)<-gsub(".FATALITIES","",row.names(dfFAT))
dfINJ<-cbind(dfINJ,row.names(dfINJ))
dfFAT<-cbind(dfFAT,row.names(dfFAT))
#label the columns
names(dfINJ)<-c("Total","Type","Event")
names(dfFAT)<-c("Total","Type","Event")

#combine the two data frames
MeanTotal<-rbind(dfINJ,dfFAT)

#take out the top 7 weather events
top7_mean<-MeanTotal[c("Heat","Heat1","Flood","Flood1","Wind","Wind1","Hurricane","Hurricane1","Tornado","Tornado1","Ice","Ice1","Tsunami","Tsunami1"),]

#plot it
library(ggplot2)
qplot(Event,Total,data=top7_mean,fill=Type,xlab="Weather Event",ylab="Total number of people",main="Mean number of people injured or killed by top 7 weather events")+geom_histogram(stat = "identity")
###############################
#How many tornado and tsunami events per state
StormData$STATE<-as.factor(StormData$STATE)
temp2<-split(StormData,StormData$STATE)
tornado<-sapply(temp2, function(x) sum(x == "Tornado"))
tsunami<-sapply(temp2, function(x) sum(x == "Tsunami"))
#List it in a table
as.table(tornado)
as.table(tsunami)

###################################
#Total up the amount of property damage due to different weather events
StormData$property<-StormData$PROPDMGEXP
StormData$crop<-StormData$CROPDMGEXP
#take into account the modifier
StormData$property[grep("B|b",StormData$PROPDMGEXP)]<-c(1000000000)
StormData$property[grep("M|m",StormData$PROPDMGEXP)]<-c(1000000)
StormData$property[grep("K|k",StormData$PROPDMGEXP)]<-c(1000)
#multiply by the sig figs
StormData$property<-as.numeric(StormData$property) * StormData$PROPDMG
#Now do crop damage
#take into account the modifier
StormData$crop[grep("B|b",StormData$CROPDMGEXP)]<-c(1000000000)
StormData$crop[grep("M|m",StormData$CROPDMGEXP)]<-c(1000000)
StormData$crop[grep("K|k",StormData$CROPDMGEXP)]<-c(1000)
#multiply by the sig figs
StormData$crop<-as.numeric(StormData$crop) * StormData$CROPDMG

temp<-split(StormData,StormData$myevent)
#Sum the total damages due to different weather types
CropDamage<-sapply(temp,function(x) colSums(x[c("crop")]))
PropertyDamage<-sapply(temp,function(x) colSums(x[c("property")]))

which.max(CropDamage)
which.max(PropertyDamage)

#reformat for plotting
#make a data frame for crop and property damage. 
dfCD<-data.frame(CropDamage,rep("crop",22))
dfPD<-data.frame(PropertyDamage,rep("property",22))
row.names(dfCD)<-gsub(".crop","",row.names(dfCD))
row.names(dfPD)<-gsub(".property","",row.names(dfPD))
dfCD<-cbind(dfCD,row.names(dfCD))
dfPD<-cbind(dfPD,row.names(dfPD))
#label the columns
names(dfCD)<-c("Damages","Type","Event")
names(dfPD)<-c("Damages","Type","Event")

#combine the two data frames
total<-rbind(dfCD,dfPD)

#take out the top 5 weather events
top5<-total[c("Hail","Hail1","Flood","Flood1","Wind","Wind1","Hurricane","Hurricane1","Tornado","Tornado1"),]

#plot it
library(ggplot2)
qplot(Event,Damages,data=top5,fill=Type,xlab="Weather Event",ylab="Damages ($)",main="Total damages caused by top 5 weather events")+geom_histogram(stat = "identity")

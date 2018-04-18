#Load Data to work with - 812 Project Files
Setwd("~/812-Project Files")
getwd()
Bodydata = read.csv(Bodylenghtdata_Patterson_812.csv)

#Set smaller name - Easier to work with 
BD = Bodylenghtdata_Patterson_812.csv

#Regular Expression to remove input errors - Trim out all input errors in site locations between the 6 samples dates 
# 1) fix F -> f (Rank)
BDRank <- gsub("F","f",BD$Rank)
#or use
BD2 <- stringr::str_replace_all(BD$Rank, "F", "f")

# 2) fix "ei" -> "ie" (Rank)
BDRank=gsub("ei","ie",BDRank)

# 3) fix "close" -> "near" (Rank)
BDRank=gsub("Close","Near",BDRank)

# 4) fix dates "Day 25" -> "25"
BDDay=gsub("Day ","",BD$Day)

#Use dplyr to convert to one table with fixed values

#First convert day to numeric value
BDDAY1=as.numeric(BDDay)
#convert Treatment1 to Factor
Treatment1=as.factor(BD$Treatment)
#convert Replicate1 to factor
Replicate1=as.factor(BD$Replicate)

#cbind to form new data set
#All
FixedData=cbind(BDRank,BD,Replicate1,Treatment1,BDDAY1)
#Just Treatment
FixedData2=cbind(Treatment1,BDDAY1,BD)
#Just Replicate
FixedData3=cbind(Replicate1,BDDAY1,BD)
#Remove uneeded coloumns and make new table useing dpylr
#All + Adjusted Rank
BL=dplyr::select(FixedData,BDRank,Treatment1,Replicate1,Length,BDDAY1)
#Adjusted Treatment
BL2=dplyr::select(FixedData2,Treatment1,Length,BDDAY1)
#Adjusted Replicate
BL3=dplyr::select(FixedData3,Replicate1,Length,BDDAY1)

# Using dplyr to wrangle data by Rank, Replicate and by Treatment
#Rank by Day
library(dplyr)
AvRank=BL%>%
  group_by(BDRank,BDDAY1)%>%
  summarise(avg = mean(Length))%>%
  arrange(avg)
#Treatment by Day
library(dplyr)
AvTreatment=BL2%>%
  group_by(Treatment1,BDDAY1)%>%
  summarise(avg = mean(Length))%>%
  arrange(avg)
AvTreatment
#Replicate by Day
library(dplyr)
AvReplicate=BL3%>%
  group_by(Replicate1,BDDAY1)%>%
  summarise(avg = mean(Length))%>%
  arrange(avg)

#Inital Plot for visulaitzation of growth
library(ggplot2)
#Basic Plot - Treatment
p1<-ggplot(data=AvTreatment)+geom_point(aes(x=BDDAY1,y=avg),color="red",size=3)
p1

#Basic Plot Replicate
p1<-ggplot(data=AvReplicate)+geom_point(aes(x=BDDAY1,y=avg),color="red",size=3)
p1


#Subset all days but 70 and 77
BB=dplyr::filter(AvRank,avg<3.9)
#Subset into the three ranks 
Near=dplyr::filter(BB,BDRank=="Near-field")
Far=dplyr::filter(BB,BDRank=="far-field")
Control=dplyr::filter(BB,BDRank=="Control")

#Nearfeild plot
pNear <- ggplot(data=Near) + geom_point(aes(x=BDDAY1,y=avg), color="black", size=3) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
pNear

#Near feild linear model
#no intercpt model
lmNear0 <- lm(avg~ -1 + as.numeric(BDDAY1), data=Near)
summary(lmNear0)
coef(lmNear0)
#with intercept model
lmNear <- lm(avg~ as.numeric(BDDAY1), data=Near)
summary(lmNear)
#plot with model
predicted_df <- data.frame(avg_Predict = predict(lmNear, Near))
pNear+geom_line(color='red',size=1.5,data = predicted_df, aes(x=Near$BDDAY1, y=avg_Predict)) + labs(x="Experimental Day") + labs(y="Average Length (cm) - Near-feild")
#confidence intervals & diagnostic plots
confint(lmNear)
plot(lmNear)


#Farfeild plot
pFar <- ggplot(data=Far) + geom_point(aes(x=BDDAY1,y=avg), color="black", size=3) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
pFar

#Far feild linear model
#no intercept model
lmFar0 <- lm(avg~ -1 + as.numeric(BDDAY1), data=Far)
summary(lmFar0)
coef(lmFar0)
#with intercept model
lmFar <- lm(avg~as.numeric(BDDAY1), data=Far)
summary(lmFar)

#plot with model
predicted_far <- data.frame(avg1_Predict = predict(lmFar, Far))
pFar+geom_line(color='red',size=1.5,data = predicted_far, aes(x=Far$BDDAY1, y=avg1_Predict)) + labs(x="Experimental Day") + labs(y="Average Length (cm) - Far-feild")
#confidence intervals & diagnostic plots
confint(lmFar)
plot(lmFar)
coef(lmFar)
lm()

#Control plot
pControl <- ggplot(data=Control) + geom_point(aes(x=Control$BDDAY1,y=Control$avg), color="black", size=3) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
pControl

#Control linear model
#no intercept model
lmControl0 <- lm(Control$avg~ -1 + as.numeric(Control$BDDAY1), data=Control)
summary(lmControl0)
coef(lmControl0)
#with int. model
lmControl <- lm(Control$avg~as.numeric(Control$BDDAY1), data=Control)
summary(lmControl)
#plot with model
predicted_Control <- data.frame(avg2_Predict = predict(lmControl, Control))
pControl+geom_line(color='red',size=1.5,data = predicted_Control, aes(x=Control$BDDAY1, y=avg2_Predict)) + labs(x="Experimental Day") + labs(y="Average Length (cm) - Control")
#confidence intervals & diagnostic plots
confint(lmControl)
plot(lmControl)
coef(lmControl)
lm()


#Polynomial Growth Curve
lm3 <- lm(AvTreatment$avg ~  -1 + AvTreatment$BDDAY1 + I(AvTreatment$BDDAY1^2) + I(AvTreatment$BDDAY1^3))
summary(lm3)
vif(lm3)
coef(lm3)

#Model AvTreatment
pAvTreatment <- ggplot(data=AvTreatment) + geom_point(aes(x=AvTreatment$BDDAY1,y=AvTreatment$avg), color="red", size=3) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
pAvTreatment

#Model Polynomial Growth
predicted_growth<-data.frame(avg3_Predict = predict(lm3,AvTreatment))
pAvTreatment+geom_line(color='black',size=1.5,data = predicted_growth, aes(x=AvTreatment$BDDAY1, y=avg3_Predict)) + labs(x="Experimental Day") + labs(y="Average Length (cm) - All")

#Get coefs. of the 3 models
coef(lmNear)
coef(lmFar)
coef(lmControl)

#Boxplot by Rank
Data<- read.table(textConnection(Input),header=TRUE)
Data$Treatment = as.factor(Data$Treatment)
library(ggplot2)
#Boxplot by Rank
Plot_Rank_ZScore<-ggplot(Data, aes(x = Rank, y = Length, fill=Rank))
BodyLength<-Plot_Rank_ZScore+geom_boxplot()+theme(plot.background=element_blank(),
                                                  panel.grid.major=element_blank(),
                                                  panel.grid.minor=element_blank(),
                                                  panel.border=element_blank(),
                                                  panel.background=element_blank(),
                                                  axis.line=element_line(colour="black"),
                                                  axis.title.x=element_text(size=20,colour="black"),
                                                  axis.title.y=element_text(size=20,colour="black"),
                                                  axis.text.x=element_text(size=10,colour="black"),
                                                  axis.text.y=element_text(size=10,colour="black"),
                                                  legend.text=element_text(size=12),
                                                  legend.title=element_text(size=15),
                                                  legend.background=element_rect(color="black",size=.5, linetype="solid"),legend.position=c(.88,.85))+ labs(x="Experimental Rank") + labs(y="Body Length (cm)")
BodyLength

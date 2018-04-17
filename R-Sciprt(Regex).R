#Load Data to work with
Setwd("~/812-Project Files")
getwd()
Bodydata = read.csv(BodyData.csv)

#Set smaller name
BD = Bodylenghtdata_Patterson_812

#Regular Expression to remove input errors
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
#convert day to numeric value
BDDAY1=as.numeric(BDDay)
#onvert treatment to Factor
Treatment1=
as.factor(BD$Treatment)
#convert replicate to factor
Replicate1=as.factor(BD$Replicate)

#cbind to form new data set
FixedData=cbind(BDRank,BD,Replicate1,Treatment1,BDDAY1)
FixedData2=cbind(Treatment1,BDDAY1,BD)
FixedData3=cbind(Replicate1,BDDAY1,BD)
#remove uneeded coloumns and make new table useing dpylr
BL=
dplyr::select(FixedData,BDRank,Treatment1,Replicate1,Length,BDDAY1)
BL2=
  dplyr::select(FixedData2,Treatment1,Length,BDDAY1)
BL3=
  dplyr::select(FixedData3,Replicate1,Length,BDDAY1)

# Using dplyr to wrangle data by Rank and by Treatment
library(dplyr)
AvRank=BL%>%
  group_by(BDRank,BDDAY1)%>%
  summarise(avg = mean(Length))%>%
  arrange(avg)

library(dplyr)
AvTreatment=BL2%>%
  group_by(Treatment1,BDDAY1)%>%
  summarise(avg = mean(Length))%>%
  arrange(avg)
AvTreatment

library(dplyr)
AvReplicate=BL3%>%
  group_by(Replicate1,BDDAY1)%>%
  summarise(avg = mean(Length))%>%
  arrange(avg)

library(ggplot2)

p1<-ggplot(data=AvTreatment)+geom_point(aes(x=BDDAY1,y=avg),color="red",size=3)
p1


p1<-ggplot(data=AvReplicate)+geom_point(aes(x=BDDAY1,y=avg),color="red",size=3)
p1


#subset all but 70 and 77
BB=dplyr::filter(AvRank,avg<3.9)
Near=dplyr::filter(BB,BDRank=="Near-field")
Far=dplyr::filter(BB,BDRank=="far-field")
Control=dplyr::filter(BB,BDRank=="Control")

#Nearfeild plot
pNear <- ggplot(data=Near) + geom_point(aes(x=BDDAY1,y=avg), color="black", size=3) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
pNear

#Near feild linear model
#no int
lmNear0 <- lm(avg~ -1 + as.numeric(BDDAY1), data=Near)
summary(lmNear0)
coef(lmNear0)
#with int
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
#no int
lmFar0 <- lm(avg~ -1 + as.numeric(BDDAY1), data=Far)
summary(lmFar0)
coef(lmFar0)
#with int
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
#no int
lmControl0 <- lm(Control$avg~ -1 + as.numeric(Control$BDDAY1), data=Control)
summary(lmControl0)
coef(lmControl0)
#with int.
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

#Boxplot by Rank z-score
Input=("Treatment	Rank	Replicate	Length
O	Control	O1	3.818
       O	Control	O1	3.403
       O	Control	O1	3.326
       O	Control	O1	3.461
       O	Control	O1	3.136
       O	Control	O1	3.762
       O	Control	O1	3.666
       O	Control	O1	3.29
       O	Control	O1	3.703
       O	Control	O1	3.543
       O	Control	O2	3.736
       O	Control	O2	3.726
       O	Control	O2	3.754
       O	Control	O2	3.718
       O	Control	O2	3.523
       O	Control	O2	3.348
       O	Control	O2	3.378
       O	Control	O2	3.527
       O	Control	O2	2.814
       O	Control	O2	3.704
       O	Control	O3	3.577
       O	Control	O3	3.813
       O	Control	O3	3.513
       O	Control	O3	3.094
       O	Control	O3	3.044
       O	Control	O3	3.526
       O	Control	O3	3.724
       O	Control	O3	3.413
       O	Control	O3	3.551
       O	Control	O3	3.554
       N	Control	N1	3.709
       N	Control	N1	3.565
       N	Control	N1	3.215
       N	Control	N1	3.441
       N	Control	N1	3.33
       N	Control	N1	3.447
       N	Control	N1	3.252
       N	Control	N1	3.582
       N	Control	N1	3.697
       N	Control	N1	3.516
       N	Control	N2	3.63
       N	Control	N2	3.604
       N	Control	N2	4.021
       N	Control	N2	4.229
       N	Control	N2	3.363
       N	Control	N2	3.834
       N	Control	N2	3.686
       N	Control	N2	3.618
       N	Control	N2	3.743
       N	Control	N2	3.674
       N	Control	N3	3.702
       N	Control	N3	2.754
       N	Control	N3	3.469
       N	Control	N3	3.331
       N	Control	N3	3.974
       N	Control	N3	3.923
       N	Control	N3	3.208
       N	Control	N3	3.501
       N	Control	N3	3.402
       N	Control	N3	3.425
       A	Close-field	A1	3.113
       A	Close-field	A1	3.073
       A	Close-field	A1	3.536
       A	Close-field	A1	3.312
       A	Close-field	A1	3.436
       A	Close-field	A1	3.632
       A	Close-field	A1	3.647
       A	Close-field	A1	2.821
       A	Close-field	A1	3.475
       A	Close-field	A1	3.161
       A	Close-field	A2	2.838
       A	Close-field	A2	3.174
       A	Close-field	A2	3.735
       A	Close-field	A2	3.234
       A	Close-field	A2	3.346
       A	Close-field	A2	2.85
       A	Close-field	A2	3.828
       A	Close-field	A2	3.434
       A	Close-field	A2	3.217
       A	Close-field	A2	3.559
       A	Close-field	A3	3.645
       A	Close-field	A3	3.58
       A	Close-field	A3	3.617
       A	Close-field	A3	3.014
       A	Close-field	A3	3.347
       A	Close-field	A3	3.735
       A	Close-field	A3	3.397
       A	Close-field	A3	3.695
       A	Close-field	A3	3.568
       A	Close-field	A3	2.664
       B	Close-field	B1	2.774
       B	Close-field	B1	3.144
       B	Close-field	B1	3.055
       B	Close-field	B1	3.592
       B	Close-field	B1	3.472
       B	Close-field	B1	3.235
       B	Close-field	B1	3.31
       B	Close-field	B1	3.475
       B	Close-field	B1	3.334
       B	Close-field	B1	3.296
       B	Close-field	B2	3.817
       B	Close-field	B2	3.538
       B	Close-field	B2	3.331
       B	Close-field	B2	3.146
       B	Close-field	B2	3.047
       B	Close-field	B2	3.594
       B	Close-field	B2	3.308
       B	Close-field	B2	3.535
       B	Close-field	B2	3.404
       B	Close-field	B2	3.454
       B	Close-field	B3	3.454
       B	Close-field	B3	3.566
       B	Close-field	B3	3.612
       B	Close-field	B3	3.375
       B	Close-field	B3	3.541
       B	Close-field	B3	3.54
       B	Close-field	B3	3.429
       B	Close-field	B3	3.451
       B	Close-field	B3	3.688
       B	Close-field	B3	3.263
       C	Close-field	C1	2.818
       C	Close-field	C1	2.918
       C	Close-field	C1	2.73
       C	Close-field	C1	2.706
       C	Close-field	C1	2.926
       C	Close-field	C1	2.858
       C	Close-field	C1	2.923
       C	Close-field	C1	2.786
       C	Close-field	C1	2.743
       C	Close-field	C1	2.8
       C	Close-field	C3	3.676
       C	Close-field	C3	3.494
       C	Close-field	C3	3.13
       C	Close-field	C3	3.617
       C	Close-field	C3	3.481
       C	Close-field	C3	2.825
       C	Close-field	C3	4.034
       C	Close-field	C3	3.024
       C	Close-field	C3	3.432
       C	Close-field	C3	3.213
       C	Close-field	C2	2.812
       C	Close-field	C2	3.2
       C	Close-field	C2	2.972
       C	Close-field	C2	3.701
       C	Close-field	C2	3.541
       C	Close-field	C2	2.715
       C	Close-field	C2	3.203
       C	Close-field	C2	3.506
       C	Close-field	C2	3.433
       C	Far-field	C2	3.347
       D	Far-field	D1	3.494
       D	Far-field	D1	3.783
       D	Far-field	D1	3.841
       D	Far-field	D1	3.594
       D	Far-field	D1	3.648
       D	Far-field	D1	3.665
       D	Far-field	D1	3.483
       D	Far-field	D1	3.667
       D	Far-field	D1	3.717
       D	Far-field	D1	3.625
       D	Far-field	D2	3.705
       D	Far-field	D2	3.456
       D	Far-field	D2	3.417
       D	Far-field	D2	3.787
       D	Far-field	D2	2.614
       D	Far-field	D2	3.443
       D	Far-field	D2	3.603
       D	Far-field	D2	3.488
       D	Far-field	D2	3.831
       D	Far-field	D2	3.717
       D	Far-field	D3	3.489
       D	Far-field	D3	4.063
       D	Far-field	D3	3.441
       D	Far-field	D3	3.837
       D	Far-field	D3	4.184
       D	Far-field	D3	4.08
       D	Far-field	D3	3.726
       D	Far-field	D3	2.406
       D	Far-field	D3	3.766
       D	Far-field	D3	3.866
       E	Far-field	E1	3.453
       E	Far-field	E1	3.246
       E	Far-field	E1	3.543
       E	Far-field	E1	3.681
       E	Far-field	E1	3.033
       E	Far-field	E1	3.334
       E	Far-field	E1	3.466
       E	Far-field	E1	3.556
       E	Far-field	E1	3.413
       E	Far-field	E1	3.651
       E	Far-field	E2	3.775
       E	Far-field	E2	3.91
       E	Far-field	E2	3.891
       E	Far-field	E2	3.627
       E	Far-field	E2	3.514
       E	Far-field	E2	3.583
       E	Far-field	E2	3.574
       E	Far-field	E2	3.796
       E	Far-field	E2	3.795
       E	Far-field	E2	3.394
       E	Far-field	E3	3.324
       E	Far-field	E3	3.337
       E	Far-field	E3	3.087
       E	Far-field	E3	3.226
       E	Far-field	E3	3.087
       E	Far-field	E3	3.526
       E	Far-field	E3	3.343
       E	Far-field	E3	3.569
       E	Far-field	E3	3.254
       E	Far-field	E3	3.209
       F	Far-field	F1	3.673
       F	Far-field	F1	3.27
       F	Far-field	F1	3.419
       F	Far-field	F1	3.613
       F	Far-field	F1	3.373
       F	Far-field	F1	3.54
       F	Far-field	F1	3.541
       F	Far-field	F1	3.407
       F	Far-field	F1	3.521
       F	Far-field	F1	3.667
       F	Far-field	F2	3.481
       F	Far-field	F2	3.629
       F	Far-field	F2	3.822
       F	Far-field	F2	3.838
       F	Far-field	F2	3.716
       F	Far-field	F2	3.594
       F	Far-field	F2	3.791
       F	Far-field	F2	3.523
       F	Far-field	F2	3.561
       F	Far-field	F2	4.169
       F	Far-field	F3	3.423
       F	Far-field	F3	3.056
       F	Far-field	F3	3.384
       F	Far-field	F3	3.426
       F	Far-field	F3	3.442
       F	Far-field	F3	3.555
       F	Far-field	F3	3.144
       F	Far-field	F3	3.373
       F	Far-field	F3	3.335
       F	Far-field	F3	3.461
                            ")
Data<- read.table(textConnection(Input),header=TRUE)
Data$Treatment = as.factor(Data$Treatment)
library(ggplot2)
#Boxplot by Rank z-score
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

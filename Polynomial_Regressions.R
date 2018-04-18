#This R file was used as model calbration and selction for an overall growth curve
#Polynomial regressions where fit to all six sample dates - while growth rates are calulated on only 3 dates (25,42,55)
library(ggplot2)
#pd<-position_dodge(width=10)
theme_set(theme_bw())
#inital Plot 
pl <- ggplot(Near,aes(x=BDDAY1, y=avg)) +
               geom_point(aes(x=BDDAY1, y=avg), size=2, colour="#993399") +
  xlab("Experimental Day") + ylab("Body Length (cm)") +
  geom_smooth(method="lm", se=F)
print(pl)


#Polynomial 0 Model - test model 
attach(BB)
lm0 <- lm(avg ~ 1)
lm0
mean(avg)
pl + geom_hline(yintercept=coef(lm0)[1],size=1, colour="#339900")

#Polynomial of a 1st degree + test model 
lm1 <- lm(avg ~ BDDAY1, data=BB)
coef(lm1)
summary(lm1)
confint(lm1)
#Diagnostic Plots - test model 
pl  + geom_abline(intercept=coef(lm1)[1],slope=coef(lm1)[2],size=1, colour="#339900")
par(mfrow = c(1, 2))
plot(lm1, which=c(1,2))

#predictive power of model 1 - Model diagnostic plots and testing with extreme values 
set.seed(100)
n <- nrow(A)
i.training <- sort(sample(n,round(n*0.8)))
A.training <- A[i.training,]
A.test  <- A[-i.training,]
pred1a.test <- predict(lm1, newdata=A.test)
lm1.training <- lm(Length ~ Day, data=A.training)
pred1b.test <- predict(lm1.training, newdata=A.test)
data.frame(A.test, pred1a.test, pred1b.test)
Length.test <- A.test$Length
par(mfrow=c(1,2))
plot(pred1b.test, Length.test)
abline(a=0, b=1, lty=2)
plot(pred1b.test, pred1a.test)
abline(a=0, b=1, lty=2)
cor.test <- cor(pred1a.test, Length.test)
R2.test <- cor.test^2
R2.test

#exterme values test
i.training <- c(6:200)
A.training <- c[i.training,]
A.test  <- A[-i.training,]
Length.test <- A.test$Length
pred1a.test <- predict(lm1, newdata=A.test)
lm1.training <- lm(Length ~ Day, data=A.training)
pred1b.test <- predict(lm1.training, newdata=A.test)
par(mfrow=c(1,2))
plot(pred1b.test, Length.test)
abline(a=0, b=1, lty=2)
plot(pred1b.test, pred1a.test)
abline(a=0, b=1, lty=2)


#confidence interval and prediction interval
alpha <- 0.05
df.new <- data.frame(Day=(40:82))
conf.Length <- predict(lm1, newdata = df.new, interval="confidence", level=1-alpha)
head(conf.Length)
#prediction test of new measure
pred.Length <- predict(lm1, newdata = df.new, interval="prediction", level=1-alpha)
head(pred.Length)
#Interval Plot
df.new[c("fit","lwr.conf", "upr.conf")] <- conf.Length
df.new[c("lwr.pred", "upr.pred")] <- pred.Length[,2:3]
pl +
  geom_ribbon(data=df.new, aes(x=Day, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") +
  geom_ribbon(data=df.new, aes(x=Day, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +
  geom_line(data=df.new, aes(x=Day, y=fit), colour="#339900", size=1)



#second degree Polynomial
#Center Terms
BB$BDDAY1<-BB$BDDAY1-mean(BB$BDDAY1)
lm2 <- lm(avg ~  BB$BDDAY1 + I(BB$BDDAY1^2))
lm2c <- lm(avg ~ BDDAY1 + I(cBB$BDDAY1^2), data=BB)
summary(lm2)
library(car)
vif(lm2)
#Diagnostic Plots
pl  + geom_abline(intercept=coef(lm2)[1],slope=coef(lm2)[2],size=1, colour="#339900")
par(mfrow = c(1, 2))
plot(lm2c, which=c(1,2))


f <- function(x,c) coef(lm2)[1] + coef(lm2)[2]*x + coef(lm2)[3]*x^2
#f <- function(x,c) coef(lm2)[1]*x + coef(lm2)[2]*x^2
pl  + geom_abline(intercept=coef(lm1)[1],slope=coef(lm1)[2],size=1, colour="#339900") +
  stat_function(fun=f, colour="red", size=1)

#third degree Polynomial
lm3 <- lm(BB$avg ~  BB$BDDAY1 + I(BB$BDDAY1^2) + I(BB$BDDAY1^3))
lm3c <- lm(Length ~ -1 + Day + I(cDay^2) + I(cDay^3), data=A)
summary(lm3)
vif(lm3)
coef(lm3)

f <- function(x,c) coef(lm2)[1] + coef(lm2)[2]*x + coef(lm2)[3]*x^2 + coef(lm3)[4]*x^3
#f <- function(x,c) coef(lm2)[1]*x + coef(lm2)[2]*x^2
pl  + geom_abline(intercept=coef(lm1)[1],slope=coef(lm1)[2],size=1, colour="#339900") +
  stat_function(fun=f, colour="red", size=1)
#Diagnostics
pl  + geom_abline(intercept=coef(lm3)[1],slope=coef(lm3)[2],size=1, colour="#339900")
par(mfrow = c(1, 2))
plot(lm3, which=c(1,2))


#Fitting polynomial without intercept - Model 2
lm2.noint <- lm(Length ~ -1 + Day + I(Day^2))
coef(lm2.noint)
X <- model.matrix(lm2.noint)
head(X)
df.new <- data.frame(Day=(42:82))
#plot without intercept
conf.Length <- predict(lm2.noint, newdata = df.new, interval="confidence", level=1-alpha)
pred.Length <- predict(lm2.noint, newdata = df.new, interval="prediction", level=1-alpha)
df.new[c("fit","lwr.conf", "upr.conf")] <- conf.Length
df.new[c("lwr.pred", "upr.pred")] <- pred.Length[,2:3]
pl +
  geom_ribbon(data=df.new, aes(x=Day, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") +
  geom_ribbon(data=df.new, aes(x=Day, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +
  geom_line(data=df.new, aes(x=Day, y=fit), colour="#339900", size=1)

#Fitting polynomial without intercept - Model 3
lm3.noint <- lm(BB$avg ~ -1 + BB$BDDAY1 + I(BB$BDDAY1^2) + I(BB$BDDAY1^3))
summary(lm3.noint)
coef(lm3.noint)
X2 <- model.matrix(lm3.noint)
head(X)
df.new <- data.frame(Day=(14:100))
#plot without intercept
conf.Length <- predict(lm3.noint, newdata = df.new, interval="confidence", level=1-alpha)
pred.Length <- predict(lm3.noint, newdata = df.new, interval="prediction", level=1-alpha)
df.new[c("fit","lwr.conf", "upr.conf")] <- conf.Length
df.new[c("lwr.pred", "upr.pred")] <- pred.Length[,2:3]
pl +
  geom_ribbon(data=df.new, aes(x=Day, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") +
  geom_ribbon(data=df.new, aes(x=Day, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +
  geom_line(data=df.new, aes(x=Day, y=fit), colour="#339900", size=1)

#Information Criteria
AIC(lm0,lm1,lm2,lm3,lm3.noint,lm2.noint,lm2c)
BIC(lm0,lm1,lm2,lm3,lm3.noint,lm2.noint)


#Model Information
summary.lm(lm3.noint)
#Model Comparison
anova(lm3.noint,lm3)

#Data Transformation
#try ploting with a log transformation just for fun - could be linear
library(gridExtra)
pl1 <- pl + scale_x_log10()
pl2 <- pl + scale_y_log10()
grid.arrange(pl1, pl2, nrow=1)
print(pl + scale_x_log10() + scale_y_log10() )
lm1.log <- lm((Length) ~ log(Day
                                ))
summary(lm1.log)

par(mfrow=c(1,2))
plot(log(Day),log(Length))
abline(a=coef(lm1.log)[1], b=coef(lm1.log)[2], col="#339900")
plot(lm1.log, which=1)

alpha <- 0.05
df.new <- data.frame(Day=(14:82))
conf.Length <- predict(lm1.log, newdata = df.new, interval="confidence", level=1-alpha)
pred.Length <- predict(lm1.log, newdata = df.new, interval="prediction", level=1-alpha)

#Graphs for log trasnformed 
df.new[c("fit","lwr.conf", "upr.conf")] <- conf.Length
df.new[c("lwr.pred", "upr.pred")] <- pred.Length[,2:3]
ggplot(A) + geom_point(aes(x=log(Day), y=(Length)), size=2, colour="#993399") +
  geom_ribbon(data=df.new, aes(x=log(Day), ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") +
  geom_ribbon(data=df.new, aes(x=log(Day), ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +
  geom_line(data=df.new, aes(x=log(Day), y=fit), colour="#339900", size=1) +
  xlab("log-Day") + ylab("log-Length (cm)")



#Exponetial values plot 
ggplot(A) + geom_point(aes(x=Day, y=exp(Length), size=2, colour="#993399")) +
  geom_ribbon(data=df.new, aes(x=Day, ymin=exp(lwr.pred), ymax=exp(upr.pred)), alpha=0.1, inherit.aes=F, fill="blue") +
  geom_ribbon(data=df.new, aes(x=Day, ymin=exp(lwr.conf), ymax=exp(upr.conf)), alpha=0.2, inherit.aes=F, fill="#339900") +
  geom_line(data=df.new, aes(x=Day, y=exp(fit)), colour="#339900", size=1) +
  xlab("speed (mph)") + ylab("stopping distance (ft)")


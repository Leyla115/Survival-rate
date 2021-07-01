library(dplyr)
library(ggplot2)
library(stargazer)
library(lubridate)
library(tidyr)
library(foreign)

dff <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/recid.dta")
dff$crime=(!dff$cens)*1

library(survival)

#generate survival function for supervision

kmsurvivalh1 <- survfit(Surv(durat, crime) ~ super, data = dff)
summary(kmsurvivalh1)
plot(kmsurvivalh1)
survdiff(Surv(durat, crime) ~ super, data = dff)
#We can canclude that who supervised released from prison (supervisiosn) has higher survival rate than not supervision

#The log-rank test (or Mantel-Haenszel) 
# H0: survival curves are not different (there is no difference between survival rates)
# H1: survival curves are different. 

#We do not reject NH that survival rates for supervision is not different

#generate survival function for drug history
kmsurvivalh11 <- survfit(Surv(durat, crime) ~ drugs, data = dff)
summary(kmsurvivalh11)
plot(kmsurvivalh11)
survdiff(Surv(durat, crime) ~ drugs, data = dff)
f# H0: survival curves are not different (there is no difference between survival rates)
# H1: survival curves are different. 


#Yes drug histopry makes things worse, cause who has drug history has lower survival rate 

# 3. Calculate and draw hazard rate by variable super and drugs
library(muhaz)
kmhazard2 <- kphaz.fit(dff$durat, dff$crime, strata = dff$super)

tempdf <- data.frame(kmhazard2$time, kmhazard2$haz, kmhazard2$strata)

ggplot(data = tempdf, aes(x=kmhazard2.time, y = kmhazard2.haz, color = as.factor(kmhazard2.strata))) +  geom_step()+  
  geom_smooth(se = FALSE) + labs(x="time", y = "hazard rate")

#Hazard rate decline over time, probability to commite a crime decrease
#we can see that people who do not use supervision have more hazerd rate
#So who have supervision have less probability to comite a crime (Hazard rate is lower )

kmhazard22 <- kphaz.fit(dff$durat, dff$crime, strata = dff$drugs)

tempdf <- data.frame(kmhazard22$time, kmhazard22$haz, kmhazard22$strata)

ggplot(data = tempdf, aes(x=kmhazard22.time, y = kmhazard22.haz, color = as.factor(kmhazard22.strata))) +  
  geom_step()+   geom_smooth(se = FALSE) + labs(x="time", y = "hazard rate")


##Hazard rate decline over time, probability to commite a crime decrease
# BUt people who has drug history has higher hazard rate than the who has not


#. 4 Estimate Cox PH model with additional variable super in the list of explanatory variables.

cox.model2 <- coxph(Surv(durat, event=crime) ~ super + workprg + priors + tserved + 
                      felon + alcohol + drugs + black + married + educ + age,  data=dff, method="breslow", x=TRUE)
summary(cox.model2)

#beta(super) is negative so, it shows people who have supervision have lower hazard rate to commite a crime
# 


cox.model22 <- coxph(Surv(durat, event=crime) ~ drugs + workprg + priors + tserved + 
                      felon + alcohol + drugs + black + married + educ + age,  data=dff, method="breslow", x=TRUE)
summary(cox.model22)

#beta(drugs) show that people who have drug history has higher hazard to commite a crime
#p also is ,oww adn less than 0.05 have higher hazard rate and love survival rate
#also accordimg to exp(coef) people who have drug history have 1.317 or (13.1) higher hazar rate than thoose have not drug history.

#5. Test the proportionality assumption of variables super and drugs in the estimated model.

cox.zph(cox.model2, transform="identity")
cox.zph(cox.model2, transform="rank")
cox.zph(cox.model2, transform="km")


cox.zph(cox.model22, transform="identity")
cox.zph(cox.model22, transform="rank")
cox.zph(cox.model22, transform="km")


#H0 the effect of variable is not related to time
#H1 related to time
#p<0.1 we reject NH
#if the variable depend on time it is problamatic
# S0 super and drug is nor problamtic


# 6 Estimate exponential, Weibull and lognormal regression models with the additional variable super in the list of explanatory variables.

myexponential2 <- survreg( Surv(durat, event=crime) ~ 1 + super + workprg + priors + 
                             tserved + felon + alcohol + drugs + black + married + educ + age, data=df, dist="exponential")

summary(myexponential2)


trhz <- round(cbind(exp(myexponential2$coefficients), 
                    exp(-myexponential2$coefficients))[2:11,],3)  #exclude the first row
colnames(trhz) = c("Time ratio", "Hazard rate ratio")
trhz

#people who has supervision(released from prison) have average time to crime  1.049 times more
#people who has supervision(released from prison) have 0.953 time higher hazard ration

#people who have drug history have average time to crime 0.746 times less or 25.4% less
#who have drug history have 1.340 time higher hazard rate

#who is priors have average time to crime 0.914 times less or 8.6% less
#who is prior have 1.094 time higher hazard rate


myweibull2 <- survreg(Surv(durat, event=crime) ~ 1 + super + workprg + priors + 
                        tserved + felon + alcohol + drugs + black + married + educ + age, data=df, dist="weibull")
summary(myweibull2)

weibtrhz <- round(cbind(exp(myweibull2$coefficients), 
                        exp(-myweibull2$coefficients/myweibull2$scale))[2:11,],3)  #exclude the first row
colnames(weibtrhz) = c("Time ratio", "Hazard rate ratio")
weibtrhz

#people who has supervision(released from prison) have average time to crime  1.064 times more
#people who has supervision(released from prison) have 0.951 time higher hazard ration

#people who have drug history have average time to crime 0.707 times less or 29.3% less
#who have drug history have 1.323 time higher hazard rate

#who is priors have average time to crime 0.897 times less or 10.03% less
#who is prior have 1.091 time higher hazard rate


mylognormal2 <- survreg( Surv(durat, event=crime) ~ 1 + super + workprg + priors + 
                           tserved + felon + alcohol + drugs + black + married + educ + age, data=df, dist="lognormal")
summary(mylognormal2)

lognormalbtr <- round(cbind(mylognormal2$coefficients, exp(mylognormal2$coefficients))[2:11,], 4)
colnames(lognormalbtr) = c("Betas", "Time ratios") 
lognormalbtr

#people who has supervision(released from prison) have average time to crime  1.0840 times more

#people who have drug history have average time to crime 0.7447 times less or 29.53% less

#who is priors have average time to crime 0.8732 times less or 12.68% less compare to whp not priors



#Should we use Weibull model compared to exponential model? 

#H0 p=1 exponential model is fine
#H1 p is not equl 1 Weibul model is fine
#If log(scale) different from 0, we reject H0
#So, wibul model is better for our case cause 0.2159 =log)scale)


#7. Compare new models with Akaike Information Criteria. Is the lognormal model still the best?

AIC(myexponential2, myweibull2, mylognormal2)

#yes, lognormal still better 


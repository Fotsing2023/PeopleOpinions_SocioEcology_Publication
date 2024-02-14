
# Socioeconomic Data analysis done by Ernest Fotsing

##Clear environment and Load Packages
----------------------------------------
rm(list = ls())
library(gdata)
library(lme4)
library(car)
library(GGally)
library(AICcmodavg)
library(broom)
library(visreg)# for quick response curve
library(sjPlot)# summary model in a table
library(stargazer)
library(influence.ME) #for assessing influential random effect levels
library(usdm) # for function vif
library(AICcmodavg) # for aictab
library(MASS) # for negative binomial model
library(ncf) # for spline correlogram
library(corrplot) #for making cool correlation visualizations/plots

library(tidyverse) #for piping and mutate function
theme_set(theme_classic(12))

# load the data
setwd("C:/Users/ErnestFotsing")

library(readxl)

ErnestDataR <- read_excel("ErnestDataR.xlsx")
xdata=(ErnestDataR)

sort(unique(xdata$Age))
xdata$VilDistPark[xdata$Village=="MNDJA" & xdata$VilDistPark>3]=3
xdata$Age=factor(xdata$Age, levels=c("15-24", "25-35", "36-45", "46-55", "≥56"))

table(xdata$Age, useNA="always")
sum(table(xdata$Age, useNA="always"))
xdata$VilDistPark
table(xdata$VilDistPark)
table(xdata$Gender, useNA="always")
table(xdata$EthGr, useNA="always")
table(xdata$MainOccup, xdata$SecondOccup, useNA="always")
xdata$SecondOccup=gsub(x=xdata$SecondOccup, pattern="mecanic", replacement="mechanic")
table(xdata$MainOccup)
sel.data=subset(xdata, MainOccup!="fisherman")
nrow(xdata)
nrow(sel.data)
table(sel.data$EdLevel, useNA="always")
nrow(sel.data)
sel.data=subset(sel.data, EdLevel%in%c("No education", "Primary", "Secondary"))
nrow(sel.data)
table(sel.data$TimeLCom, useNA="always")
plot(table(sel.data$TimeLCom))
table(sel.data$NumChild, useNA="always")
plot(table(sel.data$NumChild))
sel.data$ChimpRecog
table(sel.data$VilDistPark, sel.data$Village)
plot(table(sel.data$VilDistPark, sel.data$Village))

## ernest analysis

str(sel.data$Gender)
str(sel.data$Age)
str(sel.data$MainOccup)
View(sel.data$MainOccup)
view_df(sel.data)

sel.data2<-sel.data

sel.data2$Gender <- as.factor(sel.data$Gender)

FacAge<-sel.data$Age <- as.factor(sel.data$Age)

FacEdl<-sel.data$EdLevel <- as.factor(sel.data$EdLevel)

FacMainOcup<-sel.data$MainOccup <- as.factor(sel.data$MainOccup)

FacTL<-sel.data$TimeLCom <- as.factor(sel.data$TimeLCom)

FacVDP<-sel.data$VilDistPark <- as.factor(sel.data$VilDistPark)

ggplot(data = sel.data2,
       mapping = aes(x = Gender, 
                    y =  as.numeric(ChimpRecog) +
  geom_boxplot()))

#################
str(sel.data)
par(mar = c(1, 1, 1, 1))
par(mfrow = c(3,3))
#windows(10, 10)
hist(as.numeric(sel.data$TimeLCom, useNA="always"))
hist(as.numeric(sel.data$Village, useNA="always"))
hist(as.numeric(sel.data$MainOccup, useNA="always"))
hist(as.numeric(sel.data$SecondOccup, useNA="always"))
hist(as.numeric(sel.data$Age, useNA="always"))
hist(as.numeric(sel.data$EdLevel, useNA="always"))
hist(as.numeric(sel.data$NumChild, useNA="always"))
hist(as.numeric(sel.data$Gender, useNA="always"))

myvar2<- c("Gnum", "Vnum", "Agnum", "MoPnum", "Edlnum", "TLnum", "Nchnum", "Sednum")


str(sel.data$Gender)
str(sel.data)
ggplot(data = sel.data, aes(x= Gender, y = ChimpRecog)) +
  geom_boxplot()

data_num <- as.data.frame(apply(sel.data, 2, as.numeric))
numeric(myvar1)<-myvar1
Gnum<-as.numeric(sel.data$Gender, useNA="always")
Vnum<-as.numeric(sel.data$Vilage, useNA="always")
Agnum<-as.numeric(sel.data$Agnum, useNA="always")
MoPnum<-as.numeric(sel.data$MainOccup, useNA="always")
Edlnum<-as.numeric(sel.data$EdLevel, useNA="always")
TLnum<-as.numeric(sel.data$TimeLCom, useNA="always")
Nchnum<-as.numeric(sel.data$NumChild, useNA="always")
Sednum<-as.numeric(sel.data$SecondOccup, useNA="always")

myvar2<- c("Gnum", "Vnum", "Agnum", "MoPnum", "Edlnum", "TLnum", "Nchnum", "Sednum")

 sel.data$sel.dataNum<-as.numeric(sel.data) 

myvar3<-as.numeric(myvar2)
  myvar2 %>%
  dplyr::select(Gnum, Vnum, Agnum, MoPnum, Edlnum,TLnum, Nchnum, Sednum) %>%
 names()# here I build a vector with all of my predictors

windows(15, 10) # set my windows margins
par(mfrow = c(3,3))
for(x in myvar3) {
  hist(myvar2[x,],
       main = "",
       xlab = x,
       col = "black",
       border = "grey")
}


##binomial models:
#-----------------------------------------------------------------------
source("diagnostic_fcns.r")

##ChimpRecog (1):
xx.fe.re=fe.re.tab(fe.model="ChimpRecog ~ Age + VilDistPark + Gender + MainOccup + 
                   EdLevel + TimeLCom + NumChild.", re="(1|EthGr)",
                   data=sel.data)
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.ChimpRecog=xx.fe.re$data
table(data.ChimpRecog$ChimpRecog)
datata.ChimpRecog$z.TimeLCom=as.vector(scale(data.ChimpRecog$TimeLCom))
data.ChimpRecog$z.NumChild.=as.vector(scale(data.ChimpRecog$NumChild.))
full.ChimpRecog=glmer(ChimpRecog ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.|EthGr),
  control=contr, family=binomial, data=data.ChimpRecog)

summary(full.ChimpRecog)$varcor
full.ChimpRecog.woc=glmer(ChimpRecog ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.ChimpRecog)

summary(full.ChimpRecog.woc)$varcor
logLik(full.ChimpRecog)
logLik(full.ChimpRecog.woc)
ranef.diagn.plot(full.ChimpRecog.woc)
round(max(vif(full.ChimpRecog.woc)[, 3]^2), 3)        ##1.58
null.ChimpRecog.woc=glmer(ChimpRecog ~ 1 + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=contr, family=binomial, data=data.ChimpRecog)

as.data.frame(anova(null.ChimpRecog.woc, full.ChimpRecog.woc, test="Chisq"))

tests.full.ChimpRecog.woc=as.data.frame(drop1(full.ChimpRecog.woc, test="Chisq"))

round(summary(full.ChimpRecog.woc)$coefficients, 3)
exp(0.083)
exp(-8.9152630)
exp(10.529128)
X<-c(0.083,-0.552,-1.374,-1.636,0.337,0.712,-0.739,-0.281,0.615,0.398,0.320,0.471)
exp(X)
round(summary(full.ChimpRecog.woc)$coefficients, 3)

boot.full.ChimpRecog.woc$ci.estimates
exp(boot.full.ChimpRecog.woc$ci.estimates)
##odds ratio
to.plot=exp(boot.full.ChimpRecog.woc$ci.estimates)

### Update new
to.plot=exp(round(summary(full.ChimpRecog.woc)$coefficients, 3))
to.plot<- exp(round(summary(full.ChimpRecog.woc)$coefficients, 3))
to.plot= exp(round(summary(full.ChimpRecog.woc)$coefficients, 3)) 
plot.model(to.plot=exp(round(summary(full.ChimpRecog.woc)$coefficients, 3)), vlat=1)
plot.model(to.plot=exp(round(summary(full.ChimpRecog.woc)$coefficients, 3)), vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))
##estimates in link space

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}

###plot.model boot.full.ChimpRecog.woc$ci.estimates
par(mfrow =c(1, 2))
windows(10, 7)
to.plot= boot.full.ChimpRecog.woc$ci.estimates   
   
plot.model(to.plot=boot.full.ChimpRecog.woc$ci.estimates, vlat=1)

plot.model(to.plot=boot.full.ChimpRecog.woc$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))
                                                           
####### plot model 2
to.plot=boot.full.ChimpObs.woc$ci.estimates   
plot.model(to.plot=boot.full.ChimpObs.woc$ci.estimates, vlat=1)
plot.model(to.plot=boot.full.ChimpObs.woc$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))

#### -------------------------------modification----------------------
##ChimpObs (2):
xx.fe.re=fe.re.tab(fe.model="ChimpObs ~ Age + VilDistPark + Gender + MainOccup + EdLevel + TimeLCom + NumChild.", re="(1|EthGr)", data=sel.data)
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.ChimpObs=xx.fe.re$data
data.ChimpObs$z.VilDistPark=as.vector(scale(data.ChimpObs$VilDistPark))
data.ChimpObs$z.TimeLCom=as.vector(scale(data.ChimpObs$TimeLCom))
data.ChimpObs$z.NumChild.=as.vector(scale(data.ChimpObs$NumChild.))
table(data.ChimpObs$ChimpObs)
full.ChimpObs=glmer(ChimpObs ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.|EthGr),
  control=contr, family=binomial, data=data.ChimpObs)
summary(full.ChimpObs)$varcor
full.ChimpObs.woc=glmer(ChimpObs ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.ChimpObs)
summary(full.ChimpObs.woc)$varcor
logLik(full.ChimpObs)
logLik(full.ChimpObs.woc)
ranef.diagn.plot(full.ChimpObs.woc)
round(max(vif(full.ChimpObs.woc)[, 3]^2), 3)##1.654
null.ChimpObs.woc=glmer(ChimpObs ~ 1 + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=contr, family=binomial, data=data.ChimpObs)
as.data.frame(anova(null.ChimpObs.woc, full.ChimpObs.woc, test="Chisq"))
tests.full.ChimpObs.woc=as.data.frame(drop1(full.ChimpObs.woc, test="Chisq"))

round(summary(full.ChimpObs.woc)$coefficients, 3)
boot.full.ChimpObs.woc$ci.estimates
exp(boot.full.ChimpObs.woc$ci.estimates)
x<-c(1.45290908,-0.02337045,4.0682312)
exp(x)


# new model with only significant predictors

full.ChimpObs.woc2=glmer(ChimpObs ~ Age + Gender + EdLevel + 
                        (1 | EthGr),control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.ChimpObs)

summary(full.ChimpObs.woc2)

#------------------Explore response curve----------------------

par(mfrow = c(2,2))

png(file="saving_plot2.png",
    width=300, height=150)

    visreg(full.ChimpObs.woc, "Age",
    ylab = "Probability to have seen a chimpanzee",
             xlab = "Age group of respondent",
             scale = "response", cex.lab=0.7, cex.axis = 0.6)
      
      png(file="ProtoseeChimp.png",
          width=600, height=350)
      hist(Temperature, col="gold")
      dev.off()

visreg(bestModel, "open.space1km",
       ylab = "Probability of starling presence",
       xlab = "Proportion of open developed land within 1km",
       scale = "response")

#-------------estimates in link space---------------------

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="variables", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}

to.plot=boot.full.ChimpObs.woc$ci.estimates   
plot.model(to.plot=boot.full.ChimpObs.woc$ci.estimates, vlat=1)
plot.model(to.plot=boot.full.ChimpObs.woc$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))

##plot.models---------------------------------------------------------:

data.ChimpObs$Age.25.35=data.ChimpObs$Age.25.35-mean(data.ChimpObs$Age.25.35)
data.ChimpObs$Age.36.45=data.ChimpObs$Age.36.45-mean(data.ChimpObs$Age.36.45)
data.ChimpObs$Age.46.55=data.ChimpObs$Age.46.55-mean(data.ChimpObs$Age.46.55)
data.ChimpObs$Age..56=data.ChimpObs$Age..56-mean(data.ChimpObs$Age..56)
data.ChimpObs$Gender.Male=data.ChimpObs$Gender.Male-mean(data.ChimpObs$Gender.Male)
data.ChimpObs$MainOccup.Hunter=data.ChimpObs$MainOccup.Hunter-mean(data.ChimpObs$MainOccup.Hunter)
data.ChimpObs$MainOccup.trader=data.ChimpObs$MainOccup.trader-mean(data.ChimpObs$MainOccup.trader)
data.ChimpObs$EdLevel.Primary=data.ChimpObs$EdLevel.Primary-mean(data.ChimpObs$EdLevel.Primary)
data.ChimpObs$EdLevel.Secondary=data.ChimpObs$EdLevel.Secondary-mean(data.ChimpObs$EdLevel.Secondary)
plot.full.ChimpObs.woc.gender=glmer(ChimpObs ~ Age.25.35+Age.36.45+Age.46.55+Age..56 + z.VilDistPark + Gender + MainOccup.Hunter+MainOccup.trader + EdLevel.Primary+EdLevel.Secondary + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.ChimpObs)
plot.full.ChimpObs.woc.edlevel=glmer(ChimpObs ~ Age.25.35+Age.36.45+Age.46.55+Age..56 + z.VilDistPark + Gender.Male + MainOccup.Hunter+MainOccup.trader + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.ChimpObs)


## Run new model with significant predictors to chech explained variance

##ChimpName (3):
xx.fe.re=fe.re.tab(fe.model="ChimpName ~ Age + VilDistPark + Gender + MainOccup + EdLevel + TimeLCom + NumChild.", re="(1|EthGr)", data=sel.data)
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.ChimpName=xx.fe.re$data
data.ChimpName$z.VilDistPark=as.vector(scale(data.ChimpName$VilDistPark))
data.ChimpName$z.TimeLCom=as.vector(scale(data.ChimpName$TimeLCom))
data.ChimpName$z.NumChild.=as.vector(scale(data.ChimpName$NumChild.))
table(data.ChimpName$ChimpName)
full.ChimpName=glmer(ChimpName ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.|EthGr),
  control=contr, family=binomial, data=data.ChimpName)
summary(full.ChimpName)$varcor
full.ChimpName.woc=glmer(ChimpName ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.ChimpName)
summary(full.ChimpName.woc)$varcor
logLik(full.ChimpName)
logLik(full.ChimpName.woc)
ranef.diagn.plot(full.ChimpName.woc)
round(max(vif(full.ChimpName.woc)[, 3]^2), 3)##1.258
null.ChimpName.woc=glmer(ChimpName ~ 1 + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=contr, family=binomial, data=data.ChimpName)
as.data.frame(anova(null.ChimpName.woc, full.ChimpName.woc, test="Chisq"))

tests.full.ChimpName.woc=as.data.frame(drop1(full.ChimpName.woc, test="Chisq"))

round(summary(full.ChimpName.woc)$coefficients, 3)

#####-------------------------------------------------#############
##ChimpCulTabous (4):
xx.fe.re=fe.re.tab(fe.model="ChimpCulTabous ~ Age + VilDistPark + Gender + MainOccup + EdLevel + TimeLCom + NumChild.", re="(1|EthGr)", data=sel.data, other.vars="ChimpHunting")
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.ChimpCulTabous=xx.fe.re$data
data.ChimpCulTabous$z.VilDistPark=as.vector(scale(data.ChimpCulTabous$VilDistPark))
data.ChimpCulTabous$z.TimeLCom=as.vector(scale(data.ChimpCulTabous$TimeLCom))
data.ChimpCulTabous$z.NumChild.=as.vector(scale(data.ChimpCulTabous$NumChild.))
table(data.ChimpCulTabous$ChimpCulTabous)
data.ChimpCulTabous$ChimpCulTabous[data.ChimpCulTabous$ChimpCulTabous==2]=1
full.ChimpCulTabous=glmer(ChimpCulTabous ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.|EthGr),
  control=contr, family=binomial, data=data.ChimpCulTabous)
summary(full.ChimpCulTabous)$varcor
full.ChimpCulTabous.woc=glmer(ChimpCulTabous ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.ChimpCulTabous)
summary(full.ChimpCulTabous.woc)$varcor
logLik(full.ChimpCulTabous)
logLik(full.ChimpCulTabous.woc)
ranef.diagn.plot(full.ChimpCulTabous)
round(max(vif(full.ChimpCulTabous)[, 3]^2), 3)##1.582
null.ChimpCulTabous.woc=glmer(ChimpCulTabous ~ 1 + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=contr, family=binomial, data=data.ChimpCulTabous)
as.data.frame(anova(null.ChimpCulTabous.woc, full.ChimpCulTabous.woc, test="Chisq"))

tests.full.ChimpCulTabous.woc=as.data.frame(drop1(full.ChimpCulTabous.woc, test="Chisq"))

round(summary(full.ChimpCulTabous.woc)$coefficients, 3)
boot.full.ChimpCulTabous.woc$ci.estimates
exp(boot.full.ChimpCulTabous.woc$ci.estimates)
2.454862e+01

##odds ratio
to.plot=exp(boot.full.ChimpCulTabous.woc$ci.estimates)
to.plot=boot.full.ChimpCulTabous.woc$ci.estimates
##estimates in link space

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}
to.plot= boot.full.ChimpCulTabous.woc$ci.estimates   

plot.model(to.plot=boot.full.ChimpCulTabous.woc$ci.estimates, vlat=1)

plot.model(to.plot=boot.full.ChimpCulTabous.woc$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))


###################################################-------------------------------

##ChimpHunting (5):
table(data.ChimpCulTabous$ChimpHunting)
full.ChimpHunting=glmer(ChimpHunting ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.|EthGr),
  control=contr, family=binomial, data=data.ChimpCulTabous)
summary(full.ChimpHunting)$varcor
full.ChimpHunting.woc=glmer(ChimpHunting ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.ChimpCulTabous)
summary(full.ChimpHunting.woc)$varcor
logLik(full.ChimpHunting)
logLik(full.ChimpHunting.woc)
ranef.diagn.plot(full.ChimpHunting)
round(max(vif(full.ChimpHunting.woc)[, 3]^2), 3)##1.656
data.ChimpHunting=data.ChimpCulTabous
null.ChimpHunting.woc=glmer(ChimpHunting ~ 1 + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=contr, family=binomial, data=data.ChimpCulTabous)
as.data.frame(anova(null.ChimpHunting.woc, full.ChimpHunting.woc, test="Chisq"))

tests.full.ChimpHunting.woc=as.data.frame(drop1(full.ChimpHunting.woc, test="Chisq"))

round(summary(full.ChimpHunting.woc)$coefficients, 3)
boot.full.ChimpHunting.woc$ci.estimates
exp(-16.798919747)
5.061997e-08
2.157558e+01
exp(boot.full.ChimpHunting.woc$ci.estimates)

##odds ratio
to.plot=boot.full.ChimpHunting.woc$ci.estimates

##estimates in link space

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}

to.plot=boot.full.ChimpHunting.woc$ci.estimates   
plot.model(to.plot=boot.full.ChimpHunting.woc$ci.estimates, vlat=1)
plot.model(to.plot=boot.full.ChimpHunting.woc$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))


##plot.model:
data.ChimpCulTabous$Age.25.35=data.ChimpCulTabous$Age.25.35-mean(data.ChimpCulTabous$Age.25.35)
data.ChimpCulTabous$Age.36.45=data.ChimpCulTabous$Age.36.45-mean(data.ChimpCulTabous$Age.36.45)
data.ChimpCulTabous$Age.46.55=data.ChimpCulTabous$Age.46.55-mean(data.ChimpCulTabous$Age.46.55)
data.ChimpCulTabous$Age..56=data.ChimpCulTabous$Age..56-mean(data.ChimpCulTabous$Age..56)
data.ChimpCulTabous$Gender.Male=data.ChimpCulTabous$Gender.Male-mean(data.ChimpCulTabous$Gender.Male)
data.ChimpCulTabous$MainOccup.Hunter=data.ChimpCulTabous$MainOccup.Hunter-mean(data.ChimpCulTabous$MainOccup.Hunter)
data.ChimpCulTabous$MainOccup.trader=data.ChimpCulTabous$MainOccup.trader-mean(data.ChimpCulTabous$MainOccup.trader)
data.ChimpCulTabous$EdLevel.Primary=data.ChimpCulTabous$EdLevel.Primary-mean(data.ChimpCulTabous$EdLevel.Primary)
data.ChimpCulTabous$EdLevel.Secondary=data.ChimpCulTabous$EdLevel.Secondary-mean(data.ChimpCulTabous$EdLevel.Secondary)
plot.full.ChimpHunting.woc.gender=glmer(ChimpHunting ~ Age.25.35+Age.36.45+Age.46.55+Age..56 + z.VilDistPark + Gender + MainOccup.Hunter+MainOccup.trader + EdLevel.Primary+EdLevel.Secondary + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.ChimpCulTabous)

################################################################################################EatChimp (6):

xx.fe.re=fe.re.tab(fe.model="EatChimp ~ Age + VilDistPark + Gender + MainOccup + EdLevel + TimeLCom + NumChild.", re="(1|EthGr)", data=sel.data)
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.EatChimp=xx.fe.re$data
table(data.EatChimp$EatChimp)
data.EatChimp$z.VilDistPark=as.vector(scale(data.EatChimp$VilDistPark))
data.EatChimp$z.TimeLCom=as.vector(scale(data.EatChimp$TimeLCom))
data.EatChimp$z.NumChild.=as.vector(scale(data.EatChimp$NumChild.))
full.EatChimp=glmer(EatChimp ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.|EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.EatChimp)
summary(full.EatChimp)$varcor
full.EatChimp.woc=glmer(EatChimp ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.EatChimp)
summary(full.EatChimp.woc)$varcor
logLik(full.EatChimp)
logLik(full.EatChimp.woc)
ranef.diagn.plot(full.EatChimp.woc)
round(max(vif(full.EatChimp.woc)[, 3]^2), 3)##1.621
null.EatChimp.woc=glmer(EatChimp ~ 1 + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=contr, family=binomial, data=data.EatChimp)
as.data.frame(anova(null.EatChimp.woc, full.EatChimp.woc, test="Chisq"))

tests.full.EatChimp.woc=as.data.frame(drop1(full.EatChimp.woc, test="Chisq"))

round(summary(full.EatChimp.woc)$coefficients, 3)
boot.full.EatChimp.woc$ci.estimates
exp(boot.full.EatChimp.woc$ci.estimates)
##odds ratio
to.plot=boot.full.EatChimp.woc$ci.estimates
##estimates in link space

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}

to.plot=boot.full.EatChimp.woc$ci.estimates   
plot.model(to.plot=boot.full.EatChimp.woc$ci.estimates, vlat=1)
plot.model(to.plot=boot.full.EatChimp.woc$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))


################################################

#response curve

visreg(full.EatChimp.woc, "z.TimeLCom", ylab = "Probability to eat chimpanzee", xlab = "Time living in the community", scale = "response", cex.lab = 0.7, cex.axis = 0.6)

plot(x = data.EatChimp$TimeLCom,
     y = data.EatChimp$EatChimp,
     xlab = "Time living in the community",
     ylab = "Probability to eat chimpanzee",
     cex.lab = 0.8, cex.axis = 0.7, xaxs='i', xlim=c(3, 70), ylim= c(0, 1))

TimeL.lm <- lm(formula = EatChimp ~ TimeLCom,
                  data = data.EatChimp)
abline(TimeL.lm,
       col = "red", lwd = 1)

###############################################################################################
##ChimpDeseases (7):

sel.data$ChimpDeseases[sel.data$ChimpDeseases=="I don't know"]=NA
sel.data$ChimpDeseases[sel.data$ChimpDeseases==""]=NA
xx.fe.re=fe.re.tab(fe.model="ChimpDeseases ~ Age + VilDistPark + Gender + MainOccup + EdLevel + TimeLCom + NumChild.", re="(1|EthGr)", data=sel.data)
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.ChimpDeseases=xx.fe.re$data
table(data.ChimpDeseases$ChimpDeseases)
data.ChimpDeseases$rv=as.numeric(data.ChimpDeseases$ChimpDeseases=="Yes")
data.ChimpDeseases$z.VilDistPark=as.vector(scale(data.ChimpDeseases$VilDistPark))
data.ChimpDeseases$z.TimeLCom=as.vector(scale(data.ChimpDeseases$TimeLCom))
data.ChimpDeseases$z.NumChild.=as.vector(scale(data.ChimpDeseases$NumChild.))
full.ChimpDeseases=glmer(rv ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.|EthGr),
  control=contr, family=binomial, data=data.ChimpDeseases)
summary(full.ChimpDeseases)$varcor
full.ChimpDeseases.woc=glmer(rv ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.ChimpDeseases)
summary(full.ChimpDeseases.woc)$varcor
logLik(full.ChimpDeseases)
logLik(full.ChimpDeseases.woc)
ranef.diagn.plot(full.ChimpDeseases.woc)
round(max(vif(full.ChimpDeseases.woc)[, 3]^2), 3)##1.627
null.ChimpDeseases.woc=glmer(rv ~ 1 + 
  (1 + z.VilDistPark + z.TimeLCom + z.NumChild.||EthGr),
  control=contr, family=binomial, data=data.ChimpDeseases)
as.data.frame(anova(null.ChimpDeseases.woc, full.ChimpDeseases.woc, test="Chisq"))

tests.full.ChimpDeseases.woc=as.data.frame(drop1(full.ChimpDeseases.woc, test="Chisq"))
round(summary(full.ChimpDeseases.woc)$coefficients, 3)

###########################################################################################--
##KnowParkLimit (8):
xx.fe.re=fe.re.tab(fe.model="KnowParkLimit ~ Age + VilDistPark + Gender + 
                   MainOccup + EdLevel + TimeLCom + NumChild.", re="(1|EthGr)", 
                   data=sel.data)
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary
data.KnowParkLimit=xx.fe.re$data
table(data.KnowParkLimit$KnowParkLimit)
data.KnowParkLimit$z.VilDistPark=as.vector(scale(data.KnowParkLimit$VilDistPark))
data.KnowParkLimit$z.TimeLCom=as.vector(scale(data.KnowParkLimit$TimeLCom))
data.KnowParkLimit$z.NumChild.=as.vector(scale(data.KnowParkLimit$NumChild.))
full.KnowParkLimit=glmer(KnowParkLimit ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.TimeLCom + z.NumChild.|EthGr),
  control=contr, family=binomial, data=data.KnowParkLimit)
summary(full.KnowParkLimit)$varcor
full.KnowParkLimit.woc=glmer(KnowParkLimit ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.KnowParkLimit)
summary(full.KnowParkLimit.woc)$varcor
logLik(full.KnowParkLimit)
logLik(full.KnowParkLimit.woc)
ranef.diagn.plot(full.KnowParkLimit.woc)
round(max(vif(full.KnowParkLimit.woc)[, 3]^2), 3)##1.27
null.KnowParkLimit.woc=glmer(KnowParkLimit ~ 1 + 
  (1 + z.TimeLCom + z.NumChild.||EthGr),
  control=contr, family=binomial, data=data.KnowParkLimit)
as.data.frame(anova(null.KnowParkLimit.woc, full.KnowParkLimit.woc, test="Chisq"))

tests.full.KnowParkLimit.woc=as.data.frame(drop1(full.KnowParkLimit.woc, test="Chisq"))

round(summary(full.KnowParkLimit.woc)$coefficients, 3)
boot.full.KnowParkLimit.woc$ci.estimates
exp(boot.full.KnowParkLimit.woc$ci.estimates)
exp(-0.9443384)

##odds ratio
to.plot=boot.full.KnowParkLimit.woc$ci.estimates
##plot models:
data.KnowParkLimit$Age.25.35=data.KnowParkLimit$Age.25.35-mean(data.KnowParkLimit$Age.25.35)
data.KnowParkLimit$Age.36.45=data.KnowParkLimit$Age.36.45-mean(data.KnowParkLimit$Age.36.45)
data.KnowParkLimit$Age.46.55=data.KnowParkLimit$Age.46.55-mean(data.KnowParkLimit$Age.46.55)
data.KnowParkLimit$Age..56=data.KnowParkLimit$Age..56-mean(data.KnowParkLimit$Age..56)
data.KnowParkLimit$Gender.Male=data.KnowParkLimit$Gender.Male-mean(data.KnowParkLimit$Gender.Male)
data.KnowParkLimit$MainOccup.trader=data.KnowParkLimit$MainOccup.trader-mean(data.KnowParkLimit$MainOccup.trader)
data.KnowParkLimit$EdLevel.Secondary=data.KnowParkLimit$EdLevel.Secondary-mean(data.KnowParkLimit$EdLevel.Secondary)
plot.full.KnowParkLimit.woc.num.child=glmer(KnowParkLimit ~ Age.25.35+Age.36.45+Age.46.55+Age..56 + z.VilDistPark + Gender.Male + MainOccup.Hunter+MainOccup.trader + EdLevel.Primary+EdLevel.Secondary + z.TimeLCom + z.NumChild. + 
  (1 + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
  family=binomial, data=data.KnowParkLimit)

##################
to.plot=boot.full.KnowParkLimit.woc$ci.estimates

##estimates in link space

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}

to.plot=boot.full.KnowParkLimit.woc$ci.estimates   
plot.model(to.plot=boot.full.KnowParkLimit.woc$ci.estimates, vlat=1)
plot.model(to.plot=boot.full.KnowParkLimit.woc$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))

###################################
#response curve

visreg(full.KnowParkLimit.woc, "z.NumChild.", ylab = "Probability to know the park limit", xlab = "Number of child", scale = "response", cex.lab = 0.7, cex.axis = 0.6)

plot(x = data.KnowParkLimit$NumChild.,
     y = data.KnowParkLimit$KnowParkLimit,
     xlab = "Number of child",
     ylab = "Probability to know the park limit",
     cex.lab = 0.8, cex.axis = 0.7, xaxs='i', xlim=c(0, 12), ylim= c(0, 1))

TimeL.lm <- lm(formula = EatChimp ~ TimeLCom,
               data = data.EatChimp)
abline(TimeL.lm,
       col = "red", lwd = 1.5)

##############################################------------------------------------
##LawPoachingChimp (9):
##
xx.fe.re=fe.re.tab(fe.model="LawPoachingChimp ~ Age + VilDistPark + Gender + MainOccup + EdLevel + TimeLCom + NumChild.", re="(1|EthGr)", data=sel.data)
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.LawPoachingChimp=xx.fe.re$data
table(data.LawPoachingChimp$LawPoachingChimp)
data.LawPoachingChimp$z.VilDistPark=as.vector(scale(data.LawPoachingChimp$VilDistPark))
data.LawPoachingChimp$z.TimeLCom=as.vector(scale(data.LawPoachingChimp$TimeLCom))
data.LawPoachingChimp$z.NumChild.=as.vector(scale(data.LawPoachingChimp$NumChild.))
full.LawPoachingChimp=glmer(LawPoachingChimp ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.TimeLCom + z.NumChild.|EthGr),
  control=contr, family=binomial, data=data.LawPoachingChimp)
summary(full.LawPoachingChimp)$varcor
full.LawPoachingChimp.woc=glmer(LawPoachingChimp ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.LawPoachingChimp)
summary(full.LawPoachingChimp.woc)$varcor
logLik(full.LawPoachingChimp)
logLik(full.LawPoachingChimp.woc)
ranef.diagn.plot(full.LawPoachingChimp.woc)
round(max(vif(full.LawPoachingChimp.woc)[, 3]^2), 3)##1.641
null.LawPoachingChimp.woc=glmer(LawPoachingChimp ~ 1 + 
  (1 + z.TimeLCom + z.NumChild.||EthGr),
  control=contr, family=binomial, data=data.LawPoachingChimp)
as.data.frame(anova(null.LawPoachingChimp.woc, full.LawPoachingChimp.woc, test="Chisq"))

tests.full.LawPoachingChimp.woc=as.data.frame(drop1(full.LawPoachingChimp.woc, test="Chisq"))

round(summary(full.LawPoachingChimp.woc)$coefficients, 3)
boot.full.LawPoachingChimp.woc$ci.estimates
exp(boot.full.LawPoachingChimp.woc$ci.estimates)
exp(1.10956046)

####plot estimate coefs

to.plot=boot.full.LawPoachingChimp.woc$ci.estimates

##estimates in link space

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}

to.plot=boot.full.LawPoachingChimp.woc$ci.estimates   
plot.model(to.plot=boot.full.LawPoachingChimp.woc$ci.estimates, vlat=1)
plot.model(to.plot=boot.full.LawPoachingChimp.woc$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))


######################################------------------------------
##OpParkMagCom (10a):
xx.fe.re=fe.re.tab(fe.model="OpParkMagCom ~ Age + VilDistPark + Gender + MainOccup 
                   + EdLevel + TimeLCom + NumChild.", re="(1|EthGr)", data=sel.data)
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.OpParkMagCom=xx.fe.re$data
table(data.OpParkMagCom$OpParkMagCom)
data.OpParkMagCom$z.VilDistPark=as.vector(scale(data.OpParkMagCom$VilDistPark))
data.OpParkMagCom$z.TimeLCom=as.vector(scale(data.OpParkMagCom$TimeLCom))
data.OpParkMagCom$z.NumChild.=as.vector(scale(data.OpParkMagCom$NumChild.))
full.OpParkMagCom=glmer(OpParkMagCom ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.TimeLCom + z.NumChild.|EthGr),
  control=contr, family=binomial, data=data.OpParkMagCom)
summary(full.OpParkMagCom)$varcor
full.OpParkMagCom.woc=glmer(OpParkMagCom ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.TimeLCom + z.NumChild.||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.OpParkMagCom)
summary(full.OpParkMagCom.woc)$varcor
logLik(full.OpParkMagCom)
logLik(full.OpParkMagCom.woc)
ranef.diagn.plot(full.OpParkMagCom.woc)
round(max(vif(full.OpParkMagCom.woc)[, 3]^2), 3)##2.241
null.OpParkMagCom.woc=glmer(OpParkMagCom ~ 1 + 
  (1 + z.TimeLCom + z.NumChild.||EthGr),
  control=contr, family=binomial, data=data.OpParkMagCom)
as.data.frame(anova(null.OpParkMagCom.woc, full.OpParkMagCom.woc, test="Chisq"))

tests.full.OpParkMagCom.woc=as.data.frame(drop1(full.OpParkMagCom.woc, test="Chisq"))

round(summary(full.OpParkMagCom.woc)$coefficients, 3)
boot.full.OpParkMagCom.woc$ci.estimates
exp(boot.full.OpParkMagCom.woc$ci.estimates)

write.csv(exp(boot.full.OpParkMagCom.woc$ci.estimates), "OR_IC)OpParkMagCom.csv")

install.packages("writexl")

##### import to excel file 
#### create a data frame
df1 <- data.frame(round(summary(full.OpParkMagCom.woc)$coefficients, 3))
df2 <- data.frame(boot.full.OpParkMagCom.woc$ci.estimates)
df <- data.frame(exp(boot.full.OpParkMagCom.woc$ci.estimates))

write_xlsx(df1,"./Summary_OpParkMagCom.xlsx")
write_xlsx(df2,"./fullCoef_OpParkMagCom.xlsx")
write_xlsx(df,"./OR_ICOpParkMagCom.xlsx")
print (df)
#### call the library function below 
library("writexl")

### write the function to save your file in excel file

write_xlsx(df,"./OR_IC)OpParkMagCom.xlsx")
for (orig in boot.full.OpParkMagCom.woc$ci.estimates) {print(exp(orig))
  
}
expB<-for (orig in boot.full.OpParkMagCom.woc$ci.estimates) {print(exp(orig))
  
}

to.plot=boot.full.OpParkMagCom.woc$ci.estimates

##estimates in link space

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}

to.plot=boot.full.full.OpParkMagCom.woc$ci.estimates   
plot.model(to.plot=boot.full.OpParkMagCom.woc$ci.estimates, vlat=1)
plot.model(to.plot=boot.full.OpParkMagCom.woc$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))

#########--------------------------response curve


full.OpParkMagCom.woc=glmer(OpParkMagCom ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
                              (1 + z.TimeLCom + z.NumChild.||EthGr),
                            control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.OpParkMagCom)

visreg(full.OpParkMagCom.woc, "Age", ylab = "Probability to be able to manage the park", xlab = "Age", scale = "response", cex.lab = 0.7, cex.axis = 0.6)

plot(x = data.OpParkMagCom$Age,
     y = data.OpParkMagCom$OpParkMagCom,
     xlab = "Age",
     ylab = "Probability to manage the park",
     cex.lab = 0.8, cex.axis = 0.7, xaxs='i', xlim=c(0, 12), ylim= c(0, 1))

Age.lm2 <- lm(formula = OpParkMagCom ~ Age,
               data = data.OpParkMagCom)
abline(Age.lm2,
       col = "red", lwd = 1.5)

###############################################################################################
#------OpChimpExtinction (10b):
xx.fe.re=fe.re.tab(fe.model="OpChimpExtinction ~ Age + VilDistPark + Gender + 
                   MainOccup + EdLevel + TimeLCom + NumChild.", re="(1|EthGr)", 
  data=subset(sel.data, OpChimpExtinction!=2))
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.OpChimpExtinction=xx.fe.re$data
table(data.OpChimpExtinction$OpChimpExtinction)
data.OpChimpExtinction$z.VilDistPark=as.vector(scale(data.OpChimpExtinction$VilDistPark))
data.OpChimpExtinction$z.TimeLCom=as.vector(scale(data.OpChimpExtinction$TimeLCom))
data.OpChimpExtinction$z.NumChild.=as.vector(scale(data.OpChimpExtinction$NumChild.))
full.OpChimpExtinction=glmer(OpChimpExtinction ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.TimeLCom|EthGr),
  control=contr, family=binomial, data=data.OpChimpExtinction)
summary(full.OpChimpExtinction)$varcor
full.OpChimpExtinction.woc=glmer(OpChimpExtinction ~ Age + z.VilDistPark + Gender + MainOccup + EdLevel + z.TimeLCom + z.NumChild. + 
  (1 + z.TimeLCom||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.OpChimpExtinction)
summary(full.OpChimpExtinction.woc)$varcor
logLik(full.OpChimpExtinction)
logLik(full.OpChimpExtinction.woc)
ranef.diagn.plot(full.OpChimpExtinction.woc)
round(max(vif(full.OpChimpExtinction.woc)[, 3]^2), 3)##1.7
null.OpChimpExtinction.woc=glmer(OpChimpExtinction ~ 1 + 
  (1 + z.TimeLCom||EthGr),
  control=contr, family=binomial, data=data.OpChimpExtinction)
as.data.frame(anova(null.OpChimpExtinction.woc, full.OpChimpExtinction.woc, test="Chisq"))
tests.full.OpChimpExtinction.woc=as.data.frame(drop1(full.OpChimpExtinction.woc, test="Chisq"))

round(summary(full.OpChimpExtinction.woc)$coefficients, 3)
### import in excel 
df1 <- data.frame(round(summary(full.OpChimpExtinction.woc)$coefficients, 3))
write_xlsx(df1,"./Summary_ChimpExtinction.xlsx")
######################################
boot.full.OpChimpExtinction.woc$ci.estimates
### import in excel
df2 <- data.frame(boot.full.OpChimpExtinction.woc$ci.estimates)
write_xlsx(df2,"./fullCoefs_ChimpExtinction.xlsx")

#############################
exp(boot.full.OpChimpExtinction.woc$ci.estimates)
df <- data.frame(exp(boot.full.OpChimpExtinction.woc$ci.estimates))
write_xlsx(df,"./OR_IC_ChimpExtinction.xlsx")

to.plot=boot.full.OpChimpExtinction.woc$ci.estimates

##estimates in link space

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}

to.plot=boot.full.OpChimpExtinction.woc$ci.estimates   
plot.model(to.plot=boot.full.OpChimpExtinction.woc$ci.estimates, vlat=1)
plot.model(to.plot=boot.full.OpChimpExtinction.woc$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))

######----------------------##ordinal models----------------#
##LastDateChimpObs (11):
##
sort(unique(sel.data$LastDateChimpObs))
data.LastDateChimpObs=subset(sel.data, !LastDateChimpObs%in%c("", "I don't remember"))
nrow(sel.data)
nrow(data.LastDateChimpObs)
xx.fe.re=fe.re.tab(fe.model="LastDateChimpObs ~ Age + VilDistPark + MainOccup", re="(1|EthGr)", 
  data=subset(data.LastDateChimpObs, MainOccup!="trader"))
nrow(data.LastDateChimpObs)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.LastDateChimpObs=xx.fe.re$data
data.LastDateChimpObs$z.VilDistPark=as.vector(scale(data.LastDateChimpObs$VilDistPark))
data.LastDateChimpObs$LastDateChimpObs=factor(data.LastDateChimpObs$LastDateChimpObs, levels=c("Before 2016", "2016", "2017", "2018", "2019", "2020", "2021"))
table(data.LastDateChimpObs$Age)
table(data.LastDateChimpObs$VilDistPark)
table(data.LastDateChimpObs$MainOccup)
table(data.LastDateChimpObs$EthGr)
table(data.LastDateChimpObs$LastDateChimpObs)
library(ordinal)
full.LastDateChimpObs=clmm(LastDateChimpObs ~ Age + z.VilDistPark + MainOccup + (1|EthGr),
  data=data.LastDateChimpObs, nAGQ=7)
##=>convergence issues...
##### 
install.packages("swirl")
library(swirl)
swirl()
ernest

################################################################################################PerChimpPopTrend (12):

sort(unique(sel.data$PerChimpPopTrend))
table(sel.data$Village)
data.PerChimpPopTrend=subset(sel.data, PerChimpPopTrend!="" & PerChimpPopTrend!="I don't know")
nrow(sel.data)
nrow(data.PerChimpPopTrend)
table(data.PerChimpPopTrend$MainOccup)
xx.fe.re=fe.re.tab(fe.model="PerChimpPopTrend ~ Age + VilDistPark + MainOccup + Gender + EdLevel + LastDateChimpObs", re="(1|Village) + (1|EthGr)", 
  data=droplevels(subset(data.PerChimpPopTrend, MainOccup!="trader" & LastDateChimpObs!="I don't remember" & LastDateChimpObs!="")))
nrow(data.PerChimpPopTrend)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.PerChimpPopTrend=xx.fe.re$data
data.PerChimpPopTrend$LastDateChimpObs
data.PerChimpPopTrend$LastDateChimpObs=factor(data.PerChimpPopTrend$LastDateChimpObs, levels=levels(data.PerChimpPopTrend$LastDateChimpObs)[c(7, 1:6)])
data.PerChimpPopTrend$z.VilDistPark=as.vector(scale(data.PerChimpPopTrend$VilDistPark))
data.PerChimpPopTrend$LastDateChimpObs.2016=as.numeric(data.PerChimpPopTrend$LastDateChimpObs=="2016")
data.PerChimpPopTrend$LastDateChimpObs.2016=-mean(data.PerChimpPopTrend$LastDateChimpObs.2016)
data.PerChimpPopTrend$LastDateChimpObs.2017=-mean(data.PerChimpPopTrend$LastDateChimpObs.2017)
data.PerChimpPopTrend$LastDateChimpObs.2018=-mean(data.PerChimpPopTrend$LastDateChimpObs.2018)
data.PerChimpPopTrend$LastDateChimpObs.2019=-mean(data.PerChimpPopTrend$LastDateChimpObs.2019)
data.PerChimpPopTrend$LastDateChimpObs.2020=-mean(data.PerChimpPopTrend$LastDateChimpObs.2020)
data.PerChimpPopTrend$LastDateChimpObs.2021=-mean(data.PerChimpPopTrend$LastDateChimpObs.2021)
data.PerChimpPopTrend$Age.25.35=data.PerChimpPopTrend$Age.25.35-mean(data.PerChimpPopTrend$Age.25.35)
data.PerChimpPopTrend$Age.36.45=data.PerChimpPopTrend$Age.36.45-mean(data.PerChimpPopTrend$Age.36.45)
data.PerChimpPopTrend$Age.46.55=data.PerChimpPopTrend$Age.46.55-mean(data.PerChimpPopTrend$Age.46.55)
data.PerChimpPopTrend$Age..56=data.PerChimpPopTrend$Age..56-mean(data.PerChimpPopTrend$Age..56)
data.PerChimpPopTrend$Gender.Male=data.PerChimpPopTrend$Gender.Male-mean(data.PerChimpPopTrend$Gender.Male)
data.PerChimpPopTrend$MainOccup.Hunter=data.PerChimpPopTrend$MainOccup.Hunter-mean(data.PerChimpPopTrend$MainOccup.Hunter)
data.PerChimpPopTrend$EdLevel.Primary=data.PerChimpPopTrend$EdLevel.Primary-mean(data.PerChimpPopTrend$EdLevel.Primary)
data.PerChimpPopTrend$EdLevel.Secondary=data.PerChimpPopTrend$EdLevel.Secondary-mean(data.PerChimpPopTrend$EdLevel.Secondary)
data.PerChimpPopTrend$PerChimpPopTrend=factor(data.PerChimpPopTrend$PerChimpPopTrend, levels(data.PerChimpPopTrend$PerChimpPopTrend)[c(2, 3, 1)])
full.PerChimpPopTrend=clmm(PerChimpPopTrend ~ Age + z.VilDistPark + MainOccup + Gender + EdLevel + LastDateChimpObs + 
  (1|Village) +
  (0 + Age.25.35|Village) + (0 + Age.36.45|Village) +(0 + Age.46.55|Village) + (0 + Age..56|Village) +
  (0 + EdLevel.Primary|Village) + (0 + EdLevel.Secondary|Village) +
  (0 + Gender.Male|Village) +
  (0 + LastDateChimpObs.2016|Village) + (0 + LastDateChimpObs.2017|Village) + (0 + LastDateChimpObs.2018|Village) + (0 + LastDateChimpObs.2019|Village) + (0 + LastDateChimpObs.2020|Village) + (0 + LastDateChimpObs.2021|Village) +
  (0 + MainOccup.Hunter|Village) +
  (1|EthGr),
  data=data.PerChimpPopTrend)
null.PerChimpPopTrend=clmm(PerChimpPopTrend ~ 1 + 
  (1|Village) +
  (0 + Age.25.35|Village) + (0 + Age.36.45|Village) +(0 + Age.46.55|Village) + (0 + Age..56|Village) +
  (0 + EdLevel.Primary|Village) + (0 + EdLevel.Secondary|Village) +
  (0 + Gender.Male|Village) +
  (0 + LastDateChimpObs.2016|Village) + (0 + LastDateChimpObs.2017|Village) + (0 + LastDateChimpObs.2018|Village) + (0 + LastDateChimpObs.2019|Village) + (0 + LastDateChimpObs.2020|Village) + (0 + LastDateChimpObs.2021|Village) +
  (0 + MainOccup.Hunter|Village) +
  (1|EthGr),
  data=data.PerChimpPopTrend)
as.data.frame(anova(null.PerChimpPopTrend, full.PerChimpPopTrend))

tests.PerChimpPopTrend=as.data.frame(drop1(full.PerChimpPopTrend, test="Chisq"))

library(ordinal)
stab.full.PerChimpPopTrend=clmm.stab(mres=full.PerChimpPopTrend, data=data.PerChimpPopTrend, 
                                     formula="PerChimpPopTrend ~ Age + z.VilDistPark + 
                                     MainOccup + Gender + EdLevel + LastDateChimpObs +  (1|Village) + (0 + Age.25.35|Village) + (0 + Age.36.45|Village) +(0 + Age.46.55|Village) + (0 + Age..56|Village) + (0 + EdLevel.Primary|Village) + (0 + EdLevel.Secondary|Village) + (0 + Gender.Male|Village) + (0 + LastDateChimpObs.2016|Village) + (0 + LastDateChimpObs.2017|Village) + (0 + LastDateChimpObs.2018|Village) + (0 + LastDateChimpObs.2019|Village) + (0 + LastDateChimpObs.2020|Village) + (0 + LastDateChimpObs.2021|Village) + (0 + MainOccup.Hunter|Village) + (1|EthGr)",
                                     para=T, n.cores=100, contr=NULL, nAGQ=1L)
boot.full.PerChimpPopTrend=clmm_boot_fun(m=full.PerChimpPopTrend, n.boots=1000, use.u=F, use=NULL, para=T, n.cores=100, level=0.95, resol=1000, return.ind.boots=F)

###############"
round(summary(full.PerChimpPopTrend$coefficients, 3))
round(full.PerChimpPopTrend$coefficients, 3)
### import in excel 
df1 <- data.frame(round(summary(full.PerChimpPopTrend)$coefficients, 3))
write_xlsx(df1,"C:/Users/AT PN MPEM ET DJIM/Desktop/Mundry Analysis//Summary_PerChimpPopTrend.xlsx")
######################################
round(summary(full.ChimpObs.woc)$coefficients, 3)
boot.full.ChimpObs.woc$ci.estimates
exp(boot.full.ChimpObs.woc$ci.estimates)

### import in excel
boot.full.PerChimpPopTrend$ci.estimates

DF<-boot.full.PerChimpPopTrend$ci.estimates
DFO<-exp(boot.full.PerChimpPopTrend$ci.estimates)
DF <- data.frame(boot.full.PerChimpPopTrend$ci.estimates)

DFC<- for (orig in boot.full.PerChimpPopTrend$ci.estimates) {print(exp(orig))
  
}

DFC<- for (orig in boot.full.PerChimpPopTrend$ci.estimates) {print(round(exp(orig),3))
  
}

  for (orig in DFC) {print(round(orig,3))
    
  }

exp(22.324)
print(DF)
write_xlsx(DF,"./fullCoefs_PerChimpPopTrend.xlsx")

#############################
exp(boot.full.PerChimpPopTrend$ci.estimates)
df <- data.frame(exp(boot.full.PerChimpPopTrend$ci.estimates))
write_xlsx(df,"./OR_IC_PerpChimpTrend.xlsx")

to.plot=boot.full.PerChimpPopTrend$ci.estimates

##estimates in link space

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}

to.plot=boot.full.PerChimpPopTrend$ci.estimates   
plot.model(to.plot=boot.full.PerChimpPopTrend$ci.estimates, vlat=1)
plot.model(to.plot=boot.full.PerChimpPopTrend$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))

###############################################################################
##DelPark (13a):
sort(unique(sel.data$DelPark))
xx.fe.re=fe.re.tab(fe.model="DelPark ~ Age + VilDistPark + MainOccup + Gender + EdLevel + TimeLCom", re="(1|Village) + (1|EthGr)", 
  data=droplevels(subset(sel.data, MainOccup!="trader" & DelPark!="")))
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.DelPark=xx.fe.re$data
data.DelPark$z.VilDistPark=as.vector(scale(data.DelPark$VilDistPark))
data.DelPark$z.TimeLCom=as.vector(scale(data.DelPark$TimeLCom))
data.DelPark$Age.25.35=data.DelPark$Age.25.35-mean(data.DelPark$Age.25.35)
data.DelPark$Age.36.45=data.DelPark$Age.36.45-mean(data.DelPark$Age.36.45)
data.DelPark$Age.46.55=data.DelPark$Age.46.55-mean(data.DelPark$Age.46.55)
data.DelPark$Age..56=data.DelPark$Age..56-mean(data.DelPark$Age..56)
data.DelPark$Gender.Male=data.DelPark$Gender.Male-mean(data.DelPark$Gender.Male)
data.DelPark$MainOccup.Hunter=data.DelPark$MainOccup.Hunter-mean(data.DelPark$MainOccup.Hunter)
data.DelPark$EdLevel.Primary=data.DelPark$EdLevel.Primary-mean(data.DelPark$EdLevel.Primary)
data.DelPark$EdLevel.Secondary=data.DelPark$EdLevel.Secondary-mean(data.DelPark$EdLevel.Secondary)
data.DelPark$DelPark=factor(data.DelPark$DelPark, levels=levels(data.DelPark$DelPark)[c(2, 3, 1)])
table(sel.data$VilDistPark, sel.data$Village)

full.DelPark=clmm(DelPark ~ Age + z.VilDistPark + MainOccup + Gender + EdLevel + z.TimeLCom +
  (1|Village) +
  (0 + Age.25.35|Village) +(0 + Age.36.45|Village) +(0 + Age.46.55|Village) +(0 + Age..56|Village) +
  (0 + EdLevel.Primary|Village) +(0 + EdLevel.Secondary|Village) +
  (0 + Gender.Male|Village) +
  (0 + MainOccup.Hunter|Village) +
  (0 + TimeLCom|Village) + 
  (1|EthGr) +
  (0 + z.TimeLCom|EthGr),
  data=data.DelPark)
null.DelPark=clmm(DelPark ~ 1 +
  (1|Village) +
  (0 + Age.25.35|Village) +(0 + Age.36.45|Village) +(0 + Age.46.55|Village) +(0 + Age..56|Village) +
  (0 + EdLevel.Primary|Village) +(0 + EdLevel.Secondary|Village) +
  (0 + Gender.Male|Village) +
  (0 + MainOccup.Hunter|Village) +
  (0 + TimeLCom|Village) + 
  (1|EthGr) +
  (0 + z.TimeLCom|EthGr),
  data=data.DelPark)
as.data.frame(anova(null.DelPark, full.DelPark))

tests.full.DelPark=as.data.frame(drop1(full.DelPark, test="Chisq"))

############################
round(summary(full.DelPark)$coefficients, 3)
boot.full.DelPark$ci.estimates
for (orig in boot.full.DelPark$ci.estimates) {print(round(orig),3)
  
}
exp(1.889)
exp(boot.full.DelPark$ci.estimates)

#############################################
## import in excel

df3 <- data.frame(round(summary(full.DelPark)$coefficients, 3))
write_xlsx(df3,"./Summary_DelPark.xlsx")
######################################
boot.full.PerChimpPopTrend$ci.estimates

DF<-boot.full.PerChimpPopTrend$ci.estimates
DFO<-exp(boot.full.PerChimpPopTrend$ci.estimates)
DF <- data.frame(boot.full.PerChimpPopTrend$ci.estimates)

for (orig in boot.full.PerChimpPopTrend$ci.estimates) {print(round(exp(orig),3))
  
}

########-----OpinionChimpProtection (13b):

table(sel.data$DelPark=="", sel.data$OpinionChimpProtection=="")
sort(unique(sel.data$OpinionChimpProtection))
xx.fe.re=fe.re.tab(fe.model="OpinionChimpProtection ~ Age + VilDistPark + MainOccup + 
                   Gender + EdLevel + TimeLCom", re="(1|Village) + (1|EthGr)", 
  data=droplevels(subset(sel.data, MainOccup!="trader")))
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary

data.OpinionChimpProtection=xx.fe.re$data
table(data.OpinionChimpProtection$OpinionChimpProtection)
data.OpinionChimpProtection$rv=as.numeric(data.OpinionChimpProtection$OpinionChimpProtection=="Good initiative")

data.OpinionChimpProtection$z.VilDistPark=as.vector(scale(data.OpinionChimpProtection$VilDistPark))
data.OpinionChimpProtection$z.TimeLCom=as.vector(scale(data.OpinionChimpProtection$TimeLCom))
data.OpinionChimpProtection$Age.25.35=data.OpinionChimpProtection$Age.25.35-mean(data.OpinionChimpProtection$Age.25.35)
data.OpinionChimpProtection$Age.36.45=data.OpinionChimpProtection$Age.36.45-mean(data.OpinionChimpProtection$Age.36.45)
data.OpinionChimpProtection$Age.46.55=data.OpinionChimpProtection$Age.46.55-mean(data.OpinionChimpProtection$Age.46.55)
data.OpinionChimpProtection$Age..56=data.OpinionChimpProtection$Age..56-mean(data.OpinionChimpProtection$Age..56)
data.OpinionChimpProtection$Gender.Male=data.OpinionChimpProtection$Gender.Male-mean(data.OpinionChimpProtection$Gender.Male)
data.OpinionChimpProtection$MainOccup.Hunter=data.OpinionChimpProtection$MainOccup.Hunter-mean(data.OpinionChimpProtection$MainOccup.Hunter)
data.OpinionChimpProtection$EdLevel.Primary=data.OpinionChimpProtection$EdLevel.Primary-mean(data.OpinionChimpProtection$EdLevel.Primary)
data.OpinionChimpProtection$EdLevel.Secondary=data.OpinionChimpProtection$EdLevel.Secondary-mean(data.OpinionChimpProtection$EdLevel.Secondary)
full.OpinionChimpProtection=glmer(OpinionChimpProtection ~ Age + z.VilDistPark + MainOccup + Gender + EdLevel + z.TimeLCom +
  (1 + Age.25.35+Age.36.45+Age.46.55+Age..56 + EdLevel.Primary+EdLevel.Secondary + Gender.Male + MainOccup.Hunter + z.TimeLCom||Village) + (1 + TimeLCom||EthGr),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.OpinionChimpProtection)
ranef.diagn.plot(full.OpinionChimpProtection)
null.OpinionChimpProtection=glmer(OpinionChimpProtection ~ 1 +
  (1 + Age.25.35+Age.36.45+Age.46.55+Age..56 + EdLevel.Primary+EdLevel.Secondary + Gender.Male + MainOccup.Hunter + z.TimeLCom||Village) + (1 + TimeLCom||EthGr),
  control=contr, family=binomial, data=data.OpinionChimpProtection)
as.data.frame(anova(null.OpinionChimpProtection, full.OpinionChimpProtection, test="Chisq"))

tests.OpinionChimpProtection=as.data.frame(drop1(full.OpinionChimpProtection, test="Chisq"))

round(summary(full.OpinionChimpProtection)$coefficients, 3)

### import in excel 
df1 <- data.frame(round(summary(full.OpinionChimpProtection)$coefficients, 3))
write_xlsx(df1,"./Summary_OpinionChimpProtection.xlsx")

######################################
boot.full.OpinionChimpProtection$ci.estimates
### import in excel
df2 <- data.frame(boot.full.OpinionChimpProtection$ci.estimates)
write_xlsx(df2,"./fullCoefs_OpinionChimpProtection.xlsx")

### response curve
visreg(full.OpinionChimpProtection, "MainOccup", ylab = "Probability of opinion", xlab = "MainOccup", scale = "response", cex.lab = 0.7, cex.axis = 0.6)

plot(x = data.OpinionChimpProtection$MainOccup,
     y = data.OpinionChimpProtection$OpinionChimpProtection ,
     xlab = "MainOccup",
     ylab = "Probability to have possitive opinion",
     cex.lab = 0.8, cex.axis = 0.7, xaxs='i', xlim=c(0, 12), ylim= c(0, 1))

lm3 <- lm(formula = OpinionChimpProtection ~ MainOccup,
              data = data.OpParkMagCom)
abline(lm3,
       col = "red", lwd = 1.5
       

       plot(x = sel.data$MainOccup,
            y = data.OpinionChimpProtection$OpinionChimpProtection ,
            xlab = "MainOccup",
            ylab = "Probability to have possitive opinion",
            cex.lab = 0.8, cex.axis = 0.7, xaxs='i', xlim=c(0, 12), ylim= c(0, 1))
       
       
       visreg(full.OpParkMagCom.woc, "Age", ylab = "Probability to be able to manage the park", xlab = "Age", scale = "response", cex.lab = 0.9, cex.axis = 0.8)
       
  plot(x = sel.data$OpinionChimpProtection,
            y = sel.data$EdLevel,
            xlab = "Opinion about chimpanzee protection",
            ylab = "Education level",
            cex.lab = 0.8, cex.axis = 0.7)
       
       abline(Age.lm2,
              col = "red", lwd = 1.5) 
       
       ggplot(sel.data, aes(x=OpinionChimpProtection, y = EdLevel),
              geom_point(aes(color = Gender, shape = Gender), size = 2)

              ggplot(sel.data, aes(x=OpinionChimpProtection, y= EdLevel)
              + geom_point(aes(color=Gender, shape=Gender),
                           size=2))
       
ggplot(sel.data, aes(x=OpinionChimpProtection, y=EdLevel)) + geom_point(aes(color=Gender, shape=Gender), size = 3) 
+ scale_color_brewer(palette="Dark2") + 
  ggtitle("population perception") + 
  xlab("opinion") +
  ylab("Education level") +
  scale_color_gradient(low="blue", high="yellow")


ggplot(sel.data, aes(x=PerChimpPopTrend, y=EdLevel)) + geom_boxplot(aes(fill=EdLevel)) + guides(fill="none") + theme_bw()

+ theme(axis.text.y=element_text(angle=90, hjust=0.5)) + axis.title.x=element_blank()

      
#############################
exp(boot.full.OpinionChimpProtection$ci.estimates)
df <- data.frame(exp(boot.full.OpinionChimpProtection$ci.estimates))
write_xlsx(df,"C:/Users/AT PN MPEM ET DJIM/Desktop/Mundry Analysis//OR_IC_OpinionChimpProtection.xlsx")

to.plot=boot.full.OpinionChimpProtection$ci.estimates

##estimates in link space

plot.model<-function(to.plot, vlat=0, mypar=list(mar=c(3, 7, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)){
  hll=0.2
  dev.off()
  par(mypar)
  plot(1, 1, type="n", ylim=c(nrow(to.plot), 1), xlim=range(to.plot, na.rm=T), xlab="estimate", ylab="", yaxt="n")
  segments(x0=to.plot[, 1], x1=to.plot[, 1], y0=(1:nrow(to.plot))-hll, y1=(1:nrow(to.plot))+hll)
  arrows(x0=to.plot[, 2], x1=to.plot[, 3], y0=(1:nrow(to.plot)), y1=(1:nrow(to.plot)), code=3, len=0.05, angle=90)
  mtext(at=1:nrow(to.plot), line=0.2, text=rownames(to.plot), adj=1, side=2)
  abline(v=0, lty=3)
}

to.plot=boot.full.OpinionChimpProtection$ci.estimates   
plot.model(to.plot=boot.full.OpinionChimpProtection$ci.estimates, vlat=1)
plot.model(to.plot=boot.full.OpinionChimpProtection$ci.estimates, vlat=0, 
           list(mar=c(3, 7.5, 0.2, 0.2), mgp=c(1.7, 0.3, 0),
                tcl=-0.15, las=1))

################################################################################################ChimpObsPlace (14):
table(sel.data$ChimpObsPlace)
xx.fe.re=fe.re.tab(fe.model="ChimpObsPlace ~ Age + VilDistPark + Gender + EdLevel + MainOccup + TimeLCom", re="(1|Village)", 
  data=droplevels(subset(sel.data, MainOccup!="trader" & ChimpObsPlace!="Savana" & ChimpObsPlace!="")))
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary
data.ChimpObsPlace=xx.fe.re$data

table(data.ChimpObsPlace$ChimpObsPlace)
data.ChimpObsPlace$rv=as.numeric(data.ChimpObsPlace$ChimpObsPlace=="Village")
data.ChimpObsPlace$z.VilDistPark=as.vector(scale(data.ChimpObsPlace$VilDistPark))
data.ChimpObsPlace$z.TimeLCom=as.vector(scale(data.ChimpObsPlace$TimeLCom))
data.ChimpObsPlace$Age.25.35=data.ChimpObsPlace$Age.25.35-mean(data.ChimpObsPlace$Age.25.35)
data.ChimpObsPlace$Age.36.45=data.ChimpObsPlace$Age.36.45-mean(data.ChimpObsPlace$Age.36.45)
data.ChimpObsPlace$Age.46.55=data.ChimpObsPlace$Age.46.55-mean(data.ChimpObsPlace$Age.46.55)
data.ChimpObsPlace$Age..56=data.ChimpObsPlace$Age..56-mean(data.ChimpObsPlace$Age..56)
data.ChimpObsPlace$Gender.Male=data.ChimpObsPlace$Gender.Male-mean(data.ChimpObsPlace$Gender.Male)
data.ChimpObsPlace$MainOccup.Hunter=data.ChimpObsPlace$MainOccup.Hunter-mean(data.ChimpObsPlace$MainOccup.Hunter)
data.ChimpObsPlace$EdLevel.Primary=data.ChimpObsPlace$EdLevel.Primary-mean(data.ChimpObsPlace$EdLevel.Primary)
data.ChimpObsPlace$EdLevel.Secondary=data.ChimpObsPlace$EdLevel.Secondary-mean(data.ChimpObsPlace$EdLevel.Secondary)

full.ChimpObsPlace=glmer(rv ~ Age + z.VilDistPark + Gender + EdLevel + MainOccup + z.TimeLCom + 
  (1 + Age.25.35+Age.36.45+Age.46.55+Age..56 + Gender.Male + EdLevel.Primary+EdLevel.Secondary + MainOccup.Hunter + z.TimeLCom||Village),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.OpinionChimpProtection)
ranef.diagn.plot(full.ChimpObsPlace)
null.ChimpObsPlace=glmer(rv ~ 1 + 
  (1 + Age.25.35+Age.36.45+Age.46.55+Age..56 + Gender.Male + EdLevel.Primary+EdLevel.Secondary + MainOccup.Hunter + z.TimeLCom||Village),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), family=binomial, data=data.OpinionChimpProtection)
as.data.frame(anova(null.ChimpObsPlace, full.ChimpObsPlace, test="Chisq"))

tests.full.ChimpObsPlace=as.data.frame(drop1(full.ChimpObsPlace, test="Chisq"))

#####################################################
##RelComPM (15):
table(sel.data$RelComPM)
xx.fe.re=fe.re.tab(fe.model="RelComPM ~ Age + Gender + EdLevel + MainOccup + TimeLCom", re="(1|Village)", 
  data=droplevels(subset(sel.data, MainOccup!="trader" & RelComPM!="")))
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary
data.RelComPM=xx.fe.re$data
data.RelComPM$z.TimeLCom=as.vector(scale(data.RelComPM$TimeLCom))
data.RelComPM$Age.25.35=data.RelComPM$Age.25.35-mean(data.RelComPM$Age.25.35)
data.RelComPM$Age.36.45=data.RelComPM$Age.36.45-mean(data.RelComPM$Age.36.45)
data.RelComPM$Age.46.55=data.RelComPM$Age.46.55-mean(data.RelComPM$Age.46.55)
data.RelComPM$Age..56=data.RelComPM$Age..56-mean(data.RelComPM$Age..56)
data.RelComPM$Gender.Male=data.RelComPM$Gender.Male-mean(data.RelComPM$Gender.Male)
data.RelComPM$MainOccup.Hunter=data.RelComPM$MainOccup.Hunter-mean(data.RelComPM$MainOccup.Hunter)
data.RelComPM$EdLevel.Primary=data.RelComPM$EdLevel.Primary-mean(data.RelComPM$EdLevel.Primary)
data.RelComPM$EdLevel.Secondary=data.RelComPM$EdLevel.Secondary-mean(data.RelComPM$EdLevel.Secondary)
data.RelComPM$RelComPM=factor(data.RelComPM$RelComPM, levels(data.RelComPM$RelComPM)[c(5, 2, 3, 1, 4)])
table(data.RelComPM$RelComPM)
full.RelComPM=clmm(RelComPM ~ Age + Gender + EdLevel + MainOccup + z.TimeLCom + 
  (1|Village) + (0+Age.25.35|Village)+ (0+Age.36.45|Village) + (0+Age.46.55|Village) + (0+Age..56|Village) + (0+Gender.Male|Village) + 
  (0+EdLevel.Primary|Village) + (0+EdLevel.Secondary|Village) + (0+MainOccup.Hunter|Village) + (0+z.TimeLCom|Village),
  data=data.RelComPM)
null.RelComPM=clmm(RelComPM ~ 1 + 
  (1|Village) + (0+Age.25.35|Village)+ (0+Age.36.45|Village) + (0+Age.46.55|Village) + (0+Age..56|Village) + (0+Gender.Male|Village) + 
  (0+EdLevel.Primary|Village) + (0+EdLevel.Secondary|Village) + (0+MainOccup.Hunter|Village) + (0+z.TimeLCom|Village),
  data=data.RelComPM)
as.data.frame(anova(null.RelComPM, full.RelComPM))

tests.full.RelComPM=as.data.frame(drop1(full.RelComPM, test="Chisq"))

########################################################

round(summary(full.RelComPM)$coefficients, 3)
boot.full.RelComPM$ci.estimates
for (orig in boot.full.RelComPM$ci.estimates) {print(round(orig),3)
  
}

#############################################
## import in excel

df4 <- data.frame(round(summary(full.RelComPM)$coefficients, 3))
write_xlsx(df4,"./Summary_RelComPM.xlsx")
######################################
boot.full.RelComPM$ci.estimates

DF<-boot.full.PerChimpPopTrend$ci.estimates
DFO<-exp(boot.full.PerChimpPopTrend$ci.estimates)
DF <- data.frame(boot.full.PerChimpPopTrend$ci.estimates)

for (orig in boot.full.RelComPM$ci.estimates) {print(round(exp(orig),3))
  
}
exp(-3.41530577)

#########################################################################
##OpPM (16):
table(sel.data$OpPM)
xx.fe.re=fe.re.tab(fe.model="OpPM ~ Age + Gender + EdLevel + MainOccup + TimeLCom", re="(1|Village)", 
  data=droplevels(subset(sel.data, MainOccup!="trader" & OpPM!="" & OpPM!="I can't judge")))
nrow(sel.data)
nrow(xx.fe.re$data)
xx.fe.re$summary
data.OpPM=xx.fe.re$data
data.OpPM$z.TimeLCom=as.vector(scale(data.OpPM$TimeLCom))
data.OpPM$Age.25.35=data.OpPM$Age.25.35-mean(data.OpPM$Age.25.35)
data.OpPM$Age.36.45=data.OpPM$Age.36.45-mean(data.OpPM$Age.36.45)
data.OpPM$Age.46.55=data.OpPM$Age.46.55-mean(data.OpPM$Age.46.55)
data.OpPM$Age..56=data.OpPM$Age..56-mean(data.OpPM$Age..56)
data.OpPM$Gender.Male=data.OpPM$Gender.Male-mean(data.OpPM$Gender.Male)
data.OpPM$MainOccup.Hunter=data.OpPM$MainOccup.Hunter-mean(data.OpPM$MainOccup.Hunter)
data.OpPM$EdLevel.Primary=data.OpPM$EdLevel.Primary-mean(data.OpPM$EdLevel.Primary)
data.OpPM$EdLevel.Secondary=data.OpPM$EdLevel.Secondary-mean(data.OpPM$EdLevel.Secondary)
data.OpPM$OpPM=factor(data.OpPM$OpPM, levels=levels(data.OpPM$OpPM)[c(4, 1, 2, 3, 5)])
full.OpPM=clmm(OpPM ~ Age + Gender + EdLevel + MainOccup + z.TimeLCom +
  (1|Village) + (0+Age.25.35|Village) + (0+Age.36.45|Village) + (0+Age.46.55|Village) + (0+Age..56|Village) + (0+Gender.Male|Village) + 
  (0+EdLevel.Primary|Village) + (0+EdLevel.Secondary|Village) + (0+MainOccup.Hunter|Village) + (0+z.TimeLCom|Village),
  data=data.OpPM)
null.OpPM=clmm(OpPM ~ 1 +
  (1|Village) + (0+Age.25.35|Village) + (0+Age.36.45|Village) + (0+Age.46.55|Village) + (0+Age..56|Village) + (0+Gender.Male|Village) + 
  (0+EdLevel.Primary|Village) + (0+EdLevel.Secondary|Village) + (0+MainOccup.Hunter|Village) + (0+z.TimeLCom|Village),
  data=data.OpPM)
as.data.frame(anova(null.OpPM, full.OpPM))

tests.full.OpPM=as.data.frame(drop1(full.OpPM, test="Chisq"))
save.image("./ernest_workspace.RData")
load("./ernest_workspace.RData")

##Plottings again binomial models:
####################################################################################################################################################
load("~/ernest/ernest_workspace.RData")
library(lme4)
library(kyotil)
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
##1:
stab.full.ChimpRecog.woc=glmm.model.stab(model.res=full.ChimpRecog.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.ChimpRecog.woc=boot.lmer(m=full.ChimpRecog.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/roger/ernest_workspace.RData")
boot.full.ChimpRecog.woc$ci.estimates
##2:
stab.full.ChimpObs.woc=glmm.model.stab(model.res=full.ChimpObs.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.ChimpObs.woc=boot.lmer(m=full.ChimpObs.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
boot.plot.full.ChimpObs.woc.gender=boot.lmer(m=plot.full.ChimpObs.woc.gender, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use="Gender", circ.var.name=NULL, circ.var=NULL, use.u=F)
boot.plot.full.ChimpObs.woc.edlevel=boot.lmer(m=plot.full.ChimpObs.woc.edlevel, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use="EdLevel", circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

##3:
stab.full.ChimpName.woc=glmm.model.stab(model.res=full.ChimpName.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.ChimpName.woc=boot.lmer(m=full.ChimpName.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

##4:
stab.full.ChimpCulTabous.woc=glmm.model.stab(model.res=full.ChimpCulTabous.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.ChimpCulTabous.woc=boot.lmer(m=full.ChimpCulTabous.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

##5:
stab.full.ChimpHunting.woc=glmm.model.stab(model.res=full.ChimpHunting.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.ChimpHunting.woc=boot.lmer(m=full.ChimpHunting.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
boot.plot.full.ChimpHunting.woc.gender=boot.lmer(m=plot.full.ChimpHunting.woc.gender, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use="Gender", circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

##6:
stab.full.EatChimp.woc=glmm.model.stab(model.res=full.EatChimp.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.EatChimp.woc=boot.lmer(m=full.EatChimp.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

##7:
stab.full.ChimpDeseases.woc=glmm.model.stab(model.res=full.ChimpDeseases.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.ChimpDeseases.woc=boot.lmer(m=full.ChimpDeseases.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

##8:
stab.full.KnowParkLimit.woc=glmm.model.stab(model.res=full.KnowParkLimit.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.KnowParkLimit.woc=boot.lmer(m=full.KnowParkLimit.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
boot.plot.full.KnowParkLimit.woc.num.child=boot.lmer(m=plot.full.KnowParkLimit.woc.num.child, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use="z.NumChild.", circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

##9:
stab.full.LawPoachingChimp.woc=glmm.model.stab(model.res=full.LawPoachingChimp.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.LawPoachingChimp.woc=boot.lmer(m=full.LawPoachingChimp.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

##10a:
stab.full.OpParkMagCom.woc=glmm.model.stab(model.res=full.OpParkMagCom.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.OpParkMagCom.woc=boot.lmer(m=full.OpParkMagCom.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

##10b:
stab.full.OpChimpExtinction.woc=glmm.model.stab(model.res=full.OpChimpExtinction.woc, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.OpChimpExtinction.woc=boot.lmer(m=full.OpChimpExtinction.woc, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")


##binomial models:
####################################################################################################################################################
##13b:
stab.full.OpinionChimpProtection=glmm.model.stab(model.res=full.OpinionChimpProtection, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.OpinionChimpProtection=boot.lmer(m=full.OpinionChimpProtection, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

##14:
stab.full.ChimpObsPlace=glmm.model.stab(model.res=full.ChimpObsPlace, contr=contr, ind.cases=F, para=T, data=NULL, use=NULL, 
	n.cores=100)
boot.full.ChimpObsPlace=boot.lmer(m=full.ChimpObsPlace, discard.warnings=F, nboots=1000, para=T, resol=1000, level=0.95, 
	n.cores=100, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F)
save.image("~/ernest/ernest_workspace.RData")

####################################################################################################################################################
##ordinal models:
####################################################################################################################################################
##11:
stab.full.LastDateChimpObs=clmm.stab(mres=full.LastDateChimpObs, data=data.LastDateChimpObs, formula="LastDateChimpObs ~ Age + z.VilDistPark + MainOccup + (1|EthGr)", 
  para=T, n.cores=100, contr=NULL, nAGQ=1L)
boot.full.LastDateChimpObs=clmm_boot_fun(m=full.LastDateChimpObs, n.boots=1000, use.u=F, use=NULL, para=T, n.cores=100, level=0.95, resol=1000, return.ind.boots=F)
save.image("~/ernest/ernest_workspace.RData")

##12:
library(ordinal)
stab.full.PerChimpPopTrend=clmm.stab(mres=full.PerChimpPopTrend, data=data.PerChimpPopTrend, 
  formula="PerChimpPopTrend ~ Age + z.VilDistPark + MainOccup + Gender + EdLevel + LastDateChimpObs +  (1|Village) + (0 + Age.25.35|Village) + (0 + Age.36.45|Village) +(0 + Age.46.55|Village) + (0 + Age..56|Village) + (0 + EdLevel.Primary|Village) + (0 + EdLevel.Secondary|Village) + (0 + Gender.Male|Village) + (0 + LastDateChimpObs.2016|Village) + (0 + LastDateChimpObs.2017|Village) + (0 + LastDateChimpObs.2018|Village) + (0 + LastDateChimpObs.2019|Village) + (0 + LastDateChimpObs.2020|Village) + (0 + LastDateChimpObs.2021|Village) + (0 + MainOccup.Hunter|Village) + (1|EthGr)",
  para=T, n.cores=100, contr=NULL, nAGQ=1L)
boot.full.PerChimpPopTrend=clmm_boot_fun(m=full.PerChimpPopTrend, n.boots=1000, use.u=F, use=NULL, para=T, n.cores=100, level=0.95, resol=1000, return.ind.boots=F)
save.image("~/ernest/ernest_workspace.RData")

##13a:
stab.full.DelPark=clmm.stab(mres=full.DelPark, data=data.DelPark, 
  formula="DelPark ~ Age + z.VilDistPark + MainOccup + Gender + EdLevel + z.TimeLCom + (1|Village) + (0 + Age.25.35|Village) + (0 + Age.36.45|Village) + (0 + Age.46.55|Village) (0 + Age..56|Village) + (0 + EdLevel.Primary|Village) (0 + EdLevel.Secondary|Village) + (0 + Gender.Male|Village) + (0 + MainOccup.Hunter|Village) + (0 + TimeLCom|Village) +  (1|EthGr) + (0 + z.TimeLCom|EthGr)", 
  para=T, n.cores=100, contr=NULL, nAGQ=1L)
boot.full.DelPark=clmm_boot_fun(m=full.DelPark, n.boots=1000, use.u=F, use=NULL, para=T, n.cores=100, level=0.95, resol=1000, return.ind.boots=F)
save.image("~/ernest/ernest_workspace.RData")

####################################################################################################################################################
##ordinal models:
####################################################################################################################################################
##15:
stab.full.RelComPM=clmm.stab(mres=full.RelComPM, data=data.RelComPM, 
  formula="RelComPM ~ Age + Gender + EdLevel + MainOccup + z.TimeLCom +  (1|Village) + (0+Age.25.35|Village)+ (0+Age.36.45|Village) + (0+Age.46.55|Village) + (0+Age..56|Village) + (0+Gender.Male|Village) +  (0+EdLevel.Primary|Village) + (0+EdLevel.Secondary|Village) + (0+MainOccup.Hunter|Village) + (0+z.TimeLCom|Village)",
  para=T, n.cores=100, contr=NULL, nAGQ=1L)
boot.full.RelComPM=clmm_boot_fun(m=full.RelComPM, n.boots=1000, use.u=F, use=NULL, para=T, n.cores=100, level=0.95, resol=1000, return.ind.boots=F)
save.image("~/ernest/ernest_workspace.RData")

##16:
stab.full.OpPM=clmm.stab(mres=full.OpPM, data=data.OpPM, 
  formula="OpPM ~ Age + Gender + EdLevel + MainOccup + z.TimeLCom + (1|Village) + (0+Age.25.35|Village) + (0+Age.36.45|Village) + (0+Age.46.55|Village) + (0+Age..56|Village) + (0+Gender.Male|Village) +  (0+EdLevel.Primary|Village) + (0+EdLevel.Secondary|Village) + (0+MainOccup.Hunter|Village) + (0+z.TimeLCom|Village)", para=T, n.cores=100, contr=NULL, nAGQ=1L)
boot.full.OpPM=clmm_boot_fun(m=full.OpPM, n.boots=1000, use.u=F, use=NULL, para=T, n.cores=100, level=0.95, resol=1000, return.ind.boots=F)
save.image("~/ernest/ernest_workspace.RData")


##plotting:

hll=0.2

to.plot=aggregate(data.ChimpObs$ChimpObs, data.ChimpObs[, c("EthGr", "Gender"), drop=F], mean)
to.plot$n=aggregate(data.ChimpObs$ChimpObs, data.ChimpObs[, c("EthGr", "Gender"), drop=F], length)$x
for.segm=tapply(data.ChimpObs$ChimpObs, data.ChimpObs[, c("EthGr", "Gender"), drop=F], mean)
X11(width=4, height=4)
par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
plot(as.numeric(to.plot$Gender), to.plot$x, cex=sqrt(to.plot$n), pch=19, col=grey(level=0.4, alpha=0.4), type="n",
  xlim=c(0.5, 2.5), xaxs="i", xaxt="n", xlab="Gender", ylab="Probablity to have seen a chimpanzee")
mtext(text=colnames(for.segm), at=1:ncol(for.segm), line=0.2, side=1)
segments(x0=1, x1=2, y0=for.segm[, 1], y1=for.segm[, 2], lty=2)
points(as.numeric(to.plot$Gender), to.plot$x, cex=sqrt(to.plot$n), pch=19, col="white")
points(as.numeric(to.plot$Gender), to.plot$x, cex=sqrt(to.plot$n), pch=19, col=grey(level=0.4, alpha=0.4))
ci=boot.plot.full.ChimpObs.woc.gender$ci.fitted
segments(x0=(1:2)-hll, x1=(1:2)+hll, y0=ci$fitted, y1=ci$fitted, lwd=2)
arrows(x0=(1:2), x1=(1:2), y0=ci$lwr, y1=ci$upr, code=3, angle=90, len=0.05)
savePlot(file="./ernest/FIG_chimpObs_by_gender.png", type="png")
dev.copy2pdf(file="./ernest/FIG_chimpObs_by_gender.pdf")

to.plot=aggregate(data.ChimpObs$ChimpObs, data.ChimpObs[, c("EthGr", "EdLevel"), drop=F], mean)
to.plot$n=aggregate(data.ChimpObs$ChimpObs, data.ChimpObs[, c("EthGr", "EdLevel"), drop=F], length)$x
for.segm=tapply(data.ChimpObs$ChimpObs, data.ChimpObs[, c("EthGr", "EdLevel"), drop=F], mean)
dev.off()
X11(width=4, height=4)
par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
plot(as.numeric(to.plot$EdLevel), to.plot$x, cex=sqrt(to.plot$n), pch=19, col=grey(level=0.4, alpha=0.4), type="n",
  xlim=c(0.5, 3.5), xaxs="i", xaxt="n", xlab="Education level", ylab="Probablity to have seen a chimpanzee")
mtext(text=colnames(for.segm), at=1:ncol(for.segm), line=0.2, side=1)
segments(x0=1, x1=2, y0=for.segm[, 1], y1=for.segm[, 2], lty=2)
segments(x0=2, x1=3, y0=for.segm[, 1], y1=for.segm[, 2], lty=2)
points(as.numeric(to.plot$EdLevel), to.plot$x, cex=sqrt(to.plot$n), pch=19, col="white")
points(as.numeric(to.plot$EdLevel), to.plot$x, cex=sqrt(to.plot$n), pch=19, col=grey(level=0.4, alpha=0.4))
ci=boot.plot.full.ChimpObs.woc.edlevel$ci.fitted
segments(x0=(1:3)-hll, x1=(1:3)+hll, y0=ci$fitted, y1=ci$fitted, lwd=2)
arrows(x0=(1:3), x1=(1:3), y0=ci$lwr, y1=ci$upr, code=3, angle=90, len=0.05)
savePlot(file="./ernest/FIG_chimpObs_by_EdLevel.png", type="png")
dev.copy2pdf(file="-/ernest/FIG_chimpObs_EdLevelr.pdf")

to.plot=aggregate(data.ChimpHunting$ChimpHunting, data.ChimpHunting[, c("EthGr", "Gender"), drop=F], mean)
to.plot$n=aggregate(data.ChimpHunting$ChimpHunting, data.ChimpHunting[, c("EthGr", "Gender"), drop=F], length)$x
for.segm=tapply(data.ChimpHunting$ChimpHunting, data.ChimpHunting[, c("EthGr", "Gender"), drop=F], mean)
dev.off()
X11(width=4, height=4)
par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1)
plot(as.numeric(to.plot$Gender), to.plot$x, cex=sqrt(to.plot$n), pch=19, col=grey(level=0.4, alpha=0.4), type="n",
  xlim=c(0.5, 2.5), xaxs="i", xaxt="n", xlab="Gender", ylab="Probablity to hunt chimpanzees")
mtext(text=colnames(for.segm), at=1:ncol(for.segm), line=0.2, side=1)
segments(x0=1, x1=2, y0=for.segm[, 1], y1=for.segm[, 2], lty=2)
points(as.numeric(to.plot$Gender), to.plot$x, cex=sqrt(to.plot$n), pch=19, col="white")
points(as.numeric(to.plot$Gender), to.plot$x, cex=sqrt(to.plot$n), pch=19, col=grey(level=0.4, alpha=0.4))
ci=boot.plot.full.ChimpHunting.woc.gender$ci.fitted
segments(x0=(1:2)-hll, x1=(1:2)+hll, y0=ci$fitted, y1=ci$fitted, lwd=2)
arrows(x0=(1:2), x1=(1:2), y0=ci$lwr, y1=ci$upr, code=3, angle=90, len=0.05)
savePlot(file="./ernest/FIG_ChimpHunting_by_gender.png", type="png")
dev.copy2pdf(file="./ernest/FIG_ChimpHunting_by_gender.pdf")

source("ernest/r_functions/no_int_plot.r")
to.plot=aggregate(data.KnowParkLimit$KnowParkLimit, data.KnowParkLimit[, c("z.NumChild.", "EthGr")], mean)
to.plot$n=aggregate(data.KnowParkLimit$KnowParkLimit, data.KnowParkLimit[, c("z.NumChild.", "EthGr")], length)$x
range(data.KnowParkLimit$NumChild)
x.labels=0:12
x.at=(x.labels-mean(data.KnowParkLimit$NumChild))/sd(data.KnowParkLimit$NumChild)
dev.off()
X11(height=5)
no.int.plot(
	plot.data=to.plot, covariate="z.NumChild.", coefs=NULL, response=to.plot$x, link="logit",
  grid.resol=NA, 
	xlab="Number children", ylab="Probability to know the park limits", x.at=x.at, x.labels=x.labels, y.at=NULL, y.labels=NULL, xlim=NULL, ylim=c(0, 1),
	cex.lab=1, cex.axis=1, size.fac=1, col=grey(level=0.4, alpha=0.4), pch=19, conf.int=boot.plot.full.KnowParkLimit.woc.num.child$ci.fitted, ci.type="area", ci.col="grey", lty=2, lwd=1, pt.lwd=1,
	my.par=list(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1), quiet=T, reset.par=F, mean.fun=mean,
	weights=to.plot$n)
lines(x=boot.plot.full.KnowParkLimit.woc.num.child$ci.fitted$z.NumChild., y=boot.plot.full.KnowParkLimit.woc.num.child$ci.fitted$fitted, lty=2)
savePlot(file="./ernest/FIG_KnowParkLimit_by_NumChild.png", type="png")
dev.copy2pdf(file="./ernest/FIG_KnowParkLimit_by_NumChild.pdf")

save.image("./ernest/ernest_workspace.RData")
load("./ernest/ernest_workspace.RData")

###Exploration

A<-as.factor(sel.data$ChimpRecog)
A
B<-sel.data$LastDateChimpObs
B
str(B)
plot(B, x=LastDateChimpObs, Y= ChimpRecog)
c<-C(A, B)
levels(C)
str(C)
LastDateChimpObs<-as.factor(sel.data$LastDateChimpObs)
str(B2)
levels(B2)

plot(B2, A, type="l", lwd=3, col="dodgerblue")

names(sel.data)
ggplot(sel.data3, aes(x=as.factor(sel.data3$LastDateChimpObs), y= as.factor(sel.data3$ChimpRecog))) + 
  geom_point(aes(color= Village, shape=Village)) +
  scale_color_brewer(palette="Dark2") +
  ggtitle("variation") + 
  xlab("Last date of chimp obs") + ylab("observation") +
  theme_bw() + 
  theme(axis.text.y=element_text(angle=90, hjust=0.5)) # turn y-axis text by 90 degrees and adjust horizontally by 0.5


sel.data3=subset(sel.data, ChimpRecog!="NA")
sel.data3=subset(sel.data, LastDateChimpObs!="I don't remember")

ggplot(data = sel.data,
       mapping = aes(x=as.factor(sel.data$LastDateChimpObs), y= as.factor(sel.data$ChimpRecog))) +
  geom_boxplot()

ggplot(data = sel.data, aes(x = as.factor(sel.data$LastDateChimpObs),
                        y = as.factor(sel.data$ChimpRecog),
                        fill = as.factor(sel.data$Village))) +
  geom_boxplot()


## good plot

par(mfrow = c())
# according to village
# remove all NA
sel.dataNA <- na.omit(sel.data)

plot(x =as.factor(sel.data$Village), 
      y =  as.factor(sel.data$ChimpRecog), type="l", lwd=2, pch=1, cex=2, col="dodgerblue", xlab="Village", ylab="Chimpanzee identification")

# main occupation
plot(x =as.factor(sel.data$MainOccup), 
     y =  as.factor(sel.data$ChimpRecog), type="l", lwd=2, pch=1, cex=2, col="dodgerblue", xlab="Main occupation", ylab="Chimpanzee identification")


#number of child
ggplot(data = sel.data, aes(x = as.factor(sel.data$Village), y = sel.data$NumChild.,
fill = LastDateChimpObs), size=1) + geom_boxplot() + xlab("Village") + ylab("Number of child") 

#significant diff
#timeLCom
Sel.dataNA <- na.omit(sel.data)
par(mfrow=c(2, 2))
par(mar = c(5.1, 4.1, 4.1, 2.1))  

Sel.dataN <- na.omit(sel.data)
sel=subset(sel.data, LastDateChimpObs!="NA")
ggplot(data = sel, 
       aes(x = as.factor(sel$Village), 
           y = sel$TimeLCom,
           fill = LastDateChimpObs), cex.lab=1.5) + geom_boxplot() + xlab("Village") + ylab("Time spend in the community") + 
  theme(text = element_text(size=8),                      axis.text.x = element_text(angle=90, hjust=1))


par(mfrow=c(2, 2))
par(mar = c(4, 4, 2, 1.5))
#Gender
plot(x =as.factor(sel.data$Gender), 
     y =  as.factor(sel.data$ChimpRecog), type="l", lwd=2, pch=1, cex=2, col="dodgerblue", xlab="Gender", ylab="Chimpanzee identification")

#ethnic group
par(mar=c(3.5, 3.5, 2, 1), mgp=c(2.4, 0.8, 0))

plot(x =as.factor(sel.data$EthGr), 
     y =  as.factor(sel.data$ChimpRecog), col="dodgerblue", xlab="Ethnic group", ylab="Chimpanzee identification", cex=0.2)

#age
plot(x =as.factor(sel.data$Age), 
     y =  as.factor(sel.data$ChimpRecog), type="l", lwd=2, pch=1, cex=2, col="dodgerblue", xlab="Age", ylab="Chimpanzee identification")


# Edlevel
ggplot(data = sel.dataNA,
       aes(x = as.factor(sel.dataNA$Village), y = as.factor(sel.dataNA$EdLevel), fill = LastDateChimpObs), 
       size=2) + geom_boxplot()
+ xlab("Village") + ylab("Education level") 


ggplot(data = sel, 
       aes(x = as.factor(sel$Village), 
           y = as.factor(sel$ChimpObsPlace),
           cex.lab=1.5)) + geom_point() + xlab("Village") + ylab("Place of chimpanzee sigthing") + 
  theme(text = element_text(size=8),                      axis.text.x = element_text(angle=90, hjust=1))
hist(as.numeric(sel$ChimpObsPlace)) 


ggplot(data = sel, 
       aes(x = as.factor(sel$TimeLCom), 
      y =  as.factor(sel$PerChimpPopTren),
      cex.lab=1.5)) + geom_boxplot() + xlab("Village") + ylab("Time spend in the community") + 
  theme(text = element_text(size=8),                      axis.text.x = element_text(angle=90, hjust=1))

#fit linear model with time living in community
lmTL <- lm(EatChimp ~ TimeLCom,
               data = sel)
print(lmTL)
dev.off()
par(mfrow = c(1, 1))
plot(lmTL)

TLC <- ggplot(data = sel,
                 mapping = aes(x = TimeLCom, y = EatChimp)) +
  geom_point(shape = 21,
             fill = "tomato",
             size = 3,
             alpha = 0.25) +
  labs(x = " Time living in the community (years)",
       y = "Probability to eat chimpanzee")

TLC + geom_abline(intercept = coef(lmTL)[1],
                  slope = coef(lmTL)[2])

# fitting model age 

lmAge <- lm(ChimpObs ~ Age,
           data = sel)

Gage <- ggplot(data = sel,
              mapping = aes(x = as.numeric(sel$Age)), y = ChimpObs) +
  geom_point(x = as.numeric(sel$Age), 
             y = sel$ChimpObs,
    shape = 21,
             fill = "tomato",
             size = 3,
             alpha = 0.25) +
  labs(x = " Age of respondent(years)",
       y = "Probability to have seen chimpanzee")
Gage

Gage + geom_abline(intercept = coef(lmAge)[1],
                  slope = coef(lmAge)[2])



# plot Opinion vs ED level
ggplot(data = sel,
               mapping = aes(x = as.factor(sel$EdLevel)), y = as.factor(sel$OpinionChimpProtection)) +
  geom_point(x = as.factor(sel$EdLevel), 
             y = as.factor(sel$OpinionChimpProtection),
             shape = 21,
             fill = "tomato",
             size = 3,
             alpha = 0.25) +
  labs(x = "Opinion about chimpanzee protection)",
       y = "Education level")


plot(x =as.factor(sel$EdLevel), 
    y =  as.factor(sel$OpinionChimpProtection), type="b", lwd=2, pch=1, cex=2, 
col="dodgerblue", 
xlab="Opinion about chimpanzee protection)", 
ylab="Education level")


ggplot(data = sel, 
       aes(x = as.factor(sel$OpinionChimpProtection), 
           y = as.factor(sel$EdLevel),
           fill =as.factor(sel$Gender), cex.lab=1.5)) + geom_point() + xlab("Respondent opinion") + ylab("Education level") + 
  scale_color_brewer(palette="Dark2") +
  theme(text = element_text(size=8),                      axis.text.x = element_text(angle=90, hjust=1))

# good plot for Respondent opinion according to chimp opinion
ggplot(sel, aes(x=OpinionChimpProtection, y=EdLevel)) +
  geom_point(aes(color=Gender, 
  shape=Gender), size = 2) + 
  scale_color_brewer(palette="Dark2") +
  xlab("Respondent opinion") + 
  ylab("Education level") +
  theme_bw() + 
  theme(axis.text.y=element_text(angle=90, hjust=0.5))

#### probability to be able to manage the park according to age

ggplot(data = sel, 
       aes(x = as.factor(sel$Age), 
           y = as.factor(sel$EdLevel),
           fill =as.factor(sel$Gender), cex.lab=1.5)) + geom_point() + xlab("Respondent opinion") + ylab("Education level") + 
  scale_color_brewer(palette="Dark2") +
  theme(text = element_text(size=8),                      axis.text.x = element_text(angle=90, hjust=1))

dev.off()
# response curve prob to be able to mange the park by the community
visreg(full.OpParkMagCom.woc, "Age", ylab = "Probability to be able to manage the park", xlab = "Age", scale = "response", cex.lab = 0.8, cex.axis = 0.8)


# probability have seen a chimp according to age
visreg(full.ChimpObs.woc, "Age",
       ylab = "Probability to have seen a chimpanzee",
       xlab = "Age group of respondent",
       scale = "response", cex.lab=0.8, cex.axis = 0.8)

# boxplot 

ggplot(data = sel, 
       aes(x = as.factor(sel$Village), 
           y = as.factor(sel$CostDamageCFA),
           fill =as.factor(sel$Gender), cex.lab=1.5)) + geom_boxplot() + xlab("village") + ylab("Estimated cost of damage") + 
  scale_color_brewer(palette="Dark2") +
  theme(text = element_text(size=8),                      axis.text.x = element_text(angle=90, hjust=1))


B<-sel$CostDamageCFA
B
hist(B)

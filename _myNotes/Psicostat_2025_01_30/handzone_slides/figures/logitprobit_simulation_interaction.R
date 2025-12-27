
####################################

#rm(list=ls())
library(ggplot2)
library(scales)
library(gridExtra)
library(glmmTMB)
plogis_trans <- function() {
  trans_new("plogis", transform = plogis, inverse = qlogis)
}
probit_trans <- function() {
  trans_new("probit", transform = pnorm, inverse = qnorm)
}

####################################

# simulation for false positives

trueLF = "probit"
usedLF = "logit"
niter = 200

N = 200
k = 20

b0 = 1.5
b1 = -0.7
b2 = -0.8

pval = rep(NA,niter)
for(i in 1:niter){
  id = rep(1:N,each=k*2)
  rInt = rep(rnorm(N,0,1),each=k*2)
  group = rep(0:1,each=k*N)
  condition = rep(0:1,each=k,times=N)
  
  yLin = b0 + b1*group + b2*condition
  if(trueLF=="logit") yProb = plogis(yLin)
  if(trueLF=="probit") yProb = pnorm(yLin)
  y = rbinom(length(yLin),1,yProb)
  df = data.frame(y,id,group,condition)
  
  fit = glmmTMB(y ~ group*condition, data=df, family=binomial(link=usedLF))
  pval[i] = summary(fit)$coefficients$cond["group:condition","Pr(>|z|)"]
}
mean(pval<0.05)

group = as.factor(group)
condition = as.factor(condition)
ggplot(data=data.frame(),aes(x=condition,y=yProb,group=group,color=group,shape=group,linetype=group))+
  geom_point(size=3)+
  geom_line(linewidth=1)

####################################

#### ICC
library(performance)
trueLF = "probit"
usedLF = "probit"
N = 5000
k = 20
b0 = 1.5
b1 = -0.7
b2 = -0.8
id = rep(1:N,each=k*2)
rInt = rep(rnorm(N,0,1),each=k*2)
group = rep(0:1,each=k*N)
condition = rep(0:1,each=k,times=N)
yLin = b0 + b1*group + b2*condition + rInt
if(trueLF=="logit") yProb = plogis(yLin)
if(trueLF=="probit") yProb = pnorm(yLin)
y = rbinom(length(yLin),1,yProb)
df = data.frame(y,id,group,condition)
fit = glmmTMB(y ~ group*condition+(1|id), data=df, family=binomial(link=usedLF))
icc(fit)
  
####################################

# just plot 

b0 = 1.5
b1 = -0.2
b2 = -0.9
s1 = expand.grid(group=c(0,1),condition=c(0,1))
s1$yLin = b0 + b1*s1$group + b2*s1$condition
s1$prob = pnorm(s1$yLin)
s1$group = as.factor(s1$group)
s1$condition = as.factor(s1$condition)
lines_at = seq(-5,5,.5)
fs = 30
gg = ggplot(s1,aes(x=condition,y=prob,group=group,shape=group,color=group,linetype=group)) +
  geom_point(size=5) +
  geom_line(linewidth=1.5)
gg1 = gg + 
  geom_hline(yintercept=pnorm(lines_at),color="darkgray") +
  ggtitle("link='probit',\n no interaction") + 
  theme(text=element_text(size=fs), title=element_text(size=fs*.7), axis.title=element_text(size=fs))
gg2 = gg + 
  geom_hline(yintercept=seq(0,1,length.out=10),color="darkgray") +
  ggtitle("link='identity',\n negative interaction") + 
  theme(text=element_text(size=fs), title=element_text(size=fs*.7), axis.title=element_text(size=fs))
gg3 = gg + 
  geom_hline(yintercept=plogis(lines_at),color="darkgray") +
  ggtitle("link='logit',\n positive interaction") + 
  theme(text=element_text(size=fs), title=element_text(size=fs*.7), axis.title=element_text(size=fs))

grid.arrange(gg1,gg2,gg3,ncol=3)

####################################

# MAFC
library(psyphy)

N = 50000
b0 = -1.7
b1 = 1.2
b2 = 1.2
b12 = -0.2
group = rbinom(N,1,.5)
cond = rbinom(N,1,.5)
X = b0+b1*group+b2*cond+b12*group*cond
y = rbinom(N,1,mafc.probit(2)$linkinv(X))
df = data.frame(group,cond,y)
fitP = glm(y~group*cond,data=df,family=binomial(link="probit"))
summary(fitP)
fitL = glm(y~group*cond,data=df,family=binomial(link="logit"))
summary(fitL)
fitM = glm(y~group*cond,data=df,family=binomial(link=mafc.probit(2)))
summary(fitM)

####################################





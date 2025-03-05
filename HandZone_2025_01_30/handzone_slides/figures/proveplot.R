
################################

library(ggplot2)
library(effects)

################################

# 2 x 2 categories

N = 5e4
A = rbinom(N,1,.5)
B = rbinom(N,1,.5)

b0 = 1
b1 = 1
b2 = 0.5

yLin = b0 + A*b1 + B*b2
shape=5
rt = rgamma(N,shape=shape,scale=20*exp(yLin))
df = data.frame(rt,A,B)
condition = paste(paste0("A",A),paste0("B",B))
df$condition = as.factor(condition)
df$A = as.factor(df$A)
df$B = as.factor(df$B)
ggplot(df)+
  geom_density(aes(x=rt,group=condition,color=condition),linewidth=1.5)+
  theme(text=element_text(size=24))+
  coord_cartesian(xlim=c(min(df$rt),quantile(df$rt,.9975)))

fit = glm(rt~A*B,data=df,family=gaussian(link="log"))
summary(fit)
eff = data.frame(allEffects(fit)$"A:B")
ggplot(eff,aes(x=A,y=fit,group=B,color=B,shape=B,linetype=B))+
  geom_point(size=6)+
  geom_line(linewidth=1.5)+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=0)+
  theme(text=element_text(size=24))+
  ylab("rt")

fit_log = glm(rt~A*B,data=df,family=Gamma(link="log"))
fit_id = glm(rt~A*B,data=df,family=Gamma(link="identity"))

nd <- expand.grid(A = c("0","1"), B = c("0","1"))
nd$p_log <- predict(fit_log, newdata = nd, type = "response")
nd$p_id <- predict(fit_id, newdata = nd, type = "response")


df$sim = simulate(fit)$sim_1
ggplot(df)+
  geom_density(aes(x=rt,group=condition,color=condition),linewidth=1)+
  geom_density(aes(x=sim,group=condition,fill=condition),color=NA,alpha=.4)+
  theme(text=element_text(size=24))

################################

# continuous predictor

N = 1e4
b0 = 0.5
b1 = 1
b2 = 0.5

group = rbinom(N,1,.5)
x = runif(N,0.5,3.5)
yLin = b0 + b1*group + b2*x
shape = 5
rt = rgamma(N,shape=shape,scale=exp(yLin)/shape)
df = data.frame(rt,x,group)
df$group = as.factor(df$group)

fit = glm(rt~group*x,data=df,family=Gamma(link="identity"))
#fit = glm(rt~group*x,data=df,family=Gamma(link="log"))
summary(fit)

eff = data.frame(allEffects(fit,xlevels=list(x=seq(0.5,3.5,.1)))$"group:x")
ggplot(eff,aes(x=x,group=group,color=group,y=fit))+
  geom_point(data=df,aes(x=x,y=rt),size=1,alpha=.2)+
  geom_line(linewidth=1)+
  coord_cartesian(ylim=c(min(df$rt),quantile(df$rt,.999)))

df$sim = simulate(fit)$sim_1
ggplot(df)+
  geom_density(aes(x=rt,group=group,color=group),linewidth=1)+
  geom_density(aes(x=sim,group=group,fill=group),color=NA,alpha=.4)
# plot(df$rt, df$sim)

####

N = 1e4
b0 = 0.5
b1 = 1
b2 = 0.5

niter = 10
deltaLog = rep(NA,niter)
deltaId = rep(NA,niter)
for(i in 1:niter){
  group = rbinom(N,1,.5)
  x = runif(N,0.5,3.5)
  yLin = b0 + b1*group + b2*x
  shape = 5
  rt = rgamma(N,shape=shape,scale=yLin/shape)
  df = data.frame(rt,x,group)
  df$group = as.factor(df$group)
  
  fitId = glm(rt~group*x,data=df,family=Gamma(link="identity"))
  fitLog = glm(rt~group*x,data=df,family=Gamma(link="log"))
  deltaLog[i] = cv.glm(df,fitLog,K=10)$delta[2]
  deltaId[i] = cv.glm(df,fitId,K=10)$delta[2]
}
hist(deltaLog-deltaId)

################################



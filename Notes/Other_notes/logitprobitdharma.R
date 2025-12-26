
###############################################

library(DHARMa)
library(lme4)

###############################################

N = 1e4
id = rep(1:100,each=100)
x = rnorm(N,0,1)
y = rbinom(N,1,plogis(1+x*.4+rep(rnorm(100,0,1),each=100)))
df = data.frame(x,y,id)

fit = glmer(y~x+(1|id),family=binomial(link="probit"))

testUniformity(fit)

###############################################

N = 1e4
id = rep(1:100,each=100)
x = rep(rnorm(100,0,1),each=100)
y = rbinom(N,1,plogis(1+x*2+rep(rnorm(100,0,1),each=100)))
df = data.frame(x,y,id)

fit = glmer(y~x+(1|id),family=binomial(link="probit"))

testUniformity(fit)

###############################################


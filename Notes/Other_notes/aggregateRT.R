
########################################

library(tidyr)
library(dplyr)
library(glmmTMB)

set.seed(0)

########################################

# simulate data

N = 1000
k = 20

A = 0:1
B = 0:1

data = crossing(
  subject = 1:N,
  A = A,
  B = B,
  trial = 1:k
) %>%
  mutate(response = NA)
df = data.frame(data)
rInt = rnorm(N,0,0.5)
df$rInt = rInt[df$subject]

df$response = rgamma(nrow(df), rate = 1/exp(1 + df$A*0.6 + df$B*0.8 + df$rInt), shape=10)
hist(df$response)

########################################

fit = glmmTMB(response ~ A*B+(1|subject), family=Gamma(link="log"), data=df)
summary(fit)

dfx = df
dfx$response = log(dfx$response)
dfx = aggregate(dfx,by=list(df$subject,df$A,df$B),FUN=mean)

fit = lm(response ~ A*B, data=dfx)
summary(fit)

########################################





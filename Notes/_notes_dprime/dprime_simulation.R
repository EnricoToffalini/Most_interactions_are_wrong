
#####################################

library(psyphy)
library(brms)

#####################################

# nt = number of trials
# c = criterion (0 = unbiased)
# d = dprime
# ps = probability of signal trials
# sr = variance ratio for unequal variance signal detection, 1 = equal variance

sim_sdt <- function(nt, d, c = 0, ps = 0.5, sr = 1, id = NA){
  ns <- floor(ps * nt)
  nn <- nt - ns
  is_signal <- rep(c(0, 1), nn, ns)
  x <- ifelse(is_signal == 1, rnorm(ns, d/2, sr), rnorm(nn, -d/2, 1))
  say_signal <- ifelse(x > c, 1, 0)
  is_signal <- factor(is_signal, levels = c(1,0))
  say_signal <- factor(say_signal, levels = c(1,0))
  # data.frame(y = say_signal, isold = is_signal, x)
  data.frame(sayold = say_signal, isold = is_signal, id = id)
}

df = data.frame(sayold=NA,isold=NA,id=NA)
N = 10
for(i in 1:N){
  df = rbind(df,sim_sdt(nt=50,d=rnorm(1,0,1),id=i))
}
df$sayold = ordered(df$sayold)
df$isold = ordered(df$isold)

#####################################

fit = brm(sayold ~ 1 + isold + (1 + isold | id),
                        family = bernoulli(link = "probit"),
                        data = df,
                        cores = 4,
)


#####################################




#######################################

library(effectsize)
set.seed(0)

#######################################

simData = function(N_subj = NA, items = 9, points = 5){

  minScore = items
  maxScore = items*points
  
  # subject-level predictors
  x = rnorm(N_subj, 0, 1)
  group = rbinom(N_subj,1,.5)
  tau = 0.5
  id_subj = rep(1:N_subj, each = items)
  randInt = rep(rnorm(N_subj, 0, tau), each = items)
  
  # item effects (random intercepts / difficulty)
  sigma_item = 0.4
  b_item = rep(rnorm(items, 0, sigma_item), times = N_subj)
  
  # latent response
  z = 0.3*rep(x, each=items) + 0.6*rep(group, each=items) + randInt + b_item + rnorm(N_subj * items)
  generalIntercept = 1.2
  
  # ordinal response
  th = -generalIntercept + seq(as.numeric(quantile(z,.1)),as.numeric(quantile(z,.9)),length.out=points-1)
  y = as.integer(cut(z, breaks = c(-Inf, th, Inf), labels = 1:points))
  
  # --- long dataframe: all items ---
  # df_long = data.frame(
  #   id = id_subj,
  #   item = rep(1:items, times = N_subj),
  #   x = rep(x, each = items),
  #   y = y
  # )
  # head(df_long)
  
  # --- collapsed dataframe: sum scores ---
  sumscore = tapply(y, id_subj, sum)
  
  df = data.frame(
    id = 1:N_subj,
    x = x,
    group = group,
    sumscore = as.integer(sumscore)
  )
  
  return(list(df=df, minScore=minScore, maxScore=maxScore))
}

# Preliminary checks
simCheck = simData(N_subj = 1e4)
df_check = simCheck$df
# simple descriptions of the relationship between sumscores and x, group
cor(df_check$sumscore, df_check$x)
cohens_d(sumscore~group, data=df_check)
# view distribution of sumscores
hist(df_check$sumscore)
# check median point of scale and its relative position in 0-1
median(df_check$sumscore)
(median(df_check$sumscore)-simCheck$minScore) / (simCheck$maxScore-simCheck$minScore)

#######################################

# TYPE-I ERROR SIMULATIONS

N_subj = 500

niter = 2000

######

# naive test of interaction using identity-link function data analysis

pvals = rep(NA,niter)
for(i in 1:niter){
  df = simData(N_subj=N_subj)$df
  fit = lm(sumscore ~ x*group, data=df)
  pvals[i] = summary(fit)$coefficients["x:group","Pr(>|t|)"]
}
mean(pvals<0.05)

######

# less inappropriate logit model

pvals = rep(NA,niter)
for(i in 1:niter){
  sim = simData(N_subj=500)
  df = sim$df
  df$sumscore01 = (df$sumscore-sim$minScore)/(sim$maxScore-sim$minScore) 
  df$sumscore01 = 0.01 + df$sumscore01 * 0.98 # avoid extremes 0 and 1
  fit = glm(sumscore01 ~ x*group, data=df, family=gaussian(link="probit"))
  pvals[i] = summary(fit)$coefficients["x:group","Pr(>|t|)"]
}
mean(pvals<0.05)

#######################################



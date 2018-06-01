setwd('~/Desktop/qmss/spring17/thesis/code/')
df = read.csv('../data/clean/surveys.csv')
df = read.csv('../data/clean/svo_without_bad.csv')

library("brms")
library("bayesplot")
library("rstanarm")
library("shinystan")
library("tidyverse")

## Resources
# https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html
# https://github.com/paul-buerkner/brms
# http://mc-stan.org/users/documentation/case-studies/identifying_mixture_models.html
# https://courseworks.columbia.edu/access/content/group/QMSSG4065_001_2017_1/LectureNotes/G4065_slides_03-23-2017.pdf

# brms
# 

# Regression information
# https://kevinstadler.github.io/blog/bayesian-ordinal-regression-with-random-effects-using-brms/
# Non-linear models w/ brms: https://cran.r-project.org/web/packages/brms/vignettes/brms_nonlinear.html

df$income[df$income==11] = -1
y <- df$svo
prosocial <- df$prosocial

fit1 <- stan_glm(
  svo ~ religiosity + politics + income + gender + dependence_development,
  family=gaussian(),
  data=df,
  seed=111
)

print(fit1)
color_scheme_set("brightblue")

y_rep1 <- posterior_predict(fit1, draws = 500)

ppc_dens_overlay(df$svo, y_rep1[1:300,])
ppc_stat(df$svo, y_rep1, stat="min")
ppc_stat(df$svo, y_rep1, stat="max")


### Let's go with a hierarchiy!
fit2 <- brm(
  svo ~ (durationSeconds + religiosity + politics + income + dependence_development + trust_development | region),
  data=df,
  family = "logistic"
)

summary(fit2, waic = TRUE)
y_rep2 <- posterior_predict(fit2, draws = 500)
plot(fit2)
ppc_dens_overlay(df$svo, y_rep2[1:100,])
ppc_stat(df$svo, y_rep2, stat="min")
ppc_stat(df$svo, y_rep2, stat="max")

fit3 <- brm(
  svo ~ (1 + religiosity + politics + income + dependence_development + trust_development | region),
  data = df
)
summary(fit3)

launch_shinystan(fit3)

### Let's go with a MIXTURE model
mix3 <- mixture(gaussian, gaussian)
prior3 <- c(
  prior(normal(8, 3), Intercept, nlpar = mu1),
  prior(normal(38, 3), Intercept, nlpar = mu2)
)

fit3 <- brm(
  bf(svo ~ religiosity + politics + income + dependence_development + trust_development, theta1 = 1, theta2 = 2), 
  df, 
  family = mix3, 
  prior = prior3, 
  inits = 0, 
  chains = 2
)
summary(fit3)
pp_check(fit3)
pp_mixture(fit3)

mix4 <- mixture(gaussian, gaussian, gaussian)
prior4 <- c(
  prior(normal(8, 2), Intercept, nlpar = mu1),
  prior(normal(23, 2), Intercept, nlpar = mu2),
  prior(normal(38, 2), Intercept, nlpar = mu3)
)
fit4 <- brm(
  bf(svo ~ (1 | gender) + religiosity + politics + income + dependence_development + trust_development, theta1 = 1, theta2 = 2, theta3 = 3), 
  df, 
  family = mix4,
  prior = prior4, 
  inits = 0, 
  chains = 2
)
summary(fit4)
pp_check(fit4)


fit5 <- brm(
  bf(svo ~ (religiosity + politics + income + dependence_development + trust_development | race), theta1 = 1, theta2 = 2), 
  train, 
  family = mix3,
  prior = prior3,
  inits = 0,
  chains = 4
)
summary(fit5)
pp_check(fit5)

fit6 <- brm(
  bf(svo ~ (1 | gender) + religiosity + politics + income + dependence_development + trust_development, theta1 = 1, theta2 = 2), 
  train,
  family = mix3, 
  prior = prior3,
  inits = 0,
  chains = 4
)
summary(fit6)
pp_check(fit6)

predict <- function(fit, newdata) {
  ppm <- pp_mixture(fit, newdata = newdata)
  ## classify every observation according to 
  ## the most likely component
  y_pred <- as.vector(apply(ppm[, 1, ], 1, which.max))
  y_pred
}

## extract point estimates for each observation
head(ppm[, 1, ])
y_pred <- predict(fit3, train)
y_actual <- ifelse(train$svo <= 22.5, 1, 2)
y_diff <- y_actual - y_pred
sum(ifelse(y_diff != 0, 0, 1)) / length(y_actual)
LOO(fit5)
table(y_pred)

pp_mixture(fit3, test)[, 1, ]

count <- length(df)
index <- sample(1:count, size = trunc(.75 * count))
test <- df %>%
  filter(row_number() %in% index)
train <- df %>%
  filter(!(row_number() %in% index))


ggplot() + geom_point(aes(religiosity, svo), data=df)

### Helper Functions
invlogit <- function(x) { 1/(1+exp(-x)) }
jitter_binary <- function(a, jitt=.05) {
  ifelse(a==0, runif(length(a), 0, jitt), runif(length(a), 1-jitt, 1))
}

### Logistic Regression
### Fit 1 simple fully-pooled logistic regression with trust development
y_jitter <- jitter_binary(prosocial)
fit1 <- brm(
  prosocial ~ 1 + trust_development,
  data = df,
  family = bernoulli(),
  control = list(adapt_delta = 0.99),
  chains = 4,
  cores = 2
)
coef(fit1)
fixef(fit1)
summary(fit1)
plot(fit1)
plot(marginal_effects(fit1), points = TRUE)

old.par <- par(mfrow=c(2, 1))
plot(df$trust_development, y_jitter)
curve(invlogit(fixef(fit1)[1] + fixef(fit1)[2] * x), add=TRUE)
plot(df$trust_development, y_jitter)
curve(invlogit(fixef(fit1)[1] + fixef(fit1)[2] * x), add=TRUE)
par(old.par)

### Fit 2, logistic regression with varying intercepts
fit2 <- brm(
  prosocial ~ (1 + politics | region:gender),
  data = df,
  family = bernoulli(),
  control = list(adapt_delta = 0.99),
  chains = 4,
  cores = 2
)
coef(fit2)
fixef(fit2)
summary(fit2)
plot(fit2)
plot(marginal_effects(fit2), points = TRUE)

coef_pol <- coef(fit2)$region

old.par <- par(mfrow=c(3, 2))
plot(df$politics, y_jitter)
curve(invlogit(coef_pol[1] + coef_pol[21] * x), add=TRUE)

plot(df$politics, y_jitter)
curve(invlogit(coef_pol[2] + coef_pol[22] * x), add=TRUE)

plot(df$politics, y_jitter)
curve(invlogit(coef_pol[3] + coef_pol[23] * x), add=TRUE)

plot(df$politics, y_jitter)
curve(invlogit(coef_pol[4] + coef_pol[24] * x), add=TRUE)

plot(df$politics, y_jitter)
curve(invlogit(coef_pol[5] + coef_pol[25] * x), add=TRUE)

par(old.par)

samples_h <- posterior_samples(fit2, c("sd_", "cor_"))
mcmc_areas(samples_h, point_est = "mean", prob = .8)
y_rep2 <- posterior_predict(fit2, draws = 500)
length(y_rep2)
ppc_dens_overlay(prosocial, y_rep2)

### Fit 3
fit2 <- brm(
  prosocial ~ durationSeconds + religiosity + politics + income + education + dependence_development + trust_development + relationship_development + (1 | region),
  data = df,
  family = bernoulli(),
  control = list(adapt_delta = 0.99),
  chains = 4,
  cores = 2
)

summary(fit2, waic = TRUE)
coef(fit2)
plot(fit2)
plot(marginal_effects(fit2), points = TRUE)

y_rep2 <- posterior_predict(fit2, draws = 500)
ppc_dens_overlay(prosocial[1:759], y_rep2)
ppc_stat(prosocial[1:759], y_rep2, stat="min")
ppc_stat(prosocial[1:759], y_rep2, stat="max")
#launch_shinystan(fit2)

fit3 <- brm(
  prosocial ~ (1 + religiosity | politics),
  data = df,
  family = bernoulli(),
  control = list(adapt_delta = 0.95),
  chains = 2,
  cores = 2
)

summary(fit3, waic = TRUE)
plot(fit3)
plot(marginal_effects(fit3), points = TRUE)

y_rep3 <- posterior_predict(fit3, draws = 500)
ppc_dens_overlay(prosocial[1:812], y_rep3)
ppc_stat(prosocial[1:812], y_rep2, stat="min")
ppc_stat(prosocial[1:812], y_rep2, stat="max")

fit4 <- brm(
  prosocial ~ durationSeconds + (1 | politics),
  data = df,
  family = bernoulli(),
  control = list(adapt_delta = 0.99),
  chains = 2,
  cores = 2
)

plot(fit4)
plot(marginal_effects(fit4), points = TRUE)

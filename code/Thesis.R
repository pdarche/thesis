setwd('~/Desktop/qmss/spring17/thesis/code/')
df = read.csv('../data/clean/surveys.csv')

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


y <- df$svo

fit1 <- stan_glm(
  svo ~ religiosity + politics + income + gender + dependence_development,
  family = gaussian(),
  data = df,
  seed = 111
)

print(fit1)
color_scheme_set("brightblue")

y_rep1 <- posterior_predict(fit1, draws = 500)


ppc_dens_overlay(df$svo, y_rep1[1:300,])
ppc_stat(df$svo, y_rep1, stat="min")
ppc_stat(df$svo, y_rep1, stat="max")


### Let's go with a hierarchiy!
fit2 <- brm(
  svo ~ (religiosity + politics + income + dependence_development + trust_development | state:race),
  data = df,
  adapt_delta=.9
)

summary(fit2, waic = TRUE)
y_rep2 <- posterior_predict(fit2, draws = 500)
ppc_dens_overlay(df$svo, y_rep2[1:100,])
ppc_stat(df$svo, y_rep2, stat="min")
ppc_stat(df$svo, y_rep2, stat="max")

fit3 <- brm(
  svo ~ (1 + religiosity + politics + income + dependence_development + trust_development | state),
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



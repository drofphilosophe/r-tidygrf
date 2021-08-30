library(tidyverse)
library(tidymodels)
library(tidygrf)

N = 1000
full.data <- tibble(e=rnorm(N,0,1))

for(i in 1:10) {
  full.data %>%
    mutate(
      !!str_c("x",i) := runif(N)
    ) -> full.data
}

full.data %>%
  mutate(
    y = e + x1 + x2*x3^2
  ) -> full.data

formula.rf = as.formula("y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10")

#Build a model
grf_rf(mode="regression",trees=100) %>%
  set_engine("grf") %>%
  fit(formula.rf,data=full.data) -> rf.fit

rf.fit %>%
  predict(new_data=full.data) %>%
  rename(yhat.rf = 1) %>%
  bind_cols(full.data) -> full.data


ggplot(data=full.data) +
  geom_point(aes(y=y,x=yhat.rf,color="Regression")) +
  geom_abline(slope=1,intercept=0,color="black") +
  scale_color_manual(
    values=c("Regression" = "blue","Local Linear" = "orange")
  ) +
  theme_bw() +
  labs(
    x="Predicted",
    y="Acutal"
  )

print(llf.fit$fit$model.params$variable_importance)

##########################
## Test Weights
##########################
full.data %>%
  mutate(
    w = runif(n()),
    y = e + if_else(w>0.5,5*x1,x1)
  ) %>%
  select(-contains("yhat"))-> full.data

formula.rf = as.formula("y ~ x1 + x2 + x3")

#Build a model
grf_rf(mode="regression") %>%
  set_args(trees=100,weights=.dat()$w) %>%
  set_engine("grf") %>%
  fit(formula.rf,data=full.data) -> rf.fit

print(rf.fit$fit)


rf.fit %>%
  predict(new_data=full.data) %>%
  rename(yhat.rf = 1) %>%
  bind_cols(full.data) -> full.data

grf_rf(mode="regression",trees=100) %>%
  set_engine("grf") %>%
  fit(formula.rf,data=full.data) -> rf.fit

rf.fit %>%
  predict(new_data=full.data) %>%
  rename(yhat2.rf = 1) %>%
  bind_cols(full.data) -> full.data



ggplot(data=full.data) +
  geom_point(aes(x=x1,y=yhat.rf,color="Weighted")) +
  geom_point(aes(x=x1,y=yhat2.rf,color="Unweighted")) +
  geom_abline(slope=1,intercept=0,color="black") +
  scale_color_manual(
    values=c("Weighted" = "blue","Unweighted" = "orange")
  ) +
  theme_bw() +
  labs(
    x="Predicted",
    y="Acutal"
  )















#############################
## Test handling of NAs
#############################
full.data %>%
  mutate(
    x1 = if_else(row_number() %% 20 == 0, as.double(NA),x1),
    x2 = if_else(row_number() %% 20 == 5, as.double(NA),x2)
  ) %>%
  select(-contains("yhat"))-> full.data

formula.rf = as.formula("y ~ x1 + x2 + x3")

#Build a model
grf_rf(mode="regression") %>%
  set_args(trees=100) %>%
  set_engine("grf") %>%
  fit(formula.rf,data=full.data) -> rf.fit

print(rf.fit$fit)


rf.fit %>%
  predict(new_data=full.data) %>%
  rename(yhat.rf = 1) %>%
  bind_cols(full.data) -> full.data

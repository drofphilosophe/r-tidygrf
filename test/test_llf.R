
## Remember on updates to run the following:
if(FALSE) {
  devtools::document()
  devtools::install("C:/Users/archs/Documents/GitHub/R/packages/tidygrf")
}


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
  mutate(k=floor(runif(n=N,0,5))) -> full.data

full.data %>%
  mutate(
    y = e + x1 + x2*x3^2
  ) -> full.data

formula.rf = as.formula("y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + factor(k)")

#Build a model
grf_llf(mode="regression",trees=100) %>%
  set_engine("grf") %>%
  fit(formula.rf,data=full.data) -> llf.fit

llf.fit %>%
  predict(new_data=full.data,type="numeric") %>%
  rename(yhat = 1) %>%
  bind_cols(full.data) -> full.data

ggplot(data=full.data) +
  geom_point(
    aes(y=y,x=yhat), color="black"
  ) +
  theme_bw() +
  labs(
    x="Predicted",
    y="Acutal"
  )

print(llf.fit$fit$model.params$variable_importance)


#######################
## Construct data with a factor variable
######################
N = 1000
full.data <- tibble(e=rnorm(N,0,1))

full.data %>%
  mutate(
    i=floor(runif(N)*10)
  ) -> full.data

for(i in 1:5) {
  full.data %>%
    mutate(
      !!str_c("x",i) := runif(N)
    ) -> full.data
}

full.data %>%
  mutate(
    y = e + case_when(
      i == 0 ~ x1 + x2,
      i == 1 ~ x1 + x3,
      i == 2 ~ 2*x1 + x2 + 4*x3,
      i == 3 ~ 4*x2 - x3,
      i == 4 ~ x1 - x2,
      TRUE ~ as.double(NA)
    )
  ) -> full.data

formula.rf = as.formula("y ~ 0 + x1 + x2 + x3 + x4 + x5 + factor(i)")

#Build a model
grf_llf(mode="regression",trees=100) %>%
  set_engine("grf") %>%
  fit(formula.rf,data=full.data) -> llf.fit

llf.fit %>%
  predict(new_data=full.data,type="numeric") %>%
  rename(yhat = 1) %>%
  bind_cols(full.data) -> full.data

ggplot(data=full.data) +
  geom_point(
    aes(y=y,x=yhat, color=i)
  ) +
  geom_abline(slope=1,intercept=0,color="orange",size=1.5) +
  theme_bw() +
  labs(
    x="Predicted",
    y="Acutal"
  )


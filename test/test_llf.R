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
grf_llf(mode="regression",trees=100) %>%
  set_engine("grf") %>%
  fit(formula.rf,data=full.data) -> llf.fit

llf.fit %>%
  predict(new_data=full.data) %>%
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

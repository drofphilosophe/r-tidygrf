# r-tidygrf
Tidymodels for generalized random forests using grf.

This package implmements a tidymodels interface for the package grf. 
Currently only regression forest and local linear forest models are supported. 
Training and test data can be provided as `data.frame` or `tibble` objects.
Models support the `formula` interface. 

# Installation
You can install the latest version of this package using the following command in R
```r
devtools::install_github("drofphilosophe/r-tidygrf")
```

# Example usage
```r
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

#Train a model
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

 ```

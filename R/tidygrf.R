.onLoad <- function(libname,pkgname) {
  #######################
  ## Inititate each model
  #######################
  writeLines("----Initializing tidymodels for grf-----")
  writeLines("Version 0.02")
  writeLines("Model: Regression Forests")
  make_grf_rf()
  print(parsnip::show_model_info("grf_rf"))
  writeLines("Model: Local Linear Forests")
  make_grf_llf()
  print(parsnip::show_model_info("grf_llf"))
}

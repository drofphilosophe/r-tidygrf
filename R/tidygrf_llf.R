#########################################
## tidygrf_llf.R
##
## This package provides parsnip definitions
## to wrap ll_regression_forest from the grf package
## so it can be used with tidymodels.
##
## Remember on updates to run the following:
## devtools::document()
## devtools::install("G:/My Drive/Programming/R/packages/tidygrf")
#########################################

#############################
## Fit Function
#############################
#' @export
grf_llf <- function(mode = "regression",  sub_classes = NULL, ...) {
  # Check for correct mode
  if (mode  != "regression") {
    stop("`mode` should be 'regression'", call. = FALSE)
  }

  # Capture the arguments in quosures
  args <- list(sub_classes = rlang::enquo(sub_classes))

  # Save some empty slots for future parts of the specification
  out <- list(args = args, eng_args = NULL,
              mode = mode, method = NULL, engine = NULL)

  # set classes in the correct order
  class(out) <- parsnip::make_classes("grf_llf")
  out
}

#ll_regresion_forest takes the x and y arguments as positional
#This wrapper function makes all arguments positional
#As to be compatible with tidymodels matrix estimators
#' @export
llf_wrapper <- function(y=NULL,x=NULL,...) {
  #Extract the arguments and remove y and x from the list
  #Then call ll_regresion_forest
  .args <- as.list(match.call()[-1])
  #Remove x and y from .args
  .args$x <- NULL
  .args$y <- NULL
  #Call ll_regression forest with positional arguments x and y
  #And the remaining named arguments in .args then return the
  #result
  return(
    do.call(
      grf::ll_regression_forest,c(list(x,y),.args)
    )
  )
}

############################
## Define a new tidymodels model
############################
##You wrap this in a function
make_grf_llf <- function() {
  ##################
  ## llf wrapper
  ##################
  parsnip::set_new_model("grf_llf")
  #LLFs are regression models
  parsnip::set_model_mode(model = "grf_llf", mode = "regression")
  #Define the engine
  parsnip::set_model_engine(
    "grf_llf",
    mode = "regression",
    eng = "grf"
  )
  parsnip::set_dependency("grf_llf", eng = "grf", pkg = "grf")

  #############################
  ##  Set arguments
  #############################
  parsnip::set_model_arg(
    model = "grf_llf",
    eng = "grf",
    parsnip = "trees",
    original = "num.trees",
    func = list(fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "grf_llf",
    eng = "grf",
    parsnip = "mtry",
    original = "mtry",
    func = list(fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "grf_llf",
    eng = "grf",
    parsnip = "min_n",
    original = "min.node.size",
    func = list(fun = "min_n"),
    has_submodel = FALSE
  )


  parsnip::set_fit(
    model="grf_llf",
    eng="grf",
    mode="regression",
    value=list(
      interface="matrix",
      protect=c("x","y"),
      func=c(fun="llf_wrapper"),
      defaults = list()

    )
  )


  ####################################
  ## Prediction function
  ####################################
  grf_llf_pred_info <- list(
    pre = NULL, #Pre-processing command
    #Post-processing command - Here I just want one component of the prediction
    post = function(x,object) {
      return(tibble::tibble(x[1]))
    },
    func = c(fun="predict"), #Reference to the predict method
    args = list(
      #<user argument name> = <value passed to func>
      #Quoting means the argument will be evaluated at runtime
      object = quote(object$fit),
      newdata = quote(new_data),
      type = "numeric"
    )
  )

  parsnip::set_pred(
    model = "grf_llf",
    eng = "grf",
    mode = "regression",
    type = "numeric",
    value = grf_llf_pred_info
  )
}




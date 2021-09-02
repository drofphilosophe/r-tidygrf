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
grf_llf <- function(mode = "regression",  weights=NULL, trees=2000, mtry=NULL,min_n=5,...) {
  # Check for correct mode
  if (mode  != "regression") {
    stop("`mode` should be 'regression'", call. = FALSE)
  }

  # Capture the arguments in quosures
  args <- list(
    weights = rlang::enquo(weights),
    trees = rlang::enquo(trees),
    mtry = rlang::enquo(mtry),
    min_n = rlang::enquo(min_n)
  )

  # Save some empty slots for future parts of the specification
  out <- list(args = args,
              eng_args = NULL,
              mode = mode,
              method = NULL,
              engine = NULL
              )

  # set classes in the correct order
  class(out) <- parsnip::make_classes("grf_llf")
  out
}

#ll_regresion_forest takes the x and y arguments as positional
#This wrapper function makes all arguments positional
#As to be compatible with tidymodels matrix estimators
#' @export
llf_wrapper <- function(formula,data,weights=NULL,trees=2000,mtry=NULL,min_n=5) {
  #print(weights)
  w_vec = as.numeric(c())
  ##If no weights are provided, make them a vector of 1s
  if(is.null(weights)) {
    writeLines("No weights provided. Assuming uniform weights")
    w_vec = rep(1,nrow(data))
  } else if(is.character(weights)) {
    writeLines(paste0("Weight variable: ", weights))
    #If a variable name is provided it is the weighting column
    w_vec = data %>% pull(w_vec) %>% as.vector()
  } else if(is.numeric(weights)) {
    writeLines("Using weight vector")
    #If weights is a vector, then make sure it is the same length as data
    if(length(weights) != nrow(data)) {
      stop("Weights provided as a vector that is not the same length as the data")
    }
    w_vec = as.vector(weights)
  } else {
    writeLines("Misspecified argument for weights\n")
    print(weights)
    stop("Aborting")
  }

  writeLines(paste("Data have", nrow(data), "rows"))
  writeLines(paste("Weights have", length(w_vec), "rows"))
  writeLines(paste("Weights range from",min(w_vec),"to",max(w_vec)))

  #Create an environment with the formula and data
  #This is some weirdness with R that arguments are resolved
  #in the current environment but the formula expects everything
  #it needs to be in its own enviornment
  envir <- list2env(list(w_vec=w_vec), parent=environment(formula))
  environment(formula) <- envir

  ##Construct the design matrix
  model.frame(
    formula=formula,
    data=data,
    weights=w_vec,
    na.action=na.omit
  ) -> mf

  #Extract all of the components
  y <- matrix(model.response(mf), ncol=1)
  x <- matrix(model.matrix(formula,mf), nrow=nrow(y))
  w <- matrix(model.weights(mf), ncol=1)

  #Call regression_forest with positional arguments x and y
  #Specifically set all parameters
  writeLines("Training Regression Forest")
  writeLines(paste("Number of Trees", trees))
  writeLines(paste("Number of variables included in each tree", mtry))
  writeLines(paste("Minimum Leaf Size", min_n))

  #Train the model
  grf::ll_regression_forest(
    x,y,
    num.trees = trees,
    min.node.size=min_n
  )->trained.model

  #Return the model with some additional stuff
  data.frame(
    list(
      feature = names(data.frame(model.matrix(formula,mf),check.names = FALSE)),
      variable.importance = grf::variable_importance(trained.model)[,1]
    )
  ) -> vi

  return(
    list(
      trained.model=trained.model,
      model.params=list(
        formula=paste(as.character(formula)[2],"~",as.character(formula)[3]),
        outcome=as.character(formula)[2],
        features=names(data.frame(model.matrix(formula,mf),check.names = FALSE)),
        variable_importance = vi
      )
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
  ## Define Encoding
  #############################
  parsnip::set_encoding(
    model = "grf_llf",
    eng = "grf",
    mode = "regression",
    options = list(
      predictor_indicators = "one_hot",
      compute_intercept = FALSE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  #############################
  ##  Set arguments
  #############################
  parsnip::set_model_arg(
    model = "grf_llf",
    eng = "grf",
    parsnip = "trees",
    original = "trees",
    func = list(fun = "llf_wrapper"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "grf_llf",
    eng = "grf",
    parsnip = "mtry",
    original = "mtry",
    func = list(fun = "llf_wrapper"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "grf_llf",
    eng = "grf",
    parsnip = "min_n",
    original = "min_n",
    func = list(fun = "llf_wrapper"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "grf_llf",
    eng = "grf",
    parsnip = "weights",
    original = "weights",
    func = list(fun = "llf_wrapper"),
    has_submodel = FALSE
  )


  parsnip::set_fit(
    model="grf_llf",
    eng="grf",
    mode="regression",
    value=list(
      interface="formula",
      protect=c("formula","data"),
      func=c(fun="llf_wrapper"),
      defaults = list()

    )
  )




  parsnip::set_pred(
    model = "grf_llf",
    eng = "grf",
    mode = "regression",
    type = "numeric",
    ####################################
    ## Prediction function
    ####################################
    value = list(
      #Pre-processing command
      #As far as I can tell, the return value from this function
      #is passed as new_data to the predict function below
      pre = function(x,fitobject) {
        #I want to pass NAs to the design matrix here
        #First I extract the current system NA action
        na_action <- options('na.action')
        #Change the action to na.pass
        options(na.action='na.pass')
        #create the design matrix
        x <- model.matrix(as.formula(fitobject$fit$model.params$formula),x)
        #Resent the system NA action
        options(na.action=na_action$na.action)

        return(x)
      },
      #Post-processing command
      #This takes the predict object and the modeling object as arguments
      #And then the parsnip prediction routine passes what this function returns
      #Here I just want one component of the prediction
      post = function(x,object) {
        return(tibble::tibble(x[1]))
      },
      func = c(fun="predict"), #Reference to the predict method
      args = list(
        #<user argument name> = <value passed to func>
        #Quoting means the argument will be evaluated at runtime
        #object = quote(object$fit$trained.model),
        object = quote(object$fit$trained.model),
        newdata = quote(new_data)
      )
    )
  )
}




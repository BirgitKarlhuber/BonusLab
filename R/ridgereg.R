#' Ridge Regression - Function for a multiple regression model
#' 
#' This functions estimates the coefficients and fitted values by optimizing a penalized least square problem. 
#'
#' @param formula an object of class 'formula': a symbolic description of the model to be fitted.
#' 
#' @param data an 'data.frame' containing the variables in the model. 
#' 
#' @param lambda a penalty parameter (numeric), normally small (sowhere between 0 and 0.1)
#'
#' @return Returns an object of class 'ridgereg'. 
#'
#' @export 
#' 
#' @examples
#' data("iris")
#' library(BonusLab)
#' ridgereg <- getFromNamespace("ridgereg", "BonusLab")
#' ridgereg_model <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.01)


# RC class object
ridgereg <- setRefClass("ridgereg",
                      fields = list (formula = "formula",
                                     data = "data.frame",
                                     data_name = "character",
                                     lambda = "numeric",
                                     coefficients = "matrix",
                                     fitted.values = "matrix"
                                     ))


# methods for the RC class object
ridgereg$methods(
  initialize = function(formula, data, lambda){
    stopifnot(class(formula)=="formula", is.data.frame(data))
    stopifnot(is.numeric(lambda))
    
    .self$formula <<- formula
    .self$data <<- data
    .self$data_name <<- deparse(substitute(data))
    .self$lambda <<- lambda
    
    X <- as.matrix(model.matrix(formula, data))
    y <- as.matrix(data[all.vars(formula, max.names=1)])
    
    
    
    # cat(nrow(t(X)%*%X),ncol(t(X)%*%X))
    dim_matrix <- ncol(X)
    matrix_I <- matrix(1, nrow=dim_matrix, ncol=dim_matrix)
    matrix_I[1,1] <- 0 # set to 0, since typically, penalization of the intercept is not 
    # desired in ridge regression so that β1 should be excluded from the penalty term
    
    .self$coefficients <<- solve(t(X)%*%X + lambda*matrix_I)%*%t(X)%*%y # ridge regression coefficient
    .self$fitted.values <<- round(X%*%coefficients,3) # fitted_values
    
   },
  
  print = function(){
    char_formula <- as.character(formula)
    coef <- as.vector(coefficients)
    names(coef) <- rownames(coefficients)
    
    cat(paste("ridgereg(formula = ", char_formula[2]," ", char_formula[1], " ",
              char_formula[3], ", data = ", data_name, ", lambda = ", lambda,
              ")\n\nCoefficients:\n",sep=""))
    base::print(coef)
  },
  
  pred = function(){
    return(fitted.values)
  }, 
  
  predict = function(new_formula, new_data){
    stopifnot(is.data.frame(new_data))
    
    X_new <- as.matrix(model.matrix(new_formula, new_data))
    
    new_fitted_values <- round(X_new%*%coefficients,3)
    return(new_fitted_values)
  },
  
  coef = function(){
    coef <- as.vector(coefficients)
    names(coef) <- rownames(coefficients)
    return(coef)
  }
 
) 

# ridgereg_model <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.01)
# ridgereg_model$coef()
# 
# library(MASS)
# lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.01)


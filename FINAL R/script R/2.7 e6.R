# 6) Crearea unei functii P care permite calculul diferitelor tipuri de probabilitati asociate
# unei variabile aleatoare continue(similar functiei P din pachetul discreteRV)

library(intervals)

# Define the continuous random variable class cRV
#' Continuous Random Variable
#'
#' @param pdf probability density function
#'
#' @return cRV object
#' @export
#' @examples
#' pdf1 <- function(x) { (0 <= x & x <= 1) * (3 * x^2) }
#' X <- cRV(pdf1)
cRV <- function(pdf) {
  structure(
    list(
      pdf = pdf,
      cdf = function(x) {
        ifelse(is.vector(x), sapply(x, function(x) integrate(pdf, -Inf, x)$value), integrate(pdf, -Inf, x)$value)
      }
    ),
    class = "cRV"
  )
}

# Define the less than operator `<` for cRV objects
#' Less Than Operator for cRV Objects
#'
#' @param X cRV object
#' @param x numeric value
#' @return cRVrez object
#' @export
"<.cRV" <- function(X, x) {
  if (class(X) != "cRV") stop("X is not a continuous random variable.")
  if (class(x) != "numeric") stop("x must be a numeric value")
  
  interv <- Intervals(
    matrix(
      c(-Inf, x),
      byrow = TRUE,
      ncol = 2
    ),
    closed = c(FALSE, FALSE),
    type = "R"
  )
  
  rez <- X$cdf
  class(rez) <- "cRVrez"
  attr(rez, "interval") <- interv
  
  return(rez)
}

# Define the greater than operator `>` for cRV objects
#' Greater Than Operator for cRV Objects
#'
#' @param X cRV object
#' @param x numeric value
#' @return cRVrez object
#' @export
">.cRV" <- function(X, x) {
  if (class(X) != "cRV") stop("X is not a continuous random variable.")
  if (class(x) != "numeric") stop("x must be a numeric value")
  
  interv <- Intervals(
    matrix(
      c(x, Inf),
      byrow = TRUE,
      ncol = 2
    ),
    closed = c(FALSE, FALSE),
    type = "R"
  )
  
  rez <- X$cdf
  class(rez) <- "cRVrez"
  attr(rez, "interval") <- interv
  
  return(rez)
}

# Define the or operator `|` for cRVrez objects
#' OR Operator for cRVrez Objects
#'
#' @param Xres cRVrez object
#' @param Yres cRVrez object
#' @return cRVrez object
#' @export
"|.cRVrez" <- function(Xres, Yres) {
  ORret <- Xres
  attr(ORret, "interval") <- interval_union(attr(Xres, "interval"), attr(Yres, "interval"))
  return(ORret)
}

# Define the and operator `&` for cRVrez objects
#' AND Operator for cRVrez Objects
#'
#' @param Xres cRVrez object
#' @param Yres cRVrez object
#' @return cRVrez object
#' @export
"&.cRVrez" <- function(Xres, Yres) {
  ORret <- Xres
  attr(ORret, "interval") <- interval_intersection(attr(Xres, "interval"), attr(Yres, "interval"))
  return(ORret)
}

# Define the probability calculation function Pr
#' Probability Calculation for cRVrez Objects
#'
#' @param crez cRVrez object
#' @return numeric probability
#' @export
Pr <- function(crez) {
  if (class(crez) != "cRVrez") stop("Parameter has incorrect type")
  
  # Calculate the integral over the interval using cdf
  calcFunction <- function(interv) {
    rvl <- 0
    if (interv[2] != -Inf)
      rvl <- crez(interv[2])
    
    lvl <- 0
    if (interv[1] != -Inf)
      lvl <- crez(interv[1])
    
    return(rvl - lvl)
  }
  
  sum(apply(attr(crez, "interval"), 1, calcFunction))
}

# # Define the density function pdf1
# pdf1 <- function(x) { (0 <= x & x <= 1) * (3 * x^2) }
# 
# # Create the continuous random variable X
# X <- cRV(pdf1)
# 
# # Calculate the cumulative distribution function (cdf) of X at a specific value
# cdf_value <- X$cdf(0.5)
# print(cdf_value)  # The probability that X is less than or equal to 0.5
# 
# # Perform comparisons on X and obtain the resulting cRVrez objects
# x_less_than_0.2 <- X < 0.2
# x_greater_than_0.5 <- X > 0.5
# 
# # Calculate the probabilities associated with the cRVrez objects
# prob_less_than_0.2 <- Pr(x_less_than_0.2)
# prob_greater_than_0.5 <- Pr(x_greater_than_0.5)
# 
# print(prob_less_than_0.2)  # The probability that X is less than 0.2
# print(prob_greater_than_0.5)  # The probability that X is greater than 0.5
# 
# # Combine conditions using the OR operator
# combined_cond <- x_less_than_0.2 | x_greater_than_0.5
# 
# # Calculate the probability associated with the combined condition
# prob_combined_cond <- Pr(combined_cond)
# print(prob_combined_cond)  # The probability that X is either less than 0.2 or greater than 0.5
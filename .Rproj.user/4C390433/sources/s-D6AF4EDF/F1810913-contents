#' Random Number Generator
#'
#' This function allows you to generate your desired number of random numbers
#' from your chosen distribution.
#'
#' @param num Number of required output
#' @param distribution Distribution of output. Should be one of "normal", "binomial", "poisson"
#' @param mu Required for NORMAL distribution only - distribution mean
#' @param sigma Required for NORMAL distribution - distribution standard deviation
#' @param trials required for BINOMIAL distribution - number of yes/no trials in the test
#' @param p required for BINOMIAL distribution - probability of success (i.e. 'yes' outcome)
#' @param l required for POISSON distribution - lambda is the rate perameter (i.e. events per interval of time)
#' @keywords random normal binomial poisson
#' @return random numbers
#' @import stats
#' @export
#' @examples
#' x_n <- random_number(num = 5, distribution = "normal",
#'                      mu = 0, sigma = 1,
#'                      trials = NULL, p = NULL,
#'                      l = NULL)
#' x_b <- random_number(num = 5, distribution = "binomial",
#'                      mu = NULL, sigma = NULL,
#'                      trials = 10, p = 0.5,
#'                      l = NULL)
#' x_p <- random_number(num = 20, distribution = "poisson",
#'                      mu = NULL, sigma = NULL,
#'                      trials = NULL, p = NULL,
#'                      l = 5)
#' summary(x_n)
#' summary(x_b)
#' summary(x_p)
#'
#'
#'
#'
random_number <- function(num, distribution,
                          mu = NULL, sigma = NULL,
                          trials = NULL, p = NULL,
                          l = NULL) {
  if (distribution %in% "normal") {
    output <- rnorm(n = num, mean = mu , sd = sigma)
    class(output) <- "normal"
    print(output)
  } else {
    if (distribution %in% "binomial") {
      output <- rbinom(n = num, size = trials, prob = p)
      class(output) <- "binomial"
      print(output)
    } else {
      if (distribution %in% "poisson") {
        output <- rpois(n = num, lambda = l)
        class(output) <- "poisson"
        print(output)
      } else {
        output <- "You dont seem to have entered a valid distribution -
        it should be one of normal, binomial, poisson (in quotation marks)."
        print(output)
      }
    }
    }
  }



#' Methods (by Class)
#'
#' This function allows you to use the summary function for
#' object of class "normal", "binomial" and "poisson"
#'
#' @param object random numbers of class "normal"
#' @param ... further options, see summary
#' @return summary stats
#' @eobjectport
#'
summary.normal <- function(object, ...) {
  c(n = length(object), mean = mean(object), sd = sd(object), range = range(object))
}

#' Methods (by Class)
#'
#' Allows you to use the summary function for
#' object of class "binomial"
#'
#' @param object random numbers of class "binomial"
#' @param ... further options, see summary
#' @return summary stats
#' @eobjectport
#'
summary.binomial <- function(object, ...) {
  c(n = length(object), mean = mean(object), sd = sd(object), range = range(object))
}

#' Methods (by Class)
#'
#' This function allows you to use the summary function for
#' object of class "poisson"
#'
#' @param object random numbers of class "poisson"
#' @param ... further options, see summary
#' @return summary stats
#' @eobjectport
#'
summary.poisson <- function(object, ...) {
  c(n = length(object), mean = mean(object), sd = sd(object), range = range(object))
}



#' Calculate the number of breedings needed to obtain a fixed numberf of litters with a certain confidence
#'
#' Here the number of fertile mice \code{n_litters} follows the binomial
#' distribution with probability of success being \code{effective_fertility_p}
#' and the number of trials being the number of breedings. The function returns
#' the number of breedings needed to achieve  \code{n_litters} with probability
#' no less than \code{condifence_p}.
#'
#' @param condifence_p a number between 0 and 1
#' @param effective_fertility_p a number between 0 and 1
#' @param n_litters an integer >= 1, the number of litters
#' determined by the function calculate_needed_litters with the
#' method='textbook' option
#' @return The function returns the number of breedings
#' needed to achieve  \code{n_litters} with probability no less than \code{condifence_p}.
#' @examples
#' calculate_needed_breedings_textbook(
#'     condifence_p=0.95,
#'     effective_fertility_p=0.6,
#'     n_litters=10)
#'
calculate_needed_breedings_textbook <- function(
  condifence_p,
  effective_fertility_p,
  n_litters
){
  stopifnot(condifence_p < 1 & condifence_p > 0)
  stopifnot(effective_fertility_p < 1 & effective_fertility_p > 0)
  stopifnot(is.wholenumber(n_litters) & n_litters >= 1)

  Nbreedings <- optimise(
    f=function(nn){
        abs(
            qbinom(p = 1-condifence_p,
                   prob = effective_fertility_p,
                   size = nn) - n_litters)
    },
    lower=1,upper = n_litters*10/effective_fertility_p)
  return(Nbreedings[1])
}

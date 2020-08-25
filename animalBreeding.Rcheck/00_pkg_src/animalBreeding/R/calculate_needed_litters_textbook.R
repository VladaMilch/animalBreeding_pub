#' Calculate the number of litters needed to obtain a fixed numberf of mice
#' with a certain confidence
#'
#' Here the number of offsprings one animal produces, i.e the litter size,
#' is assumed to follow a Gaussian distribution with mean \code{litter_mean} and
#' the standard deviation \code{litter_sd}. The function returns
#' the number of litters needed to obtain \code{n_needed} offsprings
#' with probability
#' no less than \code{condifence_p}.
#'
#' Note: In the textbook, a litter cannot have size 0.
#'
#'
#' @param condifence_p a number between 0 and 1
#' @param litter_mean the average litter size of the bred mice
#' @param litter_sd standart deviation of the litter size, equals 2.5
#' by default - as specified in the textbook
#' @param n_needed  number of baby mice needed to be born
#' @return The function returns the number of litters
#' needed to obtain \code{n_needed} offsprigns with probability no less than
#' \code{condifence_p}.
#' @examples
#' calculate_needed_breedings_textbook(
#'     condifence_p=0.95,
#'     effective_fertility_p=0.6,
#'     n_litters=10)
#'
#' @export
calculate_needed_litters_textbook <- function(
    condifence_p,
    litter_mean,
    litter_sd=2.5,
    n_needed
){
    # Nlitters
    quantile_of_sum <- function(Nlitters){
        qnorm(p=1-condifence_p,
            mean=litter_mean*Nlitters,
            sd = litter_sd*sqrt(Nlitters))
    }
    neededL <- optimise(
        f=function(nn){
            abs(quantile_of_sum(nn)-n_needed)
          },
        lower = 1, upper = n_needed)
    res = round(neededL[[1]]) # WARNING! mathematically does not guarantee P >= condifence_p
    return(res)
}

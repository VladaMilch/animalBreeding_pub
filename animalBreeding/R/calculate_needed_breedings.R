#' Calculate the number of breedings needed to guarantee a fixed number of
#' offsprings with a certain probability
#'
#' At the moment only one model is implemented (the textbook).
#'
#' @param condifence_p a number between 0 and 1
#' @param effective_fertility_p a number between 0 and 1
#' @param litter_mean the average litter size of the bred animals
#' @param litter_sd standart deviation of the litter size, equals 2.5
#' by default - as specified in the textbook
#' @param n_needed  number of offsprings needed to be born
#' @param method the model used for the calculation, can be "textbook"
#'
#' @return The function returns the number of breedings
#' needed to guarantee \code{n_needed} offprings with probability no less than
#' \code{condifence_p}.
#' @examples
#' calculate_needed_breedings(
#'     condifence_p=0.95,
#'     effective_fertility_p=0.6,
#'     n_needed = 30,
#'     litter_mean = 7,
#'     litter_sd = 2.5
#'     n_litters=10)
#'
#' @export
calculate_needed_breedings <- function(
    condifence_p,
    effective_fertility_p,
    n_needed,
    litter_mean,
    litter_sd = 2.5,
    method = "textbook",
    calculation_type = "textbook_exact"
){
    # method reflects one of the available models
    stopifnot(method %in% c("textbook"))

    if(method=="textbook"){
        condifence_p1 = 1 - (1-condifence_p)/2
        nlit <- calculate_needed_litters_textbook(
            condifence_p=condifence_p1,
            litter_mean=litter_mean,
            litter_sd=litter_sd,
            n_neede=n_needed)
        nbre <- calculate_needed_breedings_textbook(
            condifence_p=condifence_p1,
            effective_fertility_p=effective_fertility_p,
            n_litters=nlit,
            calculation_type = calculation_type
        )
        return(nbre)
    }
}

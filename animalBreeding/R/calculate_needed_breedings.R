#' Calculate the number of breedings needed to guarantee a fixed number of
#' offsprings with a certain probability
#'
#' At the moment only one model is implemented (the textbook).
#'
#' @param confidence_p a number between 0 and 1
#' @param effective_fertility_p a number between 0 and 1
#' @param litter_mean the average litter size of the bred animals
#' @param litter_sd standart deviation of the litter size, equals 2.5
#' by default - as specified in the textbook
#' @param n_needed  number of offsprings needed to be born
#' @param method the model used for the calculation, can be "textbook"
#'
#' @return The function returns the number of breedings
#' needed to guarantee \code{n_needed} offprings with probability no less than
#' \code{confidence_p}.
#' @examples
#' calculate_needed_breedings(
#'     confidence_p=0.95,
#'     effective_fertility_p=0.6,
#'     n_needed = 30,
#'     litter_mean = 7,
#'     litter_sd = 2.5
#'     n_litters=10)
#'
#' @export
calculate_needed_breedings <- function(
    confidence_p,
    effective_fertility_p,
    n_needed,
    litter_mean = NULL,
    offsprings_n_sample = NULL, # should be a large vector, n of offs for a single mouse
    litter_sd = 2.5,
    method = "textbook",
    calculation_type = ""
    #calculation_type = "textbook_exact"
){
    # arguments needed for every function
    stopifnot(confidence_p < 1 & confidence_p > 0)
    stopifnot(is.wholenumber(n_needed) & n_needed > 0)
    stopifnot(method %in% c("textbook", "binomial", "empirical", "poisson"))
    
    # stopifnot(confidence_p < 1 & confidence_p > 0)
    # stopifnot(effective_fertility_p > 0 & effective_fertility_p <=1)
    # stopifnot(is.wholenumber(n_needed) & n_needed > 0)
    # stopifnot(all(is.wholenumber(offsprings_n_sample)))
    # stopifnot(litter_sd > 0)
    # stopifnot(method %in% c("textbook", "binomial", "empirical", "poisson"))
    
    
    if(method=="textbook"){
        stopifnot(litter_sd > 0)
        stopifnot(litter_mean > 0)
        confidence_p1 = 1 - (1-confidence_p)/2
        nlit <- calculate_needed_litters_textbook(
            confidence_p=confidence_p1,
            litter_mean=litter_mean,
            litter_sd=litter_sd,
            n_neede=n_needed)
        nbre <- calculate_needed_breedings_textbook(
            confidence_p=confidence_p1,
            effective_fertility_p=effective_fertility_p,
            n_litters=nlit,
            calculation_type = calculation_type
        )
        return(nbre)
    }
    if(method=="binomial"){
        nbre <- calculate_needed_breedings_binomial(
            confidence_p = confidence_p, 
            effective_fertility_p = effective_fertility_p, 
            n_needed = n_needed, 
            litter_mean = litter_mean
        )
        return(nbre)
    }
    if(method=="empirical"){
        nbre <- calculate_needed_breedings_empirical(
            confidence_p = confidence_p, 
            effective_fertility_p = effective_fertility_p, 
            n_needed = n_needed, 
            offsprings_n_sample = offsprings_n_sample
        )
        return(nbre)
    }
    if(method=="poisson"){
        nbre <- calculate_needed_breedings_poisson(
            confidence_p = confidence_p, 
            effective_fertility_p = effective_fertility_p, 
            n_needed = n_needed, 
            litter_mean = litter_mean
        )
        return(nbre)
    }
}

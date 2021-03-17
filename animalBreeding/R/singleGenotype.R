#' Calculates the smallest number of breeding needed to yield a desired number 
#' of offsprings of a single genotype with a given probability of success
#'
#' @param confidence_p 
#' @param birth_days takes values 1,2,3 or 4: 
#' the desired offsprings should be born within `birth_days`; 
#' alternatibely, a number between 0 and 1 specifying the effective fertility 
#' of the animal
#' @param n_offsprings the desired number of offsprings
#' @param litter_mean average number of offpring for one animal
#' @param method takes values "festing" or "poisson"
#'
#' @return
#' @export
#'
#' @examples
singleGenotype <- function(
  confidence_p = 0.9,
  birth_days,
  n_offsprings,
  litter_mean = NULL,
  method = "poisson"
){
  # arguments needed for every function
  stopifnot(confidence_p < 1 & confidence_p > 0)
  stopifnot(is.wholenumber(n_offsprings) & n_offsprings > 0)
  stopifnot(method %in% 
              c("festing", "poisson"))
  try(if(!(birth_days %in% c(1,2,3,4) | (birth_days <= 1 & birth_days > 0) )) 
    stop("birth_days can only take values 1, 2, 3 or 4"))
  
  if (birth_days < 0){
    effective_fertility_p = birth_days
  }else{ # persentages from the Festing book, Table 3.11
    effective_fertility_p = (cumsum(c(13.4, 13.4, 35.0, 17.7))/100)[birth_days]
    }
  
  if(method=="festing"){
    litter_sd = 2.5
  }else{
    litter_sd = NULL
  }
  
  nbre <- calculate_needed_breedings(
    confidence_p = confidence_p, 
    effective_fertility_p = effective_fertility_p,
    n_needed = n_offsprings, 
    litter_mean = litter_mean,
    litter_sd = litter_sd,
    method = method)
  return(nbre)
}


# @TODO same input - animals born as output: move expect_born_poisson here 
#' Optimal number of breedings, multiple genotypes setup
#' 
#' Calculates the smallest number of breedings needed to yield a desired number 
#' of offsprings of multiple genotypes with a given probability of success
#'
#' @param confidence_p 
#' @param birth_days takes values 1,2,3 or 4: 
#' the desired offsprings should be born within `birth_days`; 
#' alternatibely, a number between 0 and 1 specifying the effective fertility 
#' of the animal
#' @param genotypes_p probabilities of a single offspring to have each of the respective genotypes, should sum to 1
#' @param genotypes_N required number of offsprings of the respective genotypes
#' @param litter_mean average number of offpring for one animal
#' @param sex_distribution takes values "unimportant", "all one sex", "balanced"; 
#' reflects the distribution of sexes among the offsprings: If "balanced", 
#' equal numbers of male and female offspring should be born for each genotype; 
#' if "all one sex", all of the required offsprings should be either male or female
#' if "unimportant", the offsprings of any sex suffice. 
#' Female and male offpsings are born with the same frequency (0.5).
#'
#' @return
#' @export
multiGenotype <- function(
  confidence_p = 0.9,
  birth_days = 3,
  genotypes_p = c(0.25,0.5,0.25),
  genotypes_N = c(0,0,10),  
  sex_distribution = c("unimportant", "all one sex", "balanced"),
  litter_mean = NULL
){
  # genotypes_N
  stopifnot(all(is.wholenumber(genotypes_N)))
  # sex_distribution
  stopifnot(
    sex_distribution %in% c("unimportant", "all one sex", "balanced")
    )
  
  # birthday value is valid
  try(if(!(birth_days %in% c(1,2,3,4) | (birth_days <= 1 & birth_days > 0) )) 
    stop("birth_days can only take values 1, 2, 3 or 4"))
  
  # birthday --> effective_fertility_p
  if (birth_days < 0){
    effective_fertility_p = birth_days
  }else{ # persentages from the Festing book, Table 3.11
    effective_fertility_p = (cumsum(c(13.4, 13.4, 35.0, 17.7))/100)[birth_days]
  }
  
  # other checks inside the breed_genotype function
  
  
  if(sex_distribution == "unimportant"){
    nbre <- breed_genotype(
      confidence_p = confidence_p, 
      effective_fertility_p = effective_fertility_p, 
      genotypes_p = genotypes_p,
      genotypes_N = genotypes_N, 
      litter_mean = litter_mean, 
      method = "poisson")
  }
  if(sex_distribution == "all one sex"){
    nbre <- breed_genotype(
      confidence_p = confidence_p, 
      effective_fertility_p = effective_fertility_p, 
      genotypes_p = c(genotypes_p/2, 0.5),
      genotypes_N = c(genotypes_N, 0), 
      litter_mean = litter_mean, 
      method = "poisson")
  }
  if(sex_distribution == "balanced"){
    if( try( !all(is.wholenumber(genotypes_N/2) ) )){
      stop(
        message = "For balanced experiment setup, all genotype_N values should be even.\n")
    }
    nbre <- breed_genotype(
      confidence_p = confidence_p, 
      effective_fertility_p = effective_fertility_p, 
      genotypes_p = c(genotypes_p/2, genotypes_p/2),
      genotypes_N = c(genotypes_N, genotypes_N)/2, 
      litter_mean = litter_mean, 
      method = "poisson")
  }
  return(nbre)
}


# @TODO same input - animals born as output: move expect_born_poisson here 
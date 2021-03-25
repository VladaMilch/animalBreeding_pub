#' Expected total number of animals born in a breeding setup
#'
#' @param breedingObj object created by multiGenotype function
#'
#' @return
#' @export
expectBorn <- function(breedingObj){
  kk = breedingObj$required_breedings
  doofN <- distr::convpow(breedingObj$doof1_obj, N=kk)
  
  return( hist(
    doofN@r(n = 10000), 
    freq = FALSE, 
    xlab = "Pups born", 
    ylab="Probability",
    main = "Total number of born animals", 
    col = "lightblue",
    breaks = 20
    )
  )
}


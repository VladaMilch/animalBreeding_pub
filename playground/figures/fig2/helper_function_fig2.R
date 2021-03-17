calc_data4fig_2A <- function(
  req_pups = c(12,15),
  genotype_probability = 1, 
  confiP = 0.9, 
  effertyP = 0.7, 
  litmean = 7, 
  litsd = 2.5
){
  
  # required number of breeding for confidence 90%, Festing
  # for one genotype
  # HOWEVER! if genotype probability is < 1 
  # (i.e. several genotypes with a single non-zero required genotype),
  # THEN, we require x/probability_genotype instead of x
  # only for festing
  req_festing <- sapply(
    X = req_pups, 
    FUN = function(x){
      x_required <- ceiling(x/genotype_probability)
      calculate_needed_breedings(
        confidence_p = confiP, 
        effective_fertility_p = effertyP, 
        n_needed = x_required, 
        litter_mean = litmean, 
        litter_sd = litsd, # default, in accordance with the Festing book
        method = "festing" # 
      )
    }
  )
  
  
  # Poisson 90%
  req_poisson <- sapply(
    X = req_pups, 
    FUN = function(x)breed_genotype(
      confidence_p = confiP, 
      genotypes_p = c(genotype_probability, 1-genotype_probability),
      genotypes_N = c(x,0),
      effective_fertility_p = effertyP, 
      litter_mean = litmean, 
      method = "poisson" 
    )
  )
  
  req_pups_total <- ceiling(req_pups/genotype_probability)
  # intuitive Mendel: required numberf of breedings ti get the desired number of pups
  req_mendel <- ceiling(req_pups_total/(effertyP*litmean))
  
  reqDF <- data.frame(
    req_pups, req_poisson, req_festing, req_mendel)
  
  return(reqDF)
}
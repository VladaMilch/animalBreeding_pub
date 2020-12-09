library("animalBreeding")
setwd("/home/compbio/vmilchev/animalBreedingProj/playground/figures/fig4_other/")
source("./helper_functions.R")

# the desired number of pups
req_pups_1000 <- c(seq(10, 40, 10), seq(50, 150, 25), seq(200, 500, 100), 700, 1000)


calc_data4fig4 <- function(
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
  
  
fe70 <- lapply(X = c(1, 0.5, 0.25, 0.125, 0.0625), FUN = function(genoP){
  calc_data4fig4(
    req_pups = req_pups_1000,
    genotype_probability = genoP, 
    confiP = 0.9, 
    effertyP = 0.7, litmean = 7)
})

fe100 <- lapply(X = c(1, 0.5, 0.25, 0.125, 0.0625), FUN = function(genoP){
    calc_data4fig4(
      req_pups = req_pups_1000,
      genotype_probability = genoP, 
      confiP = 0.9, 
      effertyP = 1, litmean = 7)
  })



required_F70 %>%
  ggplot(aes(x=req_pups)) + 
  #geom_point(aes(y=req_br_festing50, col = "Festing 50%"), colour = "red", alpha=0.5, size=0.5) + 
  #geom_line(aes(y=req_br_festing50, col = "Festing 50%"), alpha=0.3, size=0.3) + 
  #geom_point(aes(y=req_br_festing90, col = "Festing 90%")) +
  geom_point(aes(y=req_br_festing95, col = "Festing 95%")) + 
  geom_point(aes(y=req_br_poisson95, col = "Poisson 95%")) + 
  geom_point(aes(y=req_mendel, col = "Mendel ( = Poisson 50% )")) + 
  labs(x="Required number of pups", 
       y = "Smallest number of breedings", 
       caption = "Mean litter size: 7,  Effective fertility: 70%", 
       title = "Model comparison for the smallest required number of breegins", colour = "Method") + 
  theme(legend.position="right") 

save(required_F70, file = "required_fertility70.RData")

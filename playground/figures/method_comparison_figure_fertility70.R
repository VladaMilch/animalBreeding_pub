load("animalBreeding")
library(devtools)
library(ggplot2)
# the desired number of pups
req_pups <- c(seq(10, 40, 10), seq(50, 150, 25), seq(200, 500, 100), 700, 1000)



# required number of breeding for confidence 50%, Festing
req_br_festing50 <- sapply(
  X = req_pups, 
  FUN = function(x)calculate_needed_breedings(
    confidence_p = 0.50, 
    effective_fertility_p = 0.7, 
    n_needed = x, 
    litter_mean = 7, 
    litter_sd = 2.5, # default, in accordance with the Festing book
    method = "festing" # 
  )
)

# required number of breeding for confidence 90%, Festing
req_br_festing90 <- sapply(
  X = req_pups, 
  FUN = function(x)calculate_needed_breedings(
    confidence_p = 0.90, 
    effective_fertility_p = 0.7, 
    n_needed = x, 
    litter_mean = 7, 
    litter_sd = 2.5, # default, in accordance with the Festing book
    method = "festing" 
  )
)

# required number of breeding for confidence 95%, Festing
req_br_festing95 <- sapply(
  X = req_pups, 
  FUN = function(x)calculate_needed_breedings(
    confidence_p = 0.95, 
    effective_fertility_p = 0.7, 
    n_needed = x, 
    litter_mean = 7, 
    litter_sd = 2.5, # default, in accordance with the Festing book
    method = "festing" 
  )
)

# required number of breeding for confidence 50%, Poisson
req_br_poisson50 <- sapply(
  X = req_pups, 
  FUN = function(x)calculate_needed_breedings(
    confidence_p = 0.50, 
    effective_fertility_p = 0.7, 
    n_needed = x, 
    litter_mean = 7, 
    method = "poisson" 
  )
)

# Poisson 90%
req_br_poisson90 <- sapply(
  X = req_pups, 
  FUN = function(x)calculate_needed_breedings(
    confidence_p = 0.90, 
    effective_fertility_p = 0.7, 
    n_needed = x, 
    litter_mean = 7, 
    method = "poisson" 
  )
)

# Poisson 95%
req_br_poisson95 <- sapply(
  X = req_pups, 
  FUN = function(x)calculate_needed_breedings(
    confidence_p = 0.95, 
    effective_fertility_p = 0.7, 
    n_needed = x, 
    litter_mean = 7, 
    method = "poisson" 
  )
)

# intuitive Mendel: required numberf of breedings ti get the desired number of pups
ceiling(req_pups/(0.7*7))

required_F70 <- data.frame(req_pups, 
           req_br_festing50, req_br_festing90, req_br_festing95, 
           req_br_poisson50, req_br_poisson90, req_br_poisson95, 
           req_mendel = ceiling(req_pups/(0.7*7))) 
required_F70 %>%
  ggplot(required_F70, aes(x=req_pups)) + 
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
             
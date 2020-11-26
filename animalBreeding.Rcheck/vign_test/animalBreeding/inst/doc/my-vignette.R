## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)
library(ggplot2)

## ---- eval = FALSE------------------------------------------------------------
#  library(devtools)
#  # install_github("IMSBComputationalBiology/animalBreedingProj",
#  #                subdir="animalBreeding",
#  #                auth_token = "f6024685998b779a9db258994a7b4e44468b754c",
#  #                force=TRUE)

## ----setup, message=FALSE, comment=FALSE, cache=FALSE, results=FALSE----------
library(animalBreeding)

## -----------------------------------------------------------------------------
calculate_needed_breedings(
    confidence_p = 0.95, 
    effective_fertility_p = 0.65, 
    n_needed = 200, 
    litter_mean = 7, 
    litter_sd = 2.5, # default, in accordance with the Festing book
    method = "festing" # or "poisson", "binomial", "empirical"
    )

## -----------------------------------------------------------------------------
calculate_needed_litters_textbook(
    confidence_p = 0.975, 
    litter_mean = 4, 
    litter_sd = 2.5,
    n_needed = 25)

## -----------------------------------------------------------------------------
calculate_needed_breedings_textbook(
    confidence_p = 0.975, 
    effective_fertility_p = 0.2, 
    n_litters = 15, 
    textbook_error = TRUE) # default FALSE, we should rename this argument or leave it out..


## -----------------------------------------------------------------------------
calculate_needed_breedings_textbook(
    confidence_p = 0.975, 
    effective_fertility_p = 0.2, 
    n_litters = 15)

## -----------------------------------------------------------------------------
# probability to get 10 or more litters from 110 breedings
1 - pbinom(q = 14, size = 113, prob = 0.2)

# probability to get 10 or more litters from 109 breedings
1 - pbinom(q = 14, size = 112, prob = 0.2)

# thus, 110 is the minimal required number of breedings to have P>0.975

## -----------------------------------------------------------------------------
# should be close to poisson, we may remove this method later on
calculate_needed_breedings(
    confidence_p = 0.95, 
    effective_fertility_p = 0.6, 
    n_needed = 200, 
    litter_mean = 7, 
    method = "binomial"
    )

# the main alternative
calculate_needed_breedings(
    confidence_p = 0.95, 
    effective_fertility_p = 0.6, 
    n_needed = 200, 
    litter_mean = 7, 
    method = "poisson"
    )


# should be the same as poisson, because I provided an "empirical" observation
# generated from a Poisson distribution
calculate_needed_breedings(
    confidence_p = 0.95, 
    effective_fertility_p = 0.6, 
    n_needed = 200, 
    offsprings_n_sample = rpois(n = 1000, lambda = 7),
    method = "empirical"
    )

# textbook
calculate_needed_breedings(
    confidence_p = 0.95, 
    effective_fertility_p = 0.6, 
    n_needed = 200, 
    litter_mean = 7, 
    litter_sd = 2.5,
    method = "festing"
    )


## -----------------------------------------------------------------------------
x <- breed_genotype(
    confidence_p = 0.95, 
    effective_fertility_p = 0.7, 
    genotypes_p = c(0.25, 0.5, 0.25), 
    genotypes_N = c(10,15,5),
    litter_mean = 4, 
    method = "poisson")
x




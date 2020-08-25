## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  library(devtools)
#  install_github("IMSBComputationalBiology/animalBreedingProj",
#                 subdir="animalBreeding",
#                 auth_token = "f6024685998b779a9db258994a7b4e44468b754c",
#                 force=TRUE)

## ----setup, message=FALSE, comment=FALSE, cache=FALSE, results=FALSE----------
library(animalBreeding)

## -----------------------------------------------------------------------------
calculate_needed_litters_textbook(
    condifence_p = 0.975, 
    litter_mean = 4, 
    litter_sd = 2.5,
    n_needed = 25)

## -----------------------------------------------------------------------------
calculate_needed_breedings_textbook(
    condifence_p = 0.975, 
    effective_fertility_p = 0.2, 
    n_litters = 15,
    calculation_type='textbook_exact')


## -----------------------------------------------------------------------------
calculate_needed_breedings_textbook(
    condifence_p = 0.975, 
    effective_fertility_p = 0.2, 
    n_litters = 15,
    calculation_type='')

## -----------------------------------------------------------------------------
# probability to get 10 or more litters from 110 breedings
1 - pbinom(q = 14, size = 113, prob = 0.2)

# probability to get 10 or more litters from 109 breedings
1 - pbinom(q = 14, size = 112, prob = 0.2)

# thus, 110 is the minimal required number of breedings to have P>0.975

## -----------------------------------------------------------------------------
calculate_needed_breedings(
    condifence_p = 0.95, 
    effective_fertility_p = 0.2, 
    n_needed = 25, 
    litter_mean = 4, 
    litter_sd = 2.5 )


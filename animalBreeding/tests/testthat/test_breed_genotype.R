require(testthat)


############# 1 ###############
####  Falling glacefully  #####

test_that(desc = "Falling glacefully: Confidence",{
  expect_error(
    breed_genotype(
      confidence_p = 1, 
      effective_fertility_p = 1, 
      litter_mean = 7, 
      method = "festing"))
  
  expect_error(
    breed_genotype(
      confidence_p = 0, 
      effective_fertility_p = 1, 
      litter_mean = 7, 
      method = "festing"))
  
})

test_that(desc = "Falling glacefully: Effective fertility",{
  expect_error(
    breed_genotype(
      confidence_p = 0.5, 
      effective_fertility_p = 0, 
      litter_mean = 7, 
      method = "festing"))
  
  expect_error(
    breed_genotype(
      confidence_p = 0.5, 
      effective_fertility_p = 1.1, 
      litter_mean = 7, 
      method = "festing"))
  
})

test_that(desc = "Falling glacefully: Methods general",{
  expect_error(
    breed_genotype(
      confidence_p = 0.5, 
      effective_fertility_p = 0.5, 
      litter_mean = 7, 
      method = "tet"))
})

# @TODO: chenage that fpr the genotype-specific errors
test_that(desc = "Falling glacefully: Offstrings, litter",{
  expect_error(
    breed_genotype(
      confidence_p = 0.5, 
      effective_fertility_p = 0.5, 
      genotypes_N = c(0), 
      genotypes_p = 1,
      litter_mean = 7, 
      method = "festing"))
  
  expect_error(
    breed_genotype(
      confidence_p = 0.5, 
      effective_fertility_p = 0.5, 
      litter_mean = 0, 
      method = "festing"))
  
})


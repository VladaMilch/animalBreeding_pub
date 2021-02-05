# expected number of animals born under the assumprtions of our model
# distribution is returned
expect_born_poisson <- function(
  effective_fertility_p,
  n_breedings,
  litter_mean
){
  freqs_r <- dpois(x = seq(1,round(4*litter_mean), 1), lambda = litter_mean)
  freqs <- freqs_r/sum(freqs_r)
  supp1 = as.numeric(c(0, seq(1,round(4*litter_mean))))
  prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
  stopifnot(sum(prob1)==1)
  
  #search_interval <- seq(1,10)
  doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
  doofN <- distr::convpow(doof1, N=n_breedings)
  return(doofN)
}

expect_born_genotype_poisson <- function(
  effective_fertility_p,
  genotype_probability,
  n_breedings,
  litter_mean
){
  freqs_r <- dpois(x = seq(1,round(4*litter_mean), 1), lambda = litter_mean)
  freqs <- freqs_r/sum(freqs_r)
  supp1 = as.numeric(c(0, seq(1,round(4*litter_mean))))
  prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
  stopifnot(sum(prob1)==1)
  
  #search_interval <- seq(1,10)
  doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
  doofN <- distr::convpow(doof1, N=n_breedings)
  return(doofN)
}


# qunatiles and the mean of the doofN
# -- the offpring distribution of N breedings
ff <- function(xx){
  distrConv <- expect_born_poisson(effective_fertility_p = 0.7, n_breedings = xx, litter_mean = 7)
  #plot(distrConv@d(x = distrConv@support))
  return(  
    c(median(distrConv@r(n = 10000)),
      distrConv@q(p=c(0.5, 0.1, 0.05)),
      floor(mean(distrConv@r(n = 10000)))
    )
  )
  
}

#debug(ff)
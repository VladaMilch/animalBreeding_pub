calculate_needed_breedings_poisson <-   function(
    confidence_p,
    effective_fertility_p,
    n_needed,
    litter_mean
){
    freqs <- dpois(x = seq(1,round(4*litter_mean), 1), lambda = litter_mean)
    freqs <- freqs/sum(freqs)
    supp1 = as.numeric(c(0, seq(1,round(4*litter_mean))))
    prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
    stopifnot(sum(prob1)==1)
    
    #search_interval <- seq(1,10)
    doof1 <- DiscreteDistribution(supp = supp1, prob = prob1)
    doofN_quantile = 0
    k=1
    while(doofN_quantile < n_needed){
      #doofN_quantile <- sapply(search_interval, FUN = function(k){distr::convpow(doof1, N=k)@q(condifence_p)})
      doofN_quantile <- distr::convpow(doof1, N=k)@q(1-confidence_p)
      k <- k+1
    }
    return(k-1)
}
  

breed_genotype <- function(
    confidence_p,
    effective_fertility_p,
    genotypes_p = c(0.25,0.5,0.25),
    genotypes_N = c(0,0,10),
    litter_mean = NULL,
    litter_sd = NULL,
    binomial_p=NULL,
    offsprings_n_sample = NULL, # should be a large vector, n of offs for a single mouse
    method = "poisson"
){
    stopifnot(length(genotypes_p)==length(genotypes_N))
    stopifnot(sum(genotypes_p)==1)
    stopifnot(all(genotypes_N>=0) & sum(genotypes_N)>0)
    stopifnot(confidence_p > 0 & confidence_p < 1)
    stopifnot(effective_fertility_p > 0 & effective_fertility_p <= 1)
    
    stopifnot(is.null(litter_mean) || litter_mean > 0)
    stopifnot(is.null(litter_sd) || litter_sd > 0)
    stopifnot(is.null(binomial_p) || (binomial_p > 0 & binomial_p <=1))
    
    stopifnot(method %in% c("festing", "binomial", "empirical", "poisson"))
    
    # object for the distribution of offsprings for 1 mother
    if(method=="poisson"){
        doof1 <- generate_poisson_doof1(
            litter_mean = litter_mean, 
            effective_fertility_p = effective_fertility_p)
    }
    if(method=="binomial"){
        if(is.null(binomial_p))
        {
          binomial_p=0.5
          warning("No input in binomial_p => Binomial model fitted with p=0.5")
        }
        doof1 <- generate_binomial_doof1(
            litter_mean = litter_mean, 
            binomial_p = binomial_p, 
            effective_fertility_p = effective_fertility_p)
    }
    if(method=="empirical"){
        if(is.null(offsprings_n_sample)){
            errorCondition(message="Empirical sample for the number of 
            offsprings produced by 1 mouse is needed to use the 
            method=empirical")
      }
        doof1 <- generate_empirical_doof1(
            effective_fertility_p = effective_fertility_p,
            offsprings_n_sample =  offsprings_n_sample)
    }
    if(method=="festing"){
        errorCondition(
          message="No genotype-specific calculation for method festing")
    }
    
    confi = 0 # confidence
    k=1
    while(confi < confidence_p){
        doofK <- distr::convpow(doof1, N=k)
        if(max(doofK@support) <= sum(genotypes_N-1)){
            k <- k+1
            next
        }
        total_offs_seq <- seq(
            from = max(sum(genotypes_N-1),0), 
            to = max(doofK@support), 
            by = 1)
        p_genotypes_by_total <- pmultinom::pmultinom(
            size = total_offs_seq, 
            lower = genotypes_N-1, # because ksi > lower (not >=)
            probs = genotypes_p, 
            method = "exact")
        d_total <- distr::d(doofK)(total_offs_seq)
        confi <- sum(p_genotypes_by_total*d_total)
        #cat(confi)
        #cat("\n")
        k <- k+1
    }
    return(k-1)
    
    
}

generate_poisson_doof1 <- function(litter_mean, 
                                   effective_fertility_p
){
    almost_certain_positive_support <- seq(
        1,
        round(qpois(p = 1-0.1^10, lambda = litter_mean)), 
        1)
    freqs <- dpois(x = almost_certain_positive_support, 
                   lambda = litter_mean)
    freqs <- freqs/sum(freqs)
    supp1 = as.numeric(c(0, almost_certain_positive_support))
    prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
    stopifnot(sum(prob1)==1)
    # distribution of offsprings for 1 mother
    doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
    return(doof1)
}


generate_binomial_doof1 <- function(
    litter_mean, 
    binomial_p, 
    effective_fertility_p
){
    binomialSize = round(litter_mean/binomial_p)
    positive_support <- seq(1,round(litter_mean/binomial_p), 1)
    freqs <- dbinom(
        x = positive_support, # only positive values
        size = binomialSize, 
        prob = binomial_p)
    freqs <- freqs/sum(freqs)
    supp1 = as.numeric(c(0, positive_support)) # full support
    prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
    # distribution of offsprings for 1 mother
    doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
    return(doof1)
}

generate_empirical_doof1 <- function(
    effective_fertility_p = effective_fertility_p, 
    offsprings_n_sample = offsprings_n_sample
){
    stopifnot(all(offsprings_n_sample >= 0))
    stopifnot(all(is.wholenumber(offsprings_n_sample)))
  
    offsprings_n_sample <- offsprings_n_sample[which(offsprings_n_sample!=0)]
    if(length(offsprings_n_sample) < 100){
        warning("Empirical sample too small? Less than 100 positive values.")
    }

    freqs <- table(offsprings_n_sample)/length(offsprings_n_sample)
    supp1 = as.numeric(c(0, names(freqs)))
    prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
    stopifnot(sum(prob1)==1)
    
    # distribution of offsprings for 1 mother
    doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
    return(doof1)
    
}

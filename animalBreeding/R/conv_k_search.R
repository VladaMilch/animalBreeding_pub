# search of the min k such that conv(k, doof1) produces n_needed 
# with confidence >= required confidence
conv_k_search <- function(
  confidence_p,
  doof1,
  genotypes_N,
  genotypes_p,
  largest_step_size = 32 # must be 2^
){
  k = max(1, 
          floor(sum(genotypes_N)/median(doof1@r(100))) - largest_step_size)
  step_size = largest_step_size

  confi_low <- calculate_confi(k=k, 
                               doof1=doof1, 
                               genotypes_N=genotypes_N, 
                               genotypes_p=genotypes_p)
    
  while(confi_low < confidence_p){
    confi_hig <- calculate_confi(k=k+step_size, 
                                   doof1=doof1, 
                                   genotypes_N=genotypes_N, 
                                   genotypes_p=genotypes_p)
    
    if(confi_hig < confidence_p){
        # keep jumping
        confi_low <- confi_hig
        k <- k+step_size
    }else if(step_size > 1){
        # confi_low stays as it is
        # confi_high by half step
        step_size <- max(step_size/2,1)
        # cat("step size decreased")
    }else if(step_size==1){
      # take k corresponding to confi_high
      break
    }
  }
  res = k+step_size
  aa = calculate_confi(k=res, 
                       doof1=doof1, 
                       genotypes_N=genotypes_N, 
                       genotypes_p=genotypes_p)
  bb = calculate_confi(k=res-1, 
                       doof1=doof1, 
                       genotypes_N=genotypes_N, 
                       genotypes_p=genotypes_p)
  stopifnot(aa >= confidence_p & bb <confidence_p)
  return(res)
}

calculate_confi <- function(
  k,
  doof1,
  genotypes_N,
  genotypes_p
){
  doofK <- distr::convpow(doof1, N=k)
  
  if(sum(genotypes_N-1) >= max(doofK@support))
  {
    return(0)
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
  return(confi)
}


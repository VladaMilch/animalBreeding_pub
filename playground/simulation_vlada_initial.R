# simulated data 
set.seed(12) # to make sure we have zeroes
empirical_total_births <- rnbinom(100, prob = 0.5, size = 5)
empirical_total_births
empirical_gntp_births <- t(sapply(
    empirical_total_births, 
    function(tot){
    rmultinom(n=1,size=tot,prob = c(1/4,1/2,1/4))
}))
colnames(empirical_gntp_births) <- c("hom", "het", "wt")


# distribution of the number of offstrings of one mother
# later empirical, now - based on the simulated data
p_nborn <- ecdf(empirical_data_births)

# number of animals needed at a specific time point
need_hom=10 # homozygous
need_het=0 
need_wt=10
need <- c(need_hom, need_het, need_wt)



# Constructing Quadratic Formula
solve_quadratic <- function(a,b,c){
  if(delta(a,b,c) > 0){ # first case D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    result = c(x_1,x_2)
    return(result)
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    x = -b/(2*a)
    return(x)
  }
  else {"There are no real roots."} # third case D<0
}

# Constructing delta
delta<-function(a,b,c){
  b^2-4*a*c
}


round(result(a = 1, b = -qnorm(p = 1-0.975), c = -30)^2)

cfp = 0.975
efp = seq(0.15, 0.9, 0.05)
nlit = 10



ff <- function(efp_current)max(solve_quadratic(
  a = 1, 
  b = -round(qnorm(p = 1-cfp),digits = 1)*sqrt(1-efp_current),
  c = -nlit)^2)/efp_current


sapply(efp, FUN = function(x)ff(x))

calculate_needed_litters <- function(
    condifence_p, 
    litter_mean, 
    litter_sd=2.5, 
    n_needed){
   # Nlitters
  quantile_of_sum <- function(Nlitters){
      qnorm(p=1-condifence_p, 
          mean=litter_mean*Nlitters, 
          sd = litter_sd*sqrt(Nlitters))
  }
  neededL <- optimise(
      f=function(nn){
          abs(quantile_of_sum(nn)-n_needed)
          }, 
      lower = 1, upper = n_needed)
  return(neededL[1])
}
  
calculate_needed_litters(n_needed = 50, litter_mean = 2, condifence_p = 0.975)




calculate_needed_breedings <- function(
    condifence_p, 
    effective_fertility_p,
    n_litters
){
    search_interval <- c((n_litters+1):10^5)
    Nbreedings <- search_interval[
        min(which(pbinom(
            q = n_litters-1, 
            size = search_interval, 
            prob = effective_fertility_p, lower.tail = F) >= condifence_p))
        ]

    return(Nbreedings)
}

calculate_needed_breedings(0.975, 0.15, 11)

ff <- function(nn){
    abs(qbinom(p = 0.05, size = nn, prob = 0.6)-10)
  }
min(which(ff(1:100)==0))

ff(23)

pbinom(q = 49, size = 430, prob = 0.15, lower.tail = F)
pbinom(q = 1, size = 10, prob = 0.4, lower.tail = T)
dbinom(x = 1, size = 10, prob = 0.4) + dbinom(x = 0, size = 10, prob = 0.4)
pbinom(q = 0, size = 10, prob = 0.4, lower.tail = F)
pbinom(q = 1, size = 10, prob = 0.4, lower.tail = F) + dbinom(x = 1, size = 10, prob = 0.4)

pbinom(q = 9, size = 110, prob = 0.15, lower.tail = F) 


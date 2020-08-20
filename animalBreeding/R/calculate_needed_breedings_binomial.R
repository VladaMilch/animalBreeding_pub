# fertility
# mean

# fit binomial distribution such that 
# the mean fits, and then add to the zeroes such that
# the fertility fits

# one mouse gives birth to np offstrings
# let's say we choose the Binomial distribution with the highest possible 
# variance (np(1-p)), achieved when p=0.5
# then, n=size = 2*round(m)

fert = 0.6
litter_mean = 7

barplot(dbinom(x = c(0:50), size = 50, prob = 0.5))

doffsprings <- function(value, litter_mean, fertility){
    stopifnot(all(value >=0))
    n = round(litter_mean)*2
    normalization_coeff <- fertility/(1-dbinom(x=0, size = n, prob = 0.5))
    res = ifelse(value==0, 1-fertility, dbinom(x=value, size = n, prob = 0.5)*normalization_coeff)
    return(res)
}

rr = doffsprings(value = seq(0,20,1), litter_mean = 10, fertility = 0.77)
sum(rr[-1])

# say, fertility is also binomial: Out of K mice, k will be fertile,
# and the probability of success is the fertilityP (as above)
# then, we have the conditional binomials (see wiki)
#
# X1, ..., Xk - off of a fertile mice (may produce 0 off) and 
# X1~Bin(n,p), then
# Y=X1+X2+..+Xk, Y~Bin(k*n, p) - total number of ofs
#
# k ~ Bin(K, fertilityP)
# Then, Y|k ~ Bin(k*n, p)
# NOPE, just that - the conditional did not work

needed_offs = 50
search_interval <- c(1:10^5)
condifence_p = 0.95
litter_mean = 7
fertility=0.77

Nbreedings <- search_interval[
    min(which(pbinom(
        q = needed_offs-1, 
        size = search_interval*2*litter_mean, 
        prob = 0.5, lower.tail = F) >= condifence_p))
    ]

confi <- function(K, needed_offs=50, fertility=0.7, litter_mean=8){
    k_values <- seq(0, K, 1)
    sum(pbinom(
        q = needed_offs-1, 
        size = k_values*2*litter_mean, 
        prob = 0.5, lower.tail = F)*
        dbinom(k_values, size = K, prob = fertility-dbinom(0, size = 2*litter_mean, prob = 0.5)))
}


barplot(sapply(c(1:20), confi))
min(which(
  sapply(c(1:20), confi)>=0.95
))
confi(13)

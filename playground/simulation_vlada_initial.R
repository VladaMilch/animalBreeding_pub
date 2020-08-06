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




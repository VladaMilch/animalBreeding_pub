# Pretty pictures

#Fix litter size (7) and effective fertility (0.6), 
#X - desired number of offsprings, Y - needed number of breedings
library(ggplot2)
library(animalBreeding)

N_range = c( seq(25, 300, 25), seq(350, 1000, 50), 
             1250, 1500, 2000, 2500, 3000, 3500, 4000)
required_power = 0.9
Y_festing_1 = sapply(N_range, FUN = function(x){
  calculate_needed_breedings(
    confidence_p = required_power, 
    effective_fertility_p = 0.6, 
    n_needed = x, 
    litter_mean = 7, 
    method = "festing"
  )
})

Y_festing_2 = sapply(N_range, FUN = function(x){
  calculate_needed_breedings(
    confidence_p = required_power, 
    effective_fertility_p = 0.6, 
    n_needed = x, 
    litter_mean = 7, 
    litter_sd = sqrt(7),
    method = "festing"
  )
})

Y_poisson <- sapply(N_range, FUN = function(x){
  calculate_needed_breedings(
    confidence_p = required_power, 
    effective_fertility_p = 0.6, 
    n_needed = x, 
    litter_mean = 7, 
    method = "poisson"
  )
})

 # variance of the binomial = np(1-p)
bin_p <- 1-2.5/litter_mean
bin_N <- litter_mean/bin_p

Y_binomial <- sapply(N_range, FUN = function(x){
  calculate_needed_breedings(
    confidence_p = required_power, 
    effective_fertility_p = 0.6, 
    n_needed = x, 
    litter_mean = 7, 
    method = "binomial", 
  )
})

# Y_empinorm <- sapply(N_range, FUN = function(x){
#   calculate_needed_breedings(
#     confidence_p = 0.95,
#     effective_fertility_p = 0.6,
#     n_needed = x, offsprings_n_sample = rnorm(n = 1000, mean = 7, sd = 2.5),
#     method = "empirical"
#   )
# })

df <- data.frame(n_offsprings=N_range,
                 poisson = Y_poisson, 
                 binomial = Y_binomial, 
                 festing1 = Y_festing_1,
                 festing2 = Y_festing_2#,
                 #empinorm = Y_empinorm
)

pdf(file = "required_breedings_by_method.pdf")

p_small <- ggplot(data=df[1:10,], aes(n_offsprings))+
  geom_line(aes(y=poisson, col = "poisson"))+
  geom_line(aes(y=binomial, col = "binomial"))+
  geom_line(aes(y=festing1, col = "festing1")) + 
  geom_line(aes(y=festing2, col = "festing2")) + 
  labs(title = "Required number of breegings for confidence level 95%",
       subtitle = "Poisson, Binomial and Festing models",
       caption = "average litter size = 7, effective fertility = 60%", 
       x = "Desired N offsprings", 
       y = "Required number of breedings")

p_large <- ggplot(data=df[,], aes(n_offsprings))+
  geom_line(aes(y=poisson, col = "poisson"))+
  geom_line(aes(y=binomial, col = "binomial"))+
  geom_line(aes(y=festing1, col = "festing1")) + 
  geom_line(aes(y=festing2, col = "festing2")) + 
  labs(title = "Required number of breegings for confidence level 95%",
       subtitle = "Poisson, Binomial and Festing models",
       caption = "average litter size = 7, effective fertility = 60%", 
       x = "Desired N offsprings", 
       y = "Required number of breedings")

print(p_small)
print(p_large)
dev.off()

calculate_needed_breedings(
  confidence_p = required_power, 
  effective_fertility_p = 0.6, 
  n_needed = 1000, 
  litter_mean = 7, 
  method = "binomial", 
)


calculate_needed_breedings(
  confidence_p = required_power, 
  effective_fertility_p = 0.6, 
  n_needed = 1000, 
  litter_mean = 7, 
  method = "festing", 
)

calculate_needed_breedings(
  confidence_p = required_power, 
  effective_fertility_p = 0.6, 
  n_needed = 1000, 
  litter_mean = 7, 
  method = "poisson", 
)

calculate_needed_breedings(
  confidence_p = required_power, 
  effective_fertility_p = 0.6, 
  n_needed = 1000, 
  litter_mean = 7, 
  method = "poisson", 
)

calculate_needed_breedings(
  confidence_p = required_power, 
  effective_fertility_p = 0.6, 
  n_needed = 1000, 
  offsprings_n_sample = round(rnorm(n = 1000, mean = 7, sd = 2.5)),
  method = "empirical"
)

setwd("/home/compbio/vmilchev/animalBreedingProj/playground/figures/")
source("helper_functions_born.R")
library(RColorBrewer)
library(animalBreeding)


###################### genotype probability = 1 ########################
born10 <- expect_born_poisson(effective_fertility_p = 0.7, 
                              n_breedings = 10, 
                              litter_mean = 7)
born3 <- expect_born_poisson(effective_fertility_p = 0.7, 
                             n_breedings = 3, 
                             litter_mean = 7)
#max(born10@support)
full_support <- seq(0, max(born10@support)) 
plot_support <- seq(0, 100)

dat <- data.frame(animals_born = plot_support,
                  b3 = born3@d(plot_support), 
                  b10 = born10@d(plot_support))
barplot(dat$b3)
barplot(dat$b10)

data2plot <- reshape2::melt(dat, id.vars = 1)
head(data2plot)

data2plot$variable <- as.factor(data2plot$variable)
pp <- ggplot(dat,aes(x=animals_born)) + 
  geom_bar(aes(y=b3), stat="identity", fill = "gray80", 
           alpha=0.5, col = "gray10") +
  geom_bar(aes(y=b10), stat="identity", fill = "gray20", 
           alpha=0.5, col = "gray10") +
  theme_pubr() + 
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  xlab("Animals born") + ylab("Probability")
pp
ggsave(pp, filename = "f1d.pdf", width = 7, height = 5)


###################### genotype probability = 0.25 ########################
d_born_genotype_3 <- rowSums(
  outer(X = c(0:33), 
        Y = full_support, 
        FUN=function(x,y){
          exp(dbinom(x = x, size = y, prob = 0.25, log = T) + 
              born3@d(y, log = TRUE))
  }))
sum(d_born_genotype_3)

d_born_genotype_10 <- rowSums(
  outer(X = c(0:33), 
        Y = full_support, 
        FUN=function(x,y){
          exp(dbinom(x = x, size = y, prob = 0.25, log = TRUE) + 
              born10@d(y, log = TRUE))
          }))
sum(d_born_genotype_10)
barplot(d_born_genotype_10)

df <- data.frame(animals_born_genotype = c(0:33), 
                 breed3 = d_born_genotype_3, breed10=d_born_genotype_10)
pp <- ggplot(df,
             aes(x=animals_born_genotype)) + 
  geom_bar(aes(y=breed3), stat="identity", 
           fill = "gray80", alpha=0.5, col = "gray10") +
  geom_bar(aes(y=breed10), stat="identity", 
           fill = "gray20", alpha=0.5, col = "gray10") +
  theme_pubr() + 
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  xlab("Animals born") + ylab("Probability")
pp
ggsave(pp, filename = "f1d_025.pdf", width = 7, height = 5)

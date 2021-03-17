setwd("/home/compbio/vmilchev/animalBreedingProj/playground/figures/figsSupplementary/")
load("../fig2/fig2b/data2b.Rdata")
source("../helper_functions_born.R")


data90success <- subset(data2b, confidence == 0.9)
born_quantiles <- do.call(
  "rbind",
  lapply(
    data90success$breedings, 
    FUN = function(x){
      # discrete distribution object
      dd <- expect_born_poisson(
        effective_fertility_p = 0.7, n_breedings = x, litter_mean = 7)
      # quantiles 
      res <- dd@q(p = c(0.1,0.5,0.9))
      return(res)
    }))
# born_quantiles = (a_ij)
# P(born <= a_ij| X_i breedings) = confidence Y_j
dim(born_quantiles) # breedings x confidence     
colnames(born_quantiles) <- paste0("confi_",c(0.1,0.5,0.9)*100)
rownames(born_quantiles) <- paste0("breed_",data90success$breedings)

data2plot <- cbind(data90success, born_quantiles)
head(data2plot)

plot1 <- ggplot(data2plot, 
                aes(x = offsprings, y = confi_50)) +
  geom_line() + 
  #geom_errorbar(aes(ymin=confi_10, ymax=confi_90), width=.1, 
  #              alpha=0.5, col = "blue") + 
  geom_ribbon(aes(ymin=confi_10, ymax=confi_90), linetype=3, 
              alpha=0.3, fill = "blue", col = "blue")+

  theme_pubr() +  
  ggtitle("Median number of born aminals for a setup with \n average litter 7 and average fertility 70%")+
  xlab("Desired number of offsprings (total)")+
  ylab("Median born")
  
plot1
ggsave(plot1, file = "supp_born_vs_required_total_c90_l7_f70.pdf",width = 5,height = 7)







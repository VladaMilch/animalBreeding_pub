library("animalBreeding")
setwd("/home/compbio/vmilchev/animalBreedingProj/playground/figures/fig4_other/")
source("./helper_functions.R")

# the desired number of pups
req_pups_1000 <- c(seq(10, 40, 10), seq(50, 150, 25), seq(200, 500, 100), 700, 1000)


calc_data4fig4 <- function(
  req_pups = c(12,15),
  genotype_probability = 1, 
  confiP = 0.9, 
  effertyP = 0.7, 
  litmean = 7, 
  litsd = 2.5
  ){

  # required number of breeding for confidence 90%, Festing
  # for one genotype
  # HOWEVER! if genotype probability is < 1 
  # (i.e. several genotypes with a single non-zero required genotype),
  # THEN, we require x/probability_genotype instead of x
  # only for festing
  req_festing <- sapply(
    X = req_pups, 
    FUN = function(x){
      x_required <- ceiling(x/genotype_probability)
      calculate_needed_breedings(
        confidence_p = confiP, 
        effective_fertility_p = effertyP, 
        n_needed = x_required, 
        litter_mean = litmean, 
        litter_sd = litsd, # default, in accordance with the Festing book
        method = "festing" # 
        )
      }
  )
  
  
  # Poisson 90%
  req_poisson <- sapply(
    X = req_pups, 
    FUN = function(x)breed_genotype(
      confidence_p = confiP, 
      genotypes_p = c(genotype_probability, 1-genotype_probability),
      genotypes_N = c(x,0),
      effective_fertility_p = effertyP, 
      litter_mean = litmean, 
      method = "poisson" 
    )
  )

  req_pups_total <- ceiling(req_pups/genotype_probability)
  # intuitive Mendel: required numberf of breedings ti get the desired number of pups
  req_mendel <- ceiling(req_pups_total/(effertyP*litmean))
  
  reqDF <- data.frame(
    req_pups, req_poisson, req_festing, req_mendel)
  
  return(reqDF)
}
  
  
fe70 <- lapply(X = c(1, 0.5, 0.25, 0.125, 0.0625), FUN = function(genoP){
  xx <- calc_data4fig4(
    req_pups = c(10,25,50,75,100,125,150,200,250,300),
    genotype_probability = genoP, 
    confiP = 0.9, 
    effertyP = 0.7, litmean = 7)
  cat(genoP)
  cat("")
  return(xx)
})

save(fe70, file = "fe70.Rdata")
# 
# fe100 <- lapply(X = c(1, 0.5, 0.25, 0.125, 0.0625), FUN = function(genoP){
#     calc_data4fig4(
#       req_pups = req_pups_1000,
#       genotype_probability = genoP, 
#       confiP = 0.9, 
#       effertyP = 1, litmean = 7)
#   })



fe70[[5]] %>%
  ggplot(aes(x=req_pups)) + 
  #geom_point(aes(y=req_br_festing50, col = "Festing 50%"), colour = "red", alpha=0.5, size=0.5) + 
  #geom_line(aes(y=req_br_festing50, col = "Festing 50%"), alpha=0.3, size=0.3) + 
  #geom_point(aes(y=req_br_festing90, col = "Festing 90%")) +
  geom_point(aes(y= req_poisson, col = "poisson")) + 
  geom_point(aes(y=req_festing, col = "festing")) + 
  geom_point(aes(y=req_mendel, col = "mendel")) + 
  labs(x="Required number of pups", 
       y = "Smallest number of breedings", 
       caption = "Mean litter size: 7,  Effective fertility: 70%", 
       title = "Model comparison for the smallest required number of breegins", colour = "Method") + 
  theme(legend.position="right") 

xx = fe70[[1]]
surplus_pois <- round(100*(xx$req_poisson-xx$req_mendel)/xx$req_mendel)
surplus_fest <- round(100*(xx$req_festing-xx$req_mendel)/xx$req_mendel)
yy <- data.frame(xx, surplus_pois, surplus_fest)

barplot(yy$surplus_fest, yy$surplus_pois)

library(lattice)
barchart(Species~Reason,data=Reasonstats,groups=Catergory, 
         scales=list(x=list(rot=90,cex=0.8)))


Reasonstats <- read.table(text="Category         Reason  Species
                                 Decline        Genuine       24
                                Improved        Genuine       16
                                Improved  Misclassified       85
                                 Decline  Misclassified       41
                                 Decline      Taxonomic        2
                                Improved      Taxonomic        7
                                 Decline        Unclear       41
                                Improved        Unclear      117", header=T)

ReasonstatsDec <- Reasonstats[which(Reasonstats$Category=="Decline"),]
ReasonstatsImp <- Reasonstats[which(Reasonstats$Category=="Improved"),]
Reasonstats3   <- cbind(ReasonstatsImp[,3], ReasonstatsDec[,3])
colnames(Reasonstats3) <- c("Improved", "Decline")
rownames(Reasonstats3) <- ReasonstatsImp$Reason

#windows()
barplot(t(Reasonstats3), beside=TRUE, ylab="number of species", 
        cex.names=0.8, las=2, ylim=c(0,120), col=c("darkblue","red"))

zz <- t(yy)
colnames(zz) <- yy$req_pups
barplot(zz[c(4,2,3),], beside = TRUE, 
        ylab="required number of of breedings", 
        xlab = "number of pups required")

barplot(zz[c(5,6),], beside = TRUE, 
        ylab="surplus % breedings", 
        xlab = "number of pups required")












# calculate surplus of animals:
# 1st: for a given n breedings, fertility etc. calculate the expected of N born
# 2nd: 

expect_born_poisson <- function(
  effective_fertility_p,
  n_breedings,
  litter_mean
){
  freqs_r <- dpois(x = seq(1,round(4*litter_mean), 1), lambda = litter_mean)
  freqs <- freqs_r/sum(freqs_r)
  supp1 = as.numeric(c(0, seq(1,round(4*litter_mean))))
  prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
  stopifnot(sum(prob1)==1)
  
  #search_interval <- seq(1,10)
  doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
  doofN <- distr::convpow(doof1, N=n_breedings)
  return(doofN)
}

ff <- function(xx){
  distrConv <- expect_born_poisson(effective_fertility_p = 0.7, n_breedings = xx, litter_mean = 7)
  plot(distrConv@d(x = distrConv@support))
  return(  median(distrConv@r(n = 10000) )
)

}

surplus_animals_born <- 100*(sapply(fe70[[1]]$req_poisson, ff) - fe70[[1]]$req_pups)/  fe70[[1]]$req_pups
plot(fe70[[1]]$req_pups, surplus_animals_born)



confidence_grid <- c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.975, 0.99)
req_pups_grid <- c(10,25,50,75,100,125,150,200,250,300)
gp_grid <- c(1, 0.5, 0.25, 0.125, 0.0625) # genotype probability


breed_genotype(confidence_p = 0.6, effective_fertility_p = 0.7, genotypes_p = c(1,0), genotypes_N = c(200,0), litter_mean = 7)
calculate_needed_breedings(confidence_p = 0.6, effective_fertility_p = 0.7, n_needed = 200, litter_mean = 7, method = "poisson")

ooo <- sapply(
  confidence_grid, FUN = function(x){
    sapply(req_pups_grid, FUN = function(y){
      calculate_needed_breedings(
        confidence_p = x, 
        effective_fertility_p = 0.7, 
        n_needed = y, 
        litter_mean = 7, 
        method = "poisson")
    })
  })

data <- expand.grid(X=paste0(confidence_grid*100,"%"), 
                    Y=as.character(req_pups_grid))
data$Z <- as.numeric(t(ooo))
# new column: text for tooltip:
data <- data %>%
  mutate(text = paste0("confidence: ", X, "\n", "pups: ", Y, "\n", "breedings: ",round(Z,2), "\n", "What else?"))

# classic ggplot, with text in aes
p <- ggplot(data, aes(X, Y, fill= Z, text=text)) + 
  geom_tile() +
  theme_ipsum()

plotly::ggplotly(p, tooltip="text")

length(confidence_grid)
colnames(ooo) <- paste0("c",confidence_grid*100)
rownames(ooo) <- paste0("p",req_pups_grid)

gplots::heatmap.2(ooo,dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none', )


# odin <- generate_poisson_doof1(litter_mean = 7,effective_fertility_p = 0.7)
# svertka <- distr::convpow(odin, N=20)
# 1-svertka@p(99)

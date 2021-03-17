library("animalBreeding")
setwd("/home/compbio/vmilchev/animalBreedingProj/playground/figures/fig4_other/")
source("./helper_functions.R")

# the desired number of pups
req_pups_1000 <- c(seq(10, 40, 10), seq(50, 150, 25), seq(200, 500, 100), 700, 1000)



  
  
# fe70 <- lapply(X = c(1, 0.5, 0.25, 0.125, 0.0625), FUN = function(genoP){
#   xx <- calc_data4fig_2A(
#     req_pups = c(10,25,50,75,100,125,150,200,250,300),
#     genotype_probability = genoP, 
#     confiP = 0.9, 
#     effertyP = 0.7, litmean = 7)
#   cat(genoP)
#   cat("")
#   return(xx)
# })
# 
# #save(fe70, file = "fe70.Rdata")
# 
# fe100 <- lapply(X = c(1, 0.5, 0.25, 0.125, 0.0625), FUN = function(genoP){
#     calc_data4fig_2A(
#       req_pups = req_pups_1000,
#       genotype_probability = genoP, 
#       confiP = 0.9, 
#       effertyP = 1, litmean = 7)
#   })

load("fe70.Rdata")
names(fe70) <- paste0("genotProbab",c(1, 0.5, 0.25, 0.125, 0.0625))

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


ff_sup_df2plot <- function(xx){
#  xx = fe70[[1]]
  surplus_pois <- round(100*(xx$req_poisson-xx$req_mendel)/xx$req_mendel)
  surplus_fest <- round(100*(xx$req_festing-xx$req_mendel)/xx$req_mendel)
  
  # total pups born
  t(sapply(xx$req_poisson, ff)[2:4,]) # 50%, 90%, 95%
  total_pups <- data.frame(t(sapply(xx$req_poisson, ff)[2:4,]))
  colnames(total_pups) <- c("born_median","born_q90","born_q95")
  
  surplus_percentage_born_average <- 100*(total_pups$born_median - xx$req_pups)/xx$req_pups
  # resulting df
  yy <- data.frame(xx, surplus_pois, surplus_fest, total_pups, surplus_percentage_born_average)  
  return(yy)
}

fe70_df2plot <- lapply(fe70, ff_sup_df2plot)
names(fe70_df2plot)[[3]]
fe70_df2plot[[3]]
ppp1 <- ggplot(fe70_df2plot[[1]], 
              aes(x=req_pups)) +
  geom_point(aes(y= req_poisson, col = "poisson")) + 
  geom_point(aes(y=req_festing, col = "festing")) + 
  geom_point(aes(y=req_mendel, col = "mendel")) + 
  theme_pubr() 

ppp1


# library(lattice)
# barchart(Species~Reason,data=Reasonstats,groups=Catergory, 
#          scales=list(x=list(rot=90,cex=0.8)))
# 
# 
# Reasonstats <- read.table(text="Category         Reason  Species
#                                  Decline        Genuine       24
#                                 Improved        Genuine       16
#                                 Improved  Misclassified       85
#                                  Decline  Misclassified       41
#                                  Decline      Taxonomic        2
#                                 Improved      Taxonomic        7
#                                  Decline        Unclear       41
#                                 Improved        Unclear      117", header=T)
# 
# ReasonstatsDec <- Reasonstats[which(Reasonstats$Category=="Decline"),]
# ReasonstatsImp <- Reasonstats[which(Reasonstats$Category=="Improved"),]
# Reasonstats3   <- cbind(ReasonstatsImp[,3], ReasonstatsDec[,3])
# colnames(Reasonstats3) <- c("Improved", "Decline")
# rownames(Reasonstats3) <- ReasonstatsImp$Reason

#windows()
# barplot(t(Reasonstats3), beside=TRUE, ylab="number of species", 
#         cex.names=0.8, las=2, ylim=c(0,120), col=c("darkblue","red"))
# 
zz <- t(yy)
colnames(zz) <- yy$req_pups
barplot(zz[c(4,2,3),], beside = TRUE, 
        ylab="required number of of breedings", 
        xlab = "number of pups required")

barplot(zz[c(5,6),], beside = TRUE, 
        ylab="surplus % breedings", 
        xlab = "number of pups required")













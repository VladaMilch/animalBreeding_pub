# calculate surplus of animals:
# 1st: for a given n breedings, fertility etc. calculate the expected of N born
# 2nd: 
setwd("/home/compbio/vmilchev/animalBreedingProj/playground/figures/")
library(RColorBrewer)
library(animalBreeding)
source("helper_functions_born.R")


breedings_made <- seq(1,30,1)
animals_born <- sapply(breedings_made, ff) # average
df_BornBred <- data.frame(
  breedings_made, 
  p50_born = animals_born[2,],
  p90_born = animals_born[3,],
  p95_born = animals_born[4,],
  mean_born = animals_born[5,])

n_breedings_mendel <- ceiling(df_BornBred$p90_born/7)
df_BornBred <- data.frame(
  df_BornBred, n_breedings_mendel, 
  born_from_mendel50 =c(0,df_BornBred$mean_born[n_breedings_mendel]))

required_pups <- seq(1:120)
breedings_90 <- sapply(required_pups, FUN = function(required_pups_current){
  #cat(required_pups_current)
  df_BornBred$breedings_made[
    which(df_BornBred$p90_born >= required_pups_current)[1]]#, df_BornBred$p90_born, required_pups)
})
ddd <- data.frame(
  required_pups, 
  breedings_90 = breedings_90, 
  breedings_mendel = ceiling(required_pups/7), 
  expected_born_breedings90 = df_BornBred$mean_born[breedings_90],
  expected_born_breedings_mendel = df_BornBred$mean_born[ceiling(required_pups/7)])

extra2mendel <- 100*(ddd$expected_born_breedings90-ddd$expected_born_breedings_mendel)/ddd$expected_born_breedings_mendel
extra2required <- 100*(ddd$expected_born_breedings90-ddd$required_pups)/ddd$required_pups
barplot(extra2required)

data2plot <- data.frame(ddd, extra2mendel, extra2required)[1:100,]
head(ddd)
plot_extra_mendel <- ggplot(data2plot,aes(x=required_pups, y=extra2mendel))+
  geom_bar(stat="identity",
           fill = "violet", col = "grey")+
           #fill=brewer.pal(n = 8, name = 'RdPu')[6],
           #col=brewer.pal(n = 8, name = 'RdPu')[7])  + 
  theme_pubr() + 
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  ylab("Percent extra animals: To Mendel") + 
  xlab("Desired number of pups")
plot_extra_required <- ggplot(data2plot,aes(x=required_pups, y=extra2required))+
  geom_bar(stat="identity",
           fill = "blue", col = "grey")+
           #fill=brewer.pal(n = 8, name = 'RdPu')[7],
           #col=brewer.pal(n = 8, name = 'RdPu')[8]) + 
  theme_pubr() + 
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  ylab("Percent extra animals: To required") + 
  xlab("Desired number of pups")



# saved animals:
# festing compared to ours -- not good for small sample size cause Festing is wrong there!
breedings_festing <- c(1,sapply(required_pups[2:length(required_pups)],
                                FUN = function(nn)calculate_needed_breedings(
                                  confidence_p = 0.9, 
                                  effective_fertility_p = 0.7, 
                                  n_needed = nn, 
                                  litter_mean = 7, 
                                  method = "festing")
))
ddd3 <- data.frame(ddd, breedings_fe_90 = breedings_festing, 
                   animals_born = sapply(breedings_festing, ff)[5] # average
)

sum(ddd)

data2plot <- ddd3[1:100,]
plot_saved_extra <- ggplot(data2plot,aes(x=required_pups, y=extra2required))+
  geom_bar(stat="identity",
           fill = "blue", col = "grey") + 
           #fill=brewer.pal(n = 8, name = 'RdPu')[7],
           #col=brewer.pal(n = 8, name = 'RdPu')[8]) + 
  theme_pubr() + 
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  ylab("Percent extra animals: To required") + 
  xlab("Desired number of pups")



#plot_extra_mendel + scale_fill_manual(values = c("#4A1486","#7570B3"))

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
plot_extra_born <- ggarrange(
  plotlist = list(plot_extra_mendel, plot_extra_required), ncol = 2, 
  heights = c(1,1))
plot_extra_born 
ggsave(plot_extra_born, filename = "plot_extra_born.pdf", width = 7, height = 5)
getwd()




aaa = 100*(df_BornBred$p50_born - df_BornBred$born_from_mendel50)/df_BornBred$born_from_mendel50
barplot(aaa[-1])

data2plot <- reshape2::melt(df_BornBred, id.vars = 1)
ddd <- subset(data2plot, variable != "p95_born")
plot2B_animals_born <- ggplot(ddd, aes(x=breedings_made, y = value, fill = variable)) +
  geom_bar(stat="identity",col="darkgrey")  + 
  theme_pubr() + 
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  ylab("Animals born") + xlab("Breedings")
plot2B_animals_born + scale_fill_manual(values = c("#4A1486","#7570B3"))


display.brewer.pal(n = 8, name = 'Dark2')
display.brewer.pal(n = 12, name = 'Paired')
display.brewer.pal(n = 9, name = 'RdPu')



born10 <- expect_born_poisson(effective_fertility_p = 0.7, 
                              n_breedings = 10, 
                              litter_mean = 7)
born3 <- expect_born_poisson(effective_fertility_p = 0.7, 
                             n_breedings = 3, 
                             litter_mean = 7)
max(born10@support)



plot_support <- seq(0, max(born10@support))
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
  geom_bar(aes(y=b3), stat="identity", fill = "gray80", alpha=0.5, col = "gray10") +
  geom_bar(aes(y=b10), stat="identity", fill = "gray20", alpha=0.5, col = "gray10") +
  theme_pubr() + 
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  xlab("Animals born") + ylab("Probability")
pp
ggsave(pp, filename = "f1d.pdf", width = 7, height = 5)




### cutoff automatisation for 0.5 into vector STARTS HERE FOR ALL CASES

####### For one Group out of Breeding

for (i in 1:15) {     
  
  
  psuccess = 0.5 # success probability per animal
  
  goodanimals  <-  i  # the target number of "good" animals you want to breed
  penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals
  
  Nmax = 1000 # maximum number of animals that you can ever produce
  
  quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
  
  
  cutoff[i] = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)

}
cutoff.all05 <- cutoff[1:15]
cutoff.all05
plot(cutoff.all05, ylim=c(1,38), ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.5 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*2, pch = 2)
legend(1,38, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")

### cutoff automatisation for 0.25 into vector


for (i in 1:15) {     
  
  
  psuccess = 0.25 # success probability per animal
  
  goodanimals  <-  i  # the target number of "good" animals you want to breed
  penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals
  
  Nmax = 1000 # maximum number of animals that you can ever produce
  
  quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
  
  
  cutoff[i] = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)
  
}
cutoff.all025 <- cutoff[1:15]
cutoff.all025
plot(cutoff.all025,  ylim=c(1,80), ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.25 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*4, pch = 2)
legend(1,80, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")




### cutoff automatisation for 0.125 into vector


for (i in 1:15) {     
  
  
  psuccess = 0.125 # success probability per animal
  
  goodanimals  <-  i  # the target number of "good" animals you want to breed
  penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals
  
  Nmax = 1000 # maximum number of animals that you can ever produce
  
  quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
  
  
  cutoff[i] = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)
  
}
cutoff.all0125 <- cutoff[1:15]
cutoff.all0125
plot(cutoff.all0125, ylim = c(1,160), ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.125 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*8, pch = 2)
legend(1,160, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")

### cutoff automatisation for 0.0625 into vector


for (i in 1:15) {     
  
  
  psuccess = 0.0625 # success probability per animal
  
  goodanimals  <-  i  # the target number of "good" animals you want to breed
  penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals
  
  Nmax = 1000 # maximum number of animals that you can ever produce
  
  quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
  
  
  cutoff[i] = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)
  
}
cutoff.all00625 <- cutoff[1:15]
cutoff.all00625
plot(cutoff.all00625, ylim = c(1,320), ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.0625 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*16, pch = 2)
legend(1,325, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")

data.cutoff <- data.frame(cutoff.all05, cutoff.all025, cutoff.all0125, cutoff.all00625)



### cutoff automatisation for ???????


for (i in 1:15) {     


psuccess = 0.5 # success probability per animal

goodanimals  <-  i  # the target number of "good" animals you want to breed
penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals

Nmax = 1000 # maximum number of animals that you can ever produce

quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 


cutoff = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)



print(cutoff)

}


### cutoff automatisation for 0.5 into vector STARTS HERE FOR ALL CASES

for (i in 1:15) {     
  
  
  psuccess = 0.5 # success probability per animal
  
  goodanimals  <-  i  # the target number of "good" animals you want to breed
  penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals
  
  Nmax = 1000 # maximum number of animals that you can ever produce
  
  quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
  
  
  cutoff[i] = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)
  
}
cutoff.all5 <- cutoff[1:15]
cutoff.all5
plot(cutoff.all, ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.25 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*2, pch = 2)
legend(1,37, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")

### cutoff automatisation for 0.25 into vector


for (i in 1:15) {     
  
  
  psuccess = 0.25 # success probability per animal
  
  goodanimals  <-  i  # the target number of "good" animals you want to breed
  penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals
  
  Nmax = 1000 # maximum number of animals that you can ever produce
  
  quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
  
  
  cutoff[i] = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)
  
}
cutoff.all25 <- cutoff[1:15]
cutoff.all25
plot(cutoff.all25, ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.25 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*4, pch = 2)
legend(1,79, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")




### cutoff automatisation for 0.125 into vector


for (i in 1:15) {     
  
  
  psuccess = 0.125 # success probability per animal
  
  goodanimals  <-  i  # the target number of "good" animals you want to breed
  penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals
  
  Nmax = 1000 # maximum number of animals that you can ever produce
  
  quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
  
  
  cutoff[i] = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)
  
}
cutoff.all125 <- cutoff[1:15]
cutoff.all125
plot(cutoff.all125, ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.125 genotype probability", pch = 19, cex = 1.2)
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
cutoff.all0625 <- cutoff[1:15]
cutoff.all0625
plot(cutoff.all0625, ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.0625 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*16, pch = 2)
legend(1,325, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")

##### now all in one plot
##0625
plot(cutoff.all0625, ylim = c(1,325), ylab = "required number of born animals", xlab = "required number of animals for experiment", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*16, pch = 2)
legend(1,325, legend =c("with 90% probability Mendel´s 0.0625", "according to Mendel 0.0625"),pch = c(19, 2), bty = "n")

###125
points(cutoff.all125, ylab = "required number of born animals", xlab = "required number of animals for experiment", pch = 19, cex = 1.2, col = "blue")
points(c(1:15), (c(1:15))*8, pch = 2, col = "blue")
legend(1,305, legend =c("with 90% probability Mendel´s 0.125", "according to Mendel 0.125"),pch = c(19, 2), bty = "n", col = "blue")

###25
points(cutoff.all25, ylab = "required number of born animals", xlab = "required number of animals for experiment", pch = 19, cex = 1.2, col = "darkgreen")
points(c(1:15), (c(1:15))*4, pch = 2, col = "darkgreen")
legend(1,285, legend =c("with 90% probability Mendel´s 0.25", "according to Mendel 0.25"),pch = c(19, 2), bty = "n", col = "darkgreen")

###5
points(cutoff.all5, ylab = "required number of born animals", xlab = "required number of animals for experiment", pch = 19, cex = 1.2, col = "red")
points(c(1:15), (c(1:15))*2, pch = 2, col = "red")
legend(1,265, legend =c("with 90% probability Mendel´s 0.5", "according to Mendel 0.5"),pch = c(19, 2), bty = "n", col = "red")

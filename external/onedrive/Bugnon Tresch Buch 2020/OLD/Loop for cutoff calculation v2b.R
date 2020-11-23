


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
plot(cutoff.all05, ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.5 genotype probability", pch = 19, cex = 1.2)
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
cutoff.all025 <- cutoff[1:15]
cutoff.all025
plot(cutoff.all025, ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.25 genotype probability", pch = 19, cex = 1.2)
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
cutoff.all0125 <- cutoff[1:15]
cutoff.all0125
plot(cutoff.all0125, ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.125 genotype probability", pch = 19, cex = 1.2)
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
plot(cutoff.all00625, ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.0625 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*16, pch = 2)
legend(1,325, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")

data.cutoff <- data.frame(cutoff.all05, cutoff.all025, cutoff.all0125, cutoff.all00625)

########## For Two Groups Same Breeding

####Loop for Cutoff with thwo groups from same breeding 0.5
for (i in 1:15) { 
  
  Nmax = 100
  ppower = 0.9
  
  pgoodKO = 0.5
  goodKO <-  i
  
  pgoodWT = 0.5
  goodWT <-  i
  
  prelativ = pgoodKO/(pgoodKO+pgoodWT)
  pgoodtotal = pgoodKO + pgoodWT
  totalgood = goodKO+goodWT
  
  pgoodfromk = function(k){
    res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO,k-1,prelativ,lower.tail = T) 
    return(pmax(res,0))
  }
  
  psuccess = numeric(Nmax)
  for (j in totalgood:Nmax){
    k = totalgood : Nmax
    psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
  }
  
  
  cutoff[i] = min(which(psuccess>=ppower))
}

cutoff.all10_10_05 <- cutoff[1:15]
plot(cutoff.all10_10_05, ylim = c(0, 45), ylab = "required number of born animals", xlab = "required number of animals for experiment (Group 1 = Group 2)", main = "0.5 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*2, pch = 2)
legend(1,45, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")
cutoff.all10_10_05




####Loop for Cutoff with thwo groups from same breeding 0.25
for (i in 1:15) { 
  
  Nmax = 100
  ppower = 0.9
  
  pgoodKO = 0.25
  goodKO <-  i
  
  pgoodWT = 0.25
  goodWT <-  i
  
  prelativ = pgoodKO/(pgoodKO+pgoodWT)
  pgoodtotal = pgoodKO + pgoodWT
  totalgood = goodKO+goodWT
  
  pgoodfromk = function(k){
    res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO,k-1,prelativ,lower.tail = T) 
    return(pmax(res,0))
  }
  
  psuccess = numeric(Nmax)
  for (j in totalgood:Nmax){
    k = totalgood : Nmax
    psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
  }

  
  cutoff[i] = min(which(psuccess>=ppower))
}

cutoff.all10_10_025 <- cutoff[1:15]
plot(cutoff.all10_10_025, ylim = c(0, 90), ylab = "required number of born animals", xlab = "required number of animals for experiment (Group 1 = Group 2)", main = "0.25 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*4, pch = 2)
legend(1,90, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")
cutoff.all10_10_025

####Loop for Cutoff with thwo groups from same breeding 0.125
for (i in 1:15) { 
  
  Nmax = 500
  ppower = 0.9
  
  pgoodKO = 0.125
  goodKO <-  i
  
  pgoodWT = 0.125
  goodWT <-  i
  
  prelativ = pgoodKO/(pgoodKO+pgoodWT)
  pgoodtotal = pgoodKO + pgoodWT
  totalgood = goodKO+goodWT
  
  pgoodfromk = function(k){
    res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO,k-1,prelativ,lower.tail = T) 
    return(pmax(res,0))
  }
  
  psuccess = numeric(Nmax)
  for (j in totalgood:Nmax){
    k = totalgood : Nmax
    psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
  }
  
  
  cutoff[i] = min(which(psuccess>=ppower))
}

cutoff.all10_10_0125 <- cutoff[1:15]
plot(cutoff.all10_10_0125, ylim = c(0, 180), ylab = "required number of born animals", xlab = "required number of animals for experiment (Group 1 = Group 2)", main = "0.125 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*8, pch = 2)
legend(1,180, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")
cutoff.all10_10_0125

####Loop for Cutoff with thwo groups from same breeding 0.125
for (i in 1:15) { 
  
  Nmax = 500
  ppower = 0.9
  
  pgoodKO = 0.0625
  goodKO <-  i
  
  pgoodWT = 0.0625
  goodWT <-  i
  
  prelativ = pgoodKO/(pgoodKO+pgoodWT)
  pgoodtotal = pgoodKO + pgoodWT
  totalgood = goodKO+goodWT
  
  pgoodfromk = function(k){
    res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO,k-1,prelativ,lower.tail = T) 
    return(pmax(res,0))
  }
  
  psuccess = numeric(Nmax)
  for (j in totalgood:Nmax){
    k = totalgood : Nmax
    psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
  }
  
  
  cutoff[i] = min(which(psuccess>=ppower))
}

cutoff.all10_10_00625 <- cutoff[1:15]
plot(cutoff.all10_10_00625, ylim = c(0, 360), ylab = "required number of born animals", xlab = "required number of animals for experiment (Group 1 = Group 2)", main = "0.0625 genotype probability", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*16, pch = 2)
legend(1,360, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")
cutoff.all10_10_00625

##### bring it all together into one frame

data.cutoff10_10 <- data.frame(cutoff.all10_10_05, cutoff.all10_10_025, cutoff.all10_10_0125, cutoff.all10_10_00625)


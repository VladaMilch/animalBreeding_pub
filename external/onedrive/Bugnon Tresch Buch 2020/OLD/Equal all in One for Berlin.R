###### Mendel 0.5

maxgood = 15

nneeded = function(goodKO,goodWT,ppower=0.9,pgoodKO=0.5,pgoodWT=0.5,Nmax=100){
  prelativ = pgoodKO/(pgoodKO+pgoodWT)
  pgoodtotal = pgoodKO + pgoodWT
  totalgood = goodKO+goodWT
  
  # given k animals that are either KO or WT, calculate the probability that
  # at least <goodWT> WT animals and at least <goodKO> KO animals
  pgoodfromk = function(k){
    # res gives the probability to draw at most k-goodWT knockout animals (left pbionm),
    # but not less than goodKO knockout animals (righgt pbinom)
    res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO-1,k,prelativ,lower.tail = T) 
    return(pmax(res,0))
  }
  
  # psuccess[j] contains the probability of a successful draw, if you produce j animals in total
  psuccess = numeric(Nmax)
  for (j in totalgood:Nmax){
    k = totalgood : Nmax
    psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
  }
  
  # find the smallest number of total animals
  # for which the success probability is above the desired threshold
  needed = min(which(psuccess>=ppower))
  return(needed)
}


maxgood = 15
required_power = 0.9
neededmat = matrix(Inf,ncol=maxgood,nrow=maxgood)
for(j in 1:maxgood){
  for(k in 1:maxgood){
    neededmat[j,k] = nneeded(goodKO=j,goodWT=k,ppower=required_power)
  }
}

maxneeded = max(neededmat)



### get data all groups and save it
ALL5 <- neededmat[c(1:15), c(1:15)]
ALL5


### get data equal groups and plot it
EQUAL5 <- c(ALL5[1,1],ALL5[2,2],ALL5[3,3],ALL5[4,4],ALL5[5,5],ALL5[6,6],ALL5[7,7],ALL5[8,8],ALL5[9,9],ALL5[10,10],ALL5[11,11],ALL5[12,12],ALL5[13,13],ALL5[14,14],ALL5[15,15])
EQUAL5

MENDEL5 <- c(1:15)/0.5
MENDEL5
plot(EQUAL5, ylim=c(1,45), ylab = "required number of born animals", xlab = "required number of animals for experiment (size group A = size group B)", main = "0.5 genotype probability", pch = 19, cex = 1.2)
points(MENDEL5, pch = 2)
legend(1,325, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")

###### Mendel 0.25

maxgood = 15

nneeded = function(goodKO,goodWT,ppower=0.9,pgoodKO=0.25,pgoodWT=0.25,Nmax=100){
  prelativ = pgoodKO/(pgoodKO+pgoodWT)
  pgoodtotal = pgoodKO + pgoodWT
  totalgood = goodKO+goodWT
  
  # given k animals that are either KO or WT, calculate the probability that
  # at least <goodWT> WT animals and at least <goodKO> KO animals
  pgoodfromk = function(k){
    # res gives the probability to draw at most k-goodWT knockout animals (left pbionm),
    # but not less than goodKO knockout animals (righgt pbinom)
    res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO-1,k,prelativ,lower.tail = T) 
    return(pmax(res,0))
  }
  
  # psuccess[j] contains the probability of a successful draw, if you produce j animals in total
  psuccess = numeric(Nmax)
  for (j in totalgood:Nmax){
    k = totalgood : Nmax
    psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
  }
  
  # find the smallest number of total animals
  # for which the success probability is above the desired threshold
  needed = min(which(psuccess>=ppower))
  return(needed)
}


maxgood = 15
required_power = 0.9
neededmat = matrix(Inf,ncol=maxgood,nrow=maxgood)
for(j in 1:maxgood){
  for(k in 1:maxgood){
    neededmat[j,k] = nneeded(goodKO=j,goodWT=k,ppower=required_power)
  }
}

maxneeded = max(neededmat)


### get data all groups and save it
ALL25 <- neededmat[c(1:15), c(1:15)]
ALL25


### get data equal groups and plot it
EQUAL25 <- c(ALL25[1,1],ALL25[2,2],ALL25[3,3],ALL25[4,4],ALL25[5,5],ALL25[6,6],ALL25[7,7],ALL25[8,8],ALL25[9,9],ALL25[10,10],ALL25[11,11],ALL25[12,12],ALL25[13,13],ALL25[14,14],ALL25[15,15])
EQUAL25

MENDEL25 <- c(1:15)/0.25
MENDEL25
plot(EQUAL25, ylim=c(1,85), ylab = "required number of born animals", xlab = "required number of animals for experiment (size group A = size group B)", main = "0.25 genotype probability", pch = 19, cex = 1.2)
points(MENDEL25, pch = 2)
legend(1,325, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")

###### Mendel 0.125

maxgood = 15

nneeded = function(goodKO,goodWT,ppower=0.9,pgoodKO=0.125,pgoodWT=0.125,Nmax=500){
  prelativ = pgoodKO/(pgoodKO+pgoodWT)
  pgoodtotal = pgoodKO + pgoodWT
  totalgood = goodKO+goodWT
  
  # given k animals that are either KO or WT, calculate the probability that
  # at least <goodWT> WT animals and at least <goodKO> KO animals
  pgoodfromk = function(k){
    # res gives the probability to draw at most k-goodWT knockout animals (left pbionm),
    # but not less than goodKO knockout animals (righgt pbinom)
    res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO-1,k,prelativ,lower.tail = T) 
    return(pmax(res,0))
  }
  
  # psuccess[j] contains the probability of a successful draw, if you produce j animals in total
  psuccess = numeric(Nmax)
  for (j in totalgood:Nmax){
    k = totalgood : Nmax
    psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
  }
  
  # find the smallest number of total animals
  # for which the success probability is above the desired threshold
  needed = min(which(psuccess>=ppower))
  return(needed)
}


maxgood = 15
required_power = 0.9
neededmat = matrix(Inf,ncol=maxgood,nrow=maxgood)
for(j in 1:maxgood){
  for(k in 1:maxgood){
    neededmat[j,k] = nneeded(goodKO=j,goodWT=k,ppower=required_power)
  }
}

maxneeded = max(neededmat)



### get data all groups and save it
ALL125 <- neededmat[c(1:15), c(1:15)]
ALL125


### get data equal groups and plot it
EQUAL125 <- c(ALL125[1,1],ALL125[2,2],ALL125[3,3],ALL125[4,4],ALL125[5,5],ALL125[6,6],ALL125[7,7],ALL125[8,8],ALL125[9,9],ALL125[10,10],ALL125[11,11],ALL125[12,12],ALL125[13,13],ALL125[14,14],ALL125[15,15])
EQUAL125

MENDEL125 <- c(1:15)/0.125
MENDEL125
plot(EQUAL125, ylim=c(1,168), ylab = "required number of born animals", xlab = "required number of animals for experiment (size group A = size group B)", main = "0.125 genotype probability", pch = 19, cex = 1.2)
points(MENDEL125, pch = 2)
legend(1,168, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")

###### Mendel 0.0625

maxgood = 15

nneeded = function(goodKO,goodWT,ppower=0.9,pgoodKO=0.0625,pgoodWT=0.0625,Nmax=500){
  prelativ = pgoodKO/(pgoodKO+pgoodWT)
  pgoodtotal = pgoodKO + pgoodWT
  totalgood = goodKO+goodWT
  
  # given k animals that are either KO or WT, calculate the probability that
  # at least <goodWT> WT animals and at least <goodKO> KO animals
  pgoodfromk = function(k){
    # res gives the probability to draw at most k-goodWT knockout animals (left pbionm),
    # but not less than goodKO knockout animals (righgt pbinom)
    res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO-1,k,prelativ,lower.tail = T) 
    return(pmax(res,0))
  }
  
  # psuccess[j] contains the probability of a successful draw, if you produce j animals in total
  psuccess = numeric(Nmax)
  for (j in totalgood:Nmax){
    k = totalgood : Nmax
    psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
  }
  
  # find the smallest number of total animals
  # for which the success probability is above the desired threshold
  needed = min(which(psuccess>=ppower))
  return(needed)
}


maxgood = 15
required_power = 0.9
neededmat = matrix(Inf,ncol=maxgood,nrow=maxgood)
for(j in 1:maxgood){
  for(k in 1:maxgood){
    neededmat[j,k] = nneeded(goodKO=j,goodWT=k,ppower=required_power)
  }
}

maxneeded = max(neededmat)


### get data all groups and save it
ALL0625 <- neededmat[c(1:15), c(1:15)]
ALL0625


### get data equal groups and plot it
EQUAL0625 <- c(ALL0625[1,1],ALL0625[2,2],ALL0625[3,3],ALL0625[4,4],ALL0625[5,5],ALL0625[6,6],ALL0625[7,7],ALL0625[8,8],ALL0625[9,9],ALL0625[10,10],ALL0625[11,11],ALL0625[12,12],ALL0625[13,13],ALL0625[14,14],ALL0625[15,15])
EQUAL0625

MENDEL0625 <- c(1:15)/0.0625
MENDEL0625
plot(EQUAL0625, ylim=c(1,350), ylab = "required number of born animals", xlab = "required number of animals for experiment (size group A = size group B)", main = "0.0625 genotype probability", pch = 19, cex = 1.2)
points(MENDEL0625, pch = 2)
legend(1,350, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")

##### now all in one plot
##0625
plot(EQUAL0625, ylim = c(1,325), ylab = "required number of born animals", xlab = "required number of animals for experiment", pch = 19, cex = 1.2)
points(c(1:15), (c(1:15))*16, pch = 2)
legend(1,325, legend =c("with 90% probability Mendel´s 0.0625", "according to Mendel 0.0625"),pch = c(19, 2), bty = "n")

###125
points(EQUAL125, ylab = "required number of born animals", xlab = "required number of animals for experiment", pch = 19, cex = 1.2, col = "blue")
points(c(1:15), (c(1:15))*8, pch = 2, col = "blue")
legend(1,305, legend =c("with 90% probability Mendel´s 0.125", "according to Mendel 0.125"),pch = c(19, 2), bty = "n", col = "blue")

###25
points(EQUAL25, ylab = "required number of born animals", xlab = "required number of animals for experiment", pch = 19, cex = 1.2, col = "darkgreen")
points(c(1:15), (c(1:15))*4, pch = 2, col = "darkgreen")
legend(1,285, legend =c("with 90% probability Mendel´s 0.25", "according to Mendel 0.25"),pch = c(19, 2), bty = "n", col = "darkgreen")

###5
points(EQUAL5, ylab = "required number of born animals", xlab = "required number of animals for experiment", pch = 19, cex = 1.2, col = "red")
points(c(1:15), (c(1:15))*2, pch = 2, col = "red")
legend(1,265, legend =c("with 90% probability Mendel´s 0.5", "according to Mendel 0.5"),pch = c(19, 2), bty = "n", col = "red")

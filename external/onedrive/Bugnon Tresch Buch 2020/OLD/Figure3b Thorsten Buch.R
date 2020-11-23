###### Mendel 0.5

maxgood = 15

nneeded05 = function(goodKO,goodWT,ppower=0.8,pgoodKO=0.5,pgoodWT=0.5,Nmax=100){
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
    neededmat[j,k] = nneeded05(goodKO=j,goodWT=k,ppower=required_power)
  }
}

maxneeded = max(neededmat)
image(t(neededmat))


### get data equal groups and plot it
EQUAL <- c(ALL[1,1],ALL[2,2],ALL[3,3],ALL[4,4],ALL[5,5],ALL[6,6],ALL[7,7],ALL[8,8],ALL[9,9],ALL[10,10],ALL[11,11],ALL[12,12],ALL[13,13],ALL[14,14],ALL[15,15])
EQUAL
write.csv(EQUAL, file = "EQUALrequiredno05twogroups")
MENDEL <- c(1:15)/pgoodKO
MENDEL
plot(EQUAL, ylim=c(1,90), ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.5 genotype probability", pch = 19, cex = 1.2)
points(MENDEL, pch = 2)
legend(1,325, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")









###### Mendel 0.25

maxgood = 15

nneeded = function(goodKO,goodWT,ppower=0.8,pgoodKO=0.25,pgoodWT=0.25,Nmax=100){
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
image(t(neededmat))


### get data equal groups and plot it
EQUAL <- c(ALL[1,1],ALL[2,2],ALL[3,3],ALL[4,4],ALL[5,5],ALL[6,6],ALL[7,7],ALL[8,8],ALL[9,9],ALL[10,10],ALL[11,11],ALL[12,12],ALL[13,13],ALL[14,14],ALL[15,15])
EQUAL
write.csv(EQUAL, file = "EQUALrequiredno025twogroups")
MENDEL <- c(1:15)/pgoodKO
MENDEL
plot(EQUAL, ylim=c(1,90), ylab = "required number of born animals", xlab = "required number of animals for experiment", main = "0.25 genotype probability", pch = 19, cex = 1.2)
points(MENDEL, pch = 2)
legend(1,325, legend =c("with 90% probability", "according to Mendel"),pch = c(19, 2), bty = "n")
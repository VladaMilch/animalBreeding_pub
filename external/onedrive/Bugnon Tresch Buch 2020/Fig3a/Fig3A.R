###### wie einen höher, aber X und Y Achse vertauscht
penough = 0.8 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals

Nmax = 100 # maximum number of animals that you can ever produce

quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
print(quantilevec)
quantilevec.frame <- data.frame(quantilevec, c(1:100))
plot(quantilevec.frame)
plot(quantilevec.frame, xlab = "necessary group size", ylab = "total animals needed to be born")



######
Nmax = 100
ppower = 0.8 

pgoodKO = 0.25
goodKO = 10

pgoodWT = 0.25
goodWT = 10

prelativ = pgoodKO/(pgoodKO+pgoodWT)
pgoodtotal = pgoodKO + pgoodWT
totalgood = goodKO+goodWT

pgoodfromk = function(k){
  res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO-1,k,prelativ,lower.tail = T)  
  return(pmax(res,0))
}

psuccess = numeric(Nmax)
for (j in totalgood:Nmax){
  k = totalgood : Nmax
  psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
}

plot(psuccess, xlab = "number of total animals born", ylab = "proability of success of 10 genotype A and 10 genotype B")
cutoff = min(which(psuccess>=ppower))
##abline(v=cutoff,col="red")
print(cutoff)


###### for group size A and B of 3 and power of 90

###### wie einen höher, aber X und Y Achse vertauscht
penough = 0.9 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals

Nmax = 100 # maximum number of animals that you can ever produce

quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
print(quantilevec)
quantilevec.frame <- data.frame(quantilevec, c(1:100))
plot(quantilevec.frame)
plot(quantilevec.frame, xlab = "necessary group size", ylab = "total animals needed to be born")



######
Nmax = 40
ppower = 0.9 

pgoodKO = 0.25
goodKO = 3

pgoodWT = 0.25
goodWT = 3

prelativ = pgoodKO/(pgoodKO+pgoodWT)
pgoodtotal = pgoodKO + pgoodWT
totalgood = goodKO+goodWT

pgoodfromk = function(k){
  res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO-1,k,prelativ,lower.tail = T)  
  return(pmax(res,0))
}

psuccess = numeric(Nmax)
for (j in totalgood:Nmax){
  k = totalgood : Nmax
  psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
}

plot(psuccess, xlab = "number of total animals born", ylab = "proability of success of 3 genotype A and 3 genotype B")
cutoff = min(which(psuccess>=ppower))
abline(v=cutoff,col="red")
abline(h = 0.9, col = "blue")
print(cutoff)
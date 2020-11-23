Nmax = 40
ppower = 0.9 

pgoodKO = 0.5
goodKO = 3

pgoodWT = 0.5
goodWT = 0

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

plot(psuccess, xlab = "number of total animals born", ylab = "proability of success of 3 genotype A", main = "0.5 genotype probability")
cutoff = min(which(psuccess>=ppower))
abline(v=cutoff,col="red")
abline(h = 0.9,col="blue")
print(cutoff)
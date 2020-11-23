Nmax = 40
ppower = 0.9 # P(desired numbers are born) >= 0.9

pgoodKO = 0.25 # probability of a born animal to be KO
goodKO = 3 # desired number of KO animals

pgoodWT = 0.25
goodWT = 0 # desired number of WT animals

prelativ = pgoodKO/(pgoodKO+pgoodWT)
pgoodtotal = pgoodKO + pgoodWT
totalgood = goodKO+goodWT

pgoodfromk = function(k){
  res = pbinom(
      q=k-goodWT,
      size=k,
      prob=prelativ,
      lower.tail=T) - 
    pbinom(q=goodKO-1,
           size=k,
           prob=prelativ,
           lower.tail = T) 
  res2 = 1 - sum(dbinom(x=c(0:(goodKO-1)), size=k, prob=c(0.25)))
  print(res)
  print(res2)
  cat("\n")
  return(pmax(res,0))
}
pgoodfromk(10)

psuccess = numeric(Nmax)
for (j in totalgood:Nmax){
  k = totalgood : Nmax
  psuccess[j] = sum( dbinom(k,j,pgoodtotal) * pgoodfromk(k) )
}

plot(psuccess, xlab = "number of total animals born", 
     ylab = "proability of success of 3 genotype A", 
     main = "0.25 genotype probability")
cutoff = min(which(psuccess>=ppower))
abline(v=cutoff,col="red")
abline(h = 0.9,col="blue")
main = "0.25 genotype probability"
print(cutoff)
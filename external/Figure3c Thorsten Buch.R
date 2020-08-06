

maxgood = 15

nneeded <- function(
    goodKO,
    goodWT,
    ppower=0.8,
    pgoodKO=0.25,
    pgoodWT=0.25,
    Nmax=100)
{
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
contour(1:maxgood,1:maxgood,neededmat,nlevels=15)

neededmat[10,10]
text(x=10+0.4,y=10+0.4,neededmat[10,10])
points(10,10,pch=19,col="red")



psuccess = 0.25 # succes probability per animal

goodanimals = 3  # the target number of "good" animals you want to breed
penough = 0.8 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals

Nmax = 1000 # maximum number of animals that you can ever produce

quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
print(quantilevec)
plot(quantilevec,xlab="Number of animals",ylab="Lower bound of good animals obtained with a probability <penough>")

cutoff = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)

abline(v=cutoff,col="red")

print(cutoff)

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
  res = pbinom(k-goodWT,k,prelativ,lower.tail=T) - pbinom(goodKO,k-1,prelativ,lower.tail = T) 
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



#######


nneeded = function(ppower=0.8,pgooKO=0.25,goodKO=10,pgoodWT=0.25,goodWT=10,Nmax=100){
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

needed = min(which(psuccess>=ppower))
return(needed)
}


maxgood = 15
neededmat = matrix(Inf,ncol=maxgood,nrow=maxgood)
for(j in 1:maxgood){
  for(k in 1:maxgood){
    neededmat[j,k] = nneeded(goodKO=j,goodWT=k)
  }
}

image(t(neededmat))
contour(1:maxgood,1:maxgood,neededmat)



N = 100
mat = matrix(0,ncol=N+1,nrow=N+1)
probx = 0.5
proby = 0.5

### Initialize first genotype
for (x in 0:N){
    mat[x+1,] = dbinom(x,N,probx) * dbinom(0:N,N-x,proby/(1-probx))
}

image(mat)
max(abs(mat - t(mat)))
sum(mat)

### Given a number of animals N
# Given a N x N probability matrix to have (x animals of type X, y animals of type Y),
# calculate the N x N probability matrix to obtain (a animals of type AX, b animals of type BY) 

proba = 0.25
probb = 0.25

hilf = matrix(0,ncol=N+1,nrow=N+1)

for (a in 0:N){
  for (b in 0:N){
      hilf[1:(a+1),1:(b+1)] =  hilf[1:(a+1),1:(b+1)] + mat[a+1,b+1] * dbinom(0:a,a,proba) %*% t(dbinom(0:b,b,probb)) 
  }
}

image(hilf)
max(abs(hilf - t(hilf)))
sum(hilf)


distr = hilf
distr = t(apply(distr,1,function(x){rev(cumsum(rev(x)))}))
distr = apply(distr,2,function(x){rev(cumsum(rev(x)))})
image(distr)
max(abs(distr-t(distr)))

contour(0:15,0:15,distr[1:16,1:16])

power_symmetric = diag(distr)

minpower = 0.8
nobtained = max(which(power_symmetric>= minpower))-1
cat("Number of (a,a) animals obtained with a probability >=",minpower," : a=",nobtained,".\n")

plot(0:N,power_symmetric,col=c("red","black")[2-(0:N <= nobtained)],pch=c(1,19)[1+(0:N==(nobtained))],
     xlim=c(0,16),xlab="Min. number of animals in each group",ylab="probability of event")
abline(h=minpower,col="grey",lty=3)
abline(v=nobtained,col="red",lty=3)
axis(side=1,at=nobtained,nobtained,col.ticks="red")


#######

########
Nmin = 10
Nmax = 200
Nsteps = 10
Nseq = round(seq(from=Nmin,to=Nmax,length=Nsteps))
powermatrix = matrix(0,nrow=Nsteps,ncol=Nmax+1)

for (Nindex in 1:Nsteps){
  cat(Nindex," ")
  N = Nseq[Nindex]
  mat = matrix(0,ncol=N+1,nrow=N+1)
  probx = 0.5
  proby = 0.5
  
  ### Initialize first genotype
  for (x in 0:N){
    mat[x+1,] = dbinom(x,N,probx) * dbinom(0:N,N-x,proby/(1-probx))
  }
  
  ### Given a number of animals N
  # Given a N x N probability matrix to have (x animals of type X, y animals of type Y),
  # calculate the N x N probability matrix to obtain (a animals of type AX, b animals of type BY) 
  
  proba = 0.25
  probb = 0.25
  
  hilf = matrix(0,ncol=N+1,nrow=N+1)
  
  for (a in 0:N){
    for (b in 0:N){
      hilf[1:(a+1),1:(b+1)] =  hilf[1:(a+1),1:(b+1)] + mat[a+1,b+1] * dbinom(0:a,a,proba) %*% t(dbinom(0:b,b,probb)) 
    }
  }
  
  distr = hilf
  distr = t(apply(distr,1,function(x){rev(cumsum(rev(x)))}))
  distr = apply(distr,2,function(x){rev(cumsum(rev(x)))})
  
  powermatrix[Nindex,1:(N+1)] = diag(distr)
}
cat("\n")


contour(Nseq,0:30,powermatrix[,1:31],xlab="Animals in total",ylab="Animals per group",main="Power Curves")


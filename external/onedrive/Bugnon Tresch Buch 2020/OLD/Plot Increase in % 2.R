### A) Plotting the percentage of additionally required animals for the 0.5 90% situation
psuccess = 0.5 # succes probability per animal

goodanimals = 3  # the target number of "good" animals you want to breed
penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals

Nmax = 200 # maximum number of animals that you can ever produce

quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
print(quantilevec)
plot(quantilevec,xlab="Number of animals",ylab="Lower bound of good animals obtained with a probability <penough>")

cutoff = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)

abline(v=cutoff,col="red")

print(cutoff)


Mendel <- c(1:200)/2


Percent.Increase <- Mendel/quantilevec*100-100



plot(Mendel/quantilevec*100-100, xlab= "Number of Animals Born", ylab =  "percent increase for 90% probability", main = "Genotype Frequency 0.5")



### B) Plotting the percentage of additionally required animals for the 0.25 90% situation
psuccess = 0.25 # succes probability per animal

goodanimals = 3  # the target number of "good" animals you want to breed
penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals

Nmax = 200 # maximum number of animals that you can ever produce

quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
print(quantilevec)
plot(quantilevec,xlab="Number of animals",ylab="Lower bound of good animals obtained with a probability <penough>")

cutoff = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)

abline(v=cutoff,col="red")

print(cutoff)


Mendel <- c(1:200)/4


Percent.Increase <- Mendel/quantilevec*100-100



plot(Mendel/quantilevec*100-100, xlab= "Number of Animals Born", ylab =  "percent increase for 90% probability",  main = "Genotype Frequency 0.25")


### C) Plotting the percentage of additionally required animals for the 0.125 90% situation
psuccess = 0.125 # succes probability per animal

goodanimals = 3  # the target number of "good" animals you want to breed
penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals

Nmax = 200 # maximum number of animals that you can ever produce

quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
print(quantilevec)
plot(quantilevec,xlab="Number of animals",ylab="Lower bound of good animals obtained with a probability <penough>")

cutoff = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)

abline(v=cutoff,col="red")

print(cutoff)


Mendel <- c(1:200)/8


Percent.Increase <- Mendel/quantilevec*100-100



plot(Mendel/quantilevec*100-100, xlab= "Number of Animals Born", ylab =  "percent increase for 90% probability",  main = "Genotype Frequency 0.125")


### C) Plotting the percentage of additionally required animals for the 0.0625 90% situation
psuccess = 0.0625 # succes probability per animal

goodanimals = 3  # the target number of "good" animals you want to breed
penough = 0.90 # the "power" of your experiment, i.e. the probability by which you want to obtain at least <goodanimals> animals

Nmax = 200 # maximum number of animals that you can ever produce

quantilevec = qbinom(1-penough,1:Nmax,psuccess) # Lower bound of good animals, for each offspring number 1:Nmax, which is achieved with prob. <penough> 
print(quantilevec)
plot(quantilevec,xlab="Number of animals",ylab="Lower bound of good animals obtained with a probability <penough>")

cutoff = min(which(quantilevec >= goodanimals)) # this is where the magic happens :-)

abline(v=cutoff,col="red")

print(cutoff)


Mendel <- c(1:200)/16


Percent.Increase <- Mendel/quantilevec*100-100



plot(Mendel/quantilevec*100-100, xlab= "Number of Animals Born", ylab =  "percent increase for 90% probability",  main = "Genotype Frequency 0.0625")

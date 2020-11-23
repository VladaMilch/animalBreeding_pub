dbin025 <- dbinom(0:6, 6, 0.25)
cumdbin025 <- c(dbin025[1], sum(dbin025[2:7]), sum(dbin025[3:7]), sum(dbin025[4:7]), sum(dbin025[5:7]), sum(dbin025[6:7]), dbin025[7])
barplot(cumdbin025, ylim = 0:1, names.arg = c("0", "1", "2", "3", "4", "5", "6"), xlab= "number animals born of correct genotype", ylab= "probability" , main = "0.25 genotype probability" , col = "darkgreen"  )

dbin05 <- dbinom(0:6, 6, 0.5)
cumdbin05 <- c(dbin05[1], sum(dbin05[2:7]), sum(dbin05[3:7]), sum(dbin05[4:7]), sum(dbin05[5:7]), sum(dbin05[6:7]), dbin05[7])
barplot(cumdbin05, ylim = 0:1, names.arg = c("0", "1", "2", "3", "4", "5", "6"), xlab= "number animals born of correct genotype", ylab= "probability", main = "0.5 genotype probability" , col = "darkgreen")

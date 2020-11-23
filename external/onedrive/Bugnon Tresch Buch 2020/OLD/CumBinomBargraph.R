barplot(dbinom(0:6,6,0.25),col="red",names.arg=0:6)

barplot(dbinom(0:6,6,0.5),col="red",names.arg=0:6)

barplot(dbinom(0:12,12,0.25),col="red",names.arg=0:12)

barplot(dhyper(0:6,6,0.25),col="red",names.arg=0:6)


######hypergeometrische Spielerei

Drawnoputback <- dhyper(0:6, 25, 75, 6, log = FALSE)
Drawnoputback
barplot(Drawnoputback)

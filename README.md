# animalBreedingProj

# animalBreeding 

- is a package, sample size calculation for an animal breeding experiment


## Installation 

There is an install_github from the devtools package, to install packages directly from github:

```{r}
# install.packages("devtools")
devtools::install_github("VladaMilch/animalBreeding_pub", subdir="animalBreeding")
library(animalBreeding)
```

## Usage

See the vignette in  animalBreeding/doc/my-vignette.html or try these examples: 

### Offsprings of a single genotype

When the breedins set up is aimed at a offsprings of single genotype, 
or simply a total number of born pups, one may use the following function to 
calculate the required number of breedings. 

Here we calculate how many breedings are required to ensure that upon mating FVB/N mice, 
there are at least 20 pups, born within a period of 3 days, with the success probability of 90%.
To run the calculation, we also assume that the average number of pups in a single litter is 6.



```{r}
n_breedings <- singleGenotype(
  confidence_p = 0.95,
  birth_days = 3,
  n_offsprings = 20,
  strain = "FVB/N",
  litter_mean = 6)

print(n_breedings)
```

## Offsprings of multiple genotypes

Assume we breed +/- and +/- mice. According to Mendel law, the probabilities of 
offsprings +/+, +/- and -/- are 25%, 50% and 25%. 
We only need 10 pups of -/- genotype, and require them to be born within a perios of 3 days. 
Further, we want all pups to be of one and the same sex (either male or female).

Lastly, we want to ensure that the above requirements are met in at least 95 out of 100 trials.


```{R}
n_breedings <- multiGenotype(
  confidence_p = 0.95, 
  birth_days = 3, 
  genotypes_N = c(0,0,10), 
  genotypes_p = c(0.25, 0.5, 0.25),
  sex_distribution = "unimportant", 
  strain = "FVB/N",
  litter_mean = 6)

n_breedings
```

One may want to know how many animals (in total) should be expected to be born with the breeding setup from above:
```{r}
expectBorn(n_breedings)
```

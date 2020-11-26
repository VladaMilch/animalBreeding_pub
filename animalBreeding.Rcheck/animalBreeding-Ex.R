pkgname <- "animalBreeding"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "animalBreeding-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('animalBreeding')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("calculate_needed_breedings")
### * calculate_needed_breedings

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calculate_needed_breedings
### Title: Calculate the number of breedings needed to guarantee a fixed
###   number of offsprings with a certain probability
### Aliases: calculate_needed_breedings

### ** Examples

calculate_needed_breedings(
    confidence_p=0.95,
    effective_fertility_p=0.6,
    n_needed = 30,
    litter_mean = 7,
    litter_sd = 2.5,
    n_litters=10)


Minimal number of breedings

Calculates the minimal number of animals to be bred in order
to achieve a given number of offsprings with a certain confidence, 
under the one of the models described below.




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calculate_needed_breedings", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calculate_needed_breedings_textbook")
### * calculate_needed_breedings_textbook

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calculate_needed_breedings_textbook
### Title: Calculate the number of breedings needed to obtain a fixed
###   numberf of litters with a certain confidence
### Aliases: calculate_needed_breedings_textbook

### ** Examples

calculate_needed_breedings_textbook(
    confidence_p=0.95,
    effective_fertility_p=0.6,
    n_litters=10)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calculate_needed_breedings_textbook", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calculate_needed_litters_textbook")
### * calculate_needed_litters_textbook

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calculate_needed_litters_textbook
### Title: Calculate the number of litters needed to obtain a fixed numberf
###   of mice with a certain confidence
### Aliases: calculate_needed_litters_textbook

### ** Examples

calculate_needed_breedings_textbook(
    confidence_p=0.95,
    effective_fertility_p=0.6,
    n_litters=10)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calculate_needed_litters_textbook", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

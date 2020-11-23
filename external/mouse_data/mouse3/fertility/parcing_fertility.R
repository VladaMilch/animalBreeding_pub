setwd("/home/compbio/vmilchev/animalBreedingProj/")
library(tidyverse)
library(tabulizer)


# count the number of breeding events 
# (= number of females and the number of cages)
mousepdf_count_females <- function(current_file)
{
  # all file into a single text string
  cf <- tabulizer::extract_text(current_file)
  
  # date and 4 numbers separated by white spaces
  pat_female = "Female,"
  pat_cage = "Cage"
  poss_female <- gregexpr(pattern = pat_female, text = cf)
  #poss_cage <- gregexpr(pattern = pat_cage, text = cf)

  return(c(length(poss_female[[1]]))) 
}


files_pdf <-   c(
  list.files(path = "./external/mouse_data/mouse3/", 
                          pattern = ".pdf", 
                          full.names = T)
  )



nfemales <- sapply(files_pdf, mousepdf_count_females)
# N females from the file: 72 191 119 NA 85 103 58

files_pdf


# attach strain names 
names(nfemales) 
save(nfemales, file = "external/mouse_data/mouse3/fertility/nfemales.Rdata")

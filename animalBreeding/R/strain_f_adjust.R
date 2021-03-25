# adjusts effective fertility based on Festing table by day and 
# http://www.informatics.jax.org/silver/tables/table4-1.shtml
strain_f_adjust <- function(
  birth_days, 
  strain
  ){

  if(!(strain %in% strain_fert_data$strain_name)){
    stop(paste("Please put existing strain name.\nAvailable strains:", 
               paste(strain_fert_data$strain_name, 
                     collapse = " "),
               " \n",
               collapse = " "))
  }
  
  if(!(birth_days %in% c(1,2,3,4))){
    stop("birth_days can only take values 1, 2, 3 or 4")
  }
  
  # strain effective fertility total
  streffe <- strain_fert_data$fertility[which(strain_fert_data$strain_name==strain)]
  # from Festing
  fert_by_day <- c(13.4, 13.4, 35, 17.7)
  result_fertility <- cumsum(fert_by_day/sum(fert_by_day))[birth_days] * streffe
  return(result_fertility)
  
}

# strain_f_adjust(birth_days = 4, strain = "A/J")

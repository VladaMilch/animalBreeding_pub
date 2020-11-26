setwd("/home/compbio/vmilchev/animalBreedingProj/")
library(tidyverse)
library(tabulizer)

mousepdf_2_table <- function(current_file)
{
  # all file into a single text string
  cf <- tabulizer::extract_text(current_file)
  
  # date and 4 numbers separated by white spaces
  pat = "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}\\s+([0-9]+\\s+){4}"
  poss <- gregexpr(pattern = pat, text = cf)
  ff <- function(match_id, input_string){
    substr(
      input_string, 
      start=poss[[1]][match_id], 
      stop =poss[[1]][match_id] + attr(poss[[1]], 
                                       which = "match.length")[match_id]-1)
  }
  
  match_list <- lapply(c(1:length(poss[[1]])), 
                       FUN = function(xx){
                         ff(match_id = xx, input_string = cf)
                       })
  extracted_df <- data.frame(
    do.call(rbind, 
            lapply(match_list, 
                   FUN=function(x){
                     unlist(strsplit(x, split = " "))})), 
    stringsAsFactors = FALSE)
  
  colnames(extracted_df) <-  c("bitrh_dates",
                           "pups_born",
                           "pups_beforeW",
                           "weaned_f",
                           "weaned_m")
  
  extracted_df$pups_born <- as.numeric(extracted_df$pups_born)
  extracted_df$pups_beforeW <- as.numeric(extracted_df$pups_beforeW)
  extracted_df$weaned_f <- as.numeric(extracted_df$weaned_f)
  extracted_df$weaned_m <- as.numeric(extracted_df$weaned_m)
  
  return(extracted_df)
}


files_pdf <- list.files(path = "./external/mouse_data/mouse3/", 
                        pattern = ".pdf", full.names = T)




generate_file_name <- function(original_pdf_fname){
  txt.file.name <- gsub(
    gsub(original_pdf_fname, 
         replacement = "_", 
         pattern = " "),
    replacement = ".txt", 
    pattern = ".pdf")
  return(txt.file.name)
}


grep_strain_name <- function(original_pdf_fname){
    fname <- strsplit(
      strsplit(original_pdf_fname, split = "mouse3//")[[1]][2],
      split = ".pdf")[[1]][1]
    interne_name <- paste0(unlist(strsplit(
      strsplit(fname, split=" interne ")[[1]][1], 
      split = " ")), collapse = "_")
    irat_name <- paste0(unlist(strsplit(
      strsplit(interne_name, split="_iRATS_")[[1]][1], 
      split = " ")), collapse = "_")
    u219_name <- paste0(unlist(strsplit(
      strsplit(irat_name, split="_U219_")[[1]][1:2], 
      split = " ")), collapse = "_")
    if(grepl(pattern = "_-", u219_name)){
      underscores_out <- gsub(pattern = "_-", 
             replacement = "", 
             x = u219_name)
      return(underscores_out)
    }else{
      return(irat_name)
    }
  }

mousebreeding <- lapply(files_pdf, mousepdf_2_table)
names(mousebreeding) <- sapply(files_pdf, grep_strain_name)

summaryDF <- data.frame(originalPDF = files_pdf, 
      parsedTBL = sapply(files_pdf, generate_file_name), 
      strainNameShort = names(mousebreeding)
)
save(mousebreeding, summaryDF, file = "./external/mouse_data/mouse3/mousebreeding.Rds")


for(filename in files_pdf){
  write.table(
    mousepdf_2_table(filename), 
    file = generate_file_name(filename), 
    row.names = FALSE,
    quote = FALSE, 
    sep = '\t')
}





##################################################################
## file by file checks (only if there is a summary at the bottom)
###################################################################

# B6cBrd
files_pdf[[1]]
df1 <- mousepdf_2_table(files_pdf[[1]])
sum(as.numeric(df1$pups_born))==629 # never true, i checked manually - mine is correct
sum(as.numeric(df1$pups_beforeW))==565
sum(as.numeric(df1$weaned_f))==212
sum(as.numeric(df1$weaned_m))==199


# B6D2F1
files_pdf[[2]]
df2 <- mousepdf_2_table(files_pdf[[2]])
sum(as.numeric(df2$pups_born))==5457
sum(as.numeric(df2$pups_beforeW))==5170
sum(as.numeric(df2$weaned_f))==2773
sum(as.numeric(df2$weaned_m))==1936

# B6J CrlF
files_pdf[[3]]
df3 <- mousepdf_2_table(files_pdf[[3]])
sum(as.numeric(df3$pups_born))==2903
sum(as.numeric(df3$pups_beforeW))==2881
sum(as.numeric(df3$weaned_f))==1213
sum(as.numeric(df3$weaned_m))==1136

# B6J Fue
files_pdf[[4]]
df4 <- mousepdf_2_table(files_pdf[[4]])
# no report in the last page

# Balbc
files_pdf[[5]]
df5 <- mousepdf_2_table(files_pdf[[5]])
# 777 722   260   290
sum(as.numeric(df5$pups_born))==777
sum(as.numeric(df5$pups_beforeW))==722
sum(as.numeric(df5$weaned_f))==260
sum(as.numeric(df5$weaned_m))==290

# Card9_KO
files_pdf[[6]]
df6 <- mousepdf_2_table(files_pdf[[6]])
# no report in the last page

# CD1 old and new
files_pdf[[7]]
files_pdf[[8]]
df7 <- mousepdf_2_table(files_pdf[[7]])
df8 <- mousepdf_2_table(files_pdf[[8]])
sum(as.numeric(df8$pups_born))==10531 # here true!!
sum(as.numeric(df8$pups_beforeW))==9341
sum(as.numeric(df8$weaned_f))==7417
sum(as.numeric(df8$weaned_m))==1134
# CD1 old - no report
# CD1 new
# 10531 9341   7417   1134    0

# DBA2 J Fue
files_pdf[9]
df9 <- mousepdf_2_table(files_pdf[[9]])
# 1149 1105 374 365    0
sum(as.numeric(df9$pups_born))==1149
sum(as.numeric(df9$pups_beforeW))==1105
sum(as.numeric(df9$weaned_f))==374
sum(as.numeric(df9$weaned_m))==365

# FcRn
files_pdf[10]
df10 <- mousepdf_2_table(files_pdf[[10]])
# 4465 4435   2024 529
sum(as.numeric(df10$pups_born))==983 # here true!!
sum(as.numeric(df10$pups_beforeW))==978
sum(as.numeric(df10$weaned_f))==471
sum(as.numeric(df10$weaned_m))==426

# NMRI
files_pdf[11]
df11 <- mousepdf_2_table(files_pdf[[11]])
# 4465 4435   2024 529
sum(as.numeric(df11$pups_born))==4465 # here also true!!
sum(as.numeric(df11$pups_beforeW))==4435
sum(as.numeric(df11$weaned_f))==2024
sum(as.numeric(df11$weaned_m))==529

# strain_names <- c(
#   sapply(
#     files_pdf, FUN =function(char_el){
#       fname <- strsplit(char_el, split = "mouse3//")[[1]][2]
#       interne_name <- paste0(unlist(strsplit(
#         strsplit(fname, split=" interne ")[[1]][1], 
#         split = " ")), collapse = "_")
#       irat_name <- paste0(unlist(strsplit(
#         strsplit(interne_name, split="_iRATS_")[[1]][1], 
#         split = " ")), collapse = "_")
#       u219_name <- paste0(unlist(strsplit(
#         strsplit(irat_name, split="_U219_")[[1]][1:2], 
#         split = " ")), collapse = "_")
#       if(grepl(pattern = "_-", u219_name)){
#         underscores_out <- gsub(pattern = "_-", replacement = "", 
#                                 x = u219_name)
#         return(underscores_out)
#       }else{
#         return(u219_name)
#       }
#     }
#   ))
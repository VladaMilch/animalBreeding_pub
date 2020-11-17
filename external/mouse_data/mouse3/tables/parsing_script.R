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
  return(extracted_df)
}



files_pdf <- list.files(path = "./external/mouse_data/mouse3/", 
                        pattern = ".pdf", full.names = T)

df1 <- mousepdf_2_table(files_pdf[[1]])
sum(as.numeric(df1$pups_born))==629 # never true, i chacked manually - mine is correct
sum(as.numeric(df1$pups_beforeW))==565
sum(as.numeric(df1$weaned_f))==212
sum(as.numeric(df1$weaned_m))==199


df2 <- mousepdf_2_table(files_pdf[[2]])
sum(as.numeric(df2$pups_born))==5457
sum(as.numeric(df2$pups_beforeW))==5170
sum(as.numeric(df2$weaned_f))==2773
sum(as.numeric(df2$weaned_m))==1936

df3 <- mousepdf_2_table(files_pdf[[3]])
sum(as.numeric(df3$pups_born))==2903
sum(as.numeric(df3$pups_beforeW))==2881
sum(as.numeric(df3$weaned_f))==1213
sum(as.numeric(df3$weaned_m))==1136

df4 <- mousepdf_2_table(files_pdf[[4]])
# no report in the last page
sum(as.numeric(df4$pups_born))
sum(as.numeric(df4$pups_beforeW))

df5 <- mousepdf_2_table(files_pdf[[5]])
# 777 722   260   290
sum(as.numeric(df5$pups_born))==777
sum(as.numeric(df5$pups_beforeW))==722
sum(as.numeric(df5$weaned_f))==260
sum(as.numeric(df5$weaned_m))==290

df6 <- mousepdf_2_table(files_pdf[[6]])
# 1149 1105 374 365    0
sum(as.numeric(df6$pups_born))==1149
sum(as.numeric(df6$pups_beforeW))==1105
sum(as.numeric(df6$weaned_f))==374
sum(as.numeric(df6$weaned_m))==365

df7 <- mousepdf_2_table(files_pdf[[7]])
# 4465 4435   2024 529
sum(as.numeric(df7$pups_born))==4465 # here true!!
sum(as.numeric(df7$pups_beforeW))==4435
sum(as.numeric(df7$weaned_f))==2024
sum(as.numeric(df7$weaned_m))==529

generate_file_name <- function(original_pdf_fname){
    txt.file.name <- gsub(
        gsub(original_pdf_fname, 
             replacement = "_", 
             pattern = " "),
        replacement = ".txt", 
        pattern = ".pdf")
    return(txt.file.name)
}


for(filename in files_pdf){
    write.table(
        mousepdf_2_table(filename), 
        file = generate_file_name(filename), 
        row.names = FALSE,
        quote = FALSE, 
        sep = '\t')
}









# #### manual check ####
# born_page_1 <- c(8,3,4,5,8,8,8,6,4,6,8,2,2,8,5,8)
# born_page_2 <- c(4,6,7,5,7,11,10,6,2,2,1,4,3,6,5,1,6,7,10)
# born_page_3 <- c(6,9,6,1,7,12,6,7,6,10,8,9,3,11,9,10)
# born_page_4 <- c(8,6,7,4,6,6,7,8,9)
# born_page_5 <- c(7,6,7,7,6,9,9)
# born_page_6 <- c(8,6,7,5,5,7,6,6,8,8,7,6,5)
# born_page_7 <- c(6,3,7,6,5,6)
# born_page_8 <- c(5,3,6,8,6,8,6,5)
# 
# all(extracted_df[1:length(born_page_1), 
#              "pups_born"]==born_page_1)
# all(extracted_df[17:(16+length(born_page_2)), 
#              "pups_born"]==born_page_2)
# all(extracted_df[36:(35+length(born_page_3)), 
#              "pups_born"]==born_page_3)
# all(extracted_df[52:(51+length(born_page_4)), 
#              "pups_born"]==born_page_4)
# 
# all(extracted_df[61:(60+length(born_page_5)), 
#              "pups_born"]==born_page_5)
# all(extracted_df[68:(67+length(born_page_6)), 
#              "pups_born"]==born_page_6)
# 
# all(extracted_df[81:(80+length(born_page_7)), 
#              "pups_born"]==born_page_7)
# all(extracted_df[87:(86+length(born_page_8)), 
#              "pups_born"]==born_page_8)
# 
# sum(c(born_page_1, 
#       born_page_2, 
#       born_page_3, 
#       born_page_4, 
#       born_page_5, 
#       born_page_6, 
#       born_page_7, 
#       born_page_8))



files_pdf <- list.files(path = "./external/mouse_data/mouse2/", 
                        pattern = ".pdf", full.names = T)


cd1_1 <- mousepdf_2_table(current_file = files_pdf[[1]])
cd1_2 <- mousepdf_2_table(current_file = files_pdf[[2]])

sum(as.numeric(cd1_1$pups_born)) # here no info.. but 64 pages - could there be a problem?

sum(as.numeric(cd1_2$pups_born)) 
sum(as.numeric(cd1_2$pups_beforeW)) == 9341
sum(as.numeric(cd1_2$weaned_f)) == 7417
sum(as.numeric(cd1_2$weaned_m)) == 1134

generate_file_name_txt <- function(original_pdf_fname){
  txt.file.name <- gsub(
    gsub(
      gsub(original_pdf_fname, 
           replacement = "_", 
           pattern = " - "),
      replacement = "_",
      pattern = " "),
    replacement = ".txt", 
    pattern = ".pdf")
  return(txt.file.name)
}

generate_file_name_csv <- function(original_pdf_fname){
  txt.file.name <- gsub(
    gsub(
      gsub(original_pdf_fname, 
           replacement = "_", 
           pattern = " - "),
      replacement = "_",
      pattern = " "),
    replacement = ".csv", 
    pattern = ".pdf")
  return(txt.file.name)
}



for(filename in files_pdf){
  write.table(
    mousepdf_2_table(filename), 
    file = generate_file_name_txt(filename), 
    row.names = FALSE,
    quote = FALSE, 
    sep = '\t')
}

for(filename in files_pdf){
  write.csv(
    mousepdf_2_table(filename), 
    file = generate_file_name_csv(filename), 
    row.names = FALSE,
    quote = FALSE)
}


# 10531 9341   7417   1134  


# generate_file_name <- function(original_pdf_fname){
#   txt.file.name <- gsub(
#     gsub(original_pdf_fname, 
#          replacement = "_", 
#          pattern = " "),
#     replacement = ".txt", 
#     pattern = ".pdf")
#   return(txt.file.name)
# }
# 
# 
# for(filename in files_pdf){
#   write.table(
#     mousepdf_2_table(filename), 
#     file = generate_file_name(filename), 
#     row.names = FALSE,
#     quote = FALSE, 
#     sep = '\t')
# }

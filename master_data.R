setwd("~/R/play/Solved_Unsolved_Mysteries")

library(dplyr)
rm(list=ls());cat('\f');gc()

wd <- list(R = "C:/Users/bende/Documents/R/play/Solved_Unsolved_Mysteries/R",
           home = "C:/Users/bende/Documents/R/play/Solved_Unsolved_Mysteries", 
           data = "C:/Users/bende/Documents/R/play/Solved_Unsolved_Mysteries/data", 
           shiny = "C:/Users/bende/Documents/R/play/Solved_Unsolved_Mysteries/shiny/unsolved_shiny")


# load all the data----
setwd(wd$data)

csv_files <- list.files(pattern = "\\.csv")

for(i in csv_files){
  temp.varname <- gsub("\\.csv$", "", i) 
  temp.varname <- paste0("data__", temp.varname)
  
  assign(x = temp.varname, read_csv(i))
  
}
rm(i,csv_files,temp.varname)

save(list=ls(pattern = "^data__"),
     file="unsolved.RData")


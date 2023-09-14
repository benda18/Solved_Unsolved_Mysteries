library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidycensus)
library(tigris)
library(data.table)
library(openssl)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()

# functions-----
lzero <- function(x, n.leading.zeroes){
  if(is.na(x)){
    out <- "NA"
  }else{
    out <- as.character(x)
  }
  
  #if(!is.na(x)){
  
  if(nchar(out) < n.leading.zeroes){
    out <- paste(c(rep("0", n.leading.zeroes - nchar(out)), 
                   out), sep = "", collapse = "")
  }
  #}
  
  return(out)
}

# import crosswalks----
for(i in list.files(pattern = "^cw\\.|^cw_")){
  # assign varname 
  temp.varname <- gsub("\\.csv$", "", i)
  # read data to temp
  temp.df      <- read_csv(i)
  # assign df to var
  assign(x = temp.varname, value = temp.df)
}
rm(i,temp.varname,temp.df)

# import data----

full.df <- read_csv("mysteries.csv") %>%
  .[complete.cases(.),] %>%
  .[!duplicated(.),] %>%
  full_join(., read_csv("master.meta.csv")) %>%
  left_join(., 
            cw.outcome_solved) %>%
  .[!is.na(.$master.outcome),]



full.df$uid_seg <- sha512(x = full.df$seg_url) %>% 
  as.character() %>%
  substr(., 0, 10)


full.df$uid_snen <-  paste("S", 
                       unlist(lapply(full.df$s_num, lzero, 2)), 
                       "E", 
                       unlist(lapply(full.df$nth_s_ep, lzero, 3)),
                       sep = "")

full.df$uid_snen_f <- factor(full.df$uid_snen)


ggplot() + 
  geom_point(data = full.df, 
             aes(x = uid_seg, y = s_num))

54000/(1440*12)

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
segoc3 <- function(url){
  # get season outcome from url
  # ex url: "https://unsolvedmysteries.fandom.com/wiki/Heirs_of_George_Marsh"
  require(glue)
  require(rvest)
  require(dplyr)
  # if there are any spaces in segname, replace with underscore to match naming
  # convetion of website
  
  #segname <- gsub(" ", "_", segname)
  #url1 <- glue("https://unsolvedmysteries.fandom.com/wiki/{segname}")
  url1 <- url
  
  htext <- rvest::read_html(url1) %>%
    html_text() %>%
    strsplit(., "\t{0,}\n{1,}\t{0,}") %>%
    unlist() %>%
    .[. != ""] %>%
    trimws()
  
  which(grepl("Episode", htext))
  grep("episode", htext, ignore.case = T, value = T)
  htext
  
  
  seg.title <- htext[1] %>% strsplit(., "\\|") %>% unlist() %>% first %>% trimws()
  seg.results <- grep("Results {0,}:", htext, value = T) %>%
    gsub("^.*Results: {0,}", "", .) %>%
    strsplit(x = ., 
             split = "\\W") %>% 
    unlist() %>%
    .[. != ""] %>%
    first() %>%
    tolower()
  
  # seg.meta <- htext %>% grep("Real Name.*\\d{4,4}", ., value = T) %>%
  #   .[nchar(.) == min(nchar(.))]
  
  seg.categories <- htext %>% 
    grep("wgCategories", ., value = T) %>% 
    gsub("\"", "", .) %>%
    strsplit(x = ., split = "],") %>%
    unlist() %>%
    grep("wgCategories", ., value = T) %>%
    gsub("wgCategories:\\S{1,1}", "", .) %>%
    strsplit(., ",") %>% unlist()
  
  out.df <- data.frame(seg_url = url, 
                       tag = seg.categories)
  return(out.df)
  #Sys.sleep(1)
}

#segoc3("https://unsolvedmysteries.fandom.com/wiki/Heirs_of_George_Marsh")

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

# cw_tags <- read_csv("https://raw.githubusercontent.com/benda18/Solved_Unsolved_Mysteries/main/composite.data.csv")
# cw_tags <- summarise(group_by(cw_tags, seg_name, tag)) %>%
#   .[complete.cases(.),] %>%
#   .[!duplicated(.),]

# import data----

full.df <- read_csv("mysteries.csv") %>%
  .[complete.cases(.),] %>%
  .[!duplicated(.),] %>%
  full_join(., read_csv("master.meta.csv")) %>%
  left_join(., 
            cw.outcome_solved) %>%
  .[!is.na(.$master.outcome),] %>%
  .[complete.cases(.),] %>%
  .[!duplicated(.),]

# # get tags----
# 
# out.tags <- NULL
# for(i in unique(full.df$seg_url)){
#   Sys.sleep(1)
#   print(i)
#   temp.tags <- try(segoc3(i))
#   if(class(temp.tags) != "try-error"){
#     out.tags <- rbind(out.tags, 
#                       temp.tags)
#   }
#   rm(temp.tags)
# }
# 
# write_csv(out.tags,"cw_tags.csv")

full.df$seg_name2 <- gsub(pattern = "^.*/", 
                          replacement = "", 
                          full.df$seg_url) %>%
  gsub("%27", "'", .) %>%
  gsub("_", " ", .)

# full.df <- left_join(full.df, 
#           cw_tags, 
#           relationship = "many-to-many", 
#           by = c("seg_name2" = "seg_name")) 



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



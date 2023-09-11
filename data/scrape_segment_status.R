library(htmltools)
library(dplyr)
library(httr)
library(httr2)
library(xml2)
library(rvest)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()


segoc <- function(url1, season = NA, episode = NA){
  # get season outcome from url
  # ex url: "https://unsolvedmysteries.fandom.com/wiki/Heirs_of_George_Marsh"
  require(rvest)
  require(dplyr)
  htext <- rvest::read_html(url1) %>%
    html_text() %>%
    strsplit(., "\t{0,}\n{1,}\t{0,}") %>%
    unlist() %>%
    .[. != ""] %>%
    trimws()
  
  seg.title <- htext[1] %>% strsplit(., "\\|") %>% unlist() %>% first %>% trimws()
  seg.results <- grep("^Results:", htext, value = T) %>%
    gsub("^Results: ", "", .) %>%
    strsplit(x = ., 
             split = "\\W") %>% 
    unlist() %>%
    .[. != ""] %>%
    first() %>%
    tolower()
  
  out.df <- data.frame(seg_name = seg.title, 
                       seg.outcome = seg.results, 
                       s_num = season, 
                       ep_num = episode)
  return(out.df)
  
}


# pull down urls----
s1ep8.1 <- segoc("https://unsolvedmysteries.fandom.com/wiki/Ann_Sigmin_and_Garey_Goff", 
      1, 8) 





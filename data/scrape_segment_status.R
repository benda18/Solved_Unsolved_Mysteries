library(htmltools)
library(dplyr)
library(httr)
library(httr2)
library(xml2)
library(rvest)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()

url1 <- "https://unsolvedmysteries.fandom.com/wiki/Gail_DeLano"

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
  seg.results <- grep("Results:", htext, value = T) %>%
    gsub("^.*Results: ", "", .) %>%
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

cw.outcome_solved <- rbind(data.frame(master.outcome = "SOLVED", 
                                      seg.outcome    = c("captured", 
                                                         "solved")), 
                           data.frame(master.outcome = "UNSOLVED", 
                                      seg.outcome    = c("wanted", 
                                                         "unresolved", 
                                                         "unsolved")))

s1ep1 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Gulf_Breeze_UFO", 1, 1), 
               segoc("https://unsolvedmysteries.fandom.com/wiki/Louis_Carlucci", 1, 1), 
               segoc("https://unsolvedmysteries.fandom.com/wiki/Joe_Shepherd", 1, 1), 
               segoc("https://unsolvedmysteries.fandom.com/wiki/Gail_DeLano", 1, 1)) %>% 
  left_join(., cw.outcome_solved)

s1ep2 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/D.B._Cooper", 1,2),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Jon_Yount", 1,2),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Diane_Brodbeck", 1,2),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Steve_Hadley", 1,2),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Don_Henry_and_Kevin_Ives", 1,2), 
               segoc("https://unsolvedmysteries.fandom.com/wiki/Dennis_Walker", 1,2)) %>% 
  left_join(., cw.outcome_solved)




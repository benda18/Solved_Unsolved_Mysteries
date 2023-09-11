library(htmltools)
library(dplyr)
library(httr)
library(httr2)
library(xml2)
library(rvest)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()

url1 <- "https://unsolvedmysteries.fandom.com/wiki/The_Queen_Mary"

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
  seg.results <- grep("Results: ", htext, value = T) %>%
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

s1ep3 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/The_Queen_Mary", 1,3), 
               segoc("https://unsolvedmysteries.fandom.com/wiki/Tallman_House", 1,3), 
               segoc("https://unsolvedmysteries.fandom.com/wiki/The_General_Wayne_Inn", 1,3), 
               segoc("https://unsolvedmysteries.fandom.com/wiki/Tatum_House", 1,3))  %>% 
  left_join(., cw.outcome_solved)

s1ep4 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Son_of_Sam", 1,4),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Heirs_of_Walter_Green", 1,4),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Harold_and_Thelma_Swain", 1,4),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Chevy_Chase_Bandit", 1,4),
               segoc("https://unsolvedmysteries.fandom.com/wiki/John_William_Farr", 1,4),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Shotgun_Bandit", 1,4),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Shopping_Bag_Bandit", 1,4)) %>% 
  left_join(., cw.outcome_solved)

s1ep5 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Bob_Dozier_and_John_Russell",1,5),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Kristen_Tomlin_and_Suzanne_Russell",1,5),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Rolex_Robbers",1,5),
               segoc("https://unsolvedmysteries.fandom.com/wiki/The_Father_of_Janet_O%27Regan",1,5),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Son_of_Sam",1,5),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Annie_Hearin",1,5)) %>% 
  left_join(., cw.outcome_solved)

s1ep6 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Christi_Nichols", 1,6),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Gail_DeLano", 1,6),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Mark_Adams", 1,6),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Steven_Cox", 1,6),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Barbara_Jean_Horn", 1,6)) %>% 
  left_join(., cw.outcome_solved)

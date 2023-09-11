library(htmltools)
library(dplyr)
library(httr)
library(httr2)
library(xml2)
library(rvest)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()

url1 <- "https://unsolvedmysteries.fandom.com/wiki/Bruno_and_Bobo"
url1 <- "https://unsolvedmysteries.fandom.com/wiki/Micki_Jo_West"

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
  
  out.df <- data.frame(seg_name = seg.title, 
                       seg.outcome = seg.results, 
                       s_num = season, 
                       ep_num = episode, 
                       tag = seg.categories)
  return(out.df)
  
}



# pull down urls----

cw.outcome_solved <- rbind(data.frame(master.outcome = "SOLVED", 
                                      seg.outcome    = c("captured", 
                                                         "solved")), 
                           data.frame(master.outcome = "UNSOLVED", 
                                      seg.outcome    = c("wanted", 
                                                         "wantedlinks",
                                                         "unresolved", 
                                                         "unsolved",
                                                         "unsolvedlinks")))

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


s1ep7 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Kurt_Sova",1,7),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Eugene_Kvet",1,7),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Shopping_Bag_Bandit",1,7),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Bob_Dozier_and_John_Russell",1,7),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Lancaster_Extortion_Writer",1,7),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Father_Reynaldo_Rivera",1,7),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Father_John_Kerrigan",1,7),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Mystery_Rock",1,7)) %>% 
  left_join(., cw.outcome_solved)

s1ep8 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Ann_Sigmin_and_Garey_Goff",1,8),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Don_Henry_and_Kevin_Ives",1,8),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Steven_Cox",1,8),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Robert_Matthews",1,8),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Kristina_Florence",1,8),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Rogest_Cain",1,8)) %>% 
  left_join(., cw.outcome_solved)

s1ep9 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Jenny_Pratt", 1,9),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Joe_Shepherd", 1,9),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Jack_Quinn", 1,9),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Clarence_and_Geneva_Roberts", 1,9),
               segoc("https://unsolvedmysteries.fandom.com/wiki/Bruno_and_Bobo", 1,9)) %>% 
  left_join(., cw.outcome_solved)

s1ep10 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Philip_Pelletier", 1,10),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Lancaster_Extortion_Writer", 1,10),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Steven_Cox", 1,10),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Matthew_Chase", 1,10),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Marilu_Geri", 1,10),
                segoc("https://unsolvedmysteries.fandom.com/wiki/The_Family_of_Dolores_Valadez", 1,10)) %>% 
  left_join(., cw.outcome_solved)


s1ep11 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Lee_Selwyn",1,11),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Steven_Cox",1,11),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Michael_Rosenblum",1,11),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Rogest_Cain",1,11),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Arthur_Frankford",1,11)) %>% 
  left_join(., cw.outcome_solved)

s1ep12 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Jeremy_Bright", 1, 12),
                segoc("https://unsolvedmysteries.fandom.com/wiki/John_Martin", 1, 12),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Heirs_of_Dan_Willains", 1, 12),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Leo_Koury", 1, 12),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Michaela_Garecht", 1, 12),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Amber_Swartz", 1, 12),
                segoc("https://unsolvedmysteries.fandom.com/wiki/The_Crew_of_the_Liebling", 1, 12)) %>% 
  left_join(., cw.outcome_solved)

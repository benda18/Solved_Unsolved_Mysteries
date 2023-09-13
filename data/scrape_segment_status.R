library(htmltools)
library(dplyr)
library(httr)
library(httr2)
library(xml2)
library(rvest)
library(openssl)
library(ggplot2)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()



lzero <- function(x, n.leading.zeroes){
  out <- as.character(x)
  if(nchar(out) < n.leading.zeroes){
    out <- paste(c(rep("0", n.leading.zeroes - nchar(out)), 
                   out), sep = "", collapse = "")
  }
  return(out)
}


url1 <- "https://unsolvedmysteries.fandom.com/wiki/Bruno_and_Bobo"
url1 <- "https://unsolvedmysteries.fandom.com/wiki/Micki_Jo_West"
url1 <- "https://unsolvedmysteries.fandom.com/wiki/David_Davis"


segoc2 <- function(segname, season = NA, episode = NA){
  # get season outcome from url
  # ex url: "https://unsolvedmysteries.fandom.com/wiki/Heirs_of_George_Marsh"
  require(glue)
  require(rvest)
  require(dplyr)
  # if there are any spaces in segname, replace with underscore to match naming
  # convetion of website
  segname <- gsub(" ", "_", segname)
  
  url1 <- glue("https://unsolvedmysteries.fandom.com/wiki/{segname}")
  
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
  
  out.df <- data.frame(seg_name = seg.title, 
                       seg.outcome = seg.results, 
                       s_num = season, 
                       ep_num = episode, 
                       tag = seg.categories)
  return(out.df)
  Sys.sleep(1)
}

s2 <- segoc2

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

s1ep13 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Micki_Jo_West",1,13),
                segoc("https://unsolvedmysteries.fandom.com/wiki/David_Davis",1,13),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Shannon_Davis",1,13),
                segoc("https://unsolvedmysteries.fandom.com/wiki/The_Brother_of_Sylvia_Wemhoff",1,13),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Margaret_Murphy",1,13),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Marlene_Santana",1,13),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Carlina_White",1,13),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Christopher_Abeyta",1,13),
                segoc("https://unsolvedmysteries.fandom.com/wiki/The_Bald_Mountain_Shooters",1,13),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Robert_Leads",1,13)) %>% 
  left_join(., cw.outcome_solved)

s1ep14 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Face_on_Mars",1,14),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Arthur_Frankford",1,14),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Kathy_Hobbs",1,14),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Philip_Pelletier",1,14),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Jackie_Harrington",1,14),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Angelo_Desideri",1,14)) %>% 
  left_join(., cw.outcome_solved)

s1ep15 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Frank_Morris_and_the_Anglin_Brothers",1,15),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Jean_Marie_Gagnon",1,15),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Thomas_Nauss_Jr.",1,15),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Carl_Alfred_Eder",1,15),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Lena_Regina_Smith",1,15),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Joseph_Mancini",1,15)) %>% 
  left_join(., cw.outcome_solved)

s1ep16 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Mickey_and_Trudy_Thompson",1,16),
                segoc("https://unsolvedmysteries.fandom.com/wiki/The_Bald_Mountain_Shooters",1,16),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Crazy_Glue_Bandit",1,16),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Bigfoot",1,16),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Heirs_of_George_J._Stein",1,16),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Joyce_McLain",1,16),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Jean_Marie_Gagnon",1,16)) %>% 
  left_join(., cw.outcome_solved)

s1ep17 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Burrowing_Burglars",1,17),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Kristle_Merzlock",1,17),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Thomas_Sawyer",1,17),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Robert_Leads",1,17),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Walter_Wenke",1,17),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Ronald_Denslow",1,17),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Permon_Gilbert",1,17)) %>% 
  left_join(., cw.outcome_solved)

s1ep18 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Donald_Smith",1,18),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Larry_Munroe",1,18),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Brushy_Bill_Roberts",1,18),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Jean_Marie_Gagnon",1,18),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Charles_Mule",1,18)) %>% 
  left_join(., cw.outcome_solved)

s1ep19 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Lisa_Marie_Kimmell",1,19),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Louis_Carlucci",1,19),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Lost_Dutchman_Mine",1,19),
                #segoc("https://unsolvedmysteries.fandom.com/wiki/Thomas_Hotard_and_Audrey_Moate",1,19),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Thomas_Hotard_and_Audrey_Moate",1,19)) %>% 
  left_join(., cw.outcome_solved)

s1ep20 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/John_Mooney",1,20),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Liz_Carmichael",1,20),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Annie_Hearin",1,20),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Terri_McClure",1,20),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Charles_Wickman",1,20)) %>% 
  left_join(., cw.outcome_solved)

s1ep21 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Dexter_Stefonek",1,21),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Jorge_Cortez",1,21),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Heirs_of_Dan_Willains",1,21),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Fumbles",1,21),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Michael_and_Sharon_Mohon",1,21),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Larry_Dennis_Miller",1,21),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Curtis_Watson",1,21),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Joseph_Hutchinson",1,21)) %>% 
  left_join(., cw.outcome_solved)

s1ep22 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Gus_Hoffman",1,22),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Matthew_Chase",1,22),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Ron_Rushton",1,22),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Patsy_Wright",1,22),
                segoc("https://unsolvedmysteries.fandom.com/wiki/The_Families_of_the_S.S._Muskogee_Crew",1,22)) %>% 
  left_join(., cw.outcome_solved)

s1ep23 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Julie_Cross",1,24-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Angelo_Desideri",1,24-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Bicycle_Bandit",1,24-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Kathy_Power",1,24-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Charles_Nungesser_and_Francois_Coli",1,24-1)) %>% 
  left_join(., cw.outcome_solved)

s1ep24 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Victorio_Peak_Treasure",1,25-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Kari_Lynn_Nixon",1,25-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/David_Rhodes",1,25-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Keelan_and_David_J._Rhodes",1,25-1)) %>% 
  left_join(., cw.outcome_solved)

s1ep25 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Sheldon_Weinberg",1,26-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Gulf_Breeze_UFO",1,26-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Joe_Shepherd",1,26-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Marlene_Santana",1,26-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Carlina_White",1,26-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Christopher_Abeyta",1,26-1)) %>% 
  left_join(., cw.outcome_solved)

s1ep26 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Barbara_Jean_Horn",1,27-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Liz_Carmichael",1,27-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/The_Brother_of_Sylvia_Wemhoff",1,27-1)) %>% 
  left_join(., cw.outcome_solved)

s1ep27 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Mike_Riemer",1,28-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Diane_Robertson",1,28-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Stephen_Harkins_and_Ruth_Cooper",1,28-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/David_Rhodes",1,28-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Keelan_and_David_J._Rhodes",1,28-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Michael_and_Sharon_Mohon",1,28-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Patsy_Wright",1,28-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Jackie_Harrington",1,28-1)) %>% 
  left_join(., cw.outcome_solved)

s1ep28 <- rbind(segoc("https://unsolvedmysteries.fandom.com/wiki/Todd_McAfee",1,29-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Ronald_Denslow",1,29-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Joseph_Hutchinson",1,29-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/Victorio_Peak_Treasure",1,29-1),
                segoc("https://unsolvedmysteries.fandom.com/wiki/The_Families_of_the_S.S._Muskogee_Crew",1,29-1)) %>% 
  left_join(., cw.outcome_solved)



# try a more advanced approach for finding urls for season 2----
x <- "Episode 29

Aired: September 20, 1989

    Update: Wanted: Charles Mule
    Lost Loves: Lt. Karen Stephens
    Legend: Roswell Crash
    Lost: Tara Calico and Michael Henley" 
ep_metadata <- function(x){
  require(readr)
  require(dplyr)
  require(lubridate)
  out <- x %>%
    read_lines() %>%
    trimws() %>%
    .[. != ""]
  
  grep("aired:.*\\d{4,4}$", out, value = T, ignore.case = T)
  
  data.frame(variable = c("num_ep", "date_aired"), 
             value    = c(grep("Episode \\d{1,3}$", out, value = T, ignore.case = T), 
                          grep("aired:.*\\d{4,4}$", out, value = T, ignore.case = T)))
  
}


  

s2e29 <- rbind(segoc2(segname = "Charles Mule", 2,29),
               segoc2("Lt. Karen Stephens", 2, 29),
               segoc2("Roswell Crash", 2,29),
               segoc2("Tara Calico", 2,29),
               segoc2("Michael Henley", 2,29))

s2e30 <- rbind(s2("Jack Brown", 2,30),
               s2("Blinking Crucifix",2,30),
               s2("Sheldon Weinberg",2,30),
               s2("Robert Dennie"),
               s2("Melvin Edward Mays"),
               s2("Leslie Isben Rogge")) %>%
  mutate(., s_num = 2, ep_num = 30)

s2e31 <- rbind(s2("Kay Hall", 2,31),
               s2("Lt. Karen Stephens"),
               s2("New York Coin Scam"),
               s2("Mabel Woods"),
               s2("The Crew of the Sara Joe"),
               s2("Avery Norris")) %>%
  mutate(., s_num = 2, ep_num = 31)


s2e32 <- rbind(s2("Greg Webb", 2, 32),
               s2("Billie and Joey Rogers"),
               s2("John Mooney"),
               s2("Gary and Terry Magno")#,
               #s2("Gene Kiley")
               ) %>%
  mutate(., s_num = 2, ep_num = 32)

s2e33 <- rbind(s2("Dale Kerstetter", 2, 33),
               s2("The Brother of Sylvia Wemhoff"),
               s2("Marfa Lights"),
               s2("Jay Cook and Tanya Van Cuylenborg"),
               s2("Bonnie Wilder"),
               s2("Julie Weflen"),
               s2("Stefanie Stroh"),
               s2("Kyle Clinkscales"),
               s2("Carlos Alvarez"),
               s2("Jose Alvarez and Juan Cristo"),
               s2("Diana Braungardt"),
               s2("David Tyll and Brian Ognjan"),
               s2("John Simmons"),
               s2("Lily Mae Huff"),
               s2("David Thies"),
               s2("Susan Cappel"))%>%
  mutate(., s_num = 2, ep_num = 33)

s2e34 <- rbind(s2("Chevy Chase Bandit", 2, 34),
               s2("Louis Carlucci"),
               s2("Rudolf Hess"),
               s2("Patricia Meehan"),
               s2("Tina Jefferson"),
               s2("Gary and Terry Magno"),
               s2("Billie and Joey Rogers"))%>%
  mutate(., s_num = 2, ep_num = 34)


s2e35 <- lapply(c("Doyle Wheeler","Salvatore Caruana",
         "Douglas Alan Costa", "Lewis Michael Geiger", 
         "Lavada Floyd", "Richard Green", "Richard Joseph Alvarado", 
         "William McGeehee", "Alvaro Zapata", 
         "Charles Wickman", "Rachael Runyan", "Gene Flannes"), 
       s2, 
       2, 35) %>%
  rbindlist()



s2e36 <- lapply(c("Rhonda Hinson", "William Eugene Hilliard", 
         "Ann Corricelli and Lena Marie Wilson", 
         "Heirs of Howard Drummond", "Don Hamilton"), 
       s2, 
       2, 36) %>%
  rbindlist()

s2e37 <- lapply(c("Sonny Liston", "Billie and Joey Rogers", 
         "Bill and Cynthia Zelinski", 
         "Elizabeth Campbell", "Crash and Dash Robberies"), 
       s2, 
       2, 37) %>%
  rbindlist()

s2e38 <- lapply(c("Maria Armstrong", "Robert Dennie", "The Friend of Stephan Ross", 
         "Ralph Sigler", "Charles Wilson Chester", 
         "Tandem Bandits", "Medusa Bandit", "Garbage Bag Bandit", 
         "Shorts Robber"), 
       s2, 
       2, 38) %>%
  rbindlist()

s2e39 <- lapply(c("John Hawkins", "Woody Kelly", "David Fisher", "Laura Burbank", 
         "Carla Wright", "Michella Welch", "Jenny Bastian", 
         "The Siblings of LeeAnn Robinson", "Leslie Isben Rogge"), 
       s2, 
       2, 39) %>%
  rbindlist()

s2e40 <- lapply(c("Ed Barbara", "Avery Norris", "Rae Ann Mossor", "The Children of Georgia Tann", 
         "Donald Eugene Webb"), 
       s2, 
       2, 40) %>%
  rbindlist()

s2e41 <- lapply(c("The Hatbox Baby",
         "John Branion",
         "Donna Branion",
         "Alberta Elaine"), 
       s2, 
       2, 41) %>%
  rbindlist()

s2e42 <- lapply(c("Cynthia Anderson",
         "Ann Corricelli and Lena Marie Wilson",
         "West End Baptist Church",
         "Melvine Aprile",
         "Tony and Sheri Aprile",
        "The Signal Mountain Murders",
         "Wardell Ford",
        "Larry Chism"), 
       s2, 
       2, 42) %>%
  rbindlist()

s2e43 <- lapply(c("George Conniff",
         "Liz Carmichael",
         "Jack Brown" ,
         "Blinking Crucifix" ,
         "William Eugene Hillard" ), 
       s2, 
       2, 43) %>%
  rbindlist()

s2e44 <- lapply(c("Brayman Road Attacker",
         "Ray Hickingbotham",
         "Ann Sigmin and Garey Goff",
         "Kevin Hughes",
         "Robert Miller"), 
       s2, 
       2, 44) %>%
  rbindlist()

s2e45 <- lapply(c("Mark Groezinger",
         "Roswell Crash",
         "Tara Calico", 
         "Michael Henley",
         "Leticia Hernandez",
         "Jessica Gutierrez",
         "David Borer",
         "Malakia Logan"), 
       s2, 
       2, 45) %>%
  rbindlist()


s2e46 <- lapply(c("United Kingdom Crop Circles",
             "Kenneth Dungee",
             "Bill and Cynthia Zelinski",
            "Keith Reinhard",
             "Tom Young",
             "William Slagle",
             "Rafael Rodriguez"), 
       s2, 
       2, 46) %>%
  rbindlist()

s2e47 <- lapply(c("Charles Morgan",
             "Ogopogo",
             "The Siblings of LeeAnn Robinson",
             "Marvin and Sandra Maple",
             "Kristi and Bobby Baskin"), 
       s2, 
       2, 47) %>%
  rbindlist()

s2e48 <- lapply(c("Robert Weeks",
             "Steven Cox",
             "John Mooney",
             "Victorio Peak Treasure",
             "John Burns",
             "David Davis",
             "Shannon Davis",
             "Billie and Joey Rogers",
             "Lt. Karen Stephens",
             "The Family of Pat Mealbach",
             "Jenny Pratt"), 
       s2, 
       2, 48) %>%
  rbindlist()

s2e49 <- lapply(c("Georgia Rudolph",
             "Steve Sandlin",
             "John \"Thumper\" Brown"), 
       s2, 
       2, 49) %>%
  rbindlist()

s2e50 <- lapply(c("Joe Owens",
             "The Nanny of Jackie Cooper",
             "Alejandro Espinosa",
             "Dale Hyde",
             "Marvin and Sandra Maple",
             "Kristi and Bobby Baskin"), 
       s2, 
       2, 50) %>%
  rbindlist()

s2e51 <- lapply(c("Linda Sharp",
         "David Fisher",
         "Laura Burbank",
         "Gertrude Pruett",
         "Dan Short",
         "William L. Toomey"), 
       s2, 
       2, 51) %>%
  rbindlist()

s2e52 <- lapply(c("Laurence Harding Jr.",
         "Pedro Uribe",
         "Hugo Balbin",
         "Luis and Ivan Arango",
         "Miguel Villegas",
         "Elmer Locker Jr.",
         'John and Cecilia Kealing',
         "Sweetheart Swindler",
         "Gainesville Killers",
         "Fumbles"), 
       s2, 
       2, 52) %>%
  rbindlist()

s2e53 <- lapply(c("John \"Thumper\" Brown",
             "Kristina Smith",
             "Kay Hall",
             "New York Coin Scam",
             "Mabel Woods"), 
       s2, 
       2, 53) %>%
  rbindlist()

s2e54 <- lapply(c("Teresita Basa",
             "Linda Sharp",
             "Dewey Demetro" ,
             "Leo Johnson Jr.",
             "Jean Anne Freeman",
             "Las Cruces Bowling Alley Massacre"), 
       s2, 
       2, 54) %>%
  rbindlist()

s2e55 <- lapply(c("Mark S. Newman and Gerald I. Levy",
         "Donald M. and Louis G. Keith", 
         "Lavona and Lavelda Rowe-Richardson",
         "Elmer Locker Jr.",
         "John and Cecilia Kealing",
         "Jesus Penalver",
         "Julio Marco Cruz",
         "Pedro Nyego",
         "Blas Canedo",
         "Pablo Rodriguez",
         "Boston Rapist",
         "Bannack Treasure"), 
       s2, 
       2, 55) %>%
  rbindlist()

s2e56 <- lapply(c("Robert Kennedy",
             "Jackie Harrington", 
             "Elizabeth Campbell",
             "Avery Norris"), 
       s2, 
       2, 56) %>%
  rbindlist()

s2e57 <- lapply(c("Ralph Probst",
             "Christina Smith",
             "Dale Hyde",
             "Fumbles",
             "Crash and Dash Robberies"), 
       s2, 
       2, 57) %>%
  rbindlist()

s2e58 <- lapply(c("Marlena Childress",
             "Sonny Liston",
             "Gertrude Pruett",
             "Jean Anne Freeman"), 
       s2, 
       2, 58) %>%
  rbindlist()

s2e59 <- lapply(c("Gretchen Burford",
         "United Kingdom Crop Circles",
         "Robert Miller"), 
       s2, 
       2, 59) %>%
  rbindlist()

season2 <- NULL
s2.vars <- ls(pattern = "^s2e\\d{1,2}$")
for(i in s2.vars){
  season2 <- rbind(season2, 
                   get(i))
}

season2 <- season2 %>%
  left_join(., 
            cw.outcome_solved) %>%
  as_tibble()



# Season Summaries----
library(readr)

season1 <- NULL
s1.vars <- ls(pattern = "^s1ep\\d{1,2}$")
for(i in s1.vars){
  season1 <- rbind(season1, 
                   get(i))
}

rbind(season1, season2)


season1 <- season1 %>%
  as_tibble()




season1$uid_ep  <- paste("S", 
                         unlist(lapply(season1$s_num, lzero, 2)), 
                         "E", 
                         unlist(lapply(season1$ep_num, lzero, 3)), 
                         sep = "")

# sha512 seg_name to get uid for seg
season1$uid_seg <- sha512(x = season1$seg_name) %>%
  as.character() %>%
  substr(., 0, 12)


season2$uid_ep  <- paste("S", 
                         unlist(lapply(season2$s_num, lzero, 2)), 
                         "E", 
                         unlist(lapply(season2$ep_num, lzero, 3)), 
                         sep = "")

# sha512 seg_name to get uid for seg
season2$uid_seg <- sha512(x = season2$seg_name) %>%
  as.character() %>%
  substr(., 0, 12)


# letter A----
ltr.a <- read_lines("Michelle Abraham
               John Addis
               Tony Alamo
               James and Lisa Albert
               DeFallah Al-Salem
               Donald Alexander
               Paul Xavier Alexander
               Michael Alfonso
               Richard Joseph Alvarado
               Jose Alvarez and Juan Cristo
               David Alex Alvarez
               Adolph Altuve
               Dr. John Anderson
               Stephen Anderson
               Frank_Morris_and_the_Anglin_Brothers
               Anthrax Killer
               Melvine Aprile
               Luis and Ivan Arango
               Garth Arathorne
               Erik Arceneaux
               Robert Arcieri
               Maria Armstrong
               Omar Arroyo
               Atlanta/Spokane Bomber") %>% 
  trimws 

ltr.b <- read_lines("Hugo Balbin
    The Bald Mountain Shooters
    Ed Barbara
    Richard Bare
    Jerome Alan Bargo
    Swoop And Squat2
    Gregory Barker
    Dorothy Barnett
    Tim Barry
    Ronald Bax
    Lance Bedgood
    Edward Bell
    Michael Benka
    Carlos Berdeja
    Bicycle Bandit
    Bike Path Rapist
    The Bird Road Rapist
    Brad Bishop
    Reuben Blackwell Sr.
    The Black Widow
    Jose Blandon
    Daniel Blaszczuk
    John Blauvelt
    Blind River Killer
    Richard Bocklage
    Charles Warren Boomer
    Boston Rapist
    Florian Bourch
    Jerry Lee Bowen
    Boynton Beach Robber
    Brayman Road Attacker
    Brazos River Attackers
    Philip Breen
    Rickey Bright
    Brian Brophil
    John \"Thumper\" Brown
    Marshall Lee Brown
    Michael Wayne Brown
    Chuck Bucrzinski
    Francis C. Buhay
    Whitey Bulger
    John Burns
    Jim Burnside
    Burrowing Burglars") %>% 
  trimws 

ltr.c <- read_lines("Ronald Cains
    Raphael Camarena
    Blas Canedo
    Gregory Caplinger
    Ricardo Caputo
    Ricardo Villanueva Cordova
    James and Dorothy Carlson
    Louis Carlucci
    Liz Carmichael
    Salvatore Caruana
    Antonio Castro
    Richard Cepulonis
    Antonio Chavez
    Charles Wilson Chester
    Chevy Chase Bandit
    Foong Chin
    Larry Chism
    Church Arsonist
    Richard Church
    Leo Cisneros
    Circleville Writer
    Mike Cline
    David Coleman
    Joseph Collins
    Timothy Combs
    Richard Condia
    Connecticut River Killer
    Terry Lee Conner and Joseph Daugherty
    Andy Cook
    D.B. Cooper
    William Cooper
    Felipe de Jesus Corona-Verbera
    Robert Corrado
    Ann Corricelli and Lena Marie Wilson
    Jorge Cortez
    The Countess
    Cowboy Bandit
    Steven Cox
    Crash and Dash Robberies
    Crazy Glue Bandit
    Matthew Crome
    Julio Marco Cruz") %>% trimws

ltr.dg <- read_lines("Dark Dante
    Astarte Davis
    David Davis
    Don Davis Jr.
    Tracy Davis
    Nelson DeCloud
    Tony DeCompo
    Regina and Margaret Defrancisco
    Juan_Gil_Ferrufino,_Mario_Portillo_and_German_DeLeon
    Dewey Demetro
    Robert Dennie
    Carl And Mary Denny
    Ronald Denslow
    Reggie DePalma
    Dennis DePue
    Randolph Dial
    John Anthony Diaz
    Ken Dickerson
    Rolex Robbers
    Thomas David Dixon
    Tom Dixon
    Kevin Dominic
    Bob Dozier and John Russell
    Travis Wade Duncan
    Catherine and James Durkin
    Original Night Stalker
    Carl Alfred Eder
    Edmonton Rapist
    John Addis
    Ira Einhorn
    Sami Eisbart
    Adam Emery
    Epes Bandits
    Eduwigis Escalante
    Michael Eschweiler
    David Estright
    Pat Fagan
    Judge John Fairbanks
    Sergio Farina
    Pat Farmer
    John William Farr
    Rita Faulkner
    Jeanette Federico
    John Feiga
    Betty Field
    Emma Figueroa
    Kelly Finnegan
    David Fisher
    Robert Fisher
    William Fischer
    Robert Fisher
    Gene Flannes
    Albert Leon Fletcher
    Francisco Flores
    Lavada Floyd
    Richard Ford
    Wardell Ford
    Trent Fouts
    Dr. Kenneth Frank
    Arthur Frankford
    Margo Freshwater
    David Freeman
    Robert Fritch
    Elizabeth_Ortiz
    Marvin Gabrion
    Mark Gagliardo
    Jean Marie Gagnon
    Gainesville Killers
    Sweetheart_Swindler
    Garbage Bag Bandit
    Armando Garcia
    Carlos Garcia
    Joseph Gardner
    David Gause
    Thomas Geers
    Larry George
    Jerry Gervasoni
    Victor Gerena
    Christian Karl Gerhartsreiter
    Samuel Glover and Marshall Kirkpatrick
    Hari Gobin
    Ann Sigmin
    Alan Golder
    Andolina Gonzalez
    Jose Gonzalez
    Neil and Terry Gott
    Grandpa Bandit
    Richard Green
    Green River Killer
    Catherine Greig
    Christopher Griffin
    Malaika Griffith
    Grocery Robbers
    Sal Guardado
    Nova Guthrie") %>% trimws

ltr.hm <- read_lines("Steve Hadley
    Michael Hansen
    Rejean Hardy
    Hugh Harlin
    John Hawkins
    Hazel Head
    Connie Jean Helton
    Maria Hernandez
    Luis Herrera
    Thomas_Hickey_and_William_McCarthy
    William Eugene Hillard
    Julius_Patterson_and_Paulette_Hite
    Cheryl Holland
    Jesse James Hollywood
    Mahfuz Huq
    Joseph Hutchinson
    Dale Hyde
    Inner City Church Fire
    Interstate 70 Killer
    John Irwin
    David Ivey
    Juan Jackson
    Jockey Bandit
    John Grundhofer
    Tom Johnson
    Elvin Jones
    William Jordan
    Kachimba
    The Unabomber
    Ahmed Kandil
    Kansas City Arsonist
    John and Cecilia Kealing
    Woody Kelly
    David Kemp
    Jonathan Kern
    Edgar Kerns
    Eric Kessler
    Rabia Khalid
    Ann Kibalo
    James Kilgore
    James Donald King
    Deroy King, Jr
    Shannon Kinne
    Butch Knight
    William Korioth
    Leo Koury
    Joseph Krantz
    Jorge Landeros
    Maria Socorro De Rodriguez LaPine
    Michael Lassen
    Laura Law
    Lesa Lee
    Robert Leeds
    Jimmy Ray LeGate
    Desiree Lingo-Perkins
    Robert Litchfield
    Elmer Locker Jr.
    Arthur Lopez Jr.
    Mario Lozano
    John Lutter
    Lionel Luviano
    David MacLeod
    Jon and Molly Maggio
    Gary Magno
    Joe Maloney
    Brooker Maltais
    Joseph Mancini
    Marvin and Sandra Maple
    Manwell Marino
    Melody Martin
    Regina Martinez
    Jason_McVean_and_Alan_Pilon
    Edgar Maynard
    Melvin Edward Mays
    Thomas_Hickey_and_William_McCarthy
    Kenneth McDuff
    William McGeehee
    Kelly Lee McGinnis
    Michael McGuffey
    Raymond McLeod
    Richard McNair
    Carl McWilliams
    Medusa Bandit
    Jorge_Mendez_and_Jose_Rios
    Augustin Mendoza
    J. D. Method
    Miami Robber
    Milk Carton Bandit
    Benny Franklin Miller
    Swoop And Squat1
    Larry Dennis Miller
    Richard Mimms
    Minnesota Brinks Heist
    Mishawaka Rapist
    Mark Mitchell
    Patrick Michael Mitchell
    Michael Mohan
    Levia Molinari
    Frank Montenegro
    Liza Montgomery
    Manny Moreno
    Joe Morrow
    Jerry Moss
    Lyle Moody
    John Mooney
    Phillip Anthony Moore
    Miguel Morales
    Frank Morris and the Anglin Brothers
    Todd Mueller
    Charles Mulet
    Larry Munroe") %>% trimws

ltr.ns <- read_lines("Nashville-Cincinnati Serial Killer
    Nassau County Robber
    Mr. Nasty
    Al_Tom_and_Ricky_Nelson
    Gary and Ted Noble
    Thomas Noss Jr.
    Lissette Nukida
    Pedro Nyego
    Stanley Obas
    Luis Ochoa
    Ohio Prostitute Killer
    Oklahoma City Convenience Store Robbers
    Steven Oliver
    Ray Olsen
    David O'Neil
    Original Night Stalker
    Elizabeth Ortiz
    Joe Owens
    Nasario Palacios
    Eugene Palmer
    Wade Mitchell Parker
    John Paul
    Karen Pelletiere
    Jesus Penalver
    Pennsylvania Bank Robber
    Juan Carlos Pereira
    Jimmie Wayne Pierce
    Danny Piñeda
    Ramon_Reyes_and_Louie_Velarde
    Derek Leonard Reynolds
    Pizza Restaurant Robbers
    Pizza Parlor Killer
    Mary Poppins Bandit
    Kathy Power
    Ora Prince
    Joseph Prushinowski
    Desiree Perkins
    Gertrude Pruitt
    Pedro Pumajero
    Luie Quezada
    Jack Quinn
    Paul Ragusa
    Roberto Ramirez
    Red Dye Robber
    Michael_St._Clair_and_Dennis_Reese
    Karl Rehberg
    Richard Relf
    David Rhodes
    Mike Riemer
    Filiberto Ojeda Rios
    Bill Roberts
    Susan Roberts
    Rochester Car Heist
    Joseph Rodia
    David Rodriguez
    Edwin Rodriguez
    Pablo Rodriguez
    Rafael Rodriguez
    Leslie Ibsen Rogge
    John Roubas
    Route 22 Killer
    Route 29 Stalker
    Jose Rubio
    Eric Rudolph
    Frederick Russell
    Ron Rushton
    Sagebrush Rebellion
    Gloria Schulze
    Raymond Scoville
    Seattle Arsonist
    Melissa Jo Sermons
    Sharon Rogers Car Bomber
    Joe Shepard
    Shopping Bag Bandit
    Shotgun Bandit
    Michael Short
    Shorts Robber
    The_Signal_Mountain_Murders
    Art Silva
    Dr. Arvind Sinha
    Billy Ray Sisson
    Robin_Stevens_and_Sherry_Seymour
    William Slagle
    Ernest Small
    Chuck Smith
    David Gordon Smith
    Dennis Keith Smith
    Joe Smith
    Lena Regina Smith
    Leticia Smith
    Shannon Smith
    Alan Verl Sneed
    Roberto Solis
    Amtrak Derailment
    Southern California Robber
    Elena Souza
    Salvatore Spinnato
    Stahl Paintings
    Paul Stamper
    Kenneth Stanton
    Michael Anthony Starr
    Rose_Turford_and_Carolyn_Stevens
    Nicholas Louis Stevens
    Jerry Strickland
    Strip Mall Rapist
    Phyllis Strub
    Stryder Styarfyr
    James Sullivan
    Michael Swango
    Sweetheart Swindler") %>% 
  trimws


season_A <- NULL
for(i in c(ltr.a, ltr.b, ltr.c, 
           ltr.dg, 
           ltr.hm, ltr.ns)){
  temp <- s2(i) %>% 
    left_join(., cw.outcome_solved)
  
  season_A <- rbind(season_A, 
                   temp)
  rm(temp)
}
print(i)


# write_data----

readr::write_csv(season1, "season1.csv")
readr::write_csv(season2, "season2.csv")
readr::write_csv(season_A, "composite.data.csv")

season_A <- read_csv("composite.data.csv")



season_A %>%
  group_by(seg_name, outcome = master.outcome) %>%
  summarise(n_ep = n_distinct(uid_ep)) %>%
  group_by(outcome) %>%
  summarise(n_segments = n_distinct(seg_name), 
            avg_times_shown = mean(n_ep)) 



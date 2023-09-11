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



# Season Summaries----
library(readr)

season1 <- NULL
s1.vars <- ls(pattern = "^s1ep\\d{1,2}$")
for(i in s1.vars){
  season1 <- rbind(season1, 
                   get(i))
}

season1 <- season1 %>%
  as_tibble()

season1$tag[is.na(season1$tag_type)] %>% 
  unique() %>%
  sort()

season1$tag_type <- NA


season1$tag_type[season1$tag %in% 
                   c("Officer-Involved Cases", "Police-Involved Cases",
                     "Military-Related Cases", "Conspiracy Cases")] <- "authority"

season1$tag_type[season1$tag %in% 
                   c("Suspicious Deaths","Disappearances","Missing Spouse Cases","Wrongly-Accused Cases" )] <- "victim"
season1$tag_type[season1$tag %in% 
                   c("Serial Killer","Escape","Wanted" )] <- "criminal"

season1$tag_type[season1$tag %in% 
                   c("Adoption Cases", "Lost Loves" ,"Amnesia",
                     "Lost Identity Cases","Inheritance")] <- "reconnections"

season1$tag_type[season1$tag %in% 
                   c("ESP Cases","Unusual Phenomenon Cases","Cryptozoology" ,
                     "Ghosts","Mysterious Creatures","UFOs" )] <- "other_worldly"
season1$tag_type[season1$tag %in% 
                   c("Miracles","Church-Related Cases")] <- "spiritual"

season1$tag_type[season1$tag %in% 
                   c("Legends"       ,       "Treasure")] <- "tall_tales"

season1$tag_type[season1$tag %in% 
                   c("Abduction","Armed Robbery","Bigamy","Forgery","Harassment",
                     "Robbery","Theft","Vehicular Manslaughter","Arson",
                     "Fraud","Burglary","Counterfeiting", "Extortion" ,
                     "Manslaughter","Rape" ,"Attempted Murder","Child Molestation",
                     "Embezzlement","Murder","Animal Cruelty Cases")] <- "crime"

season1$tag_type[season1$tag %in% 
                   c("Mafia-Related Cases" ,"Fire-Related Cases",
                     "Blood Print-Related Cases",
                     "Cult-Related Cases","Party-Related Cases","School-Related Cases",
                     "Medical-Related Cases","Music-Related Cases",
                     "Drug-Related Cases","Gang-Related Cases" ,
                     "Radio-Related Cases","ATM-Related Cases",
                     "Restaurant-Related Cases" )] <- "cat_crime"

season1$tag_type[season1$tag %in% 
                   c("Bicycle-Related Cases", 
                     "Horse-Related Cases", 
                     "Ford-Related Cases", 
                     "Toyota-Related Cases",
                     "Dodge-Related Cases",
                     "Motorcycle-Related Cases", 
                     "Train-Related Cases", 
                     "Van-Related Cases" )] <- "vehicle"

season1$tag_type[season1$tag %in% 
                   c("Netflix", "Court TV Cases", 
                     "Lifetime Cases", "Unsolved History Cases", 
                     "AMW Cases")] <- "viewing_platform"

season1$tag_type[season1$tag %in% 
                   c("Unresolved", "Captured", "Solved", 
                     "Unsolved", "Viewer Solves")] <- "outcome"
season1$tag_type[season1$tag %in% c(state.name, "England", "Washington D.C.", 
                                    "Pacific Ocean", "Atlantic Ocean", 
                                    "Yukon Territory", "Mars", "France", "Austria", 
                                    "Germany", "Hungary", "Ontario")] <- "geographic_location"

season1$tag_type[season1$tag %in% c("River-Related Cases", 
                                    "Sea-Related Cases", 
                                    "Air-Related Cases", 
                                    "Lake-Related Cases", 
                                    "Railroad-Related Cases", 
                                    "Road-Related Cases", 
                                    "Woodland-Related Cases")] <- "terrain_type"

season1$tag_type[is.na(season1$tag_type) & 
                   grepl("\\d{4,4}|Unknown Year", season1$tag)] <- "year"


# tidy

# season1$s_num2 <-  unlist(lapply(season1$s_num, lzero, 2))
# season1$ep_num2 <- unlist(lapply(season1$ep_num, lzero, 3))
season1$uid_ep  <- paste("S", 
                         unlist(lapply(season1$s_num, lzero, 2)), 
                         "E", 
                         unlist(lapply(season1$ep_num, lzero, 3)), 
                         sep = "")
# sha512 seg_name to get uid for seg
season1$uid_seg <- sha512(x = season1$seg_name) %>%
  as.character() %>%
  substr(., 0, 12)


season1 %>%
  group_by(seg_name, outcome = master.outcome) %>%
  summarise(n_ep = n_distinct(uid_ep)) %>%
  group_by(outcome) %>%
  summarise(n_segments = n_distinct(seg_name), 
            avg_times_shown = mean(n_ep)) 

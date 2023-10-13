library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidycensus)
library(tigris)
library(data.table)
library(openssl)
library(BAMMtools)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()

hundo.pct.tag.cats <- c("year", "geo")

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
 

# cw_tags <- read_csv("https://raw.githubusercontent.com/benda18/Solved_Unsolved_Mysteries/main/data/cw_tags.csv")
# cw_tags <- cw_tags %>%
#   .[complete.cases(.),] %>%
#   .[!duplicated(.),]

# import data----

full.df <- read_csv("https://raw.githubusercontent.com/benda18/Solved_Unsolved_Mysteries/main/data/mysteries2.csv")
#full.df <- read_csv("mysteries2.csv")

# # tag analysis----
# tag_freq <- full.df$tag %>% 
#   table() %>%
#   as.data.frame() %>% 
#   as_tibble()
# colnames(tag_freq) <- c("tag", "Freq")
# 
# tag_freq <- tag_freq %>% 
#   .[!grepl("\\d{4,4}s{0,1}$|year", .$tag),] %>%
#   .[!.$tag %in% 
#       c(state.name, 
#         "Washington D.C.", "Puerto Rico", "Philippines", 
#         "Canada", "Russia", "Newfoundland", "Africa", "Brazil", 
#         "Iraq", "Indonesia", "Korea", "Japan", "Jordan", 
#         "United Kingdom", "Peru", "Antarctica", 
#         "Australia", "Portugal", "Yukon Territory", 
#         "Egypt", "Guam", "Bosnia", "Ontario", 
#         "Italy", "Belgium", "Colombia", "Unknown State", 
#         "Haiti", "Mexico", "Finland", "Spain", 
#         "Bolivia", "China", "Turkey", "Caribbean", 
#         "The Netherlands", "Sweden", "Manitoba", 
#         "Tibet", "Quebec", "Greece", "Switzerland", 
#         "Scotland", "Ireland", "Yugoslavia", "Alberta", 
#         "British Columbia", "France", "Austria", "Pacific Ocean", 
#         "England", "Vietnam", "Germany", "Atlantic Ocean", 
#         "Hungary", "Cuba"),] %>%
#   .[!grepl("^Horse-|^Ford-|^Dodge-|^Semi-|^Train-|^SUV-|^Van-|^Motorcycle|^Volkswagon|^Toyota|^Honda|^Jeep|^ATV-|^Bicycle", 
#                 .$tag),] %>%
#   .[order(.$Freq,decreasing = T),] %>%
#   ungroup() %>%
#   slice_head(., prop = 0.5)
# 
# tag_freq$tag_f <- factor(tag_freq$tag, 
#                          levels = unique(tag_freq$tag[order(tag_freq$Freq)])) 
# 
# ggplot() + 
#   geom_col(data = tag_freq, 
#              aes(x = Freq, y = tag_f))


# again
cw.tags <- data.frame(tag = unique(full.df$tag), 
                      tag_type = NA) %>% as_tibble()

cw.case_types <- data.frame(case_type = c("wanted", 
                                          "lost", 
                                          "murders", 
                                          "mysteries", 
                                          "legends", 
                                          "missing"))

# search tags
cw.tags$tag %>%
  grep(" Cases$", ., ignore.case = F, value = T)

# na tags
cw.tags$tag[is.na(cw.tags$tag_type)]  %>% sort()

# institution

# profession



# cross-series-coverage
cw.tags$tag_type[cw.tags$tag %in%
                   c("AMW Cases", 
                     "Court TV Cases")] <- "tv_crossover"

# crime
cw.tags[cw.tags$tag %in% 
          c("accident",
            "Arson", "Child Abuse", "Fraud", 
            "Murder", 
            "Theft", "Abduction", "Attempted Murder", 
            "Child Molestation", "Extortion", "Piracy", 
            "Rape", "Smuggling", "Bribery", "Embezzlement", 
            "Manslaughter", "Armed Robbery", 
            "Bigamy", "Burglary", "Counterfeiting", 
            "Forgery", "Harassment", "Possession", "Robbery", 
            "Terrorism", 
            "Animal Cruelty Cases", 
            "Conspiracy Cases", "Fire-Related Cases", 
            "Mafia-Related Cases","Vehicular Manslaughter"),]$tag_type <- "crime_type"


# criminal / victim

# Uplifting / Victimless


# vehicle type
cw.tags[grepl("^Horse-|^Ford-|^Dodge-|^Semi-|^Train-|^SUV-|^Van-|^Motorcycle|^Volkswagon|^Toyota|^Honda|^Jeep|^ATV-|^Bicycle", 
              cw.tags$tag),]$tag_type <- "vehicle_type"


# outcomes----
cw.tags[cw.tags$tag %in% 
          c("Solved", 
            "Unsolved", 
            "Unresolved", 
            "Captured",
            "Viewer Solves", 
            "Wanted"),]$tag_type <- "outcomes"

# building / land-use type
cw.tags[cw.tags$tag %in% 
          c("ATM-Related Cases", 
            "Restaurant-Related Cases", "School-Related Cases",
            "Church-Related Cases", "Hotel-Related Cases" , 
            "Lake-Related Cases", "Railroad-Related Cases", 
            "River-Related Cases", "Sea-Related Cases", 
            "Woodland-Related Cases","Air-Related Cases",
            "Road-Related Cases"),]$tag_type <- "building_type.land_use"

# not needed
cw.tags[cw.tags$tag %in%
          c("Special Bulletin Cases", 
            "Unknown Airdate" ,
            "Unsolved History Cases",
            #"Court TV Cases", 
            "YouTube Album",
            #"AMW Cases",
            "Investigators","Special Alert Cases"   ) |
          grepl("needing|pages|0026E Cases|Netflix|spike tv cases|cbs cases|lifetime cases", 
                cw.tags$tag, ignore.case = T),]$tag_type <- "not_needed"

# years
cw.tags[grepl(pattern = "\\d{4,4}s{0,1}$|year", 
              x = cw.tags$tag, ignore.case = T),]$tag_type <- "year"

# states
cw.tags[cw.tags$tag %in% 
          c(state.name, "Mars",
            "Washington D.C.", "Puerto Rico", 
            "Philippines", "Canada", "Russia", 
            "Newfoundland", "Africa", "Brazil", 
            "Iraq", "Indonesia", "Korea", "Japan", "Jordan", 
            "United Kingdom", "Peru", "Antarctica", 
            "Australia", "Portugal", "Yukon Territory", 
            "Egypt", "Guam", "Bosnia", "Ontario", 
            "Italy", "Belgium", "Colombia", "Unknown State", 
            "Haiti", "Mexico", "Finland", "Spain", 
            "Bolivia", "China", "Turkey", "Caribbean", 
            "The Netherlands", "Sweden", "Manitoba", 
            "Tibet", "Quebec", "Greece", "Switzerland", 
            "Scotland", "Ireland", "Yugoslavia", "Alberta", 
            "British Columbia", "France", "Austria", "Pacific Ocean", 
            "England", "Vietnam", "Germany", "Atlantic Ocean", 
            "Hungary", "Cuba"),]$tag_type <- "state_country"



# find ones that add up to 100
cw.tags$tag_type %>% unique()


some.segs <- left_join(full.df, cw.tags) %>%
  group_by(seg_name, 
           #tag_type,
           tag_type_geo = tag_type == "building_type.land_use") %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(., 
         tag_type_geo = ifelse(is.na(tag_type_geo), 
                               F, tag_type_geo)) %>%
  group_by(seg_name, tag_type_geo) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(., 
         tag_type_type = ifelse(tag_type_geo, "with_tag", "without_tag")) %>%
  as.data.table() %>%
  dcast(., 
        seg_name ~ tag_type_type, 
        fun.aggregate = sum, 
        value.var = "n") %>%
  as.data.frame() %>%
  as_tibble() %>%
  .[.$with_tag < 1,] %>%
  .$seg_name

some.segs


# plot all vars----
all.files_as.vars <- list.files(pattern = "\\.csv") %>%
  gsub("\\.csv", "", .) 
loaded.files_as.vars <- ls()

load_these_files <- all.files_as.vars[!all.files_as.vars %in%
                                        loaded.files_as.vars] %>%
  paste(., ".csv", sep = "") %>%
  .[!. %in% c("season1.csv", "season2.csv")]

for(i in load_these_files){
  temp.varname <- gsub("\\.csv$", "", i)
  # read data to temp
  temp.df      <- read_csv(i)
  # assign df to var
  assign(x = temp.varname, value = temp.df)
}
rm(temp.varname, temp.df, i)


cat('\f')
for(i in ls()){
  if(is.data.frame(get(i))){
    #try(plot(get(i)))
    print(((get(i))))
  }
}



cw_ep.airdates
cw_ep.id
cw_MASTER_crime
cw_MASTER_outcome


cw_MASTER_tags
cw_MASTER_year
cw_s.ep.seg_name
cw_seg.name.casetype
cw_tags
cw.tags
cw.case_types
cw.outcome_solved


# graph  seg_name and seg_url ignore.case = T

library(igraph)
rbind(mutate(cw_MASTER_outcome[,c("seg_name", "seg_url")], 
             seg_name = tolower(seg_name)),
      cw_MASTER_outcome[,c("seg_name", "seg_url")]) %>%
  .[complete.cases(.),] %>%
  group_by(seg_url) %>%
  summarise(n_sn = n_distinct(seg_name)) %>%
  .[.$n_sn == 1,]
print("if the calc above ^^^ is zero rows, then data is good")



# # other stuff----
# left_join(full.df, 
#           cw.tags) %>%
#   .[.$seg_name %in% some.segs,] %>%
#   .$tag %>% unique() %>% sort()
# 
# cw.tags %>%
#   group_by(tag,
#            any_geo = any(tag_type == "state_country")) %>% 
#   summarise()

# # summary
# summary.tags.tagtypes <- left_join(full.df, 
#           cw.tags) %>%
#   group_by(seg_name, seg.outcome, tag_type) %>%
#   summarise(n_tags = n_distinct(tag, na.rm = T)) %>%
#   as.data.table() %>%
#   dcast(., 
#         seg_name + seg.outcome ~ tag_type, 
#         fun.aggregate = sum, value.var = "n_tags", 
#         fill = 0) %>%
#   as.data.frame() %>%
#   as_tibble()
# 
# summary.tags.tagtypes %>%
#   as.data.table() %>%
#   melt(., 
#        id.vars = c("seg_name", "seg.outcome")) %>%
#   as.data.frame() %>%
#   as_tibble() %>%
#   group_by(variable) %>%
#   summarise(n = n(), 
#             t_val = sum(value)) %>%
#   ungroup() %>%
#   mutate(., 
#          pct_t = t_val / n)
# 
# # naural breaks - tags
# 
# df.jenks <- full.df %>%
#   group_by(tag) %>%
#   summarise(n_seg = n_distinct(seg_name)) %>%
#   .[order(.$n_seg,decreasing = T),]
# 
# df.jenks$tag_f <- factor(df.jenks$tag, 
#                          levels = unique(df.jenks$tag[order(df.jenks$n_seg,decreasing = T)])) 
# 
# 
# n.breaks <- 3
# 
# 
# ggplot() + 
#   geom_col(data = df.jenks[df.jenks$n_seg >=BAMMtools::getJenksBreaks(df.jenks$n_seg, n.breaks)[2],], 
#            aes(y = tag_f,x = n_seg)) +
#   geom_vline(data = data.frame(x.br =BAMMtools::getJenksBreaks(df.jenks$n_seg, 
#                                    n.breaks)), 
#                                aes(xintercept = x.br))
# 
# 
# 
# tag1 <- "1810s"
# tag1 <- "1925"
# 
# get_decade <- function(tag1){
#   # already a decade? 
#   if(grepl("s$", tag1, ignore.case=T)){
#     out <- substr(tag1,0,4) %>% as.numeric()
#   }else{
#     # convert to decade
#     out <- tag1 %>% 
#       as.numeric()
#     out <- out - (out %% 10)
#   }
#   return(out)
# }
# 
# full.df$decade <- full.df$tag %>% 
#   #unique() %>% sort() %>%
#   #grep("\\d{4,4}", ., value = T) %>%
#   #.[nchar(.) <= 6] %>%
#   lapply(., FUN = get_decade) %>% unlist()
# 
# full.df %>%
#   group_by(seg_name) %>%
#   summarise(n_decade = n_distinct(decade, na.rm = T), 
#             min_decade = min(decade,na.rm = T), 
#             max_decade = max(decade,na.rm = T)) %>%
#   .[order(.$n_decade,decreasing = T),] %>%
#   ungroup() %>%
#   mutate(., 
#          diff_decade = max_decade - min_decade)

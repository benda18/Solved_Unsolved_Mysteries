library(dplyr)
library(igraph)
library(readr)


setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()

# import data----

full.df <- read_csv("https://raw.githubusercontent.com/benda18/Solved_Unsolved_Mysteries/main/data/mysteries2.csv")



full_data <- data.frame(tbl_name = "full.df", 
                        colnames = colnames(full.df))

cw_tagtype <- data.frame(tbl_name = "cw_tag.type.df", 
                         colnames = c("tag", 
                                      "tag_type",
                                      #"year", 
                                       #"state.country", 
                                       #"outcome", 
                                       "etc"))

cw_outcomes <- data.frame(tbl_name = "cw_outcomes.df", 
                          colnames = c("seg_name", 
                                       "seg.outcome", 
                                       "outcome"))

# cross.validate <- data.frame(tbl_name = "<cross-validate>", 
#                              colnames = c("seg.outcome", 
#                                           "seg_name"))

cw_statecountry <- data.frame(tbl_name = "cw_statecountry.df", 
                              colnames = c("seg_name", 
                                           "state.country"))

cw_year <- data.frame(tbl_name = "cw_seg.tag_year.df", 
                      colnames = c("seg_name", 
                                   "tag_year", "tag_decade"))

graph_from_data_frame(rbind(full_data,
                            cw_tagtype,
                            cw_year,
                            cw_statecountry,
                            # cross.validate,
                            cw_outcomes)) %>% plot.igraph()


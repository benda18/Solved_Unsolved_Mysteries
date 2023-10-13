setwd("~/R/play/Solved_Unsolved_Mysteries")

library(dplyr)
library(ggplot2)
library(ggrepel)

rm(list=ls());cat('\f');gc()

wd <- list(R = "C:/Users/bende/Documents/R/play/Solved_Unsolved_Mysteries/R",
           home = "C:/Users/bende/Documents/R/play/Solved_Unsolved_Mysteries", 
           data = "C:/Users/bende/Documents/R/play/Solved_Unsolved_Mysteries/data", 
           shiny = "C:/Users/bende/Documents/R/play/Solved_Unsolved_Mysteries/shiny/unsolved_shiny")


# load all the data----
setwd(wd$data)

load("unsolved.RData")




# Plot Ideas----

# plot1: 


ggplot() +
  geom_text(data = some.solvedlabels, 
            hjust = -0.5,
            #min.segment.length = 0,
            aes(x = date_fa, y = seg_name, label = "S"), 
            fontface = "italic", size = 3,
            #direction = "x"
  )+
  geom_point(data = goaldf_points[goaldf_points$seg_name %in% some.segnames,], 
             aes(x = date_fa, y = seg_name)) + 
  geom_segment(data = goaldf_segs[goaldf_segs$seg_name %in% some.segnames,], 
               aes(x = min_date.fa, xend = max_date.fa, 
                   y = seg_name, yend = seg_name))+
  facet_grid(master.outcome~., scales = "free_y", space = "free_y")

## write csv files to .RData----
# csv_files <- list.files(pattern = "\\.csv")
# 
# for(i in csv_files){
#   temp.varname <- gsub("\\.csv$", "", i) 
#   temp.varname <- paste0("data__", temp.varname)
#   
#   assign(x = temp.varname, read_csv(i))
#   
# }
# rm(i,csv_files,temp.varname)
# 
# save(list=ls(pattern = "^data__"),
#      file="unsolved.RData")

# # load data----
# setwd(wd$data)
# load("unsolved.RData")
# 
# # assign attr_description to each data----
# attr(data__composite.data, "data_desc")       <- "DATA, Segment X master.outcome and seg.outcome with tags x s_num x ep_num x seg_uid"
# attr(data__cw.outcome_solved, "data_desc")    <- "CROSSWALK, master.outcome x seg.outcome"
# attr(data__cw_ep.airdates, "data_desc")       <- "CROSSWALK, season and episode by date episode first aired"
# attr(data__cw_ep.id, "data_desc")             <- "CROSSWALK, season and episode by eid"
# 
# attr(data__cw_MASTER_crime, "data_desc")      <- "CROSSWALK, seg_name x seg_url x crime"
# attr(data__cw_MASTER_outcome, "data_desc")    <- "CROSSWALK, seg_name x seg_url x outcomes(2)"
# attr(data__cw_MASTER_tags, "data_desc")       <- "CROSSWALK, tag x tag category"
# attr(data__cw_MASTER_year, "data_desc")       <- "CROSSWALK, seg_name x seg_url x year(s) crime committed"
# 
# attr(data__cw_s.ep.seg_name, "data_desc")     <- "CROSSWALK, seg_name x s_num x ep_num"
# attr(data__cw_seg.name.casetype, "data_desc") <- "CROSSWALK, seg_name x seg_casetype"
# attr(data__cw_tags, "data_desc")              <- "CROSSWALK, seg_url x tag (i.e. all tags associated with each segment"
# 
# attr(data__episode_info, "data_desc")         <- "DATA, episode info x segment info x title_card_category.sc_summary"
# attr(data__join_cases_episodes, "data_desc")  <- "DATA, see data__composite.data but without tags"
# 
# attr(data__master.meta, "data_desc")          <- "DATA, list of episodes by season by segment with seg_url"
# 
# attr(data__mysteries, "data_desc")            <- "DATA, master dataset with everything"
# attr(data__mysteries2, "data_desc")           <- "DUPLICATED, identical to data__mysteries"
# 
# attr(data__season1, "data_desc")              <- "DUPLICATED"
# attr(data__season2, "data_desc")              <- "DUPLICATED"
# 
# 
# data__mysteries2.UNU <- data__mysteries2
# rm(data__mysteries2)
# data__season1.UNU    <- data__season1
# rm(data__season1)
# data__season2.UNU    <- data__season2
# rm(data__season2)
# 
# 
# save(list=ls(pattern = "^data__"), 
#      file = "unsolved.RData")


# # attribute metadata dataset
# setwd(wd$data)
# load("unsolved.RData")
# 
# temp__attribute.metadata <- NULL
# for(i in ls(pattern = "^data__")){
#   temp__attribute.metadata <- rbind(temp__attribute.metadata, 
#                                     data.frame(varname = i, 
#                                                attr.data_desc = attr(get(i), which = "data_desc")))
#    #%>% attributes
# }
# 
# attr(temp__attribute.metadata, "data_desc") <- "DATA, master reference of colnames and attribute desc data for all the other datasets"
# 
# data__attribute.metadata <- temp__attribute.metadata
# rm(temp__attribute.metadata)
# 
# save(list = ls(pattern = "^data__"), 
#      file = "unsolved.Rdata")

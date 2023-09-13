library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidycensus)
library(tigris)
library(data.table)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()

# functions-----
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

# print crosswalks
lapply(ls(pattern = "^cw\\.|^cw_"), get)

# import join.cases
join_cases_episodes <- read_csv("join_cases_episodes.csv")

# import master.meta
master.meta <- read_csv("master.meta.csv")

# import episode data----
episodes.df <- read_csv("episode_info.csv") %>%
  mutate(., date_fa = ymd(date_fa))

# import case data----
cases.df <- read_csv("composite.data.csv")


# assign further metadata to cases from episodes and other collected data----

ls()

cases.df

episodes.df

cw_ep.airdates
cw_ep.id
cw_s.ep.seg_name
cw_seg.name.casetype



join_cases_episodes <- cases.df %>% 
  .[!colnames(.) %in% c("s_num", "ep_num", "tag")] %>%
  .[!duplicated(.),] %>%
  full_join(., 
            cw_s.ep.seg_name[!duplicated(cw_s.ep.seg_name),], 
            by = "seg_name") %>%
  mutate(., 
         uid_ep = paste("S", 
                        unlist(lapply(s_num, lzero, 2)), 
                        "E", 
                        unlist(lapply(ep_num, lzero, 3)), 
                        sep = "")) 

season1 <- read_csv("season1.csv")
season2 <- read_csv("season2.csv")



join_cases_episodes %>%
  group_by(seg_name) %>%
  summarise() %>%
  ungroup() %>%
  full_join(., 
            summarise(group_by(rbind(season1,season2),
                               seg_name, s_num, ep_num)), 
          by = c("seg_name")) %>%
  .[complete.cases(.),]

join_cases_episodes[,c("seg_name", "s_num", "ep_num")] %>%
  .[!duplicated(.),] %>%
  .[complete.cases(.),]


write_csv(join_cases_episodes, 
          file = "join_cases_episodes.csv")


# older----
# # post-processing
# cases.df$tag_type <- "other"
# cases.df[cases.df$tag %in% state.name,]$tag_type <- "state"
# cases.df[grepl("\\d{4,4}", cases.df$tag),]$tag_type <- "year"
# cases.df$tag_type[cases.df$seg.outcome == tolower(cases.df$tag)] <- "outcome"
# 
# 
# cases.df %>%
#   group_by(seg_name) %>%
#   summarise(n_seasons = n_distinct(s_num), 
#             n_episodes = n_distinct(ep_num)) 
# 
# cases.df %>%
#   group_by(master.outcome) %>%
#   summarise(n_segments = n_distinct(seg_name)) %>%
#   ungroup() %>%
#   mutate(., 
#          pct_segments = n_segments / sum(n_segments))
# 
#        exp.states <- cases.df %>%
#   group_by(tag_type, tag, master.outcome) %>%
#   summarise(n_segments=n_distinct(seg_name)) %>%
#   as.data.table() %>%
#   dcast(., tag_type+tag ~ master.outcome, 
#         fun.aggregate = sum, value.var = "n_segments") %>%
#   as.data.frame() %>%
#   mutate(., 
#          t_segs = SOLVED + UNSOLVED, 
#          pct_solved = SOLVED / t_segs) %>%
#   #.[order(.$t_segs, .$pct_solved, decreasing = T),] %>%
#   .[order(.$tag_type, .$SOLVED,decreasing = T),] %>%
#   .[.$tag_type == "state",] %>%
#   left_join(., 
#             data.frame(state = state.name, 
#                        state.abbr = state.abb),
#              by = c("tag" = "state")) %>%
#   as_tibble() 
# 
# pop.states <- tidycensus::get_estimates(geography = "state",
#                                         product = "population", 
#                                         year = 2022) %>%
#   .[.$variable == "POPESTIMATE",]
# 
# exp.states <- left_join(exp.states, 
#                         pop.states[,c("NAME", "value", "year")], 
#                         by = c("tag" = "NAME"))
# 
# colnames(exp.states)[colnames(exp.states) == "value"] <- "population"
# 
# exp.states
# 
# ggplot(data = exp.states, 
#        aes(x = population, 
#            y = t_segs)) + 
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_x_continuous(labels = scales::comma)+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# 
# overall.success <- cases.df %>%
#   group_by(master.outcome) %>%
#   summarise(n_segs = n_distinct(seg_name))
# overall.successrate <- overall.success$n_segs[overall.success$master.outcome == "SOLVED"] / 
#   sum(overall.success$n_segs)
# 
# 
# # use the beta distribution to estimate the likelihood that each state
# # has exceeded the overall success rate
# 
# 
# exp.states$o_success_rate <- exp.states$SOLVED / (exp.states$SOLVED + exp.states$UNSOLVED)
# exp.states$p_beat_overall <- NA
# 
# for(i in 1:nrow(exp.states)){
#   exp.states$p_beat_overall[i] <- integrate(f = function(p) {
#     dbeta(p, 
#           exp.states$SOLVED[i] + 1,
#           exp.states$UNSOLVED[i] + 1)
#   }, 
#   overall.successrate, 1)$value
# }
# 
# exp.states
# 
# ggplot() + 
#   geom_point(data = exp.states, 
#              aes(x = o_success_rate, 
#                  y = p_beat_overall)) +
#   # geom_smooth(data = exp.states, 
#   #            aes(x = o_success_rate, 
#   #                y = p_beat_overall), 
#   #            method = "loess") +
#   geom_function(fun = function(x) x, 
#                 aes(color = "curve of y = x"))+
#   geom_vline(aes(xintercept = overall.successrate, 
#                  color = "overall solved rate"), 
#              linetype = 2)+
#   scale_x_continuous(labels = scales::percent, 
#                      breaks = c(seq(0,1,by= 0.1),overall.successrate), 
#                      minor_breaks = seq(0,1,by=0.05))+
#   scale_y_continuous(labels = scales::percent, 
#                      breaks = seq(0,1,by=0.1), 
#                      minor_breaks = seq(0,1,by=0.05))
# 
# # example of beta distribution----
# # in 41 tries, 14 wins, 27 losses
# 
# args(dbeta)
# dbeta(x = seq(0,1,by=0.1), # vector of quantiles
#       shape1 = 14, 
#       shape2 = 27) %>% 
#   plot(x = seq(0,1,by=0.25), y = ., type = "b")
# 
# prop.table(table(dbeta(x = seq(0,1,by=0.01), # vector of quantiles
#       shape1 = 14, 
#       shape2 = 27) <= 0.5))
# 
# integrate(function(p) dbeta(p,14,27),0,0.5)
# 
# 
# 

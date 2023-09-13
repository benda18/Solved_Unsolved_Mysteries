library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidycensus)
library(tigris)
library(data.table)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()

# import data----

full.df <- NULL
# for(i in list.files(pattern = "^season\\d{1,}\\.csv$")){
#   full.df <- rbind(full.df, 
#                    read_csv(i))
# }

full.df <- read_csv("composite.data.csv")

full.df$tag_type <- "other"
full.df[full.df$tag %in% state.name,]$tag_type <- "state"
full.df[grepl("\\d{4,4}", full.df$tag),]$tag_type <- "year"
full.df$tag_type[full.df$seg.outcome == tolower(full.df$tag)] <- "outcome"


full.df %>%
  group_by(seg_name) %>%
  summarise(n_seasons = n_distinct(s_num), 
            n_episodes = n_distinct(ep_num)) 

full.df %>%
  group_by(master.outcome) %>%
  summarise(n_segments = n_distinct(seg_name)) %>%
  ungroup() %>%
  mutate(., 
         pct_segments = n_segments / sum(n_segments))

       exp.states <- full.df %>%
  group_by(tag_type, tag, master.outcome) %>%
  summarise(n_segments=n_distinct(seg_name)) %>%
  as.data.table() %>%
  dcast(., tag_type+tag ~ master.outcome, 
        fun.aggregate = sum, value.var = "n_segments") %>%
  as.data.frame() %>%
  mutate(., 
         t_segs = SOLVED + UNSOLVED, 
         pct_solved = SOLVED / t_segs) %>%
  #.[order(.$t_segs, .$pct_solved, decreasing = T),] %>%
  .[order(.$tag_type, .$SOLVED,decreasing = T),] %>%
  .[.$tag_type == "state",] %>%
  left_join(., 
            data.frame(state = state.name, 
                       state.abbr = state.abb),
             by = c("tag" = "state")) %>%
  as_tibble() 

pop.states <- tidycensus::get_estimates(geography = "state",
                                        product = "population", 
                                        year = 2022) %>%
  .[.$variable == "POPESTIMATE",]

exp.states <- left_join(exp.states, 
                        pop.states[,c("NAME", "value", "year")], 
                        by = c("tag" = "NAME"))

colnames(exp.states)[colnames(exp.states) == "value"] <- "population"

exp.states

ggplot(data = exp.states, 
       aes(x = population, 
           y = t_segs)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

overall.success <- full.df %>%
  group_by(master.outcome) %>%
  summarise(n_segs = n_distinct(seg_name))
overall.successrate <- overall.success$n_segs[overall.success$master.outcome == "SOLVED"] / 
  sum(overall.success$n_segs)


# use the beta distribution to estimate the likelihood that each state
# has exceeded the overall success rate


exp.states$o_success_rate <- exp.states$SOLVED / (exp.states$SOLVED + exp.states$UNSOLVED)
exp.states$p_beat_overall <- NA

for(i in 1:nrow(exp.states)){
  exp.states$p_beat_overall[i] <- integrate(f = function(p) {
    dbeta(p, 
          exp.states$SOLVED[i] + 1,
          exp.states$UNSOLVED[i] + 1)
  }, 
  overall.successrate, 1)$value
}

exp.states

ggplot() + 
  geom_point(data = exp.states, 
             aes(x = o_success_rate, 
                 y = p_beat_overall)) +
  # geom_smooth(data = exp.states, 
  #            aes(x = o_success_rate, 
  #                y = p_beat_overall), 
  #            method = "loess") +
  geom_function(fun = function(x) x, 
                aes(color = "curve of y = x"))+
  geom_vline(aes(xintercept = overall.successrate, 
                 color = "overall solved rate"), 
             linetype = 2)+
  scale_x_continuous(labels = scales::percent, 
                     breaks = c(seq(0,1,by= 0.1),overall.successrate), 
                     minor_breaks = seq(0,1,by=0.05))+
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0,1,by=0.1), 
                     minor_breaks = seq(0,1,by=0.05))

# example of beta distribution----
# in 41 tries, 14 wins, 27 losses

args(dbeta)
dbeta(x = seq(0,1,by=0.1), # vector of quantiles
      shape1 = 14, 
      shape2 = 27) %>% 
  plot(x = seq(0,1,by=0.25), y = ., type = "b")

prop.table(table(dbeta(x = seq(0,1,by=0.01), # vector of quantiles
      shape1 = 14, 
      shape2 = 27) <= 0.5))

integrate(function(p) dbeta(p,14,27),0,0.5)




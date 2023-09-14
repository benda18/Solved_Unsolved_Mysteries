# Questions to Answer / Display with Dashboard
####........###
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidycensus)
library(tigris)
library(data.table)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")
rm(list=ls());cat('\f');gc()

# funs----
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

# import data----

mysteries <- read_csv("mysteries.csv") %>%
  .[complete.cases(.),] %>%
  .[!duplicated(.),]

master.meta <- read_csv("master.meta.csv") %>%
  .[!duplicated(.),]

full.df <- full_join(mysteries, master.meta)

full.df$uid_ep <- paste("S", 
                        unlist(lapply(full.df$s_num,lzero,2)), 
                        "E", 
                        unlist(lapply(full.df$nth_s_ep,lzero,3)), sep = "")

cw.outcome_solved <- rbind(data.frame(master.outcome = "SOLVED", 
                                      seg.outcome    = c("captured", 
                                                         "solved")), 
                           data.frame(master.outcome = "UNSOLVED", 
                                      seg.outcome    = c("wanted", 
                                                         "wantedlinks",
                                                         "unresolved", 
                                                         "unsolved",
                                                         "unsolvedlinks")))
full.df <- left_join(full.df, 
          cw.outcome_solved)

# TIDY----




# QUESTIONS----
# How many cases were solved? 
full.df[is.na(full.df$master.outcome),]$seg_name %>% unique()
full.df %>%
  group_by(master.outcome) %>%
  summarise(n = n())

# How frequently were cases re-aired?  

# Was there any relationship with how frequently cases re-aired and if they were
# solved?

# What percentage of solved cases were solved by viewer tips? 
vwr_solv <- full.df %>%
  .[.$master.outcome == "SOLVED",] %>%
  group_by(seg_name, uid_seg, master.outcome, seg.outcome) %>%
  summarise(nd_episodes = n_distinct(uid_ep), 
            nd_seasons = n_distinct(s_num),
            viewer_solved = ifelse(sum(tag == "Viewer Solves") > 0, 
                                   "solved_by_viewer", 
                                   "solved_by_other"))

vwr_solv %>%
  group_by(viewer_solved) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(., 
         pct_n = n / sum(n))



# Outcomes by Season


# likelihood better-than-even odds by tag
p_bte_odds.bytag <- full.df %>%
  group_by(tag, master.outcome) %>%
  summarise(n = n()) %>%
  as.data.table() %>%
  dcast(., 
        tag ~ master.outcome, 
        fun.aggregate = sum, 
        value.var = "n", fill = 0, 
        na.rm = T) %>%
  as.data.frame() %>%
  mutate(., 
         T_SEGS = SOLVED + UNSOLVED, 
         pct_solved_observed = SOLVED / T_SEGS, 
         pct_solved_adjusted = (SOLVED+1) / (T_SEGS+2)) %>%
  as_tibble()

p_bte_odds.bytag$p_bteobt <- NA
for(i in 1:nrow(p_bte_odds.bytag)){
  p_bte_odds.bytag$p_bteobt[i] <- integrate(function(p) dbeta(p, 
                                                              p_bte_odds.bytag$SOLVED[i]+1, 
                                                              p_bte_odds.bytag$UNSOLVED[i]+1), 0.5, 1)$value
}

f_decade <- function(yr){
  yr %/% 10 * 10
}

#p_bte_odds.bytag[p_bte_odds.bytag$p_bteobt > 0.5,]


p_bte_decade <- p_bte_odds.bytag[!is.na(as.integer(p_bte_odds.bytag$tag)) & 
                   as.integer(p_bte_odds.bytag$tag) >= 1900,] %>%
  mutate(., 
         decade = f_decade(as.integer(tag))) %>%
  group_by(decade) %>%
  summarise(t_solved = sum(SOLVED), 
            t_unsolved = sum(UNSOLVED), 
            t = t_solved + t_unsolved) %>%
  ungroup() %>%
  mutate(ts_adj = NA_integer_, 
         tus_adj = NA_integer_, 
         t_adj   = NA_integer_)

for(i in 1:nrow(p_bte_decade)){
  if(p_bte_decade$t_solved[i] == 0 | 
     p_bte_decade$t_unsolved[i] == 0){
    p_bte_decade$ts_adj[i] <- p_bte_decade$t_solved[i] + 1 
    p_bte_decade$tus_adj[i] <- p_bte_decade$t_unsolved[i] + 1 
    p_bte_decade$t_adj[i] <- p_bte_decade$ts_adj[i] + p_bte_decade$tus_adj[i]
  }else{
    p_bte_decade$ts_adj[i] <- p_bte_decade$t_solved[i] + 0 
    p_bte_decade$tus_adj[i] <- p_bte_decade$t_unsolved[i] + 0 
    p_bte_decade$t_adj[i] <- p_bte_decade$ts_adj[i] + p_bte_decade$tus_adj[i]
  }
  
}

p_bte_decade$dbeta_50to100 <- NA
for(i in 1:nrow(p_bte_decade)){
  p_bte_decade$dbeta_50to100[i] <- integrate(function(p) dbeta(p, 
                                                               p_bte_decade$ts_adj[i], 
                                                               p_bte_decade$tus_adj[i]), 0.5, 1)$value
}
p_bte_decade$o_pct.solved <- p_bte_decade$t_solved / 
  p_bte_decade$t


ggplot() + 
  geom_col(data = p_bte_decade, 
           aes(x = factor(decade), 
               y = dbeta_50to100, 
               fill = t_adj))+
  scale_fill_viridis_c(option = "C", 
                       trans = "log10")+
  scale_y_continuous(labels = scales::percent)+
  theme_dark()+
  labs(title = "Show aired more segments that took place during or after the 1960s
       and and the likelihood of solving segments during those decades was greatest as well, 
       presumably for obvious reasons (living witnesses, engaged public)", 
       subtitle = "p_value that 50% or more of a decade's segments are solved")

ggplot() + 
  geom_point(data = p_bte_odds.bytag, 
             aes(x = pct_solved_observed, 
                 y = pct_solved_adjusted))

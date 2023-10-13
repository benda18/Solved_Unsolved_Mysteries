library(dplyr)
library(readr)
library(igraph)
library(data.table)
library(ggplot2)

rm(list=ls());cat('\f');gc()

setwd("~/R/play/Solved_Unsolved_Mysteries/data")

# load files----

temp.files <- list.files(pattern = "\\.csv$")
temp.files <- temp.files[!temp.files %in% c("season1.csv", 
                                            "season2.csv")]

for(i in temp.files){
  # set var name
  temp.var_name <- gsub("\\.csv", "", i)
  temp.csv      <- read_csv(i)
  assign(paste("df_",temp.var_name, sep = ""), temp.csv)
  
  
}
rm(i, temp.files, temp.var_name, temp.csv)


# describe files----

df_structure <- NULL
for(i in ls(pattern = "^df_")){
  temp.df <- get(i)
  # var_name
  df_structure <- rbind(df_structure, 
                         data.frame(df_name = i,
                                    # col_names
                                    df_colname = names(lapply(X = temp.df, FUN = class)),
                                    # col_classes
                                    df_colclass = unlist(unname(lapply(X = temp.df, FUN = class))),
                                    # nrows
                                    df_nrow = nrow(temp.df)
                         )) %>% as_tibble()
  rm(temp.df)
}
rm(i)
df_structure


ggplot() + 
  geom_point(data = df_structure, aes(x = df_name, y = df_colname))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


df_structure[df_structure$df_colname == "seg_url",]

# describe goal----


# y-axis will be crimes / segments.
#       df_cw_MASTER_crime
# x axis will be timeline
df_cw_ep.airdates
# show dates_aired as points on timeline. 

# show whether case was solved
#       df_cw_MASTER_outcome
# optional: show crime_type
#       df_cw_MASTER_crime
# optional: show years_of_crime

df_cw_years <- full_join(ungroup(summarise(group_by_all(df_cw_tags))), 
                         ungroup(summarise(group_by_all(df_cw_MASTER_tags))), 
                         by = c("tag" = "type")) %>%
  .[.$cat == "year",] %>%
  .[!colnames(.) %in% "cat"] %>%
  mutate(.,
         crime_year = tag) %>%
  .[!colnames(.) %in% c("tag")]

df_cw_years$crime_year[df_cw_years$crime_year == "Unknown Year"] <- NA

df_cw_years <- rbind(mutate(df_cw_years[!grepl(pattern = "^\\d{4,4}$", df_cw_years$crime_year) & 
                           !is.na(df_cw_years$crime_year),], 
             crime_year = gsub("s$", "", crime_year)), 
      mutate(df_cw_years[!grepl(pattern = "^\\d{4,4}$", df_cw_years$crime_year) & 
                           !is.na(df_cw_years$crime_year),], 
             crime_year = as.character(as.numeric(gsub("s$", "", crime_year))+9))) %>%
  rbind(., 
        df_cw_years)

df_cw_years <- df_cw_years[!(grepl("s$", df_cw_years$crime_year) & 
              !is.na(df_cw_years$crime_year)),]
 
df_cw_years$crime_year <- as.numeric(df_cw_years$crime_year)

df_crime.years <- df_cw_years %>%
  group_by(seg_url) %>%
  summarise(min_crime.year = min(crime_year, na.rm = T), 
            max_crime.year = max(crime_year, na.rm = T))

df_crime.years <- df_crime.years[!is.infinite(df_crime.years$min_crime.year) |
                 !is.infinite(df_crime.years$max_crime.year) ,] 

# optional:  group_by() crime_types

goal.df <- full_join(df_cw_MASTER_crime, 
                     df_cw_MASTER_outcome, 
                     by = c("seg_url", "seg_name"))

goal.df <- full_join(goal.df, 
                     df_crime.years)

goal.df <- left_join(goal.df, 
                     left_join(ungroup(summarise(group_by_all(goal.df[,c("seg_name", "seg_url")]))), 
                               df_cw_s.ep.seg_name[!duplicated(df_cw_s.ep.seg_name),])) %>%
  group_by_all() %>%
  summarise() %>%
  ungroup()

goal.df <- left_join(goal.df, 
          df_cw_ep.airdates)



# overall outcomes by crime_type----
goal.df %>%
  group_by(crime, master.outcome) %>%
  summarise(n_segments = n_distinct(seg_url)) %>%
  as.data.table() %>%
  dcast(., 
        crime ~ master.outcome, 
        fill = 0) %>%
  as.data.frame() %>%
  mutate(., 
         TOTAL = SOLVED + UNSOLVED,
         pct_solved = SOLVED / TOTAL) %>%
  .[order(.$TOTAL, decreasing = T),]


df_cw_s.ep.seg_name
df_cw_ep.airdates


# timeline by case----

goaldf_points <- goal.df %>%
  group_by(seg_name, seg_url,
           #s_num, ep_num,
           master.outcome, 
           min_crime.year, max_crime.year, 
            date_fa) %>%
  summarise(n = n())

goaldf_segs <- goal.df %>%
  group_by(seg_name, seg_url,
           #s_num, ep_num,
           master.outcome, 
           min_crime.year, max_crime.year) %>%
  summarise(min_date.fa = min(date_fa, na.rm = T), 
            max_date.fa = max(date_fa, na.rm = T)) %>%
  .[!is.infinite(.$min_date.fa) & !is.infinite(.$max_date.fa),]



some.segnames <- sample(unique(goal.df$seg_name), size = 40, replace = T)

some.solvedlabels <- goaldf_points[goaldf_points$seg_name %in% some.segnames,] %>%
  .[.$master.outcome == "SOLVED",] %>%
  ungroup() %>%
  group_by(seg_name) %>%
  slice_max(., 
            order_by = date_fa, 
            n = 1, na_rm = T)

library(ggrepel)

ggplot() +
  geom_text(data = some.solvedlabels, hjust = -0.5,
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


library(rlang)
f <- rlang::as_function(~ .x + 1)
f(10)

g <- as_function(~ -1 * .)
g(4)

h <- as_function(~ .x - .y)
h(6, 3)

h <- as_function(function() 'foo')
?formula
h()

xnam <- paste0("x", 1:25)
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse = "+"))))
fmla2 <- reformulate(xnam, response = "y")
stopifnot(identical(fmla,fmla2))
?stopifnot(F)

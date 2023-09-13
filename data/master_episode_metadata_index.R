library(rvest)
library(dplyr)
library(glue)

rm(list=ls());cat('\f');gc()

# vars----
var.season         <- 2
var.epnum.startat1 <- 1
var.segnum         <- 1

# generate url for season
url1 <- glue("https://unsolvedmysteries.fandom.com/wiki/Season_{var.season}")

# GET SEGMENT NAME AND URL
xp.seg.info <- glue("/html/body/div[4]/div[4]/div[3]/main/div[3]/div[2]/div/ul[1]/li[{var.segnum}]/a")


# get episode and season number 
xp.ep.info  <- glue("/html/body/div[4]/div[4]/div[3]/main/div[3]/div[2]/div/h3[{var.epnum.startat1}]")

the.html.ep_num <- read_html(url1) %>%
  html_element(x = ., 
               xpath = xp.ep.info) 
ep.num <- unname(xml_attrs(rvest::html_children(the.html.ep_num)[1][[1]])["id"])
ep.num

df.season.episode <- data.frame(s_num = var.season, 
                                ep_num = ep.num, 
                                nth_s_ep = var.epnum.startat1)
df.season.episode
rm(the.html.ep_num)



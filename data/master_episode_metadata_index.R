library(rvest)
library(dplyr)
library(glue)

rm(list=ls());cat('\f');gc()

get_episode_metadata <- function(s_num, 
                                 ep_num){
  # vars----
  var.season         <- s_num
  var.epnum.startat1 <- ep_num
  var.segnum         <- 0
  
  df.season.episode <- NULL
  the.html.seg.info <- ""
  while(!is.na(the.html.seg.info)){
    var.segnum <- var.segnum+1
    
    # generate url for season
    url1 <- glue("https://unsolvedmysteries.fandom.com/wiki/Season_{var.season}")
    
    #unname(xml_attrs(rvest::html_children(the.html.seg.info)[1][[1]])["id"])
    
    # get episode and season number 
    xp.ep.info  <- glue("/html/body/div[4]/div[4]/div[3]/main/div[3]/div[2]/div/h3[{var.epnum.startat1}]")
    
    the.html.ep_num <- read_html(url1) %>%
      html_element(x = ., 
                   xpath = xp.ep.info) 
    ep.num <- unname(xml_attrs(rvest::html_children(the.html.ep_num)[1][[1]])["id"])
    
    # get segment names and info
    xp.seg.info <- glue("/html/body/div[4]/div[4]/div[3]/main/div[3]/div[2]/div/ul[{var.epnum.startat1}]/li[{var.segnum}]/a")
    
    the.html.seg.info <- read_html(url1) %>%
      html_element(x = ., 
                   xpath = xp.seg.info)
    
    
    # TODO this is broken vvv
    var.segname <- rvest::html_text(the.html.seg.info)
    
    if(!is.na(var.segname)){
      df.season.episode <- rbind(df.season.episode, 
                                 data.frame(s_num = var.season, 
                                            ep_num = ep.num, 
                                            nth_s_ep = var.epnum.startat1, 
                                            seg_num = var.segnum, 
                                            seg_name = var.segname))
    }
  }
  
  return(df.season.episode)
  #rm(the.html.ep_num)
}


the.seasons <- 3
the.episodes <- 1:3


master.meta <- NULL
for(s1 in the.seasons){
  for(e1 in the.episodes){
    temp.df <- try(get_episode_metadata(s1,e1))
    if(!class(temp.df) == "try-error"){
      master.meta <- rbind(master.meta,
                           temp.df)
    }
  }
}

master.meta

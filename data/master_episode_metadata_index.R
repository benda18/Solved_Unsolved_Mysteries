library(rvest)
library(dplyr)
library(glue)

rm(list=ls());cat('\f');gc()

s_num <- 3
ep_num <- 1
var.segnum <- 4

get_episode_metadata <- function(s_num, 
                                 ep_num){
  # vars----
  var.season         <- s_num
  var.epnum.startat1 <- ep_num
  var.segnum         <- 0
  
  df.season.episode <- NULL
  #the.html.seg.info <- ""
  var.seg.info      <- ""
  while(!is.na(var.seg.info)){
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
    #xp.seg.info <- glue("/html/body/div[4]/div[4]/div[3]/main/div[3]/div[2]/div/ul[{var.epnum.startat1}]/li[{var.segnum}]/a")
    xp.seg.info <- glue("/html/body/div[4]/div[4]/div[3]/main/div[3]/div[2]/div/ul[{var.epnum.startat1}]/li[{var.segnum}]")
    
    var.seg.info <- read_html(x = url1) %>%
      html_element(., xpath = xp.seg.info) 
    
    if(length(var.seg.info) > 0){
      var.seg.info_url <- var.seg.info %>%
        html_children() %>%
        as.character() %>%
        strsplit(., "\"") %>%
        #lapply(., grep, pattern = "\\=|<|>", value = T) %>%
        lapply(., nth, c(2)) %>%
        unlist() 
      var.segname <- var.seg.info %>%
        html_children() %>%
        as.character() %>%
        strsplit(., "\"") %>%
        #lapply(., grep, pattern = "\\=|<|>", value = T) %>%
        lapply(., nth, c(4)) %>%
        unlist() 
      var.segurl  <- glue("https://unsolvedmysteries.fandom.com/{var.seg.info_url}")
      df.season.episode <- rbind(df.season.episode, 
                                 data.frame(s_num    = var.season, 
                                            ep_num   = ep.num, 
                                            nth_s_ep = var.epnum.startat1, 
                                            seg_num  = var.segnum, 
                                            seg_name = var.segname,
                                            seg_url  = var.segurl))
      
    }
      
    
    
    
    
    # the.html.seg.info <- read_html(url1) %>%
    #   html_element(x = ., 
    #                xpath = xp.seg.info)
    # var.segname <- rvest::html_text(the.html.seg.info)
    
    #if(!is.na(var.segname)){
    #if(!is.null(var.segname)){
      # df.season.episode <- rbind(df.season.episode, 
      #                            data.frame(s_num    = var.season, 
      #                                       ep_num   = ep.num, 
      #                                       nth_s_ep = var.epnum.startat1, 
      #                                       seg_num  = var.segnum, 
      #                                       seg_name = var.segname,
      #                                       seg_url  = var.segurl))
    #}
  }
  
  return(df.season.episode)
  #rm(the.html.ep_num)
}

get_episode_metadata(3,1)

# read_html("https://unsolvedmysteries.fandom.com/wiki/Season_3") %>%
#   html_element(x = ., 
#                xpath = "/html/body/div[4]/div[4]/div[3]/main/div[3]/div[2]/div/ul[1]/li[1]") %>%
#   html_children() %>%
#   as.character() %>%
#   strsplit(., "\"") %>%
#   unlist() %>%
#   .[!grepl("\\=|<|>", .)]
# 
# 
# 
# html_children(temp.x1) %>% 
#   as.character() %>%
#   strsplit(., "\"") %>% 
#   unlist %>%
#   .[!grepl("\\=|<|>", .)]

the.seasons <- 7#2:14
the.episodes <- 1:5#:40


master.meta <- NULL
for(s1 in the.seasons){
  for(e1 in the.episodes){
    Sys.sleep(1)
    print(e1)
    temp.df <- try(get_episode_metadata(s1,e1))
    
    if(!class(temp.df) == "try-error"){
      master.meta <- rbind(master.meta,
                           temp.df)
    }
  }
}

master.meta

write_csv(master.meta, "master.meta.csv")


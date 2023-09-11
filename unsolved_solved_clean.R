# solved myseries
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

setwd("~/R/play/Solved_Unsolved_Mysteries/data")

rm(list=ls());cat('\f');gc()

# Functions----
parse_case_result <- function(x){
  out <- x
  out <- gsub("^.*Results:", 
              "", out) %>%
    trimws() %>%
    strsplit(., "\\W") %>%
    unlist() %>%
    first()
  return(out)
}


lzero <- function(x, n.leading.zeroes){
  out <- as.character(x)
  if(nchar(out) < n.leading.zeroes){
    out <- paste(c(rep("0", n.leading.zeroes - nchar(out)), 
                   out), sep = "", collapse = "")
  }
  return(out)
}

word2num <- function(word){
  #wsplit <- strsplit(tolower(word)," ")[[1]]
  wsplit <- strsplit(tolower(word)," |-")[[1]]
  one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                     six=6, seven=7, eight=8, nine=9)
  teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                sixteen=16, seventeen=17, eighteen=18, nineteen=19)
  ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                     sixty=60, seventy=70, eighty=80, ninety=90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1
  while(i <= length(wsplit)){
    j <- 1
    if(i==1 && wsplit[i]=="hundred")
      temp <- 100
    else if(i==1 && wsplit[i]=="thousand")
      temp <- 1000
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if(i < length(wsplit) && wsplit[i+1]=="hundred"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }
  #return(list(word,out))
  return(out)
}

#word2num(c("thirty-five"))

parse_epinfo <- function(x){
  require(lubridate)
  require(dplyr)
  require(readr)
  x1 <- x %>%
    # special manual removals
    # gsub("Aired: February 19, 1992 100th Episode Special \\(2 hrs\\)", "Aired: February 19, 1992 ", .) %>%
    # gsub("Aired: November 25, 1992 {1,2}\\(Live: 90 mins\\)", "Aired: November 25, 1992 ", .) %>%
    # gsub("Missing Persons Roll Call:\n", "", .) %>%
    # gsub("Final Appeal: ", "", .) %>%
    # gsub("Rapist Roll Call: |Lost Heirs Roll Call: |Con Men Roll Call: |Spanish Lotto Roll Call: |Missing Children Roll Call: |Drug Trafficker Roll Call: |Bank Robber Roll Call: ", "", .) %>%
    gsub(pattern = "Episode", "@@Episode", .) %>%
    strsplit(x = ., 
             split = "@@") %>% 
    unlist()
  out.s_num <- x1[grepl("^Season ", x1)] %>% trimws()
  
  # removes empty records 
  x2 <- x1[grepl("^Episode \\d{1,2}", 
                 x = x1)]
  x3 <- lapply(X = x2, 
               FUN = read_csv, col_names = F, progress = F)
  
  # generate cw episode # and air date
  
  df.metadata <- NULL
  for(i in 1:length(x3)){
    temp <- x3[[i]] %>% unlist %>% unname
    
    temp.df <- data.frame(season = out.s_num, 
                          episode = grep("^Episode \\d{1,2}", temp, 
                                         ignore.case = F, value = T), 
                          first_aired = grep("Aired:", temp, value = T, 
                                             ignore.case = T), 
                          segment = temp[!grepl("^Episode", temp, ignore.case = F) & 
                                           ! grepl("aired:", temp, ignore.case = T)])
    
    df.metadata <- rbind(df.metadata, 
                         temp.df) %>%
      as_tibble()
    
  }
  #return(df.metadata)
  
  out <- df.metadata
  # out$segment <- gsub("^.*ROLL CALL: |special alert: |FBI Alerts: |Drug Trafficker Roll Call: ", 
  #                     "", 
  #                     out$segment, ignore.case = T)
  
  
  # # fix some stuff with updates and other prefixes that aren't consistent
  # out$segment <- gsub("Updated: |Update: |Updates:", "Update-", out$segment)
  # out$segment_type <- gsub(pattern = ":.*$", "", out$segment)
  # #out$segment_name <- gsub("^.*")
  # out$date_first.aired <- out$first_aired %>%
  #   gsub("^.*:|\\(.*$", "", .) %>%
  #   trimws() %>% 
  #   mdy()
  # out$s_num <- gsub(pattern = "^Season|\\(.*$", 
  #                   replacement = "", 
  #                   x = out$season) %>%
  #   trimws() %>%
  #   lapply(., 
  #          word2num) %>% 
  #   unlist()
  # out$ep_num <- gsub("^Episode", "", out$episode) %>%
  #   trimws() %>%
  #   as.numeric()
  # out$segment_name <- mapply(FUN = gsub, 
  #                            pattern = out$segment_type, 
  #                            replacement = rep("", nrow(out)), 
  #                            x = out$segment) %>%
  #   unlist() %>%
  #   unname() %>%
  #   gsub("^:|\\(.*$", "", .) %>%
  #   trimws()
  # 
  # out <- out[!out$segment %in% c("Advertisement", "ADVERTISEMENT"),]
  # out$is_update <- grepl("^Update-", out$segment_type)
  # out$segment_type <- out$segment_type %>% gsub("^Update-", "", .)
  # 
  # # manual fixes to segment_type  
  # out$segment_type[out$segment_type %in% c(" Wanted")] <- "Wanted"
  # out$segment_type[out$segment_type %in% c("Lost Heirs", "Lost Loves", 
  #                                          "Lost ", " Lost", " Lost ")] <- "Lost"
  # out$segment_type[out$segment_type %in% c("Legend", "Legends")] <- "Legend"
  # 
  # return(out[,c("s_num", "ep_num", 
  #               "segment_type", "segment_name",
  #               "segment", 
  #               "date_first.aired", 
  #               "is_update")])
  return(out)
}



# vars----

season1.url <- "https://unsolvedmysteries.fandom.com/wiki/Season_1#Episode_1"

s7.info <- parse_epinfo("Season 7 (1994-95)
Episode 198

Aired: September 25, 1994

    Legend: The Ice Man
    Mystery: Frank Olson
    Update: Wanted: Larry George

Episode 199

Aired: October 2, 1994 (repeated December 30, 1994)

    Legend: La Posada Hotel
    Murder: Jonathan Francia
    Mystery: Jeanne Boylan
    Wanted: John Roubas
    Legend: Elysian Park Treasure

Episode 200

Aired: October 9, 1994

    Mystery: Father Solanus Casey
    Lost: Alexander (Savior of Siegfried Laier)
    Update: Lost: The Children of Hilda Craun
    Legend: John Wilkes Booth (repeat from September 25, 1991)

Episode 201

Aired: October 14, 1994

    Mystery: Betty Landers
    Mystery: Lori Smith
    Mystery: Elizabeth Bersanetti
    Wanted: Adam Emery
    Wanted: Phyllis Strub
    Updates: Wanted: Travis Wade Duncan
    Wanted: Alan Verl Sneed
    Wanted: Edward Zakzrewski
    Wanted: Nasario Palacios

Advertisement
Episode 202 

Aired: October 21, 1994

    Mystery: Dannion Brinkley
    Murder: John and Nancy Bosco
    Mystery: Karen Walker
    Legend: Wyrick House

Episode 203

Aired: October 28, 1994

    Legend: Mexico City UFO
    Murder: Sammy Wheeler
    Lost: Gordon Page Jr.

Episode 204

Aired: November 11, 1994 (2hrs)

    Mystery: Circleville Writer
    Murder: Ron Gillispie
    Mystery: Agatha Christie
    Update: Lost: Jim Pearson
    Mystery: Tommy Burkett
    Legend: Poverty Island Treasure
    Wanted: Tim Barry

Episode 205

Aired: December 2, 1994

    Mystery: Nancy Myer
    Murder: Jennifer Odom
    Legend: Mary Celeste
    Update: Lost: The Parents of Brenda Abbey
    Lost: Saviors of Michelle West

Advertisement
Episode 206

Aired: December 9, 1994

    Wanted: Neil and Terry Gott
    Mystery: Margaret Wilson
    Lost: Robert Borton
    Murder: Roy Caffey

Episode 207

Aired: December 16, 1994

    Legend: Gurdon Light
    Mystery: Stuart Heaton
    Murder: Krystal Naab
    Mystery: Jay Durham
    Update: Lost: Marilyn Hahnlein

Episode 208

Aired: December 23, 1994

    Mystery: Teryn Hedlund
    Lost: Philip Pelletier (repeat from December 21, 1988)
    Mystery: Father Solanus Casey (repeat from October 9, 1994)

Episode 209

Aired: January 6, 1995

    Mystery: Cyril Wecht
    Mystery: Jack Davis Jr.
    Mystery: Hank Jones/Dr. David Faux/Carol Montrose
    Lost: Phillip Thomas
    Wanted: Manwell Marino
    Wanted: Elwyn Jones

Advertisement
Episode 210

Aired: January 13, 1995

    Lost: John and Linda Sohus 
    Wanted: Christian Karl Gerhartsreiter
    Legend: Phantom Sub
    Mystery: Mike O'Mara
    Update: Lost: The Family of Terris Christie Derby
    Lost: Stallings Family
    Fugitive Hotline: Wanted: Tyrom Theis/Shannon Smith

Episode 211

Aired: January 20, 1995

    Update: Wanted: Elwyn Jones
    Legend: Padre Pio
    Murder: Kimberly Pandelios
    Mystery: 1987 Jane Doe body found off a cliff
    Mystery: 1990 Jane Doe hit by a car
    Mystery: Sumter County Does found murdered in 1976
    Lost: Sylvan Lazarus, Carl Cobb and Bernard Brady
    Update: Lost: Stallings Family

Episode 212  

Aired: February 3, 1995

    Mystery: Jeanine Price
    Murder: Melanie Uribe
    Mystery: Etta Smith
    Mystery: James Van Praagh
    Murder: Kevin Wheel
    Mystery: Doug Raskin

Advertisement
Episode 213

Aired: February 10, 1995

    Wanted: Kansas City Arsonist
    Lost: Loretta Myers
    Wanted: Wallace Thrasher
    Lost: Robert James Palmer

Episode 214

Aired: February 17, 1995

    Wanted: Rita Faulkner
    Lost: The Search of Helen Elas
    Lost: Kristi Krebs
    Wanted: Dub Wackerhagen
    Lost: Chance Wackerhagen
    Murder: Latricia White

Episode 215

Aired: February 24, 1995

    Legend: Alamo Treasure
    Wanted: Randall Utterback
    Murder: Eric and Pam Ellender
    Update: Lost: Rose Marie Luttmer

Episode 216

Aired: March 3, 1995

    Update: Legend: Alamo Treasure
    Murder: Jian Fang
    Legend: The Ice Man (repeat from September 25, 1994)
    Murder: Frank Olson (repeat from September 25, 1994)

Advertisement
Episode 217 

Aired: March 17, 1995

    Wanted: Randolph Dial 
    Lost: Bobbi Parker
    Recap: Wanted: Robert Watson
    Recap: Wanted: David Fisher
    Recap: Wanted: Jean Marie Gagnon
    Recap: Wanted: Michael Mohan
    Recap: Wanted: Edgar Kerns
    Recap: Wanted: John Yount
    Recap: Wanted: Travis Wade Duncan
    Wanted: Frank Morris and the Anglin Brothers (repeat from February 8, 1989)

Episode 218

Aired: March 24, 1995

    Mystery: Luiz Gasparetto
    Lost: Tara Breckenridge
    Lost: The Friends of Fritz Vincken
    Update: Wanted: Wade Mitchell Parker
    Wanted: Carlos Garcia

Episode 219 

Aired: April 7, 1995

    Wanted: James and Lisa Albert
    Wanted: Elizabeth Ortiz (repeat from November 25, 1992)
    Wanted: Andolina Gonzalez (repeat from October 6, 1993)
    Wanted: Kelly Finnegan (repeat from May 4, 1994)

Advertisement
Episode 220

Aired: April 14, 1995

    Mystery: Arnold Archambeau and Ruby Bruguier
    Mystery: 1987 Jane Doe (repeat from January 20, 1995)
    Legend: Mexico City UFO (repeat from October 28, 1994)

Episode 221

Aired: April 28, 1995

    Mystery: Noreen Renier
    Murder: Jake and Dora Cohn
    Murder: Rosemary Hom
    Lost: Cecilia Newball and Rene Perez Jr.
    Mystery: The Search of Bob Coleman
    Mystery: Jim McMahon
    Wanted: Richard Ford and Matthew Crome

Episode 222

Aired: May 5, 1995

    Mystery: Kathleen Burghardt
    Mystery: Eric Danowski
    Wanted: Lionel Luviano
    Mystery: Cogni-tech
    Murder: Martin Ganz
    Murder: Martin Hernandez
    Update: Lost: Stallings Family
    Lost: Keyan and Logan Ivey
    Wanted: David Ivey
    Lost: Davey Estright
    Wanted: David Estright

Episode 223

Aired: May 12, 1995

    Mystery: Patience Worth
    Wanted: John Vogel
    Lost: Justin Burgwinkel
    Fugitive Hotline: Wanted: Paul Xavier Alexander
    Murder: Dorothy Donovan

Advertisement
Episode 224

Aired: May 19, 1995

    Mystery: E.L.F.
    Lost: Saviors of Dover Family
    Wanted: Robert Arcieri
    Lost: Breanna Smith and Ryan Heath 
    Wanted: Tyger and Archie Hoag
    OTHER: Investigation into postal crimes
    OTHER: woman vanishes

Episode 225

Aired: August 18, 1995

    Wanted: Rose Turford and Carolyn Stevens

Episode 226

Aired: August 23, 1995

    Mystery: Gigi

Episode 227

Aired: August 30, 1995

    Lost: Jean Moore
    Wanted: Ray Olson
    Mystery: Luiz Gasparetto (repeat from March 24, 1995)")

s8.info <- parse_epinfo("Season Eight (1995-96)
Episode 228

Aired: October 20, 1995

    Mystery: Brandon Lee
    Update: Wanted: Ray Olson
    Update: Mystery: Gigi
    Special Bulletin: Case #1: Lost: Emilie Hardy
    Wanted: Rejean Hardy
    Special Bulletin: Case #2: Wanted: Red Dye Robber
    Wanted: Dorothy Barnett
    Lost: Savanna Todd
    Wanted: Antonio Castro
    Mystery: Joe O'Brien

Advertisement
Episode 229

Aired: October 27, 1995 (repeated December 29, 1995)

    Wanted: Brazos River Attackers
    Special Bulletin: Wanted: Ronald Earl Cains
    Update: Lost: Loretta Myers
    Murder: Matt Flores
    Legend: Kentucky Visions
    Wanted: Richard Relf
    Lost: Heather Relf

Episode 230

Aired: November 3, 1995 (repeated January 19, 1996)

    Wanted: Michael Swango
    Update: Wanted: Brazos River Attackers
    Update: Wanted: Manny Moreno
    Special Bulletin: Case #1: Lost: Renee Lamanna
    Special Bulletin: Case #2: Lost: The Daughter of Gerda Burian
    Wanted: Michael Eschweiler
    Mystery: Logan Carroll
    Lost: Amadeo Marcelo

Episode 231

Aired: November 10, 1995 (repeated March 1, 1996)

    Wanted: Guy Cummings
    Update: Lost: Amadeo Marcelo
    Update: Lost: Keyan and Logan Ivey
    Wanted: David Ivey
    Special Bulletin: Case #1: Lost: Leonard Trigg
    Wanted: Allie Ingrid Trigg
    Special Bulletin: Case #2: Lost: Seth Arathorne
    Wanted: Garth Arathorne
    Lost: Charles Southern Jr.
    Murder: Alie Berrelez
    Murder: Antranik Geuvjehizian

Advertisement
Episode 232

Aired: November 17, 1995

    Lost: Taylor Kramer
    Update: Lost: Craig Williamson
    Lost: Evonne
    Mystery: Linda Tellington-Jones
    Legend: Marie LaVeau
    Mystery: Harold Bennett
    Lost: Tim Molnar

Episode 233

Aired: December 1, 1995

    Legend: Lonnie Zamora UFO Sighting
    Update: Lost: Saviors of Dover Family
    Update: Lost: Seth Arathorne 
    Wanted: Garth Arathorne
    Special Bulletin: Case #1: Lost: Jessyca Mullenberg
    Wanted: Steven Oliver
    Special Bulletin: Case #2: Wanted: Ronald Bax
    Wanted: Carl and Mary Dennie
    Lost: The Siblings of Debbie Hamilton
    Murder: David Merrifield

Episode 234

Aired: December 8, 1995 (repeated April 5, 1996)

    Wanted: Roberto Ramirez
    Update: Murder: Linda Sobek and Kimberly Pandelios
    Lost: The Siblings of Tina Shiets
    Legend: Miracle the White Buffalo
    Wanted: David Gause
    Mystery: George Reeves

Advertisement
Episode 235

Aired: December 15, 1995 (repeated March 8, 1996)

    Legend: Caddy in Western Canada
    Murder: Sherry Hart
    Wanted: Richard Bare
    Update: Wanted: Rose Turford and Carolyn Stevens
    Update: Lost: The Siblings of Tina Shiets
    Special Bulletin: Case #1: Wanted: Rickey Bright
    Special Bulletin: Case #2: Wanted: Michael McGuffey
    Lost: Michael Hughes
    Mystery: Suzanne Davis
    Wanted: Epes Bandits
    Lost: The Family of Jeanne Martin

Episode 236

Aired: January 5, 1996 (repeated May 31, 1996)

    Mystery: Tom Kueter
    Lost: Tina Marcotte
    Wanted: Ira Einhorn
    Lost: Jim Kimball
    Murder: Brian Foguth
    Wanted: Nassau County Robber/Boynton Beach Robber
    Legend: Ghosts at the Covewood Lodge
    Update: Wanted: Richard Ford/Matthew Crome

Episode 237

Aired: January 12, 1996

    Murder: Leroy Drieth
    Update: Mystery: Johnny Lee Wilson
    Lost: Donna Kempton
    Lost: Bonnie Haim
    Legend: The Devil's Backbone

Advertisement
Episode 238

Aired: January 31, 1996

    Lost: Moses Lall and Lila Buerattan
    Wanted: Hari Gobin
    Lost: Taylor Kramer (repeat from November 17, 1995)
    Update: Lost: Craig Williamson (repeat from November 17, 1995)
    Lost: Evonne (repeat from November 17, 1995)
    Legend: Marie Laveau
    Mystery: Harold Bennett (repeat from November 17, 1995)
    Lost: Tim Molnar (repeat from November 17, 1995)

Episode 239

Aired: February 2, 1996 (repeated June 21, 1996)

    Wanted: Thomas David Dixon
    Mystery: Nancy Manni
    Update: Lost: The Daughter of Gerda Burian
    Lost: Patricia Bonner
    Murder: Mia Zapata
    Special Bulletin: Lost: Sandra Nevarez

Episode 240

Aired: February 9, 1996 (repeated June 28, 1996)

    Legend: Jean Hilliard
    Murder: Su Taraskiewicz
    Special Bulletin: Wanted: Kevin Dominic
    Wanted: Albert Leon Fletcher
    Mystery: Jessie Presley/Shannon Sites
    Update: Wanted: Richard Ford/Matthew Crome
    Lost: An adoptee's search for three siblings (possibly The Siblings of Jeff Linn)
    Lost: A viewer tip that led a Miami woman to the son she hadn't seen for 23 years (possibly Patrick Ackles)

Advertisement
Episode 241

Aired: February 16, 1996 (repeated July 12, 1996)

    Mystery: Michelle O'Malley
    Wanted: Green River Killer
    Lost: Roger Lindsley
    Murder: Martha Moxley
    Lost: A woman trying to locate her mother and siblings
    Update: Lost: An update on a family's reunion

Episode 242

Aired: February 18, 1996

    Lost: Jodi Huisentruit
    Mystery: Philip Pauli
    Legend: Lonnie Zamora UFO Sighting (repeat from December 1, 1995)
    Wanted: Carl and Mary Dennie (repeat from December 1, 1995)
    Murder: David Merrifield (repeat from December 1, 1995)

Episode 243

Aired: February 23, 1996 (repeated July 5, 1996)

    Legend: The ghosts of The Comedy Store
    Lost: Savior of Wendy Radcliffe
    Update: Lost: The Friends of Fritz Vincken
    Wanted: Bike Path Rapist
    Mystery: Keith Warren
    Special Bulletin: Wanted: John Arbogast
    Lost: George Burdynski
    Lost: An Iowa woman's success in locating two of her three siblings

Advertisement
Episode 244

Aired: March 15, 1996

    Wanted: Richard Cepulonis
    Lost: Karen Walters
    Update: Lost: Savior of Wendy Radcliffe
    Special Bulletin: Wanted: John Anthony Diaz
    Legend: The ghost of Gettysburg National Military Park
    Lost: Devin Williams
    Murder: Michael Johnston and Rochelle Robinson
    Update: Wanted: Michael Eschweiler

Episode 245

Aired: March 22, 1996 (repeated August 16, 1996)

    Lost: Saviors of Duane Miller
    Update: Lost: Patricia Bonner
    Wanted: Sam Wodke
    Mystery: Vince Foster
    Murder: Jean Ellroy

Episode 246

Aired: March 29, 1996

    Murder: David Cox
    Update: Wanted: Thomas David Dixon
    Special Bulletin: Wanted: Antonio Moses
    Wanted: Inner City Church Fire
    Legend: Miracles Mary Clamser
    Lost: The Family of LaDonna Alfano

Advertisement
Episode 247

Aired: April 12, 1996 (repeated July 10, 1996)

    Murder: Joan Jefferies
    Update: Lost: The Family of LaDonna Alfano
    Special Bulletin: Murder: Tanya Smith
    Mystery: Tony Marabella
    Wanted: Luis Ochoa
    Legend: The Wickenberg Massacre

Episode 248

Aired: April 19, 1996

    Wanted: Gloria Schulze
    Legend: Nazca Lines
    Update: Wanted: Antonio Moses
    Update: Wanted: The Unabomber
    Special Bulletin: Wanted: John Addis 
    Lost: Joann Albanese
    Lost: Charlotte Pollis
    Legend: The ghosts at the Three Partners Ranch

Episode 249

Aired: April 26, 1996 (repeated August 23, 1996)

    Mystery: Doris Duke
    Update: Lost: Rodger Lindsley
    Update: Wanted: John Anthony Diaz
    Lost: The Friends of Kim Schad
    Legend: Chupacabras
    Mystery: Matt and Wendy Jameson

Advertisement
Episode 250

Aired: May 3, 1996 (repeated August 30, 1996)

    Mystery: Scott Enyart
    Legend: Robert Kennedy
    Mystery: Robert Davidson
    Legend: Acton Campground
    Wanted: Timothy Coombs
    Mystery: Shelly Malone
    Wanted: A capture of a federal fugitive

Episode 251

Aired: May 10, 1996

    Legend: The Curse of King Tut
    Update: Wanted: Sam Wodke
    Mystery: Harvey McCloud
    Mystery: Sarah Powell
    Murder: Amtrak Derailment

Episode 252

Aired: May 17, 1996

    Murder: Joseph Cole
    Update: Lost: The Family of LaDonna Alfano
    Lost: The Mother of Tim Harrell
    Wanted: David Freeman
    Murder: Tim Good
    Mystery: Milly McGregor/Randy Spears
    Special Bulletin: Lost: Candice and Sharina Berry
    Update: Wanted: Thomas David Dixon

Advertisement
Episode 253

Aired: September 6, 1996

    Wanted: Jorge Mendez and Jose Rios
    Murder: Kristie Martin
    Murder: David Cox (repeat from March 29, 1996)
    Wanted: Thomas David Dixon (repeat from March 29, 1996)
    Mystery: Inner City Church Fire (repeat from March 29, 1996)

Episode 254

Aired: September 13, 1996

    Wanted: Kelly Lee McGinnis
    Murder: Kristie Gunderson Lee
    Lost: The Daughter of Dave and Christie Carlsen
    Legend: The Curse of King Tut (repeat from May 10, 1996)")

s9.info <- parse_epinfo("Season 9 (1996-97)
Episode 255

Aired: September 20, 1996

    Mystery: The Zodiac Killer/The Unabomber
    Update: Wanted: Carl And Mary Dennie
    Lost: April Gregory
    Lost: Kristin Smart
    Mystery: Trish Zemba
    Wanted: William Jordan
    Special Bulletin: Wanted: Andre Wilson

Advertisement
Episode 256

Aired: September 27, 1996

    Mystery: Margaux Hemingway
    Wanted: Johnathan Kern
    Update: Wanted: Albert Leon Fletcher
    Special Bulletin: Wanted: Jose Blandon
    Murder: Grant Hendrickson and Michele Cartagena
    Mystery: Kevin Reeder 
    Lost: Bruce and Rosa
    Special Bulletin: Lost: Raymond Routte
    Update: Lost: The Daughter of Dave and Christie Carlsen

Episode 257

Aired: October 18, 1996

    Murder: Bobby Fuller
    Update: Wanted: David Freeman
    Murder: Tim Good
    Special Bulletin: Wanted: David Coleman
    Mystery: Red Mercury
    Mystery: Cawood Burglary
    Update: Wanted: Kelly Lee McGinnis
    Legend: Fertility Statues

Episode 258

Aired: October 25, 1996

    Mystery: Laurie Cabot
    Murder: Martha Brailsford
    Lost: Gail Knowlton
    Wanted: Agustin Mendoza
    Wanted: Elena Souza
    Update: Lost: The Mother of Tim Harrell
    Legend: Sam Zelikson
    Special Bulletin: Lost: Meriah and Sabria Widboom

Advertisement
Episode 259

Aired: November 1, 1996

    Legend: Life on Mars
    Wanted: Tony DeCompo
    Update: Wanted: Kansas City Arsonist
    Murder: David Chase
    Update: Lost: Candice and Sharina Berry
    Update: Lost: The Siblings of Jeff Linn
    Update: Wanted: David Coleman
    Special Bulletin: Lost: Owen Walker
    Special Bulletin: Lost: Heirs of Temperee Hawkins

Episode 260

Aired: November 8, 1996

    Mystery: Jarod Allgood and Heidee Ruiz
    Wanted: Karen Pelletiere 
    Lost: William Pelletiere
    Special Bulletin: Lost: Lance Gueverra
    Special Bulletin: Lost: Jesus De La Cruz
    Lost: Susan Harrison
    Lost: The Sister of Lois Cappoziello

Episode 261

Aired: November 15, 1996

    Mystery: Howard Storm
    Murder: Alicia Showalter Reynolds
    Update: Wanted: Richard Cepulonis and Karen Walters
    Wanted: Jimmy Ray LeGate 
    Lost: Karen Lofland
    Special Bulletin: Wanted: Michael Wilburn
    Lost: Savior of Steve Newton
    Mystery: Stephanie Booker

Advertisement
Episode 262

Aired: November 22, 1996

    Mystery: O.J. Simpson
    Mystery: Qi Gong
    Wanted: Whitey Bulger and Catherine Greig
    Special Bulletin: Wanted: Robert Trenholm
    Special Bulletin: Wanted: Samuel Glover and Marshall Kirkpatrick
    Lost: Le-Zhan Williams 
    Murder: Daphne Boyden

Episode 263

Aired: December 13, 1996

    Wanted: Michael Short 
    Wanted: Melody Woods
    Update: Lost: The Daughter of Dave and Christie Carlsen
    Special Bulletin: Lost: Regina Davis
    Murder: Lynn Amos
    Lost: The Parents of Gale Samuels
    Mystery: Cokeville Elementary School Explosion

Episode 264

Aired: January 3, 1997

    Mystery: Boo / Oscar / Ringo
    Wanted: Connie Jean Helton 
    Lost: Zenith Helton
    Wanted: Zip Gun Bomber
    Murder: Sandra Orellana
    Lost: Kelli Ann Ayres
    Wanted: Elena Souza (repeat from October 25, 1996)
    Update: Wanted: Andy Cook
    Special Bulletin: Wanted: Jimmie Wayne Pierce

Advertisement
Episode 265

Aired: January 10, 1997

    Mystery: Bee Sting Healing
    Wanted: Sagebrush Rebellion
    Update: Lost: Christopher Kurowski
    Mystery: Alex Kelly
    Murder: Jamie Santos
    Special Bulletin: Wanted: Gary Wilson 
    Lost: Marek Kosciukiewicz

Episode 266

Aired: January 31, 1997

    Legend: Oliver
    Wanted: Dennis Keith Smith
    Lost: The Savior of Renee Wilson
    Lost: Susan Walsh
    Murder: Jon-Benet Ramsey
    Wanted: The search for a suspect in Mexico
    Update: Wanted: Jimmy Ray LeGate 
    Lost: Karen Lofland

Episode 267

Aired: February 7, 1997

    Mystery: Cancer Dogs
    Mystery: Tommy Zeigler
    Mystery: Kurt Cobain
    Lost: A missing persons case from 1995 (possibly Amal and Wafa Abdallah)

Advertisement
Episode 268

Aired: February 14, 1997

    Mystery: Dr. Cynthia Watson
    Special Bulletin: Murder: Ennis Cosby
    Mystery: TWA Flight 800 
    Mystery: US Marine Corps Helicopter Crash
    Update: Murder: Sandra Orellana
    Murder: Robert Dirscherl
    Update: Wanted: Andy Cook
    Special Bulletin: Wanted: David Alex Alvarez
    Mystery: Comet/Earth Collision

Episode 269

Aired: February 21, 1997

    Mystery: Olesen Family
    Murder: Patrick Kelly
    Mystery: Highway 50 Phantom 
    Lost: Christene and Nick Skubish
    Lost: Ceara O'Connell 
    Murder: Naoma and Richard Wendorf
    Special Bulletin: Wanted: Edwin Rodriguez 
    Murder: Carmen Charneco
    Special Bulletin: Wanted: Billy Ray Sisson
    Update: Lost: Kelli Ann Ayres

Episode 270

Aired: March 14, 1997

    Murder: Tupac Shakur
    Lost: Therese Rose Walsh 
    Wanted: Merle Marie Vanderheiden
    Wanted: Gary and Ted Noble
    Mystery: Three cases of spontaneous human combustion
    Mystery: Kay Fletcher
    Mystery: George Mott
    Mystery: Irving Bentley
    Lost: two women's search for their long lost brother

Advertisement
Episode 271

Aired: March 28, 1997

    Mystery: David Morehouse
    Wanted: Maria Hernandez
    Wanted: Melvin Luther Wilson
    Update: Lost: Raymond Routte
    Update: Wanted: Andre Wilson
    Special Bulletin: Wanted: Mark Gagliardo
    Mystery: Dolly the Sheep

Episode 272

Aired: April 4, 1997

    Wanted: Maria Socorro De Rodriguez LaPine (The Black Widow)
    Special Bulletin: Wanted: Mark Gagliardo (repeat from March 28, 1997)
    Mystery: TWA Flight 800
    Update: Wanted: Samuel Glover and Marshall Kirkpatrick
    Mystery: Candy and Roxy
    Legend: The ghosts of the Smith Home

Episode 273

Aired: April 11, 1997

    Lost: Jeanette Federico
    Lost: Susan Harrison (repeat from November 8, 1996)
    Wanted: Karen Pelletiere 
    Lost: William Pelletiere (repeat from November 8, 1996)

Advertisement
Episode 274

Aired: April 18, 1997

    Mystery: Rosemary Altea
    Mystery: Blair Adams
    Lost: The Son of Bobbi Page Myler
    Legend: Men In Black
    Update: Wanted: Dennis Kevin Smith
    Update: Lost: Candice and Sharina Berry
    Update: Lost: The Siblings of Jeff Linn

Episode 275

Aired: April 25, 1997

    Wanted: Don Davis Jr.
    Special Bulletin: Wanted: Trent Fouts
    Murder: Kristie Gunderson Lee (repeat from September 13, 1996)
    Mystery: Lynne Plaskett
    Mystery: Carolyn Reynolds links astrological charts to serial killers.
    Lost: Susan King

Episode 276

Aired: May 2, 1997

    Lost: Savior of Samantha Roberts
    Murder: Aimee Willard
    Wanted: David Thompson
    Mystery: Chase Bowman
    Mystery: Cynthia Armistead/Jayne Hitchcock
    Lost: The Daughter of Dave and Christie Carlsen (repeat from September 13, 1996)

Advertisement
Episode 277

Aired: May 9, 1997

    Legend: Amazon Women
    Mystery: The Oakville Blobs
    Mystery: David Shublak
    Murder: Dimitric Moore

Episode 278

Aired: May 16, 1997

    Wanted: Salvatore Spinnato
    Mystery: Reverend Ralph DiOrio
    Murder: Dana Satterfield
    Lost: The Friend of Moises Treves
    Wanted: Atlanta/Spokane Bomber
    Update: Wanted: David Gause

Episode 279

Aired: May 23, 1997

    Special Bulletin: Wanted: Jimmie Wayne Pierce (repeat from January 3, 1997)
    Mystery: Stephanie Booker (repeat from November 15, 1996)

Episode 280

Aired: August 8, 1997

    Lost: Therese Rose Walsh 
    Wanted: Merle Marie Vanderheiden (repeat from March 14, 1997)
    Wanted: Maria Hernandez (repeat from March 28, 1997)
    Wanted: Melvin Wilson (repeat from March 28, 1997)")

s10.info <- parse_epinfo("Season Ten (CBS) (1997-98)
Episode 281

Aired: November 13, 1997

    Legend: Elvis Presley
    Mystery: Joe McCarthy, Herman Stegos, and Cheryl Landon
    Mystery: Monika Rizzo
    Murder: Candy Belt and Gloria Ross

Advertisement
Episode 282

Aired: April 3, 1998

    Mystery: Joffre Ramos 
    Wanted: Luie Quezada
    Special Bulletin: Wanted: Michelle Abraham 
    Lost: Chrystal Didonato and Jessica Abraham
    Lost: Mary Agnes Gross
    Murder: Kathy Page
    Mystery: Michael Drosnin

Episode 283

Aired: April 10, 1998

    Wanted: Michael Wayne Brown 
    Lost: Donna Moses Brown
    Special Bulletin: Wanted: Ngoc Van Tran
    Wanted: Marvin Gabrion 
    Lost: Shannon Verhage, Wayne Davis, John Weeks, Robert Allen 
    Murder: Rachel Timmerman
    Mystery: Pat Brown
    Legend: Nanteos Cup

Episode 284

Aired: April 17, 1998

    Lost: Wendy Von Huben 
    Murder: Jesse Howell
    Legend: Woods Home
    Special Bulletin: Wanted: Jose Blandon
    Special Bulletin: Wanted: Marshall Lee Brown

Advertisement
Episode 285

Aired: May 22, 1998

    Legend: Chair of Death
    Lost: Savior of Wilma Drew
    Mystery: Katherine Korzilius
    Wanted: Paul Ragusa
    Wanted: Francis C. Buhay
    Update: Wanted: Trent Fouts

Episode 286

Aired: May 29, 1998

    Legend: Phoenix Lights
    Lost: The Children of Doreen Espinosa
    Wanted: Sharon Kinne
    Update: Wanted: Gary and Ted Noble
    Murder: Richard Aderson Road Rage Incident
    Murder: Robert James Road Rage Incident")

s11.info <- parse_epinfo("Season Eleven (CBS) (1999)
Episode 287

Aired: April 2, 1999

    Murder: Brook Baker
    Murder: Bugsy Siegel
    Lost: Sabrina Aisenberg
    Mystery: Audrey Santo
    Mystery: Henry Weingarten

Advertisement
Episode 288

Aired: April 9, 1999

    Wanted: Jason McVean and Alan Pilon
    Legend: The Skunk Ape
    Legend: The ghosts of the Delta Queen Riverboat
    Murder: Michelle Witherell
    Mystery: Nicolai Levashov

Episode 289

Aired: April 16, 1999

    Mystery: Jayson Artis
    Lost: Amy Wroe Bechtel
    Murder: Chansami and Abby Thammavong
    Legend: The ghosts of Loews Cottage
    Mystery: John Holland and Elizabeth Joyce

Episode 290

Aired: May 28, 1999

    Wanted: Alan Golder
    Murder: Ted Binion
    Mystery: Nostradamus and New Millennium Predictions
    Lost: Amy Bradley
    Lost: Saviors of Doris Smith
    Mystery: Thomas Wright

Advertisement
Episode 291

Aired: June 4, 1999

    Legend: Walk-Ins
    Murder: Jessica Keen
        Lost: Lost Love Benjamin Baker
        Lost: Lost Love Gerry and Terry Robinson
    Mystery: Norman and Lulu
    Wanted: Rick Vallee

Episode 292

Aired: June 11, 1999

    Murder: Cam Lyman
    Lost: Ok-Cha Wortman
    Mystery: Charlene Richard
    Mystery: HOPE
    Lost: Jim Wood")

s12.info <- parse_epinfo("Season Twelve (Lifetime) (2001-02)
Episode 293

Aired: July 2, 2001

    Murder: Linda Sherman

Episode 294

Aired: July 3, 2001

    Lost: Lauria Bible and Ashley Freeman
    Murder: Danny and Kathy Freeman

Episode 295

Aired: July 4, 2001

    Murder: Bryan Nisenfeld

Episode 296

Aired: July 5, 2001

    Lost: Traci Kenley and Bill Rundle

Episode 297

Aired: July 9, 2001

    Lost: The Family of Dolores Camarena
    Lost: Amy Wroe Bechtel (repeat from April 16, 1999)

Episode 298

Aired: July 10, 2001

    Wanted: James Sullivan
    Lost: Amy Bradley (repeat from May 28, 1999)

Advertisement
Episode 299

Aired: July 11, 2001

    Mystery: Eagle

Episode 300

Aired: July 12, 2001

    Lost: Trisha Autry

Episode 301

Aired: July 16, 2001

    Mystery: Dee Klepper and Gus Ortega

Episode 302

Aired: July 17, 2001

    Wanted: Craig Pritchert and Nova Guthrie

Episode 303

Aired: July 18, 2001

    Lost: Landon and Logan Walker
    Wanted: Craig Walker
    Wanted: Alan Golder (repeat from May 28, 1999)

Advertisement
Episode 304

Aired: July 19, 2001

    Wanted: Jerry Lee Bowen

Episode 305

Aired: July 20, 2001

    Lost: Kristen Modafferi

Episode 306

Aired: July 23, 2001

    Legend: The ghosts of the Lizzie Borden House

Episode 307

Aired: July 24, 2001

    Mystery: Sonya Fitzpatrick

Episode 308

Aired: July 25, 2001

    Legend: Jones House

Episode 309

Aired: July 26, 2001

    Mystery: Yefim Shubentsov

Advertisement
Episode 310

Aired: July 30, 2001

    Wanted: John Addis
    Murder: Joann Albanese

Episode 311

Aired: August 1, 2001

    Legend: The ghosts of the Myrtles Plantation
    Murder: Judith Smith

Episode 312

Aired: August 2, 2001

    Wanted: Jesse James Hollywood
    Murder: Jessica Keen (repeat from June 4, 1999)

Episode 313

Aired: August 6, 2001

    Wanted: Desiree Lingo-Perkins
    Wanted: Carl McWilliams
    Mystery: Jayson Artis (repeat from April 16, 1999)

Episode 314

Aired: August 7, 2001

    Lost: Wil Hendrick
    Murder: Richard Aderson
    Murder: Robert James (repeat from May 29, 1998)
    Legend: Nazca Lines (repeat from April 19, 1996)
    Mystery: Logan Carroll (repeat from November 3, 1995)

Advertisement
Episode 315

Aired: August 8, 2001

    Lost: Debra and Diana Cordova

Episode 316

Aired: August 9, 2001

    Wanted: Gordon Weaver
    Wanted: James Bulger (repeat from November 22, 1996)

Episode 317

Aired: August 13, 2001

    Lost: Leah Roberts

Episode 318

Aired: August 14, 2001

    Wanted: Arthur Lopez Jr.

Episode 319

Aired: August 15, 2001

    Mystery: Natasha Jennings

Advertisement
Episode 320

Aired: August 16, 2001

    Wanted: Clayton Waagner

Episode 321

Aired: August 20, 2001

    Murder: Jodie Bordeaux

Episode 322

Aired: August 21, 2001

    Mystery: Dr. Emily Craig
    Lost: Diane Washer
    Murder: Nancy Daddysman

Episode 323

Aired: August 22, 2001

    Wanted: Malaika Griffin

Episode 324

Aired: August 23, 2001

    Mystery: Karen and Kathy Mills
    Wanted: Shannon Kinne (repeat from May 29, 1998)
    Mystery: Carolyn Hebert/Elaine Emmi/Linda Babb (repeat from March 23, 1994)
    Murder: Joe Cole (repeat from May 17, 1996)

Advertisement
Episode 325

Aired: August 24, 2001

    Mystery: Darlie Routier
    Murder: Devon and Damon Routier

Episode 326

Aired: August 27, 2001

    Lost: David Shipenburg
    Wanted: Jason McVean and Alan Pilon (repeat from April 9, 1999)

Episode 327

Aired: August 28, 2001

    Lost: Morgan Nick
    Lost: Jacqueline Castaneda

Episode 328

Aired: August 29, 2001

    Murder: Warren Fulton and Rachael Raver
    Murder: Tina Jefferson
    Update: Lost: Susan King
    Mystery: Candy and Roxy (repeat from April 4, 1997)
    Murder: Martha Moxley (repeat from February 16, 1996)

Episode 329

Aired: August 31, 2001

    Lost: Claudia Kirschhoch
    Legend: Fertility Statues (repeat from October 18, 1996)
    Wanted: Raymond Young (repeat from November 17, 1993)
    Mystery: Patricia Stallings (repeat from May 8, 1991)

Advertisement
Episode 330

Aired: September 4, 2001

    Lost: Margie Jelovcic 
    Wanted: Randy Yager

Episode 331

Aired: September 5, 2001

    Wanted: Carlos Berdeja
    Wanted: Michael Wayne Brown 
    Lost: Donna Moses Brown (repeat from April 10, 1998)
    Mystery: Lonnie Zamora UFO Sighting (repeat from December 1, 1995)
    Wanted: William Jordan (repeat from September 20, 1996)

Episode 332

Aired: September 6, 2001

    Mystery: Michael Owen and Wendy Throop
    Lost: Gordon Page Jr. (repeat from October 28, 1994)
    Lost: Ok-Cha Wortman (repeat from June 6, 1999)

Episode 333

Aired: September 7, 2001

    Wanted: Karl and Helen Rehberg
    Lost: Ok-Cha Wortman (repeat from June 6, 1999)

Advertisement
Episode 334

Aired: September 10, 2001

    Lost: Jill Behrman

Episode 335

Aired: September 11, 2001

    Mystery: Lois Gibson
    Mystery: Jason and Phillip Bomer

Episode 336

Aired: September 12, 2001

    Murder: Mike Emert
    Wanted: Maria Hernandez (repeat from March 28, 1997)
    Lost: Michelle Fazzani (repeat from August 12, 1992)
    Mystery: West End Baptist Church (repeat from January 3, 1990)

Episode 337

Aired: October 1, 2001

    Wanted: Michael Alfonso

Episode 338

Aired: October 2, 2001

    Wanted: Heather Tallchief and Roberto Solis

Advertisement
Episode 339

Aired: October 3, 2001

    Wanted: Dr. Gregory Caplinger

Episode 340

Aired: October 4, 2001

    Lost: Lenny Dirickson
    Update: Wanted: Michael Wayne Brown 
    Lost: Donna Moses Brown
    Lost: Kristi Krebs (repeat from February 17, 1995)
    Lost: Sabrina Aisenberg (repeat from April 2, 1999)
    Murder: Lynn Amos (repeat from December 13, 1996)

Episode 341

Aired: October 15, 2001

    Lost: Star Palumbo
    Update: Wanted: Elizabeth Ortiz 
    Lost: Jonathan Ortiz
    Lost: Mary Agnes Gross (repeat from April 3, 1998)
    Mystery: Bill and Dorothy Wacker (repeat from May 25, 1994)
    Lost: April Gregory 
    Lost: Kristin Smart (repeat from September 20, 1996)

Episode 342

Aired: October 16, 2001

    Lost: Curtis Pishon

Advertisement
Episode 343

Aired: October 17, 2001

    Lost: Cindy Wismiller

Episode 344

Aired: October 18, 2001

    Lost: Wendi Long

Episode 345

Aired: November 19, 2001

    Wanted: Regina and Margaret Defrancisco

Episode 346

Aired: November 26, 2001

    Mystery: Harper's Ferry Remains

Episode 347

Aired: April 29, 2002

    Murder: Megan Curl
    Mystery: George Reeves (repeat from December 8, 1995)
    Update: Wanted: Don Davis Jr.")

s13.info <- parse_epinfo("Season Thirteen (Lifetime) (2002)
Episode 348

Aired: June 10, 2002

    Lost: Chandra Levy and Joyce Chiang
    Murder: Christine Mirzayan

Advertisement
Episode 349

Aired: June 11, 2002

    Lost: Dr. Sneha Philip

Episode 350

Aired: June 12, 2002

    Wanted: Eric Rudolph
    Mystery: Bruce Kelly (repeat from September 22, 1993)
    Murder: Chad Maurer (repeat from December 11, 1991)

Episode 351

Aired: June 13, 2002

    Lost: Amber Swartz, Ilene Misheloff, Nikki Campbell, Michaela Garecht 
    Murder: Angela Bugay
    Mystery: Wyrick House (repeat from October 21, 1994)
    Lost: Gus Hoffman (repeat from April 26, 1989)

Episode 352

Aired: June 14, 2002

    Wanted: Frank Montenegro
    Update: Murder: Mickey and Trudy Thompson

Episode 353

Aired: June 17, 2002

    Murder: Mary Morris and Mary Morris

Advertisement
Episode 354

Aired: June 18, 2002

    Wanted: Robert Fisher

Episode 355

Aired: June 19, 2002

    Wanted: Stephen Anderson

Episode 356

Aired: June 20, 2002

    Wanted: Anthrax Killer

Episode 357

Aired: June 21, 2002

    Mystery: The Family of Monica Libao

Episode 358

Aired: June 24, 2002

    Wanted: Ann Kibalo
    Lost: Samantha Kibalo

Episode 359

Aired: June 25, 2002

    Lost: Holly Krewson

Advertisement
Episode 360

Aired: June 26, 2002

    Mystery: Jessica Constant and Albert Wong

Episode 361

Aired: June 27, 2002

    Wanted: Kristine Westin and Kevin Woo

Episode 362

Aired: June 28, 2002

    Update: Mystery: Albert DeSalvo
    Murder: Mary Sullivan

Episode 363

Aired: July 1, 2002

    Wanted: Frederick Russell
    Lost: Savior of Steve Newton (repeat from November 15, 1996)
    Wanted: Rafael Camarena (repeat from April 24, 1991)

Episode 364

Aired: July 2, 2002

    Wanted: David Kemp
    Murder: Chaim Weiss (repeat from May 6, 1992)
    Wanted: Mahfuz Huq (repeat from September 23, 1992)
    Mystery: Tony Marabella (repeat from April 12, 1996)

Advertisement
Episode 365

Aired: July 3, 2002

    Wanted: Omar Arroyo

Episode 366

Aired: July 8, 2002

    Wanted: Stryder Styarfyr
    Wanted: Hazel Head

Episode 367

Aired: July 12, 2002

    Wanted: James Kilgore

Episode 368

Aired: July 15, 2002

    Mystery: Mia and Shadow

Episode 369

Aired: July 17, 2002

    Legend: The ghosts of Black Hope

Advertisement
Episode 370

Aired: July 19, 2002

    Mystery: Gazebo Baby
    Lost: The Parents of Kimberly Smith
    Wanted: Richard Cepulonis/Karen Walters (repeat from March 15, 1996)
    Legend: Chupacabras (repeat from April 26, 1996)
    Wanted: Tom Johnson (repeat from April 27, 1994)

Episode 371

Aired: July 22, 2002

    Wanted: Original Night Stalker in Sacramento
    Murder: Robert Hamrick (repeat from November 20, 1991)
    Mystery: E.L.F. (repeat from May 19, 1995)

Episode 372

Aired: July 26, 2002

    Murder: Eric Tamiyasu

Episode 373

Aired: July 29, 2002

    Legend: Mothman

Advertisement
Episode 374

Aired: August 2, 2002

    Legend: Canada Crop Circles

Episode 375

Aired: August 8, 2002

    Wanted: Jon and Molly Maggio

Episode 376

Aired: August 9, 2002

    Lost: Colleen Wood
    Mystery: Sam Zelikson (repeat from October 25, 1996)
    Murder: Jamie Santos (repeat from January 10, 1997)
    Wanted: Greg Webb (repeat from October 18, 1989)

Episode 377

Aired: August 12, 2002

    Murder: Opal ZachariasLance Bedgood
    Lost: Rachel Cooke
    Wanted: Sharon Rogers Car Bomber (repeat from February 17, 1993)
    Mystery: Bobby Fuller (repeat from October 18, 1996)
    Mystery: Luiz Gasparetto (repeat from March 24, 1995)

Advertisement
Episode 378

Aired: August 16, 2002

    Wanted: William Fischer

Episode 379

Aired: August 19, 2002

    Wanted: Frank Isley
    Lost: Lisa Myers

Episode 380

Aired: August 20, 2002

    Update: Wanted: Lyle Moody
    Murder: Donna Baldeo, Jailall Lewis, and Bunnie Terry

Episode 381

Aired: August 21, 2002

    Murder: Janice and Alyssa Owen

Episode 382

Aired: August 22, 2002

    Murder: Erica RichardsonJohn Feiga

Advertisement
Episode 383

Aired: August 23, 2002

    Update: Lost: Wendi Long
    Murder: Carson Prince
    Lost: Le-Zhan Williams
    Murder: Daphne Boyden (repeat from November 22, 1996)
    Mystery: Stuart Heaton
    Murder: Krystal Naab (repeat from December 16, 1994)

Episode 384

Aired: August 26, 2002

    Wanted: Margo Freshwater

Episode 385

Aired: August 27, 2002

    Wanted: Joe Morrow
    Mystery: Linda Tellington-Jones (repeat from November 17, 1995)
    Wanted: Brad Bishop (repeat from January 9, 1991)
    Wanted: David Viera (repeat from April 13, 1994)

Episode 386

Aired: August 28, 2002

    Wanted: Rufino Castaneda
    Mystery: Rick McCue
    Murder: Alene Courchesne (repeat from January 6, 1993)
    Lost: The Friends of Fritz Vincken (repeat from March 24, 1995)

Advertisement
Episode 387

Aired: August 29, 2002

    Murder: Diane Shawcroft and Jennifer Lueth

Episode 388

Aired: September 3, 2002

    Murder: Damien Corrente
    Wanted: Juan Gil Ferrufino, Mario Portillo and German DeLeon
    Mystery: Carolyn Reynolds (repeat from April 25, 1997)
    Lost: Saviors of Colleen Frangione (repeat from December 1, 1993)
    Lost: Clifford Sherwood and George Gumbly (repeat from October 7, 1992)

Episode 389

Aired: September 4, 2002

    Murder: Bonnie Craig
    Wanted: Randolph Dial
    Lost: Bobbi Parker (repeat from March 17, 1995)
    Mystery: Ted Loseff (repeat from October 13, 1993)

Episode 390

Aired: September 5, 2002

    Lost: Molly Bish

Episode 391

Aired: September 6, 2002

    Mystery: Fallon Cancer

Advertisement
Episode 392

Aired: September 16, 2002

    Lost: Dale Williams

Episode 393

Aired: September 17, 2002

    Wanted: Laura Law
    Legend: Padre Pio (repeat from January 20, 1995)
    Wanted: Ngoc Van Tran (repeat from April 10, 1998)
    Wanted: Maria Armstrong (repeat from November 29, 1989)

Episode 394

Aired: September 18, 2002

    Lost: Cindy Song

Episode 395

Aired: September 19, 2002

    Lost: Niqui McCown

Episode 396

Aired: September 20, 2002

    Lost: Miranda Gaddis and Ashley Pond")
# s14.info <- parse_epinfo("")
# s15.info <- parse_epinfo("")
# s16.info <- parse_epinfo("")

s2.info <- parse_epinfo("Season Two (1989-1990)
Episode 29

Aired: September 20, 1989

    Update: Wanted: Charles Mule
    Lost: Lost Loves Lt. Karen Stephens
    Legend: Roswell Crash
    Lost: Tara Calico and Michael Henley

Episode 30

Aired: September 27, 1989

    Murder: Jack Brown
    Mystery: Blinking Crucifix
    Update: Wanted: Sheldon Weinberg
    Wanted: Robert Dennie
    Wanted: Melvin Edward Mays
    Wanted: Leslie Isben Rogge
    Wanted: William Eugene Hilliard

Episode 31

Aired: October 11, 1989

    Murder: Kay Hall
    Update: Lost: Lost Loves Lt. Karen Stephens
    Mystery: New York Coin Scam
    Mystery: Mabel Woods
    Lost: The Crew of the Sara Joe
    FBI Alerts: Lost: Avery James Norris

Advertisement
Episode 32

Aired: October 18, 1989 (repeated March 14, 1990)

    Wanted: Greg Webb
    Lost: Billie and Joey Rogers
    Update: Wanted: John Mooney
    Wanted: Gary and Terry Magno
    OTHER: Gene Kiley tracks down owners of unclaimed money

Episode 33

Aired: October 25, 1989 (repeated February 28, 1990)

    Lost: Dale Kerstetter
    Update: Lost: The Brother of Sylvia Wemhoff
    Legend: Marfa Lights
    Murder: Jay Cook and Tanya Van Cuylenborg
    Wanted: Bonnie Wilder
        Lost: Julie Weflen
        Lost: Stefanie Stroh
        Lost: Kyle Clinkscales
        Lost: Carlos Alvarez
        Wanted: Jose Alvarez and Juan Cristo
        Lost: Diana Braungardt
        Lost: David Tyll and Brian Ognjan
        Lost: John Simmons
        Lost: Lily Mae Huff
        Lost: David Thies
        Lost: Susan Cappel

Advertisement
Episode 34

Aired: November 1, 1989 (repeated March 7, 1990)

    Update: Wanted: Chevy Chase Bandit
    Update: Wanted: Louis Carlucci
    Mystery: Rudolf Hess
    Lost: Patricia Meehan
    Murder: Tina Jefferson
    Update: Wanted: Gary and Terry Magno
    Update: Lost: Billie and Joey Rogers

Episode 35

Aired: November 8, 1989 (repeated April 11, 1990)

    Mystery: Doyle Wheeler
    Wanted: Salvatore Caruana
    Wanted: Douglas Alan Costa
    Wanted: Lewis Michael Geiger
    Wanted: Lavada Floyd
    Wanted: Richard Green
    Wanted: Richard Joseph Alvarado
    Wanted: William McGeehee
    Wanted: Alvaro Zapata
    Update: Wanted: Charles Wickman
    Murder: Rachael Runyan
    Wanted: Gene Flannes

Episode 36

Aired: November 15, 1989 (repeated April 18, 1990)

    Murder: Rhonda Hinson
    Update: Wanted: William Eugene Hilliard
    Wanted: Ann Corricelli and Lena Marie Wilson
    Lost: Heirs of Howard Drummond
    Mystery: Don Hamilton

Advertisement
Episode 37

Aired: November 22, 1989

    Mystery: Sonny Liston
    Update: Lost: Billie and Joey Rogers
    Lost: Bill and Cynthia Zelinski
    Lost: Elizabeth Campbell
    Wanted: Crash and Dash Robberies

Episode 38

Aired: November 29, 1989 (repeated May 9, 1990)

    Wanted: Maria Armstrong
    Update: Wanted: Robert Dennie
    Lost: The Friend of Stephan Ross
    Murder: Ralph Sigler
    Wanted: Charles Wilson Chester
    Wanted: Tandem Bandits
    Wanted: Medusa Bandit
    Wanted: Garbage Bag Bandit
    Wanted: Shorts Robber

Episode 39

Aired: December 6, 1989 (repeated May 23, 1990)

    Wanted: John Hawkins
    Wanted: Woody Kelly
    Wanted: David Fisher
    Murder: Laura Burbank
    Murder: Carla Wright
    Murder: Michella Welch
    Murder: Jenny Bastian
    Lost: The Siblings of LeeAnn Robinson
    Update: Wanted: Leslie Isben Rogge

Advertisement
Episode 40

Aired: December 13, 1989 (repeated May 18, 1990)

    Wanted: Ed Barbara
    Update: Lost: Avery Norris
    Mystery: Rae Ann Mossor
    Lost: The Children of Georgia Tann
    Wanted: Donald Eugene Webb

Episode 41

Aired: December 20, 1989

    Mystery: Sharon Elliott
    Mystery: John Branion
    Murder: Donna Branion
    Lost: The Daughter of Joseph Schambier

Episode 42

Aired: January 3, 1990 (repeated June 6, 1990)

    Lost: Cynthia Anderson
    Update: Wanted: Ann Corricelli and Lena Marie Wilson
    Mystery: West End Baptist Church
    Wanted: Melvine Aprile
    Lost: Tony and Sheri Aprile
    Murder: The Signal Mountain Murders
    Wanted: Wardell Ford/Larry Chism

Advertisement
Episode 43

Aired: January 10, 1990

    Murder: George Conniff
    Update: Wanted: Liz Carmichael
    Murder: Jack Brown (repeat from September 27, 1989)
    Legend: Blinking Crucifix (repeat from September 27, 1989)
    Wanted: William Eugene Hillard (repeat from September 27, 1989)

Episode 44

Aired: January 17, 1990 (repeated June 13, 1990)

    Wanted: Brayman Road Attacker
    Lost: Ray Hickingbotham
    Update: Wanted: Ann Sigmin and Garey Goff
    Murder: Kevin Hughes
    Lost: Robert Miller

Episode 45

Aired: January 24, 1990

    Murder: Mark Groezinger
    Legend: Roswell Crash (repeat from September 20, 1989)
    Lost: Tara Calico and Michael Henley (repeat from September 20, 1989)
    Lost: Leticia Hernandez
    Lost: Jessica Gutierrez
    Lost: David Borer
    Lost: Malakia Logan

Advertisement
Episode 46

Aired: January 31, 1990

    Legend: United Kingdom Crop Circles
    Murder: Kenneth Dungee
    Update: Lost: Bill and Cynthia Zelinski
    Lost: Keith Reinhard
    Mystery: Tom Young
    Wanted: William Slagle/Rafael Rodriguez

Episode 47

Aired: February 7, 1990

    Murder: Charles Morgan
    Legend: the story of Ogopogo
    Update: Lost: The Siblings of LeeAnn Robinson
    Wanted: Marvin and Sandra Maple
    Lost: Kristi and Bobby Baskin

Episode 48

Third Anniversary Show Aired: February 11, 1990 (repeated July 4, 1990)

    Update: Wanted: Robert Weeks
    Update: Wanted: Steven Cox
    Update: Wanted: John Mooney
    Update: Legend: Victorio Peak Treasure
    Update: Wanted: John Burns
    Update: Wanted: David Davis
    Update: Murder: Shannon Davis
    Update: Lost: Billie and Joey Rogers
    Update: Lost: Lt. Karen Stephens
    Lost: The Family of Pat Mealbach
    Mystery: Jenny Pratt

Advertisement
Episode 49

Aired: February 14, 1990 (repeated July 11, 1990 and September 8, 1991)

    Mystery: Georgia Rudolph
    Murder: Steve Sandlin
    Wanted: John \"Thumper\" Brown

Episode 50

Aired: February 21, 1990

    Wanted: Joe Owens
    Lost: The Nanny of Jackie Cooper
    Lost: Alejandro Espinosa
    Wanted: Dale Hyde
    Update: Wanted: Marvin and Sandra Maple
    Update: Lost: Kristi and Bobby Baskin

Episode 51

Aired: March 21, 1990

    Lost: Linda Sharp
    Update: Wanted: David Fisher
    Update: Murder: Laura Burbank
    Wanted: Gertrude Pruett
    Murder: Dan Short
    Mystery: William L. Toomey

Episode 52

Aired: March 28, 1990 (repeated July 25, 1990)

    Lost: Laurence Harding Jr.
    Wanted: Pedro Uribe/Hugo Balbin/Luis and Ivan Arango/Miguel Villegas
    Wanted: Elmer Locker Jr./John and Cecilia Kealing
    Wanted: Sweetheart Swindler
    Wanted: Gainesville Killers
    Update: Wanted: Fumbles

Advertisement
Episode 53

Aired: April 4, 1990

    Update: Wanted: John \"Thumper\" Brown
    Mystery: Kristina Smith
    Update: Murder: Kay Hall
    Mystery: New York Coin Scam (repeat from October 11, 1989)
    Mystery: Mabel Woods (repeat from October 11, 1989)

Episode 54

Aired: April 25, 1990

    Mystery: Teresita Basa
    Update: Lost: Linda Sharp
    Wanted: Dewey Demetro and Leo Johnson Jr.
    Lost: Jean Anne Freeman
    Murder: The Las Cruces Bowling Alley Massacre

Episode 55

Aired: May 2, 1990

    Mystery: Mark S. Newman and Gerald I. Levy
    Mystery: Donald M. and Louis G. Keith
    Mystery: Lavona and Lavelda Rowe-Richardson
    Update: Wanted: Elmer Locker Jr./John and Cecilia Kealing
    Wanted: Jesus Penalver
    Wanted: Julio Marco Cruz
    Wanted: Pedro Nyego
    Wanted: Blas Canedo
    Wanted: Pablo Rodriguez
    Wanted: Boston Rapist
    Legend: The Bannack Treasure

Advertisement
Episode 56

Aired: May 16, 1990

    Mystery: Robert Kennedy
    Lost: Jackie Harrington (repeat from February 1, 1989)
    Lost: Elizabeth Campbell (repeat from November 22, 1989)
    Update: Lost: Avery Norris (repeat from December 13, 1989)

Episode 57

Aired: August 22, 1990

    Murder: Ralph Probst
    Mystery: Christina Smith (repeat from April 4, 1990)
    Wanted: Dale Hyde (repeat from February 21, 1990)
    Update: Wanted: Fumbles (repeat from March 28, 1990)
    Wanted: Crash and Dash Robberies (repeat from November 22, 1989)

Episode 58

Aired: September 5, 1990

    Lost: Marlena Childress
    Mystery: Sonny Liston (repeat from November 22, 1989)
    Update: Wanted: Gertrude Pruett
    Lost: Jean Anne Freeman (repeat from April 25, 1990)

Episode 59

Aired: September 12, 1990

    Murder: Gretchen Burford
    Update: Legend: United Kingdom Crop Circles
    Update: Lost: Robert Miller
    Lost: Micki Jo West (repeat from January 25, 1989)")


s3.info <- parse_epinfo("Season Three (1990-91)
Episode 60

Aired: September 19, 1990

    Legend: Kecksburg UFO
    Update: OTHER: The Daughter of Lavar Bates
    Wanted: Stockton Arsonist
    Wanted: Mike Cline/Raymond Scoville/Thomas Geers/Michael Lassen

Episode 61

Aired: September 26, 1990 (repeated December 26, 1990)

    Legend: Bermuda Triangle
    Wanted: Clay Taylor
    Lost: Thomas Heck
    Wanted: Shooters of C.W. Roddy

Episode 62

Aired: October 3, 1990

    Update: Legend: Gulf Breeze UFO
    Wanted: Dr. Kenneth Frank
    Lost: Dan Wilson
    Wanted: Minnesota Brinks Heist
    Update: Wanted: Stockton Arsonist

Episode 63

Aired: October 10, 1990 (repeated March 6, 1991)

    Murder: Stanley Gryziec
    Update: Wanted: Ed Barbara
    Wanted: Kevin Poulsen
    Lost: Sharita Harding
    Mystery: KROQ Confession

Episode 64

Aired: October 17, 1990 (repeated March 13, 1991)

    Lost: Lisa Bishop
    Lost: The Crew of the Freedon
    Wanted: Florian Bourch
    Update: Wanted: Maria Armstrong
    Lost: Adam Hecht
    Legend: Lunersee Lake Treasure
    Wanted: Edgar Kerns/Kay Beeman
    Update: Lost: Sharita Harding

Episode 65

Aired: October 24, 1990 (repeated April 10, 1991)

    Murder: Pamela Mason/Rena Paquette/Danny Paquette
    Update: Lost: The Children of Georgia Tann
    Wanted: Robert Corrado
    Lost: Martha Hinkle
    Wanted: Kenneth Stanton

Episode 66

Aired: October 31, 1990

    Legend: the Gray Man's Ghost at Pawley's Island
    Lost: David Stone
    Mystery: Katie
    Update: Mystery: Georgia Rudolph
    Update: Wanted: Kenneth Stanton

Episode 67

Aired: November 7, 1990

    Update: Lost: Kari Lynn Nixon
    Legend: Amelia Earhart
    Wanted: Richard Church
    Murder: Raymond and Ruth Ann Ritter
    Mystery: Dr. Iben Browning

Episode 68

Aired: November 14, 1990 (repeated April 17, 1991)

    Lost: The Sons of Jim Fontes
    Wanted: Catherine and James Durkin
    Update: Wanted: Stockton Arsonist
    Murder: O'Neal Moore
    Mystery: Victoria Doroshenko
    Wanted: Cowboy Bandit

Episode 69

Aired: November 21, 1990

    Mystery: Norman Ladner
    Update: Lost: Sharita Harding
    Murder: Dwayne McCorkendale
    Legend: Loretto Chapel
    Lost: Nyleen Marshall

Episode 70

Aired: November 28, 1990 (repeated May 29, 1991)

    Update: Wanted: Melvine Aprile
    Update: Lost: Tony and Sheri Aprile
    Mystery: Coral Polge
    Mystery: Johnny Lee Wilson
    Murder: Pauline Martz
    Mystery: Kenneth Engie

Episode 71

Aired: December 5, 1990 (repeated June 5, 1991)

    Murder: Morris Davis
    Legend: Skeleton Canyon Treasure
    Wanted: Attacker of Debbie
    Lost: Heirs of Dorothea Allen

Episode 72

Aired: December 12, 1990 (repeated June 26, 1991)

    Lost: Mark Dennis
    Update: Wanted: Kenneth Stanton
    Wanted: Ricardo Caputo
    Mystery: Vancouver Lights

Episode 73

Aired: December 19, 1990 (repeated June 12, 1991)

    Mystery: Debbie Wolfe
    Update: Wanted: Edgar Kerns and Kay Beeman
    Lost: Conradina Olson
    Lost: Geoffrey Sullivan

Episode 74

Aired: January 2, 1991

    Lost: Judy Hyams
    Murder: Ralph Probst (repeat from August 22, 1990)
    Mystery: Mark S. Newman and Gerald I. Levy, Donald M. and Louis G. Keith, Lavona and Lavelda Rowe-Richardson(repeat from May 2, 1990)
    Murder: Gretchen Burford (repeat from September 12, 1990)

Episode 75

Aired: January 9, 1991 (repeated June 19, 1991)

    Mystery: Russell Evans
    Update: Lost: The Sons of Jim Fontes
    Wanted: Brad Bishop
    Legend: Adam's Treasure
    Wanted: Thomas Hickey and William McCarthy
    Wanted: Arthur Washington Jr.
    Wanted: Juan Jackson

Episode 76

Aired: January 23, 1991 (postponed on January 16, 1991)

    Wanted: Patrick Michael Mitchell
    Wanted: Jockey Bandit/ Mr. Nasty/ Mary Poppins Bandit
    Lost: The Family of Victor Simon
    Wanted: Joe Smith
    Legend: Kelsey House

Episode 77

Aired: January 30, 1991

    Murder: Tracy Kirkpatrick
    Update: Legend: Amelia Earhart
    Wanted: Steve Wilson
    Lost: Frank Bloomer
    Wanted: John Irwin
    Murder: Dorothea Irwin

Episode 78

Aired: February 6, 1991 (repeated July 10, 1991)

    Mystery: Betty Cash and Vickie Landrum
    Update: Lost: Monica Bonilla
    Update: Lost: Nyleen Marshall
    Wanted: Obia-Man
    Lost: Vicki Acs

Episode 79

Aired: February 13, 1991 (repeated July 17, 1991)

    Mystery: Cindy James
    Lost: Paloma Gibson
    Wanted: Melody Martin
    Legend: Butch Cassidy
    Lost: The Siblings of Aleatha Evertz

Episode 80

Aired: February 20, 1991

    Legend: The Butcher Of Kingsbury Run
    Wanted: Chuck Smith
    Lost: Charles and Christopher Smith
    Murder: Michael Francke
    Update: Wanted: Thomas Hickey and William McCarthy
    Update: Wanted: Juan Jackson
    Lost: Oded Gordon

Episode 81

Aired: February 27, 1991 (postponed on January 23, 1991)

    Wanted: Edward Maynard
    Legend: The Kecksburg UFO (repeat from September 19, 1990)
    Update: OTHER: The Daugther of Lavar Bates (repeat from September 19, 1990)

Episode 82

Aired: March 20, 1991

    Mystery: Edgar Cayce
    Mystery: Crystal Spencer
    Update: Wanted: John Irwin
    Update: Wanted: Joe Smith
    Update: Lost: Judy Hyams
    Wanted: Dennis DePue

Episode 83

Aired: March 27, 1991

    Update: Wanted: Dennis DePue
    Wanted: Pat Fagan 
    Lost: Brandon Fagan
    Lost: Dan Wilson (repeat from October 3, 1990)
    Mystery: Vancouver Lights (repeat from December 12, 1990)
    Wanted: Dr. Kenneth Frank (repeat from October 3, 1990)

Episode 84

Aired: April 3, 1991 (repeated August 7, 1991)

    Wanted: Richard Bocklage
    Update: Wanted: Edward Maynard
    Lost: Lt. Paul Whipkey
    Mystery: Lt. Charlie Guess
    Lost: Gary Bickford
    Wanted: Astarte Davis
    Lost: Jim Rice
    Update: Wanted: Pat Fagan
    Lost: Brandon Fagan

Episode 85

Aired: April 24, 1991

    Wanted: Rafael Camarena
    Update: Wanted: Dennis DePue
    Lost: Duncan Gilmore
    Wanted: Gregory Barker
    Lost: The Parents of Jeri Graves

Episode 86

Aired: May 1, 1991

    Update: Mystery: KROQ Confession
    Update: Murder: Angela Cummings
    Wanted: Pizza Parlor Killer
    Wanted: The Countess
    Murder: Bobbie Oberholtzer and Annette Schnee
    Lost: The Sisters of Jackie Dragon
    Update: Wanted: Gregory Barker

Episode 87

Aired: May 8, 1991

    Mystery: Ryan Stallings/Patricia Stallings
    Legend: The disappearance of Amelia Earhart (repeat from November 7, 1990)
    Wanted: Minnesota Brinks Heist (repeat from October 3, 1990)

Episode 88

Aired: May 15, 1991

    Wanted: Tony Alamo
    Murder: Lester Garnier
    Mystery: Katie (repeat from October 31, 1990)
    Wanted: Richard Church (repeat from November 7, 1990)
    Murder: Raymond and Ruth Ann Ritter (repeat from November 7, 1990)

Episode 89

Aired: May 22, 1991

    Wanted: James Donald King
    Mystery: Norman Ladner (repeat from November 21, 1990)
    Legend: Loretto Chapel (repeat from November 21, 1990)
    Murder: Mark Groezinger (repeat from January 24, 1990)

Episode 90

Aired: August 28, 1991

    Lost: Melvin and Daniel Nellis
    Lost: Michael Rosenblum (repeat from January 11, 1989)
    Mystery: New York Coin Scam (repeat from October 11, 1989)
    Murder: The Signal Mountain Murders (repeat from January 3, 1990)

Episode 91

Aired: September 4, 1991

    Wanted: Lesa Lee
    Update: Wanted: Sweetheart Swindler
    Wanted: The Countess (repeat from May 1, 1991)
    Wanted: Tony Alamo (repeat from May 15, 1991)
    Wanted: Rafael Camarena (repeat from April 24, 1991)

Episode 92

Aired: September 11, 1991

    Update: Legend: Bermuda Triangle
    Update: Wanted: Kevin Poulsen
    Mystery: Ryan Stallings / Patricia Stallings (repeat from May 8, 1991)
    Wanted: Pat Fagan 
    Lost: Brandon Fagan (repeat from March 27, 1991)")

s4.info <- parse_epinfo("Season Four (1991-92)
Episode 93

Aired: September 18, 1991 (repeated December 25, 1991)

    Legend: Rendlesham Forest Incident
    Murder: Shane Stewart and Sally McNelly
    Lost: The Mother of Carla Downing

Episode 94

Aired: September 25, 1991 (repeated January 8, 1992)

    Mystery: John Wilkes Booth
    Update: Lost: Paloma Gibson
    Wanted: Melody Martin
    Wanted: Art Silva
    Lost: Alex Cooper

Episode 95

Aired: October 2, 1991 (repeated January 22, 1992)

    Mystery: Tyler
    Wanted: Joe Maloney
    Legend: The Shroud Of Turin
    Murder: Ethel Kidd
    Update: Wanted: Art Silva

Episode 96

Aired: October 9, 1991

    Wanted: Connecticut River Valley Killer
    Update: Lost: The Father of Jeannie Wagner
    Wanted: Judge John Fairbanks
    Lost: Paul and Paula Scribner
    Update: Mystery: Tyler

Episode 97

Aired: October 16, 1991

    Lost: Baron 52
    Wanted: David O'Neil
    Update: Lost: Martha Hinkle
    Wanted: Paul Stamper
    Wanted: Josephine White

Episode 98

Aired: October 23, 1991

    Lost: Tommy Gibson
    Update: Mystery: Tyler
    Wanted: Jim Burnside
    Lost: John Novotny
    Murder: Garry Gibson

Episode 99

Aired: October 30, 1991 (repeated March 11, 1992)

    Legend: The ghosts of the St. James Hotel
    Legend: The ghost of the Harden House
    Murder: John Harden
    Lost: Mary Ann Perez
    Wanted: Dr. John Anderson

Episode 100  

Aired: November 3, 1991 (repeated March 25, 1992)

    Mystery: Elliott Leyton, James Fox, Reid Maloy
    Wanted: Marie Hilley
    Mystery: G. Daniel Walker
    Murder: Joan, Michelle, and Christe Rogers

Episode 101

Aired: November 6, 1991

    Mystery: Sharon Johnson
    Mystery: Sugar
    Murder: Roger Dean
    Update: Wanted: Joe Smith
    Wanted: Liza Montgomery
    Lost: Deborah Poe
    Lost: Donna Callahan
    Murder: Darlene Messer
    Lost: Arthur Jones

Episode 102

Aired: November 10, 1991

    Lost: Sherry Eyerly
    Murder: Kathy Bonderson
    Mystery: Butcher Of Kingsbury Run (repeat from February 20, 1991)
    Mystery: Sarah DiGennaro

Episode 103

Aired: November 13, 1991

    Wanted: Televangelist Bomber
    Update: Mystery: Ryan Stallings/Patricia Stallings
    Lost: Savior of Cathy Loving
    Murder: Beverly McGowan
    Wanted: Robin Stevens and Sherry Seymour

Episode 104

Aired: November 20, 1991

    Legend: Belgium UFO
    Update: Lost: The Mother of Carla Downing
    Lost: Ellie Taylor
    Lost: Patrick Ackles
    Lost: Fred Gardner
    Mystery: Robert Hamrick
    Wanted: Philip Breen

Episode 105

Aired: November 27, 1991

    Wanted: Bo Tanner
    Lost: The Mother of Barbara Smith and Barbara Ratner
    Mystery: Jeffrey Digman
    Wanted: Church Arsonist
    Update: Wanted: Paul Stamper
    Legend: Trabuca Treasure
    Murder: Teresita Basa (repeat from April 25, 1990)

Episode 106

Aired: December 4, 1991

    Wanted: Michael St. Clair and Dennis Reese
    Mystery: Dan Tondevold
    Update: Lost: Melvin and Daniel Nellis
    Lost: Annie Currie
    Lost: Joey Moss
    Wanted: Jerry Moss
    Update: Wanted: Robin Stevens and Sherry Seymour

Episode 107

Aired: December 11, 1991 (repeated May 27, 1992)

    Mystery: Chad Maurer
    Update: Lost: John Novotny
    Lost: Lorene Roberts
    Wanted: Emma Figueroa
    Wanted: Luis Herrera
    Wanted: Pedro Pumajero
    Wanted: Jose Rubio
    Wanted: Eduwigis Escalante
    Wanted: Juan Carlos Pereira
    Lost: Heath Brian Vess

Episode 108

Aired: December 18, 1991

    Mystery: Larry Race/Debbie Race
    Update: Wanted: Jim Burnside
    Murder: Warner Jane Doe
    Wanted: Pat Farmer
    Lost: Jerad Peters
    Lost: Helen Rose

Episode 109

Aired: January 15, 1992

    Murder: Philip Innes Fraser
    Lost: The Crew of the Casie Nicole
    Lost: The Child of Mac McDonald
    Wanted: William John Wood

Episode 110

Aired: January 29, 1992 (90 mins)

    Lost: Madeline and Ada Underwood
    Wanted: Connecticut River Valley Killer (repeat from October 9, 1991)
    Murder: Kathy Bonderson (repeat from November 10, 1991)
    Legend: Adam's Treasure (repeat from January 9, 1991)
    Wanted: Todd Mueller
    Lost: Paul and Paula Scribner (repeat from October 9, 1991)

Episode 111

Aired: February 5, 1992

    Legend: Medjugorje Miracles
    Wanted: Levia Molinari
    Lost: Nicholas Karopoulos
    Lost: Angela Hammond
    Murder: Trudy Darby
    Lost: Cheryl Kenney
    Lost: Lee Young

Episode 112

Aired: February 12, 1992

    Lost: Rose Marie Platt
    Update: Wanted: Robin Stevens and Sherry Seymour
    Wanted: James White
    Legend: The Yeti aka Abominable Snowman
    Mystery: Stanton Bones

Episode 113

Aired: February 19, 1992

    Mystery: Wytheville UFO Sightings
    Update: Lost: Joey Moss
    Wanted: Charles Warren Boomer (a.k.a) The Satchel Bandit
    Wanted: Ohio Prostitute Killer
    Update: Wanted: Gregory Barker
    Update: Wanted: Dale Hyde
    Update: Wanted: John Irwin
    Legend: Beaty Castle
    Mystery: Robert Kennedy (repeat from May 16, 1990)

Episode 114

Aired: February 26, 1992

    Wanted: Valley Bank Robbery
    Lost: Angeline Dewey
    Update: Lost: Heath Brian Vess
    Wanted: Cheryl Holland

Episode 115

Aired: March 4, 1992

    Wanted: Pizza Restaurant Robbers
    Update: Lost: The Child of Mac McDonald
    Lost: James Russell
    Lost: Darlene and Davida Ebmeier
    Lost: Thomas Manion
    Lost: Madding Family
    Lost: Baron 52 (repeat from October 16, 1991)
    Wanted: Paul Stamper (repeat from October 16, 1991)
    Wanted: Lesa Lee (repeat from September 4, 1991)

Episode 116

Aired: March 18, 1992

    Wanted: Richard Minns
    Lost: Heirs of Katherine Bennett
    Update: Lost: Alex Cooper
    Wanted: Sergio Farina
    Lost: Marcus Farina

Episode 117

Aired: April 1, 1992

    Mystery: Santos Family
    Lost: Tommy Gibson (repeat from October 23, 1991)
    Update: Mystery: Tyler (repeat from October 23, 1991)
    Update: Wanted: Jim Burnside (repeat from October 23, 1991)
    Update: Lost: John Novotny (repeat from October 23, 1991)

Episode 118

Aired: April 8, 1992

    Murder: Marie Lilienberg and Maria Wahlén
    Update: Mystery: Amelia Earhart
    Lost: The Family of Joe Soll
    Wanted: Tom Dixon
    Murder: Gary Simmons
    Update: Lost: The Sisters of Jackie Dragon
    Wanted: Richard Condia

Episode 119

Aired: April 15, 1992

    Murder: Jeanne Tovrea
    Wanted: Judge John Fairbanks (repeat from October 9, 1991)
    Lost: Sherry Eyerly (repeat from November 10, 1991)
    Mystery: Sarah DiGennaro (repeat from November 10, 1991)

Episode 120

Aired: April 22, 1992

    Murder: Vallejo Armored Car Murders
    Mystery: Sharon Johnson (repeat from November 6, 1991)
    Mystery: Sugar (repeat from November 6, 1991)
    Murder: Roger Dean (repeat from November 6, 1991)
    Lost: Arthur Jones (repeat from November 6, 1991)
    Wanted: Liza Montgomery (repeat from November 6, 1991)

Episode 121

Aired: April 29, 1992

    Legend: Noah's Ark
    Update: Wanted: Levia Molinari
    Lost: Nicholas Karopoulos
    Murder: Tracy Wofford-Bunn
    Lost: Dolores Stradt
    Lost: Colleen Reed
    Wanted: Kenneth McDuff

Episode 122

Aired: May 6, 1992

    Murder: Doug Johnston
    Update: Wanted: James White
    Wanted: Rochester Car Heist
    Lost: The Siblings of Martha Smith
    Murder: Chaim Weiss

Episode 123

Aired: May 13, 1992

    Wanted: Betty Field
    Lost: Christophe Day
    Updated: Wanted: Cheryl Holland
    Wanted: Televangelist Bomber (repeat from November 13, 1991)
    Lost: Savior of Cathy Loving (repeat from November 13, 1991)
    Murder: Beverly McGowan (repeat from November 13, 1991)

Episode 124

Aired: May 17, 1992 (2hrs)

    Murder: Bill Henderson
    Mystery: Wytheville UFO Sightings (repeat from February 19, 1992)
    Wanted: Charles Warren Boomer (a.k.a) The Satchel Bandit (repeat from February 19, 1992)
    Wanted: Ohio Prostitute Killer (repeat from February 19, 1992)
    Legend: Beaty Castle (repeat from February 19, 1992)
    Mystery: Robert Kennedy (repeat from May 16, 1990)

Episode 125

Aired: May 20, 1992

    Mystery: Scott Johnson
    Update: Lost: Madeline and Ada Underwood
    Legend: Belgium UFO (repeat from November 20, 1991)
    Wanted: Philip Breen (repeat from November 20, 1991)

Episode 126

Aired: August 12, 1992

    Lost: Michelle Fazzani
    Wanted: Tom Dixon
    Murder: Gary Simmons (repeat from April 8, 1992)
    Murder: Vallejo Armored Car Murders (repeat from April 22, 1992)

Episode 127

Aired: August 19, 1992

    Lost: George Owens
    Update: Murder: Bill Henderson
    Murder: Doug Johnston (repeat from May 6, 1992)

Episode 128

Aired: September 2, 1992

    Lost: The Siblings of Jim Boumgarden
    Update: Lost: Rose Marie Platt

Episode 129

Aired: September 9, 1992

    Wanted: Sal Guardado
    Update: Lost: Michelle Fazzani
    Update: Lost: Dolores Stradt
    Legend: Noah's Ark (repeat from April 29, 1992)
    Murder: Tracy Wofford-Bunn (repeat from April 29, 1992)")

s5.info <- parse_epinfo("Season Five (1992-93)
Episode 130

Aired: September 16, 1992 (repeated December 30, 1992)

    Lost: Tammy Lynn Leppert
    Legend: Hudson River UFO
    Mystery: Paducah Plane Jumper/Estelle

Episode 131

Aired: September 23, 1992

    Wanted: Mahfuz Huq
    Legend: The Lake Champlain Monster
    Mystery: Pierre April
    Update: Lost: Lorene Roberts
    Wanted: Joseph Krantz
    Lost: Martha Doe Roberts

Episode 132

Aired: September 30, 1992 (repeated January 13, 1993)

    Wanted: Seattle Arsonist
    Mystery: Huey Long/Carl Weiss
    Lost: James Gilreath

Episode 133

Aired: October 7, 1992 (repeated March 3, 1993)

    Wanted: David Gordon Smith
    Lost: Jo Beth Smith
    Lost: Clifford Sherwood/George Gumbly
    Mystery: Ed Baker
    Update: Lost: Paul and Paula Scribner
    Wanted: Phillip Anthony Moore

Episode 134

Aired: October 14, 1992

    Lost: Patricia Carlton
    Update: Wanted: Charles Warren Boomer
    Murder: Lucie Turmel
    Lost: The Family of Georgia Boyd
    Murder: Little Miss Panasoffkee
    Update: Lost: James Gilreath

Episode 135

Aired: October 21, 1992

    Mystery: Glen Loney, Rhonda and Roxanne Anderson, Catherine Webb
    Wanted: New Orleans Serial Killer
    Lost: The Savior of James Vernon
    Wanted: J. D. Method

Episode 136

Aired: October 28, 1992 (repeated March 24, 1993)

    Legend: The ghosts at Moss Beach Distillery
    Legend: The ghosts at Drumm Barracks Civil War Museum
    Lost: The Families of Dylene Zolikoff, Scott Merz, and Dawnette Barker
    Update: Mystery: Pierre April
    Mystery: Sandra Evans

Episode 137

Aired: November 4, 1992

    Legend: Falcon Lake UFO
    Lost: Jesslyn Rich
    Mystery: Terry Lucas
    Update: Lost: Savior of Cathy Loving
    Lost: A.J. Breaux

Episode 138

Aired: November 11, 1992 (repeated April 14, 1993)

    Lost: Charles Shelton
    Update: Wanted: Pat Farmer
    Update: Lost: Jerad Peters
    Wanted: Richard McNair
    Murder: Rebecca Young

Episode 139

Aired: November 18, 1992

    Mystery: George Anderson
    Update: Lost: James Gilreath
    Murder: Jaclyn Dowaliby

Episode 140

Aired: November 25, 1992 (Live: 90 mins)

    Wanted: Robert Watson
    Lost: Cathy Ferner
    Wanted: William Korioth
    Lost: Windy Korioth
    Lost: Becky Terry
    Lost: Kimberly, Katie and Kelly Hurley
    Lost: Connie Jo Hamilton
    Lost: John Harman
    Wanted: Tracy Davis
    Wanted: Michael Hansen
    Lost: Cozette Hansen
    Wanted: Susan Roberts
    Lost: Ronald Jr. and Aaron Roberts
    Wanted: Elizabeth Ortiz
    Lost: Jonathan Ortiz
    Wanted: Joseph Arthur Rodia
    Wanted: Kenneth E. Dickerson
    Update: Lost: Michelle Fazzani
    Update: Wanted: Phillip Anthony Moore

Episode 141

Aired: December 2, 1992

    Murder: Eva Shoen
    Update: Lost: Becky Terry
    Update: Lost: Windy Korioth
    Wanted: William Korioth
    Lost: Kathleen Mary Young
    Wanted: Edward Bell
    Lost: Pamela June Ray

Episode 142

Aired: December 9, 1992

    Murder: The Black Dahlia/Butcher Of Kingsbury Run
    Lost: Erick, Glynda, and Keath Nickerson
    Legend: Hotel Bullock
    Lost: Alexander Olive

Episode 143

Aired: December 16, 1992 (repeated April 4, 1993)

    Mystery: Luis Diaz
    Wanted: The Bird Road Rapist
    Lost: The Family of Charles Stubin
    Wanted: Michael Anthony Starr

Episode 144

Aired: December 23, 1992

    Mystery: Chucky McGivern
    Lost: The Search of Alexandra Stantzos
    Update: Lost: James Russell/Darlene and Davida Ebmeier/Thomas Manion/Madding Family
    Lost: Mickey Allen Samson
    Lost: Fu Teed Wong
    Lost: The Children of Uta Collins
    Lost: Anthonette Cayedito
    Lost: The Friend of Russell and Jean Johnson

Episode 145

Aired: January 6, 1993

    Mystery: Rick McCue
    Murder: Alene Courchesne
    Legend: The Lake Champlain Monster (repeat from September 23, 1992)
    Wanted: Mahfuz Huq (repeat from September 23, 1992)

Episode 146

Aired: January 20, 1993

    Murder: Neal Jennings
    Wanted: Bill Roberts
    Update: Wanted: Charles Warren Boomer (repeat from October 14, 1992)
    Wanted: Sal Guardado (repeat from September 9, 1992)
    Lost: Patricia Carlton (repeat from October 14, 1992)
    Update: Wanted: Phillip Anthony Moore (repeat from November 25, 1992)
    Murder: Little Miss Panasoffkee (repeat from October 14, 1992)

Episode 147

Aired: January 27, 1993

    Legend: Yamashita's Treasure
    Murder: Kaitlyn Arquette
    Lost: Terry Smith

Episode 148

Aired: February 3, 1993 (repeated June 2, 1993)

    Legend: Guardian UFO
    Update: Lost: Erick, Glynda, and Keath Nickerson
    Mystery: Lucy
    Wanted: Larry George
    Lost: The Siblings of Eugene Price

Episode 149

Aired: February 9, 1993

    Wanted: Rolando Cruz and Alejandro Hernandez
    Murder: Jeanine Nicarico
    Updates: Lost: Connie Jo Hamilton
    Lost: Maureen Keewatincappo/Bernie and Calvin Seaton
    Lost: The Siblings of Jim Boumgarden (repeat from September 2, 1992)

Episode 150

Aired: February 10, 1993 (repeated June 9, 1993)

    Mystery: Don Decker
    Murder: Michael Hunter
    Mystery: Tom Hughes
    Lost: Dolores Ford
    Update: Lost: The Siblings of Eugene Price

Episode 151

Aired: February 17, 1993 (repeated June 23, 1993)

    Mystery: Bashir Kouchacji
    Wanted: Sharon Rogers Car Bomber
    Lost: Savior of Phillip Macri
    Update: Lost: Martha Doe Roberts
    Update: Lost: The Families of Dylene Zolikoff, Scott Merz, and Dawnette Barker

Episode 152

Aired: February 24, 1993 (repeated June 30, 1993)

    Mystery: Chad Langford
    Update: Mystery: Tony Miller
    Lost: Scott Hill
    Mystery: Gabby's Bones

Episode 153

Aired: March 10, 1993 (repeated July 7, 1993)

    Mystery: Danny Casolaro
    Lost: Margaret Wiesner
    Wanted: Blind River Killer
    Murder: Jacqueline McAllister and Brian Major
    Update: Murder: Susan and Shane Hamwi
    Mystery: John Purvis

Episode 154

Aired: March 17, 1993

    Mystery: Mario Amado
    Legend: Falcon Lake UFO (repeat from November 4, 1992)
    Update: Lost: Savior of Cathy Loving (repeat from November 4, 1992)
    Lost: A.J. Breaux (repeat from November 4, 1992)

Episode 155

Aired: March 31, 1993

    Mystery: Dr. Martin Luther King
    Wanted: J. D. Method (repeat from October 21, 1992)
    Wanted: New Orleans Serial Killer (repeat from October 21, 1992)

Episode 156

Aired: April 7, 1993

    Legend: Image of Guadalupe
    Wanted: Stahl Painting Theft
    Update: Lost: Bernie and Calvin Seaton
    Lost: Tom Roche
    Murder: David Hurley

Episode 157

Aired: April 21, 1993

    Wanted: Julius Patterson and Paulette Hite
    Wanted: Ramzi Ahmed Yousef
    Update: Wanted: Edward Bell
    Murder: Eva Shoen (repeated from December 2, 1992)
    Lost: The Father of Jeannie Wagner (repeated from April 24, 1991)

Episode 158

Aired: April 28, 1993

    Lost: Dede Rosenthal
    Update: Wanted: Greg Webb
    Mystery: George Anderson (repeated from November 18, 1992)
    Murder: Neal Jennings
    Wanted: Bill Roberts (repeated from January 20, 1993)

Episode 159

Aired: May 5, 1993 (repeated July 21, 1993)

    Mystery: Gander Plane Crash
    Lost: Arthur Lloyd
    Wanted: Nelson DeCloud
    Update: Lost: Becky Terry

Episode 160

Aired: May 12, 1993

    Murder: Eileen Mangold
    Update: Wanted: David Gordon Smith
    Lost: Erick, Glynda, and Keath Nickerson (repeat from December 9, 1992)
    Legend: Hotel Bullock (repeat from December 9, 1992)
    Lost: Alexander Olive (repeat from December 9, 1992)

Episode 161

Aired: May 19, 1993 (2hrs) (repeated August 4 and 11, 1993)

    Mystery: Tina Resch
    Wanted: Milk Carton Bandit/Grandpa Bandit
    Lost: The Parents of Miriam
    Lost: The Crew of the L-8
    Wanted: Reggie DePalma
    Update: Mystery: Rick McCue
    Update: Murder: Alene Courchesne
    Mystery: Michael Lloyd Self
    Murder: Rhonda Johnson and Sharon Shaw

Episode 162

Aired: May 26, 1993

    Wanted: Lyle Moody
    Update: Lost: Savior of Phillip Macri
    Legend: Yamashita's Treasure (repeat from January 27, 1993)

Episode 163

Aired: August 25, 1993

    Wanted: Brian Brophil
    Update: Murder: Jaclyn Dowaliby

Episode 164

Aired: September 15, 1993

    Lost: Gordon Collins
    Lost: Kathleen Mary Young (repeat from December 2, 1992)
    Wanted: Edward Bell (repeat from December 2, 1992)
    Mystery: Pierre April (repeat from September 23, 1992)")

s6.info <- parse_epinfo("Season Six (1993-94)
Episode 165

Aired: September 22, 1993

    Mystery: Bruce Kelly
    Update: Wanted: Nelson DeCloud
    Wanted: Cachimba
    Murder: Jacquelyn Tendall
    Murder: Gail Jordan and Dawn Lamont
    Lost: Joan Gay Croft

Advertisement
Episode 166

Aired: September 29, 1993

    Mystery: Jose Gonzalez
    Wanted: Brenda Penniger and Nadine Castelle
    Lost: Frederick Valentich
    Update: Lost: Scott Hill
    Mystery: Danny Williams

Episode 167

Aired: October 6, 1993

    Legend: Anna Anderson
    Wanted: Andolina Gonzalez
    Wanted: Jerry Gervasoni

Episode 168

Aired: October 13, 1993

    Mystery: Dr. Theodore Loseff
    Mystery: Wally Spencer
    Update: Murder: Lee Selwyn
    Lost: Christopher Kurowski

Episode 169

Aired: October 20, 1993

    Wanted: Joseph Prushinowski
    Lost: Wendy Camp, Cynthia Britto, and Lisa Kregear
    Lost: Madeline Strauss
    Murder: Su-Ya Kim

Advertisement
Episode 170

Aired: October 27, 1993

    Update: Mystery: Jose Gonzalez
    Legend: The ghosts of the Mann House
    Lost: Eve Meisel
    Murder: Lisa Ziegert
    Update: Lost: Kathleen Mary Young
    Wanted: Rohrey Wychgel Shooter

Episode 171

Aired: November 3, 1993

    Update: Wanted: Jerry Gervasoni
    Legend: The Fatima Miracle
    Wanted: Wade Mitchell Parker
    Update: Lost: Jim Burke
    Wanted: David MacLeod
    Wanted: Michael Benka
    Wanted: Robert Fritch
    Wanted: Donald Alexander

Episode 172

Aired: November 10, 1993

    Wanted: John Grundhofer Kidnapper
    Murder: Jill and Julie Hansen
    Lost: Michael Seymour

Advertisement
Episode 173

Aired: November 17, 1993

    Wanted: Travis Wade Duncan
    Update: Wanted: Julius Patterson and Paulette Hite
    Legend: The Mona Lisa
    Lost: Doreen Marfeo
    Wanted: Raymond Young

Episode 174

Aired: November 24, 1993

    Update: Wanted: Robert Watson
    Wanted: Danny Marino/Al Tom and Ricky Nelson
    Wanted: David Brian Williams
    Wanted: Christopher Griffin
    Wanted: Derek Leonard Reynolds
    Murder: Larry Costine
    Wanted: Melissa Jo Sermons
    Lost: Dorothy Johnson
    Murder: Corrine Gustavson
    Wanted: Edmonton Rapist

Episode 175

Aired: December 1, 1993

    Update: Wanted: Danny Marino
    Wanted: Lissette Nukida
    Lost: Saviors of Colleen Frangione
    Murder: Gary Grant Jr.
    Lost: Sandy Breed and Bruce Clark
    Lost: Heirs of Donald Mullins
    Lost: Heirs of Mary T. Torres

Advertisement
Episode 176

Aired: December 8, 1993

    Update: Lost: Saviors of Colleen Frangione
    Lost: Pam Page
    Mystery: Bruce Kelly (repeat from September 22, 1993)
    Lost: Joan Gay Croft (repeat from September 22, 1993)

Episode 177

Aired: December 22, 1993

    Lost: The Children of Hilda Craun
    Wanted: Cachimba (repeat from September 22, 1993)
    Murder: Jacquelyn Tendall (repeat from September 22, 1993)
    Murder: Gail Jordan and Dawn Lamont (repeat from September 22, 1993)
    Mystery: Anna Anderson (repeat from October 6, 1993)

Episode 178

Aired: January 5, 1994

    Lost: Shafaa Salem
    Wanted: DeFallah Al-Salem
    Murder: Jay Given
    Lost: The Friend of Tom Vaughn
    Update: Murder: Eva Shoen
    Mystery: Daly City Man

Episode 179

Aired: January 12, 1994

    Wanted: Armando Garcia
    Update: Wanted: The Countess
    Lost: The Children of Faith Brown
    Wanted: Danny Piñeda
    Wanted: Dr. George Desmond Graham
    Wanted: Joseph Gardner
    Wanted: Ronald Oglesby
    Lost: Charles Horvath

Advertisement
Episode 180

Aired: January 19, 1994

    Update: Lost: The Children of Faith Brown
    Wanted: Grocery Robbers
    Wanted: Ramon Reyes and Louie Velarde
    Wanted: Oklahoma City Convenience Store Robbers
    Mystery: Ted Loseff (repeat from October 13, 1993)
    Update: Murder: Lee Selwyn (repeat from October 13, 1993)
    Lost: Christopher Kurowski (repeat from October 13, 1993)

Episode 181

Aired: January 26, 1994

    Lost: Amy Billig
    Update: Lost: Dorothy Johnson
    Lost: Heirs of Walter Rice
    Lost: Oliver Munson
    Lost: Rose Marie Luttmer

Episode 182

Aired: February 2, 1994

    Legend: San Pedro Mummy
    Mystery: Andre Jones
    Update: Wanted: Jerry Gervasoni
    Lost: The Family of Terris Christie Derby
    Wanted: Wadada

Episode 183

Aired: February 9, 1994

    Legend: The Ghost of Resurrection Mary
    Update: Lost: Saviors of Colleen Frangione
    Lost: Selena Edon
    Wanted: Victor Gerena
    Wanted: Filiberto Ojeda Rios
    Lost: Bruce Bradney
    Lost: The Mother of Margaret Smith
    Lost: Kimberly Karen

Episode 184

Aired: February 16, 1994

    Update: Lost: Kimberly Karen
    Legend: Miracle of Lourdes
    Update: Wanted: Brian Brophil
    Murder: Dick Hansen
    Lost: Vernicy Bradford
    Wanted: Joseph Collins

Episode 185

Aired: February 23, 1994

    Mystery: Michael Carmichael and Billy Ray Hargrove
    Mystery: Wally Spencer (repeat from October 13, 1993)
    Lost: Eve Meisel (repeat from October 27, 1993)
    Wanted: David MacLeod (repeat from November 3, 1993)
    Wanted: Benny Franklin Miller 
    Wanted: Robert Fritch (Fritch and Alexander repeat from November 3, 1993)
    Wanted: Donald Alexander (Fritch and Alexander repeat from November 3, 1993)

Advertisement
Episode 186

Aired: March 2, 1994

    Mystery: Nova and Lady
    Legend: Interceptors
    Update: Wanted: Danny Marino
    Mystery: Dave Bocks

Episode 187

Aired: March 16, 1994

    Mystery: Tony Lombardi
    Update: Lost: The Children of Faith Brown
    Wanted: Travis Wade Duncan (repeat from November 17, 1993)
    Lost: The Siblings of Eugene Price (repeat from February 3, 1993)

Episode 188

Aired: March 23, 1994

    Mystery: Carolyn Hebert, Elaine Emmi and Linda Babb
    Murder: Anita Green
    Wanted: Filemon Santiago and Gary Miller
    Wanted: Jorge Taracena
    Wanted: Cecelia Ortega
    Wanted: Miguel Morales
    Wanted: Dr. Otto Barkeem
    Lost: Heirs of Charles Lazarus
    Wanted: Dr. Arvind Sinha

Advertisement
Episode 189

Aired: April 6, 1994

    Lost: Hugh Harlin
    Murder: Dian Harlin
    Wanted: Lissette Nukida (repeat from December 1, 1993)
    Update: Wanted: Robert Watson (repeat from November 24, 1993)
    Lost: Michael Seymour (repeat from November 10, 1993)
    Wanted: Joseph Prushinowski (repeat from October 20, 1993)

Episode 190

Aired: April 13, 1994

    Mystery: Therapeutic Touch Michael Ziegler/George and Marie
    Wanted: Pennsylvania Bank Robber
    Wanted: Deroy King, Jr
    Wanted: Tampa Bay Robber
    Wanted: Miami Robber
    Wanted: Southern California Robber
    Lost: Jim Pearson
    Wanted: David Viera

Episode 191

Aired: April 20, 1994

    Update: Wanted: David Viera
    Lost: John Cheek
    Update: Lost: Kimberly Karen
    Murder: Jill and Julie Hansen (repeat from November 10, 1993)
    Lost: Madeline Strauss (repeat from October 20, 1993)

Advertisement
Episode 192

Aired: April 27, 1994

    Legend: Dutch Schultz Treasure
    Wanted: Tom Johnson
    Lost: Lauren Jackson
    Murder: Roxann and Kristopher Jeeves

Episode 193

Aired: May 4, 1994

    Mystery: After Death Visits: John and Patti Eggleston and Paige Roark
    Update: Lost: The Friend of Tom Vaughn
    Wanted: Kelly Finnegan
    Lost: The Father of Kathleen Belcher
    Wanted: Interstate 70 Killer
    Lost: East Stockton John Doe

Episode 194

Aired: May 11, 1994

    Mystery: Albert DeSalvo aka The Boston Strangler
    Lost: The Daughter of Oscar Norton
    Murder: Kevin Wheel

Episode 195

Aired: May 25, 1994 (2hrs)

    Legend: Bigfoot in the Northwest
    Lost: Craig Williamson
    Updates: Wanted: David Viera
    Mystery: Bill and Dorothy Wacker
    Murder: Jordan Children
    Legend: Angels Janie Halliday/Estela Vera
    Lost: Marilyn Hahnlein
    Murder: Charlie Anderson
    Lost: The Parents of Brenda Abbey
    Wanted: Joseph Gardner
    Wanted: Floyd Travers
    Wanted: Heather Tallchief and Roberto Solis
    Wanted: Ora Prince

Episode 196

Aired: September 7, 1994

    Update: Legend: The Shroud Of Turin
    Lost: Yves-Emmanuel Pain and Laurent Hernas
    Lost: Janie Halliday/Estela Vera (repeat from May 25, 1994)
    Lost: The Parents of Brenda Abbey (repeat from May 25, 1994)

Episode 197  

Aired: September 18, 1994 (repeated March 31, 1995)

    Legend: Hudson River UFO
    Update: Legend: Roswell Crash/Area 51
    Legend: The Allagash Abductions")



s1.info <- parse_epinfo("Season One (1988-89)
Episode 1
First Aired: October 5, 1988

Legend: Gulf Breeze UFO
Update: Wanted: Louis Carlucci
Wanted: Joe Shepherd
Lost: Gail DeLano
Episode 2
First Aired: October 12, 1988

Mystery: D.B. Cooper
Update: Wanted: Jon Yount and Diane Brodbeck
Update: Wanted: Steve Hadley
Mystery: Don Henry and Kevin Ives
Mystery: Dennis Walker
Episode 3
First Aired: October 26, 1988 (repeated April 19, 1989 and March 8, 1991)

Legend: The Queen Mary
Legend: Tallman House
Legend: The General Wayne Inn
Legend: Tatum House
Episode 4
First Aired: November 2, 1988 (repeated March 22, 1989)

Mystery: Son of Sam (part 1)
Lost: Heirs of Walter Green
Murder: Harold and Thelma Swain
Wanted: Chevy Chase Bandit
Wanted: John William Farr
Wanted: Shotgun Bandit
Wanted: Shopping Bag Bandit

Episode 5
First Aired: November 9, 1988 (repeated March 29, 1989)

Wanted: Bob Dozier and John Russell
Lost: Kristen Tomlin and Suzanne Russell
Update: Wanted: Rolex Robbers
Lost: The Father of Janet O'Regan
Mystery: Son of Sam (part 2)
Lost: Annie Hearin
Episode 6
First Aired: November 16, 1988 (repeated May 24, 1989)

Lost: Christi Nichols
Update: Lost: Gail DeLano
Wanted: Mark Adams
Wanted: Steven Cox
Murder: Barbara Jean Horn
Episode 7
First aired: November 23, 1988 (repeated May 31, 1989)

Mystery: Kurt Sova/Eugene Kvet
Update: Wanted: Shopping Bag Bandit
Update: Wanted: Bob Dozier and John Russell
Wanted: Lancaster Extortion Writer
Murder: Father Reynaldo Rivera
Lost: Father John Kerrigan
Mystery: Mystery Rock

Episode 8
First Aired: November 30, 1988 (repeated June 7, 1989)

Wanted: Ann Sigmin and Garey Goff
Update: Murder: Don Henry and Kevin Ives
Update: Wanted: Steven Cox
Mystery: Robert Matthews/Kristina Florence
Lost: Rogest Cain
Episode 9
First Aired: December 14, 1988 (repeated June 14, 1989)

Wanted: Jenny Pratt attackers
Update: Wanted: Joe Shepherd
Wanted: Jack Quinn
Mystery: Clarence and Geneva Roberts
Murder: Bruno and Bobo
Episode 10
First aired: December 21, 1988

Lost: Philip Pelletier
Update: Wanted: Lancaster Extortion Writer
Update: Wanted: Steven Cox
Lost: Matthew Chase
Murder: Marilu Geri
Lost: The Family of Dolores Valadez
ADVERTISEMENT

Episode 11
First Aired: January 11, 1989 (postponed on January 4, 1989)

Murder: Lee Selwyn
Update: Wanted: Steven Cox
Lost: Michael Rosenblum
Update: Lost: Rogest Cain
Wanted: Arthur Frankford
Episode 12
First Aired: January 18, 1989

Lost: Jeremy Bright
Murder: John Martin
Lost: Heirs of Dan Willains
Wanted: Leo Koury
Lost: Michaela Garecht/Amber Swartz
Lost: The Crew of the Liebling

Episode 13
First Aired: January 25, 1989

Lost: Micki Jo West
Update: Wanted: David Davis
Murder: Shannon Davis
Lost: The Brother of Sylvia Wemhoff/Margaret Murphy
Lost: Marlene Santana/Carlina White/Christopher Abeyta
Wanted: The Bald Mountain Shooters
Wanted: Robert Leads

Episode 14
Aired: February 1, 1989 (repeated August 9, 1989)

Legend: Face on Mars
Update: Wanted: Arthur Frankford
Murder: Kathy Hobbs
Update: Lost: Philip Pelletier
Lost: Jackie Harrington
Lost: Angelo Desideri
Episode 15
Aired: February 8, 1989 (repeated August 2, 1989 and August 15, 1990)

Mystery: The Alcatraz Escape
Wanted: Jean Marie Gagnon
Wanted: Thomas Nauss Jr.
Wanted: Carl Alfred Eder
Wanted: Lena Regina Smith
Wanted: Joseph Mancini

Episode 16
Aired: February 15, 1989 (repeated June 28, 1989)

Murder: Mickey and Trudy Thompson
Update: Wanted: The Bald Mountain Shooters
Wanted: Crazy Glue Bandit
Legend: Bigfoot in Colorado
Lost: Heirs of George J. Stein
Murder: Joyce McLain
Update: Wanted: Jean Marie Gagnon
ADVERTISEMENT

Episode 17
Aired: February 22, 1989 (repeated July 5, 1989 and September 16, 1990)

Wanted: Burrowing Burglars
Mystery: Kristle Merzlock/Thomas Sawyer
Update: Wanted: Robert Leads
Wanted: Walter Wenke
Wanted: Ronald Denslow
Murder: Permon Gilbert
Episode 18
Aired: March 1, 1989 (repeated July 12, 1989)

Murder: Donald Smith
Wanted: Larry Munroe
Mystery: Brushy Bill Roberts aka Billy the Kid
Update: Wanted: Jean Marie Gagnon
Wanted: Charles Mule

Episode 19
Aired: March 15, 1989 (repeated July 19, 1989)

Murder: Lisa Marie Kimmell
Update: Wanted: Louis Carlucci
Legend: Lost Dutchman Mine
Murder: Thomas Hotard
Lost: Audrey Moate

Episode 20
Aired: April 5, 1989 (repeated July 26, 1989)

Wanted: John Mooney
Wanted: Liz Carmichael
Update: Lost: Annie Hearin
Murder: Terri McClure
Wanted: Charles Wickman

Episode 21
Aired: April 12, 1989 (repeated August 23, 1989)

Murder: Dexter Stefonek
Wanted: Jorge Cortez
Update: Lost: Heirs of Dan Willains
Wanted: Fumbles
Wanted: Michael and Sharon Mohon
Wanted: Larry Dennis Miller
Wanted: Curtis Watson
Wanted: Joseph Hutchinson

Episode 22
Aired: April 26, 1989 (repeated August 1, 1990)

Lost: Gus Hoffman
Update: Lost: Matthew Chase
Wanted: Ron Rushton
Mystery: Patsy Wright
Lost: The Families of the S.S. Muskogee Crew
Episode 23
Aired: May 3, 1989 (repeated August 30, 1989)

Murder: Julie Cross
Update: Lost: Angelo Desideri
Wanted: The Bicycle Bandit
Wanted: Kathy Power
Legend: Charles Nungesser and Francois Coli
ADVERTISEMENT

Episode 24
Aired: May 10, 1989

Legend: Victorio Peak Treasure
Lost: Kari Lynn Nixon
Wanted: David Rhodes
Lost: Keelan and David J. Rhodes

Episode 25
Aired: May 17, 1989

Wanted: Sheldon Weinberg
Legend: Gulf Breeze UFO (repeat from October 5, 1988)
Wanted: Joe Shepherd (repeat from October 5, 1988)
Lost: Marlene Santana/Carlina White/Christopher Abeyta (repeat from January 25, 1989)

Episode 26
Aired: August 16, 1989

Murder: Barbara Jean Horn (repeat from November 16, 1988)
Update: Wanted: Liz Carmichael
Lost: The Brother of Sylvia Wemhoff (repeat from January 25, 1989)
Episode 27
Aired: September 6, 1989

Wanted: Mike Riemer
Murder: Diane Robertson/Stephen Harkins and Ruth Cooper
Update: Wanted: David Rhodes
Update: Lost: Keelan and David J. Rhodes
Wanted: Michael and Sharon Mohon (repeat from April 12, 1989)
Murder: Patsy Wright (repeat from April 26, 1989)
Lost: Jackie Harrington (repeat from February 1, 1989)
Episode 28
Aired: September 13, 1989

Murder: Todd McAfee
Update: Wanted: Ronald Denslow
Update: Wanted: Joseph Hutchinson
Legend: Victorio Peak Treasure (repeat from May 10, 1989)
Lost: The Families of the S.S. Muskogee Crew (repeat from April 26, 1989)")

# tidy first aired----
fa.remove <- "^.*aired: | \\(.*$"
# set date
for(i in ls(pattern = "^s\\d{1,2}\\.info$")){
  temp <- get(i)
  temp$date_fa <- temp$first_aired %>%
    gsub(pattern = fa.remove, 
         replacement = "", 
         x = ., 
         ignore.case = T) %>%
    mdy()
  assign(x = i, value = temp)
  rm(temp)
}

# tidy season----
for(i in ls(pattern = "^s\\d{1,2}\\.info$")){
  temp <- get(i)
  temp$s_num <- temp$season %>%
    gsub("^season | \\(.*$", 
         "", ., 
         ignore.case = T) %>%
    #lapply(., word2num) %>%
    unlist()
  assign(x = i, value = temp)
  rm(temp)
}

# tidy episode----
for(i in ls(pattern = "^s\\d{1,2}\\.info$")){
  temp <- get(i)
  temp$ep_num <- temp$episode %>%
    gsub("^episode ", 
         "", ., 
         ignore.case = T) 
  assign(x = i, value = temp)
  rm(temp)
}
s6.info

# remove advertisements----
for(i in ls(pattern = "^s\\d{1,2}\\.info$")){
  temp <- get(i)
  temp <- temp[!grepl("advertisement", temp$segment, ignore.case = T),]
  assign(x = i, value = temp)
  rm(temp)
}


s6.info

# tidy segment----


# remove repeated episode metadata from segment
for(i in ls(pattern = "^s\\d{1,2}\\.info$")){
  temp <- get(i)
  
  temp$seg_tidy <- temp$segment %>%
    gsub("\\(.*repeat.* \\d{4,4}\\)|\\(part \\d{1,1}\\)", 
         "", .) %>%
    trimws()
  
  assign(x = i, value = temp)
  rm(temp)
}

for(i in ls(pattern = "^s\\d{1,2}\\.info$")){
  temp <- get(i)
  
  temp$seg_name <- temp$seg_tidy %>%
    gsub(":", ":@@", .) %>%
    strsplit(., "@@") %>%
    lapply(., trimws) %>%
    lapply(., last) %>%
    unlist()%>%
    trimws()
  
  assign(x = i, value = temp)
  rm(temp)
}

library(ggplot2)



# segment_categories----
seg.cats <- c("Wanted", "Lost", 
              "Mystery", 
              "Murder","Murders", 
              "Legend", "Legends", 
              "OTHER")


# add new col for each category for each dataset.
for(i in ls(pattern = "^s\\d{1,2}\\.info$")){
  temp <- get(i)
  
  temp$sc_wanted  <- grepl("wanted:", temp$seg_tidy, ignore.case = T)
  temp$sc_lost    <- grepl("lost:", temp$seg_tidy, ignore.case = T)
  temp$sc_mystery <- grepl("mystery:", temp$seg_tidy, ignore.case = T)
  temp$sc_murder  <- grepl("murder:|murders:", temp$seg_tidy, ignore.case = T)
  temp$sc_legend  <- grepl("legend:|legends:", temp$seg_tidy, ignore.case = T)
  temp$sc_other   <- grepl("OTHER|OTHER:", temp$seg_tidy, ignore.case = F)
  
  assign(x = i, value = temp)
  rm(temp)
}

master <- rbind(s1.info, 
                s2.info, 
                s3.info, 
                s4.info, 
                s5.info, 
                s6.info, 
                s7.info, 
                s8.info, 
                s9.info, 
                s10.info, 
                s11.info, 
                s12.info, 
                s13.info) 


master <- master %>%
  # group_by(sc_legend, sc_lost, sc_murder, 
  #          sc_mystery, sc_wanted, sc_other) %>%
  # summarise(n = n()) %>%
  mutate(., 
         sum_sc = sc_legend + 
           sc_lost + 
           sc_murder + 
           sc_mystery + 
           sc_wanted +
           sc_other) %>%
  .[order(.$sum_sc,decreasing = F),]


# QA----
master[master$sum_sc == 0,]$seg_tidy %>% unique()

master[master$sum_sc >= 2,]$seg_tidy %>% unique()

# categorizing----
master.out <- master


master.out$sc_legend  <- ifelse(master.out$sc_legend, "legend", "")
master.out$sc_lost    <- ifelse(master.out$sc_lost, "lost", "")
master.out$sc_murder  <- ifelse(master.out$sc_murder, "murder", "")
master.out$sc_mystery <- ifelse(master.out$sc_mystery, "mystery", "")
master.out$sc_other   <- ifelse(master.out$sc_other, "other", "")
master.out$sc_wanted  <- ifelse(master.out$sc_wanted, "wanted", "")

master.out$sc_summary <- paste(master.out$sc_legend, 
                               master.out$sc_lost, 
                               master.out$sc_murder, 
                               master.out$sc_mystery, 
                               master.out$sc_other, 
                               master.out$sc_wanted, sep = "")

master.out %>%
  group_by(sc_summary) %>%
  summarise(n = n())


keep.cols <- c("date_fa", "segment", "s_num", "ep_num", 
               "seg_tidy", "seg_name", "sc_summary")

master.out <- master.out[,keep.cols]

# master.out <- master.out[!colnames(master.out) %in%
#                            c(grep("^sc_", colnames(master.out), value = T), 
#                              #"sc_summary", 
#                              "sum_sc", 
#                              "season", "episode", "first_aired", "segment")]


for(i in 1:nrow(master.out)){
  if(is.na(as.numeric(master.out$s_num[i]))){
    master.out$s_num[i] <- master.out$s_num[i] %>%
      word2num() %>% as.character()
  }
}

master.out$s_num <- as.numeric(master.out$s_num)
master.out$ep_num <- master.out$ep_num %>% as.numeric 

# make_crosswalks----

cw_ep.airdates <- master.out %>%
  group_by(s_num, ep_num, date_fa) %>%
  summarise()

cw_ep.id <- master.out %>%
  group_by(s_num, ep_num) %>%
  summarise() %>%
  ungroup() %>%
  mutate(., 
         eid = paste("s", 
                     unlist(lapply(s_num, FUN = lzero, 
                                   n.leading.zeroes = 2)), 
                     "ep", 
                     unlist(lapply(ep_num, FUN = lzero, 
                                   n.leading.zeroes = 3)), 
                     sep = ""))


master.out

# people and names----
library(lexicon)

data("common_names")
data("freq_first_names")
data("freq_last_names")
extra.names <- c("Melvine", "Aprile", 
                 "Aleatha", "Evertz", 
                 "Madding", "Mahfuz", "Huq", 
                 "Method", 
                 "Florian", "Bourch",
                 "Anthonette", "Cayedito", 
                 "Rohrey", "Wychgel", 
                 "Arvind", "Sinha", 
                 "Levia", "Molinari", 
                 "Chucky", "McGivern", 
                 "Deroy", "King", 
                 "Dub", "Wackerhagen", 
                 "Antranik", "Geuvjehizian", 
                 "Qi", "Gong", 
                 "Alie", "Berrelez", 
                 "Jessyca", "Mullenberg", 
                 "Meriah", "Sabria", "Widboom", 
                 "Ceara", "O'Connell", 
                 "Nicolai", "Levashov", 
                 "Yefim", "Shubentsov", 
                 "Darlie", "Routier", "Stryder", "Styarfyr", 
                 "Tupac", "Shakur")
#data(package = "lexicon")

cw.ppl <- master.out[,c("s_num", "ep_num", "seg_name")]



unique.ppl <- cw.ppl$seg_name %>% 
  unique()

split.ptrns <- c("and", "/")


tidy_names <- data.frame(long_ppl = unique.ppl, 
                         n_words = NA, 
                         is_name_of_1_person = NA,
                         with_split = NA, 
                         with_commonname = NA) %>%
  as_tibble() %>%
  mutate(., 
         n_words = unlist(lapply(strsplit(long_ppl, " |/"),length)))

for(i in 1:nrow(tidy_names)){
  tidy_names$with_commonname[i] <- sum(tolower(unlist(strsplit(tidy_names$long_ppl[i], 
                                                               split = " |/|-"))) %in% 
                                         tolower(c(freq_first_names$Name, 
                                                   freq_last_names$Surname, 
                                                   extra.names)), na.rm = T)
  
}



tidy_names$with_commonname %>% hist

tidy_names$long_ppl[tidy_names$with_commonname == 0]

master.out$sc_summary_f <- factor(master.out$sc_summary, 
                                  levels = c("murder", 
                                             "wanted", 
                                             "lost", 
                                             "mystery", 
                                             "legend", 
                                             "other"))


master.out %>%
  group_by(s_num, 
           sc_summary_f) %>%
  summarise(n_ep = n_distinct(ep_num), 
            n_segs = n_distinct(segment)) %>%
  ggplot(data = ., 
          aes(x = s_num, 
              y = n_segs, fill = sc_summary_f)) + 
  geom_col(position = position_fill())+
  theme(legend.position = "bottom")




logs <- function(seg,status){
  data.frame(seg_name = seg, 
             status = status)
}






cw.status <- rbind(logs("Don Kemp", "unsolved"), 
                   logs("Roger Wheeler", "solved"), 
                   logs("Brian Halloran", "unsolved"), 
                   logs("John Callahan", "solved"), 
                   logs("The Family of Pat Mealbach", "unresolved"), 
                   logs("Terry Lee Conner and Joseph Dougherty", "captured"), 
                   logs("Leo Koury", "solved"), 
                   logs("Donald Eugene Webb", "solved"), 
                   logs("Victor Gerena", "wanted"), 
                   logs("Claude Dallas", "captured"), 
                   logs("James Dyess", "captured"), 
                   logs("Danny Weeks", "captured"), 
                   logs("Thomas Harrelson", "captured"), 
                   logs("Robert Litchfield", "captured"), 
                   logs("Wanda Jean Mays", "solved"), 
                   logs("John Burns", "captured"), 
                   logs("Kyra Cook", "unresolved"), 
                   logs("Rolex Robbers", "unresolved"), 
                   logs("Robert Weeks", "captured"), 
                   logs("The Unabomber", "captured"), 
                   logs("John Lutter", "Unresolved"), 
                   logs("Glenn Consagra", "Unresolved"), 
                   logs("Freddie Douberley and Mary Lou Holmes", "Unresolved"), 
                   logs("Heirs of George Marsh", "Solved"), 
                   logs("David Davis", "Captured"), 
                   logs("Shannon Davis", "solved"), 
                   logs("Aileen Ann Conway", "Unsolved"), 
                   logs("Glen and Bessie Hyde", "unsolved"), 
                   logs("Kolb Skeleton", "Unresolved"), 
                   logs("Dottie Caylor", "Unsolved"), 
                   logs("Missy Munday and Jerry Strickland", "solved"), 
                   logs("Elmer DeBoer", "solved"), 
                   logs("Michael Martin", "unresolved"), 
                   logs("Kurt McFall", "unsolved"), 
                   logs("Heirs of Charlie Scheel", "solved"))

master.out$status <- NA

# search
master.out[grepl("mark adams", master.out$seg_tidy, 
                ignore.case = T),]$seg_tidy

# log
master.out[grepl("mark adams", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unsolved"
master.out[grepl("christi nichols", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unsolved"
master.out[grepl("annie hearin", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unresolved"
master.out[grepl("janet o'regan", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "solved"
master.out[grepl("rolex robbers", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "Unresolved"
master.out[grepl("kristin tomlin|suzanne russell", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "Unresolved"
master.out[grepl("bob dozier|john russell", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "captured"
master.out[grepl("shopping bag bandit", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "captured"
master.out[grepl("shotgun bandit", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "wanted"
master.out[grepl("john william farr", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "wanted"
master.out[grepl("chevy chase bandit", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "captured"
master.out[grepl("harold swain|thelma swain", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unresolved"
master.out[grepl("walter green", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unresolved"
master.out[grepl("son of sam", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unresolved"
master.out[grepl("tatum house", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unsolved"
master.out[grepl("general wayne inn", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unsolved"
master.out[grepl("tallman house", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unsolved"
master.out[grepl("queen mary", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unsolved"
master.out[grepl("dennis walker", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unresolved"
master.out[grepl("don henry|kevin ives", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unresolved"
master.out[grepl("steve hadley", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "captured"
master.out[grepl("jon yount|diane brodbeck", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "captured"
master.out[grepl("gulf breeze ufo", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "unresolved"
master.out[grepl("louis carlucci", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "captured"
master.out[grepl("joe shepherd", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "captured"
master.out[grepl("gail delano", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "solved"
master.out[grepl("d.b. cooper", master.out$seg_tidy, 
                 ignore.case = T),]$status <- "wanted"




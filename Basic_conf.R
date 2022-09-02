# --- Animal Crossing New Horizons Villager Popularity
# By: Alejandra Esquivel Arias
# September/2022
#  On going ---- 


# R options
options(warn = -1, scipen = 999)

# Load libraries
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, leaflet, httr, XML, googlesheets4))
googlesheets4::gs4_deauth()

#  Web Scraping ----- 
url = "https://www.animalcrossingportal.com/games/new-horizons/guides/villager-popularity-list.php#"


# making http request
resource <- GET(url = url)
# converting all the data to HTML format
parse <- htmlParse(resource)


links <- xpathSApply(parse, path="//a", xmlGetAttr, "href") %>% 
  as.tibble() %>% 
  filter(grepl('docs.google.com', value))

# Months - Year 
# May to Dec - 2020
# Complete next years. 
links <- links %>% 
  dplyr::rename(docs = 'value') %>%
  mutate(date = seq(lubridate::ymd('2020-05-01'),lubridate::today(),by='month')[1:nrow(links)], 
         year = lubridate::year(date), 
         month = lubridate::month(date)) %>% 
  dplyr::select(year, month, docs)





# =---- Testing phase -- 

### Read spreadsheeds 
# =---  Function reading_basic: 



  # https://www.kaggle.com/datasets/ampiiere/acnh-villager-popularity
  
  # Each villager belongs to a tier(1-6), and within each tier there are rankings. Each Tier has a different amount of rankings.
  # As of 2022, March:
  # The "Highest Popularity" tier contains 15 villagers
  # The "Very Popular" tier contains 25 villagers
  # The "Fairly Popular" tier contains 30 villagers
  # The "Middle Ground" tier contains 60 villagers
  # The "Less Popular" tier contains 120 villagers
  # The "Least Popular" tier contains 163 villagers
  


  url_doc <- links[1,3] %>% pull()
  
  # It doesn't work into a function by now... working on it
  # Maybe i can iterate using a loop using a 
  # list or something like that. And after that joing with the other tables. 
  # Reading spreadsheet --- 
  table_month <- googlesheets4::read_sheet(ss = url_doc, col_names = FALSE) %>% 
    as.tibble() %>% 
    set_names(c('villager', 'votes')) %>% 
    arrange(desc(votes)) %>% 
    # This line is gonna be temporal, but it works by now. 
    mutate(tier = c(rep("Highest_Popularity", 15),
                    rep("Very_Popular", 25),
                    rep("Fairly_Popular", 30),
                    rep("Middle_Ground", 60),
                    rep("Less_Popular", 120),
                    rep("Less_Popular", n() - (15+25+30+60+120))))



  tier_table <- dplyr::bind_cols(links[1,], table_month) %>% 
    dplyr::select(-docs)
  



   

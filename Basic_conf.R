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


links



# =---- Testing phase -- 
f <- links[1,3] %>% pull()
### Read spreadsheeds 

read_web_sheets <- function(){
  probando <- googlesheets4::read_sheet(ss = f, col_names = FALSE) 
  
  probando %>% 
    as.tibble() %>% 
    set_names(c('villager', 'votes'))
  
  googlesheets4::sheet_names(ss = f)

return()}

   

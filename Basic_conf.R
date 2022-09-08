# --- Animal Crossing New Horizons Villager Popularity
# By: Alejandra Esquivel Arias
# September/2022
#  On going ---- 


# R options
options(warn = -1, scipen = 999)

# Load libraries
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, leaflet, httr, XML, googlesheets4, rvest))
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
  

### Reading raw data -- we have two different formats of tables. 

#  TEMPORAL, FIRST RUN
tictoc::tic()
table_month <- list()
for(i in 1:nrow(links)){
  
  url_doc <- links[i,3] %>% pull()
  
  # It doesn't work into a function by now... working on it
  # Maybe i can iterate using a loop using a 
  # list or something like that. And after that joing with the other tables. 
  # Reading spreadsheet --- 
  table_month[[i]] <- googlesheets4::read_sheet(ss = url_doc, col_names = FALSE)
}
tictoc::toc() # 368.22/60 =  6.137 min 



# =---------------------------

# parameter
# raw_table <- table_month[[2]] 


defining_format <- function(raw_table){
  ncolums <- raw_table %>% dim(.) %>% .[2]
  
  if(ncolums > 2){
    
    # names_raw <- raw_table[1,]
    table_pre_clean <- raw_table[-1, ]  %>% 
      as.tibble() %>% 
      .[, 6:7] %>% 
      unnest() %>% 
      set_names(c('villager', 'votes'))
    
  }else{
    
    table_pre_clean <- raw_table %>% as.tibble() %>% 
      set_names(c('villager', 'votes'))
    
  }
  
  # Organizing data  
  table_fixed <- table_pre_clean %>% as.tibble() %>% 
    set_names(c('villager', 'votes')) %>% 
    arrange(desc(votes)) %>% 
    # This line is gonna be temporal, but it works by now. 
    mutate(tier = c(rep("Highest_Popularity", 15),
                    rep("Very_Popular", 25),
                    rep("Fairly_Popular", 30),
                    rep("Middle_Ground", 60),
                    rep("Less_Popular", 120),
                    rep("Less_Popular", n() - (15+25+30+60+120))))
  
return(table_fixed)}

  
defining_format(table_month[[1]] )

# Defining data. 
# We have some villager like  Raymond (it's tier one) with NA
# Needs QC. 
table_month_D <- purrr::map(.x = table_month, .f = defining_format)
  
# Raw data, should be cleaned...
# Join all data
all_data <- purrr::map2(.x =links %>% dplyr::group_split(row_number()) ,.y = table_month_D, .f = dplyr::bind_cols) %>% 
  dplyr::bind_rows(.) %>% 
  dplyr::select(-docs) %>% 
  drop_na(villager) %>% 
  dplyr::distinct(.)

   


#### =------------ Data for all of the Villagers 

library(rvest)

theurl <- "https://nookipedia.com/wiki/Villager/New_Horizons"
file<-read_html(theurl)
tables<-html_nodes(file, "table")
table1 <- html_table(tables[1], fill = TRUE) %>% .[[1]]

villagers_information <- table1 %>% 
  dplyr::select(-Poster) %>% 
  rename(villager = 'Name')



Complete_Data <- dplyr::inner_join(all_data, villagers_information, by = 'villager')

# all_data %>% 
#   dplyr::filter(villager == 'Raymond') %>% 
#   View(.)
# 
# all_data %>% dplyr::distinct(.)


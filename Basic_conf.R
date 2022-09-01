# install.packages('Tmisc')
# install.packages('datasauRus')
# install.packages('SimDesign')
# SimDesign::bias()



library(tidyverse)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, leaflet, httr, XML, googlesheets4))



url = "https://www.animalcrossingportal.com/games/new-horizons/guides/villager-popularity-list.php#"


# making http request
resource <- GET(url = url)

# converting all the data to HTML format
parse <- htmlParse(resource)


links <- xpathSApply(parse, path="//a", xmlGetAttr, "href") %>% 
  as.tibble() %>% 
  filter(grepl('docs.google.com', value))

f <- links[1,] %>% pull()




### Read spreadsheeds 
install.packages('googlesheets4')
library(googlesheets4)

googlesheets4::gs4_deauth()

probando <- googlesheets4::read_sheet(ss = f, col_names = FALSE) 

proban


probando %>% 
  as.tibble() %>% 
  set_names(c('villager', 'votes'))





googlesheets4::sheet_names(ss = f)


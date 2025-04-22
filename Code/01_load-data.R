
# load libraries ----------------------------------------------------------

library(here)
source(here('R', '00_loadpackages.R'))


# data-read-loop ----------------------------------------------------------

# initialize readin listing
mysheets_fromexcel <- list()

# get the list of all the sheet names
mysheetlist <- readxl::excel_sheets(path = here::here('data', 
                                                      'Guana_MASTER.xlsx'))

mysheetlist <- mysheetlist[-c(1,2,3)] # remove data dictionary, notes, and qaqc sheets
# create loop for the sheets
i = 1

for (i in 1:length(mysheetlist)){
  
  tempdf <- readxl::read_excel(path = here::here('data', 
                                                 'Guana_MASTER.xlsx'), 
                               sheet = mysheetlist[i]) %>% janitor::clean_names()
  tempdf <- tempdf %>% select(2:8, 14, 17)
  
  tempdf$sheetname_year <- mysheetlist[i]
  
  mysheets_fromexcel[[i]] <- tempdf 
  
}

mysheets_fromexcel

# merge all the lists into one tibble using dplyr::bind_rows()
dat <- purrr::reduce(mysheets_fromexcel, dplyr::bind_rows) %>% 
  janitor::clean_names()

rm(mysheets_fromexcel, tempdf, i, mysheetlist)


# load-data-dict ----------------------------------------------------------

dict <- readxl::read_excel(path = here::here('data', 
                                             'Guana_MASTER.xlsx'), 
                           sheet = "data-dictionary")

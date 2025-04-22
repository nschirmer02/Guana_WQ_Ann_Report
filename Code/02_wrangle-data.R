# filter flags and code ---------------------------------------------------

# look at all flags and codes in the datafile
unique(dat$flag)

# for analysis, remove all -3 flags and -2. 
# We'll keep the suspect flags <1> and below detection limit <-4>
# -3: 168, -2: 1054, -4: 2598

dat_a <-
  dat %>% 
  filter(!grepl("<-3>", flag) & 
           !grepl("<-2>", flag)) 


# wrangling ---------------------------------------------------------------

# remove wind direction data (character)
# remove f_record, flag, remarks, sheetname_year, activitytype, and component_long
# make sure result column is numeric
# remove 'GTMOLNUT', 'GTMGL1.5NUT', 'GTMGL2.5NUT', 'GTMGL3.5NUT'

sts <- c('GTMOLNUT', 'GTMGL1.5NUT', 'GTMGL2.5NUT', 'GTMGL3.5NUT')

dat_b <- 
  dat_a %>% 
  filter(component_short != "WIND_D" & !station_code %in% sts) %>% 
  select(-f_record, -flag,  -sheetname_year, -activity_type, -component_long) %>% 
  mutate(result = as.numeric(result)) 

rm(sts)


# merge and flip ----------------------------------------------------------

# merge with data dict
# keep site_acronym and wbid along with sample_date, component_short, and result
# filter to keep only long-term sites for analysis
# set site_acronym and 

dat_c <-
  dat_b %>% 
  left_join(dict) %>% 
  select(site_acronym, site_friendly, wbid, sample_date, component_short, result) %>% 
  mutate(site_friendly = factor(site_friendly,
                                levels = c('Micklers',
                                           'GL1',
                                           'GL2',
                                           'Lake Middle',
                                           'GL4',
                                           'Lake South',
                                           'River North',
                                           'GR1',
                                           'Guana River',
                                           'GR3')),
         site_acronym = factor(site_acronym,
                               levels = c('MK',
                                          'GL1',
                                          'GL2',
                                          'LM',
                                          'GL4',
                                          'LS',
                                          'RN',
                                          'GR1',
                                          'GR',
                                          'GR3'))) %>%
  tidyr::pivot_wider(id_cols = c('site_acronym', 'site_friendly', 'sample_date', 'wbid'),
                     names_from = 'component_short', 
                     values_from = 'result') %>%
  drop_na(site_acronym)


# cleanup -----------------------------------------------------------------

rm(dat_a, dat_b, dat)

dat_c<-dat_c %>%
  mutate(TN2 = ifelse(is.na(TN), TKN, TN), .before = TN) #258 values replaced

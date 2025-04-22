
se <- function(x) (sd(x, na.rm = T)/sqrt(length(x)))

# Geometric means and std errors of Entero, DO, TN/TKN, TP, and CHL 

dat_summary <- 
dat_c %>% 
  select(site_acronym, ENTERO, SALT, DO, TN, TKN, TP, NO23F, CHLa_C, Sucra, NH4F, TN2) %>% 
  group_by(site_acronym) %>% 
  summarise_all(list(~ mean(., na.rm = T), 
                     ~ sd(., na.rm = T),
                     ~ median(., na.rm = T), 
                     ~ se(.),
                     ~ min(., na.rm = T),
                     ~ max (., na.rm = T)))

dat_summary_nosite <- 
  dat_c %>% 
  select(ENTERO, SALT, DO, TN, TKN, TP, NO23F, CHLa_C, Sucra, NH4F, TN2) %>% 
  summarise_all(list(~ mean(., na.rm = T), 
                     ~ sd(., na.rm = T),
                     ~ median(., na.rm = T), 
                     ~ se(.),
                     ~ min(., na.rm = T),
                     ~ max (., na.rm = T))) %>%
  mutate(site_acronym ="AllSites")

combined_summary<-full_join(dat_summary, dat_summary_nosite) %>%
  drop_na(site_acronym)



dat_summary2 <- 
  dat_c %>% 
  select(sample_date, ENTERO, SALT, DO, TN, TKN, TP, NO23F, CHLa_C) %>% 
  group_by(year(sample_date)) %>% 
  summarise_all(list(~ mean(., na.rm = T), 
                     ~ sd(., na.rm = T),
                     ~ median(., na.rm = T), 
                     ~ se(.),
                     ~ min(., na.rm = T),
                     ~ max (., na.rm = T))) %>%
  select(! sample_date_mean) %>%
  mutate(`year(sample_date)`= as.character(`year(sample_date)`))

dat_summary_nosite2 <- 
  dat_c %>% 
  select(ENTERO, SALT, DO, TN, TKN, TP, NO23F, CHLa_C) %>% 
  summarise_all(list(~ mean(., na.rm = T), 
                     ~ sd(., na.rm = T),
                     ~ median(., na.rm = T), 
                     ~ se(.),
                     ~ min(., na.rm = T),
                     ~ max (., na.rm = T))) %>%
  mutate(`year(sample_date)` ="AllDates")

combined_summary2<-full_join(dat_summary2, dat_summary_nosite2)




writexl::write_xlsx(combined_summary, "SummaryStats_bySite_DON.xlsx")



##DON, load 04.4

dat_summary3 <- 
  dat_d %>% 
  select(site_acronym, DON) %>% 
  group_by(site_acronym) %>% 
  summarise_all(list(~ mean(., na.rm = T), 
                     ~ sd(., na.rm = T),
                     ~ median(., na.rm = T), 
                     ~ se(.),
                     ~ min(., na.rm = T),
                     ~ max (., na.rm = T)))

dat_summary_nosite <- 
  dat_d %>% 
  select(DON) %>% 
  summarise_all(list(~ mean(., na.rm = T), 
                     ~ sd(., na.rm = T),
                     ~ median(., na.rm = T), 
                     ~ se(.),
                     ~ min(., na.rm = T),
                     ~ max (., na.rm = T))) %>%
  mutate(site_acronym ="AllSites")

combined_summary<-full_join(dat_summary3, dat_summary_nosite) %>%
  drop_na(site_acronym)


dat_summary <- 
  dat_c %>% 
  select(site_acronym, WDEPTH) %>% 
  group_by(site_acronym) %>% 
  summarise_all(list(~ mean(., na.rm = T), 
                     ~ sd(., na.rm = T),
                     ~ median(., na.rm = T), 
                     ~ se(.),
                     ~ min(., na.rm = T),
                     ~ max (., na.rm = T)))


dat_summary_nosite <- 
  dat_c %>% 
  select(WDEPTH) %>% 
  summarise_all(list(~ mean(., na.rm = T), 
                     ~ sd(., na.rm = T),
                     ~ median(., na.rm = T), 
                     ~ se(.),
                     ~ min(., na.rm = T),
                     ~ max (., na.rm = T))) %>%
  mutate(site_acronym ="AllSites")

combined_summary<-full_join(dat_summary, dat_summary_nosite) %>%
  drop_na(site_acronym)


writexl::write_xlsx(combined_summary, "Stats_bySite_DEPTH.xlsx")

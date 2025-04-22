library(here)
source(here('R', '01_load-data.R'))
source(here('R', '02_wrangle-data.R'))
source(here('R', '00_vis_custom.R'))


# geometric means over time -----------------------------------------------

agm_plot <- function(param, site, ylab) {
  
  gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
  gmeansd <- function(x) exp(sd(log(x), na.rm = TRUE))
  se <- function(x) (sd(x, na.rm = T)/sqrt(length(x)))
  
  df <- dat_c %>% 
    filter(site_friendly == {{site}}) %>% 
    mutate(year = lubridate::year(sample_date)) %>%
    select(year, {{param}}) %>% 
    group_by(year) %>% 
    summarise_all(list(~ gmean(.),
                       ~ se(.))) %>% 
    mutate(site_friendly = site)
  
  ylab <- ylab
  
  p <- df %>% 
    ggplot(aes(x = year, y = gmean, color = site_friendly)) +
    geom_linerange(aes(ymax = gmean + se, ymin = gmean - se), color = "gray60") +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
    scale_color_manual(values = sitecolours) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text = element_text(color = "black")) +
    labs(x = "",
         y = ylab)
  
  print(p)
}

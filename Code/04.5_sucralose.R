library(here)
source(here('R', '01_load-data.R'))
source(here('R', '02_wrangle-data.R'))
source(here('R', '00_vis_custom.R'))


#sucralose figure--------------
sucra<-dat_c%>%
  drop_na(Sucra)%>%
  select(1:4, Sucra)

ggplot(data = dat_c%>%
         filter(site_acronym !="MK"&site_acronym !="LS"&site_acronym !="RN"),
       aes(x = site_acronym,
           y = Sucra,
           color = site_acronym)) +
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.8) +
  #geom_point(data = dat_summary%>%filter(site_acronym !="MK"&site_acronym !="LS"&site_acronym !="RN"), aes(x = site_acronym, y = Sucra_mean),  color = "black", size = 3) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  labs(x = "",
       y = "Sucralose (μg/L)")


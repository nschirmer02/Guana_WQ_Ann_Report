all_sites <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.
  
  p <- dat_c %>%
    #dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_colour_manual(name = "Station", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    #scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
    #theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "")
  
  p
}

# use the function to create full timeseries plots of whatever parameter you want, examples below

CHLA_all<-all_sites("CHLA_C", chla_y_title) +
  coord_cartesian(ylim = c(0, 220))

#this would be the 'call' for enterococcus
all_sites("ENTERO", entero_y_title) 



entLines<-dat_c %>%
  dplyr::filter(ENTERO>-1) %>%
  ggplot(aes(x = sample_date, y = ENTERO, color = site_acronym)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.5) +
  scale_colour_manual(name = "Station", values = sitecolours2) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2700)) +
  labs(y = entero_y_title,
       x = "")+
  facet_wrap(~wbid, nrow=2, scales = "free_x")+
  theme(strip.background=element_blank(), legend.title = element_blank(), axis.title = element_text(size=9), axis.text=element_text(size=9), title = element_text(size=9), legend.text = element_text(size=8), legend.position = c(0.02, 0.3), legend.key.size = unit(0.1, "cm"), strip.text = element_text(size=9))+
  #geom_hline(yintercept=35, linetype="dashed")+
  guides(color=guide_legend(nrow=5))


ggsave(plot = entLines, filename = "EntLines.pdf",  width = 120, height = 80, units = 'mm' )

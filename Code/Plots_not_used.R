### River and Lake in one panel
season_plot <- function(param, ylab) {
  
  gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
  gmeansd <- function(x) exp(sd(log(x), na.rm = TRUE))
  se <- function(x) (sd(x, na.rm = T)/sqrt(length(x)))
  
  df <- dat_c %>%
    mutate(year = lubridate::year(sample_date), month=lubridate::month(sample_date)) %>%
    arrange(year, month)%>%
    select(year, month, wbid, {{param}}) %>% 
    mutate(season=if_else(month %in% c(1, 2, 12), "winter", if_else(month %in% c(3, 4, 5), "spring", if_else(month %in% c(6, 7, 8), "summer", "fall"))))%>%
    mutate(year_season = paste(year, season, sep = "_"))%>%
    group_by(year_season, wbid)%>%
    summarise(gmean=gmean({{param}}), se=se({{param}}))%>%
    mutate(year_season=factor(year_season, levels=c("2017_summer", "2017_fall", "2018_winter",  "2018_spring", "2018_summer", "2018_fall", "2019_winter",  "2019_spring", "2019_summer", "2019_fall","2020_winter",   "2020_spring", "2020_summer", "2020_fall", "2021_winter",  "2021_spring", "2021_summer", "2021_fall", "2022_winter",  "2022_spring", "2022_summer", "2022_fall",   "2023_winter",  "2023_spring", "2023_summer",  "2023_fall","2024_winter")))
  
  ylab <- ylab
  
  p <- df %>% 
    ggplot(aes(x = year_season, y = gmean, color = wbid, group=wbid)) +
    stat_summary(fun.y=sum, geom="line", color="gray60", position=position_dodge2(width = 0.8))+
    geom_linerange(aes(ymax = gmean + se, ymin = ifelse(gmean - se < 0, 0, gmean - se)), color = "gray60", position=position_dodge2(width = 0.8)) + #keeps error bar from going below 0    
    geom_point(stat='summary', size=2, position=position_dodge2(width = 0.8)) +
    scale_x_discrete(breaks = c( "2017_summer", "2018_summer",  "2019_summer", "2020_summer",  "2021_summer", "2022_summer","2023_summer")) +
    theme_bw() +
    scale_color_manual(values = c("blue", "red"))+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text = element_text(color = "black")) +
    labs(x = "",
         y = ylab)
  
  print(p)
}

season_plot(param = CHLa_C,  ylab =chla_y_title ) + geom_hline(yintercept = 11, color = "blue", linetype = "dashed")+geom_hline(yintercept = 6.6, color = "red", linetype = "dashed")
season_plot(param = ENTERO,  ylab =entero_y_title ) + geom_hline(yintercept = 130, color = "purple", linetype = "dashed")
season_plot(param = FECCOL,  ylab =fecal_y_title) + geom_hline(yintercept = 43, color = "purple", linetype = "dashed")



##River and Lake split, color coded by season
season_plot2 <- function(param, ylab) {
  
  gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
  gmeansd <- function(x) exp(sd(log(x), na.rm = TRUE))
  se <- function(x) (sd(x, na.rm = T)/sqrt(length(x)))
  
  df <- dat_c %>%
    mutate(year = lubridate::year(sample_date), month=lubridate::month(sample_date)) %>%
    mutate(year2=if_else(month==12, year+1, year))%>%
    select(year2, month, wbid, {{param}}) %>% 
    mutate(season=if_else(month %in% c(1, 2, 12), "winter", if_else(month %in% c(3, 4, 5), "spring", if_else(month %in% c(6, 7, 8), "summer", "fall"))))%>%
    mutate(year_season = paste(year2, season, sep = "_"))%>%
    group_by(year_season, wbid, season, year2)%>%
    summarise(gmean=gmean({{param}}), se=se({{param}}))%>%
    mutate(season=factor(season, levels=c("winter", "spring", "summer", "fall")))%>%
    arrange(year2, season)%>%
    mutate(year_season=factor(year_season, levels=c("2017_summer", "2017_fall", "2018_winter",  "2018_spring", "2018_summer", "2018_fall", "2019_winter",  "2019_spring", "2019_summer", "2019_fall","2020_winter",   "2020_spring", "2020_summer", "2020_fall", "2021_winter",  "2021_spring", "2021_summer", "2021_fall", "2022_winter",  "2022_spring", "2022_summer", "2022_fall",   "2023_winter",  "2023_spring", "2023_summer",  "2023_fall","2024_winter")))
  
  ylab <- ylab
  
  p <- df %>% 
    ggplot(aes(x = year_season, y = gmean, group=1)) +
    stat_summary(fun.y=sum, geom="line", color="gray60")+
    geom_linerange(aes(ymax = gmean + se, ymin = ifelse(gmean - se < 0, 0, gmean - se)), color = "gray60") + #keeps error bar from going below 0
    geom_point(aes(color=season),stat='summary', size=2) +
    scale_x_discrete(breaks = c( "2017_summer", "2018_summer",  "2019_summer", "2020_summer",  "2021_summer", "2022_summer","2023_summer")) +
    theme_bw() +
    scale_color_manual(values = c("lightskyblue1", "steelblue3", "royalblue2", "navy"))+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text = element_text(color = "black")) +
    labs(x = "",
         y = ylab)+
    facet_grid(wbid~., scales = "free_y")
  
  print(p)
}


season_plot2(param = CHLa_C,  ylab =chla_y_title ) + geom_hline(data=dat_c%>%filter(wbid=="Lake"), aes(yintercept = 11), color = "blue", linetype = "dashed")+geom_hline(data=dat_c%>%filter(wbid=="River"), aes(yintercept = 6.6), color = "red", linetype = "dashed")

season_plot2(param = ENTERO,  ylab =entero_y_title ) + geom_hline(data=dat_c%>%filter(wbid=="Lake"), aes(yintercept = 130), color = "blue", linetype = "dashed")+geom_hline(data=dat_c%>%filter(wbid=="River"), aes(yintercept = 130), color = "red", linetype = "dashed")

season_plot2(param = FECCOL,  ylab =fecal_y_title ) + geom_hline(data=dat_c%>%filter(wbid=="Lake"), aes(yintercept = 43), color = "blue", linetype = "dashed")+geom_hline(data=dat_c%>%filter(wbid=="River"), aes(yintercept = 43), color = "red", linetype = "dashed")

season_plot2(param = )




## N Types
#prep for graphing and group by year
N_long<-N%>%
  drop_na(c(DIN, DON, PN))%>% #remove rows without all three measurements
  pivot_longer(cols = c(DIN, DON, PN), names_to = "Variable", values_to = "Value")%>%
  mutate(Year=year(sample_date))%>%
  group_by(site_friendly, Year, Variable)%>%
  mutate(Value=if_else(Value<0, 0, Value))%>%
  summarise(avg=mean(Value)) ##NOT USING GEOMETRIC MEAN BECAUSE ZEROS

ggplot(data=N_long,aes(x=site_friendly, y=avg, fill=Variable))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Year, ncol=3)+
  theme_bw()+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ylab("Nitrogen (mg/L)")+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name="Nitrogen Form")


N_pie<-N%>%
  drop_na(c(DIN, DON, PN))%>% #remove rows without all three measurements
  group_by(site_friendly)%>%
  mutate(PN=ifelse(PN<0, 0, PN))%>%
  summarise(DIN=mean(DIN), DON=mean(DON), PN=mean(PN), TN=mean(TN2))



make_pie <- function(x, y, size, groups, n, rownum) {
  angles <- c(0, 2*pi * cumsum(n)/sum(n))
  do.call("rbind", Map(function(DIN, DON, g) {
    xvals <- c(0, sin(seq(DIN, DON, len = 30)) * size, 0) + x
    yvals <- c(0, cos(seq(DIN, DON, len = 30)) * size, 0) + y
    data.frame(x = xvals, y = yvals, group = g, rownum = rownum)
  }, head(angles, -1), tail(angles, -1), groups))
}

N_pie %>%
  mutate(r = c(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)) %>%
  mutate(x = rep(1:5,2), y = rep(2:1, each=5)) %>%
  rowwise() %>%
  group_map(~ with(.x, make_pie(x, y, TN/6, #use sqrt(TN)/4 for less extreme size differences
                                c("DIN", "DON", "PN"),
                                c(DIN, DON, PN), r))) %>%
  bind_rows() %>%
  ggplot(aes(x, y, fill = group, group = interaction(group, rownum))) +
  geom_polygon() +
  annotate("text", x = rep(1:5,2), y = rep(1:2, each=5)+0.4, label = c( "Lake South", "River North", "GR1", "Guana River", "GR3", "Micklers", "GL1", "GL2", "Lake Middle", "GL4"), size = 3) +
  coord_equal() +
  theme_void(base_size = 20)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name="Nitrogen Form")+
  guides(shape = guide_legend(override.aes = list(fill = 0.1)))+
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8))

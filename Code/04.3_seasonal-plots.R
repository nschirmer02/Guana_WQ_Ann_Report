library(here)
library(cowplot)
source(here('R', '01_load-data.R'))
source(here('R', '02_wrangle-data.R'))
source(here('R', '00_vis_custom.R'))


dat_c<-dat_c%>%
  mutate(TNTP=TN2/TP)

# geometric means by year -----------------------------------------------

Annual_plot <- function(param, ylab) {
  
  gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
  gmeansd <- function(x) exp(sd(log(x), na.rm = TRUE))
  se <- function(x) (sd(x, na.rm = T)/sqrt(length(x)))
  
  df <- 
    dat_c %>% 
    mutate(year = lubridate::year(sample_date)) %>%
    select(year, wbid, {{param}}) %>% 
    group_by(year, wbid) %>% 
    summarise_all(list(~ gmean(.),
                       ~ se(.)))
  
  ylab <- ylab
  
  p <- df %>% 
    ggplot(aes(x = year, y = gmean, color = wbid)) +
    geom_linerange(aes(ymax = gmean + se, ymin = ifelse(gmean - se < 0, 0, gmean - se)), color = "gray60", position = position_dodge2(width = 0.3)) + #keeps error bar from going below 0 and messing up scale  
    geom_point(position=position_dodge2(width = 0.3)) +
    geom_line(position=position_dodge2(width = 0.3)) +
    scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
    theme_classic() +
    scale_color_manual(values = c("blue", "darkorange"))+
    theme(legend.title = element_blank(), axis.text = element_text(color = "black"), axis.title = element_text(size = 10), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) +
    labs(x = "",
         y = ylab)
  
  print(p)
}


##geom_hline represent state maximum criteria for each metric for the lake (blue) and river (red) or both (black)

a_chl<-Annual_plot(param = "CHLa_C",  ylab = chla_y_title) + geom_hline(yintercept = 11, color = "blue", linetype = "dashed")+geom_hline(yintercept = 6.6, color = "darkorange", linetype = "dashed") + theme(legend.position = c(0.2,0.8))+ylim(0, 25)


a_tn<-Annual_plot(param = "TN2",  ylab = "Total N, mg/L") + geom_hline(yintercept = 0.65, color = "darkorange", linetype = "dashed")+theme(legend.position = c(0.1,0.9), legend.text = element_text(size=8), legend.background = element_rect(color = NA), legend.key.size = unit(0.3, "cm"))+ylim(0, 2.6)

a_tp<-Annual_plot(param = "TP",  ylab = "Total Phos., mg/L") + geom_hline(yintercept = 0.105, color = "darkorange", linetype = "dashed")+theme(legend.position = "none")+ylim(0, 0.17)

a_tntp<-Annual_plot(param = "TNTP",  ylab = "TN to TP ratio") + geom_hline(yintercept = 7.2, color = "black", linetype = "dashed")+theme(legend.position = "none")+ylim(0, 28)

a_entero<-Annual_plot(param = "ENTERO",  ylab =entero_y_title ) + geom_hline(yintercept = 35, color = "black", linetype = "dashed")+theme(legend.position = "none")+ylim(0, 200)


a_nh4<-Annual_plot(param = "NH4F",  ylab ="Ammonium, mg/L" )+theme(legend.position = "none")+ylim(0, 0.19)


Annual_plot(param = "FECCOL",  ylab =fecal_y_title) + geom_hline(yintercept = 43, color = "black", linetype = "dashed")




### Geometric means by month
season_plot1 <- function(param, ylab) {
  
  gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
  gmeansd <- function(x) exp(sd(log(x), na.rm = TRUE))
  se <- function(x) (sd(x, na.rm = T)/sqrt(length(x)))
  
  df <- dat_c %>%
    mutate( month=lubridate::month(sample_date, label = TRUE)) %>%
    arrange(month)%>%
    select( month, wbid, {{param}}) %>% 
    group_by(month, wbid)%>%
    summarise(gmean=gmean({{param}}), se=se({{param}}))
  
  ylab <- ylab
  
  p <- df %>% 
    ggplot(aes(x = month, y = gmean, color = wbid, group=wbid)) +
    stat_summary(fun.y=sum, geom="line", position=position_dodge2(width = 0.3))+
    geom_linerange(aes(ymax = gmean + se, ymin = ifelse(gmean - se < 0, 0, gmean - se)), color = "gray60", position = position_dodge2(width = 0.3)) + #keeps error bar from going below 0 and messing up scale  
    geom_point(stat='summary', size=2, position=position_dodge2(width = 0.3)) +
    theme_classic() +
    scale_color_manual(values = c("blue", "darkorange"))+
    theme(legend.position = "none", legend.title = element_blank(), axis.text = element_text(color = "black"), axis.title = element_text(size = 10), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) + 
    labs(x = "",
         y = ylab)
  
  print(p)
}

s_chl<-season_plot1(param = CHLa_C,  ylab =chla_y_title ) + geom_hline(yintercept = 11, color = "blue", linetype = "dashed")+geom_hline(yintercept = 6.6, color = "darkorange", linetype = "dashed")+ylim(0, 25)


s_entero<-season_plot1(param = ENTERO,  ylab =entero_y_title ) + geom_hline(yintercept = 35, color = "black", linetype = "dashed")+ +ylim(0, 200)


season_plot1(param = FECCOL,  ylab =fecal_y_title) + geom_hline(yintercept = 43, color = "black", linetype = "dashed")

s_TN<-season_plot1(param = TN,  ylab = "Total N, mg/L") + geom_hline(yintercept = 0.65, color = "darkorange", linetype = "dashed")+ylim(0, 2.6)

s_TP<-season_plot1(param = TP,  ylab = "Total Phos., mg/L") + geom_hline(yintercept = 0.105, color = "darkorange", linetype = "dashed")+ylim(0, 0.17)

s_TNTP<-season_plot1(param = TNTP,  ylab = "TN to TP ratio") + geom_hline(yintercept = 7.2, color = "black", linetype = "dashed")+ylim(0, 28)

s_NH4<-season_plot1(param = NH4F, ylab="Ammonium, mg/L")+ylim(0, 0.19)


#build figure
(Fig<-ggdraw()+
    draw_plot(a_tn, x=0.0, y=0.73, width=0.5, height=0.27)+
    draw_plot(s_TN, x=0.5, y=0.73, width = 0.5, height =0.27)+
    draw_plot(a_nh4, x=0.0, y=0.48, width = 0.5, height =0.27)+
    draw_plot(s_NH4, x=0.5, y=0.48, width=0.5, height=0.27)+
    draw_plot(a_tp, x=0.0, y=0.23, width = 0.5, height =0.27)+
    draw_plot(s_TP, x=0.5, y=0.23, width = 0.5, height =0.27)+
    draw_plot(a_tntp, x=0.0, y=-0.02, width = 0.5, height =0.27)+
    draw_plot(s_TNTP, x=0.5, y=-0.02, width = 0.5, height =0.27))


(Fig2<-ggdraw()+
    draw_plot(a_chl, x=0.0, y=0.5, width=0.5, height=0.51)+
    draw_plot(s_chl, x=0.5, y=0.5, width = 0.5, height =0.51)+
    draw_plot(a_entero, x=0.0, y=0, width = 0.5, height =0.51)+
    draw_plot(s_entero, x=0.5, y=0, width=0.5, height=0.51))

#ggsave(plot = Fig, filename = "8panel_timeseries.pdf",  width = 190, height = 150, units = 'mm' )

ggsave(plot = Fig2, filename = "4panel_timeseries.pdf",  width = 190, height = 100, units = 'mm' )

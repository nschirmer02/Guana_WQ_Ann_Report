library(here)
library(cowplot)
source(here('R', '01_load-data.R'))
source(here('R', '02_wrangle-data.R'))
#source(here('R', '03_summary-tbl.R'))
source(here('R', '00_vis_custom.R'))
dat_c<-dat_c%>%
  mutate(TNTP=TN2/TP)
# a variety of point plots with medians overlaid

# salinity gradient jitter ------------------------------------------------

ggplot(data = dat_c,
       aes(x = site_acronym,
           y = SALT,
           color = site_acronym)) +
  geom_jitter(width = 0.2, alpha = 0.8) +
  geom_point(data = dat_summary, aes(x = site_acronym, y = SALT_median), 
             color = "black", size = 3) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Salinity, PSU")

# salinity gradient jitter with waterbody ---------------------------------

ggplot(data = dat_c,
       aes(x = site_acronym,
           y = SALT,
           color = site_acronym)) +
  geom_jitter(width = 0.2, alpha = 0.8) +
  facet_grid(~wbid, scales = "free", space = "free") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Salinity, PSU")


# salinity gradient point -------------------------------------------------

# ggplot(data = dat_c,
#        aes(x = site_acronym,
#            y = SALT,
#            color = site_acronym)) +
#   geom_point(alpha = 0.8) +
#   geom_point(data = dat_summary, aes(x = site_acronym, y = SALT_median), 
#              color = "black", size = 3) +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.text = element_text(color = "black")) +
#   labs(x = "",
#        y = "Salinity, PSU")


# salinity gradient jitter with boxplot ------------------------------------------------

##BE SURE TO REMOVE BOXPLOT OUTLIERS TO AVOID DUPLICATE POINTS (outlier_shape=NA)

ggplot(data = dat_c,
       aes(x = site_acronym,
           y = SALT,
           color = site_acronym)) +
  geom_jitter(width = 0.2, alpha = 0.8) +
  geom_boxplot(alpha=0.5) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  labs(x = "",
       y = "Salinity, PSU")


sal<-ggplot(data = dat_c, ###NOT RAINBOW
       aes(x = site_acronym,
           y = SALT,
           color = wbid)) +
  #geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA, aes(ymin=quantile(na.omit(SALT), 0.05), ymax=quantile(na.omit(SALT), 0.95))) +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_text(color = "black"), axis.title = element_text(size = 9), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) +
  labs(x = "",
       y = "Salinity, PSU")+
  scale_color_manual(values = c("blue", "darkorange"))

###Other metrics-----

##Sucrulose
suc<-ggplot(data = dat_c, ###NOT RAINBOW
            aes(x = site_acronym,
                y = Sucra,
                color = wbid)) +
  #geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA, aes(ymin=quantile(na.omit(Sucra), 0.05), ymax=quantile(na.omit(Sucra), 0.95))) +
  theme_classic() +
  theme(legend.position = c(.8,.85), legend.title = element_blank(),
        axis.text = element_text(color = "black"), axis.title = element_text(size = 9), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) +
  labs(x = "",
       y = "Sucralose, μg/L")+
  scale_color_manual(values = c("blue", "darkorange"))




##TN
tn<-ggplot(data = dat_c%>%
             filter(TN2>-1)%>%
             filter(between(TN2, quantile(TN2, 0.0), quantile(TN2, 0.95))),
       aes(x = site_acronym,
           y = TN2,
           color = wbid)) +
  geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(data=dat_c, alpha=0.5, outlier.shape = NA) +
  theme_classic() +
  theme(legend.position = c(0.85, 0.83), legend.title = element_blank(),
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  ylim(0,4)+
  labs(x = "",
       y = nitro_y_title)+
  scale_color_manual(values = c("blue", "darkorange"))


##CHL
chl<-ggplot(data = dat_c%>%
              filter(CHLa_C>-1)%>%
              filter(between(CHLa_C, quantile(CHLa_C, 0.0), quantile(CHLa_C, 0.90))),
       aes(x = site_acronym,
           y = CHLa_C,
           color = wbid)) +
  geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  labs(x = "",
       y = "Chlorophyll a, μg/L")+
  scale_color_manual(values = c("blue", "darkorange"))



##TP
tp<-ggplot(data = dat_c%>%
             filter(TP>-1)%>%
             filter(between(TP, quantile(TP, 0.0), quantile(TP, 0.90))),
       aes(x = site_acronym,
           y = TP,
           color = wbid)) +
  geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  labs(x = "",
       y = phos_y_title)+
  scale_color_manual(values = c("blue", "darkorange"))



##ENTERO
ggplot(data = dat_c,
       aes(x = site_acronym,
           y = ENTERO,
           color = site_acronym)) +
  geom_jitter(width = 0.2, alpha = 0.8) +
  geom_boxplot(alpha=0.5) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  labs(x = "",
       y = entero_y_title)

#ENTERO SHOWING 90% OF DATA
ent<-ggplot(data = dat_c%>%
         filter(ENTERO>-1)%>%
         filter(between(ENTERO, quantile(ENTERO, 0.0), quantile(ENTERO, 0.90))),
       aes(x = site_acronym,
           y = ENTERO,
           color = wbid)) +
  geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  labs(x = "",
       y = entero_y_title)+
  scale_color_manual(values =c("blue", "darkorange"))

#ent shaded
ent1<-ggplot(data = dat_c%>%
         filter(ENTERO>-1)%>%
         filter(between(ENTERO, quantile(ENTERO, 0.0), quantile(ENTERO, 0.90))),
       aes(x = site_acronym,
           y = ENTERO)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=0, ymax=35, alpha=0.2, fill="forestgreen", color=NA)+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=35, ymax=70, alpha=0.2, fill="orange", color=NA)+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=70, ymax=Inf, alpha=0.2, fill="red", color=NA)+
  #geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(aes(ymin=quantile(na.omit(ENTERO), 0.05), ymax=quantile(na.omit(ENTERO), 0.95)), alpha=0.5, outlier.shape = NA, color="black") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 9), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) +
  labs(x = "",
       y = entero_y_title)

ggplot(data = dat_c,
       aes(x = site_acronym,
           y = ENTERO)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=0, ymax=35, alpha=0.2, fill="forestgreen", color=NA)+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=35, ymax=70, alpha=0.2, fill="orange", color=NA)+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=70, ymax=Inf, alpha=0.2, fill="red", color=NA)+
  geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA, color="black") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  labs(x = "",
       y = entero_y_title)

##NH4
nh4<-ggplot(data = dat_c%>%
              filter(NH4F>-1)%>%
              filter(between(NH4F, quantile(NH4F, 0.0), quantile(NH4F, 0.90))),
       aes(x = site_acronym,
           y = NH4F,
           color = wbid)) +
  geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  labs(x = "",
       y = "Ammonium mg/L")+
  scale_color_manual(values =c("blue", "darkorange"))


#TNTP
tntp<-ggplot(data = dat_c%>%
               filter(TNTP>-1)%>%
               filter(between(TNTP, quantile(TNTP, 0.0), quantile(TNTP, 0.90))),
       aes(x = site_acronym,
           y = TNTP,
           color=wbid)) +
  geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  labs(x = "",
       y = "TN to TP Ratio ")+
  scale_color_manual(values = c("blue", "darkorange"))

#DON
don<-ggplot(data = dat_d%>%
               filter(DON>-1)%>%
               filter(between(DON, quantile(DON, 0.0), quantile(DON, 0.90))),
             aes(x = site_acronym,
                 y = DON,
                 color=wbid)) +
  geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 12), axis.text.x= element_text(size=10), axis.text.y= element_text(size=10)) +
  labs(x = "",
       y = "Dissolved Organic Nitrogen mg/L")+
  scale_color_manual(values = c("blue", "darkorange"))


(Fig<-ggdraw()+
    draw_plot(sal, x=0.0, y=0.75, width=0.5, height=0.26)+
    draw_plot(chl, x=0.5, y=0.75, width = 0.5, height =0.26)+
    draw_plot(tn, x=0.0, y=0.5, width = 0.5, height =0.26)+
    draw_plot(nh4, x=0.5, y=0.5, width=0.5, height=0.26)+
    draw_plot(tp, x=0.0, y=0.25, width = 0.5, height =0.26)+
    draw_plot(tntp, x=0.5, y=0.25, width = 0.5, height =0.26)+
    draw_plot(suc, x=0.0, y=0, width = 0.5, height =0.26)+
    draw_plot(ent, x=0.5, y=0, width = 0.5, height =0.26))


(Fig2<-ggdraw()+
    draw_plot(tn, x=0.0, y=0.66, width=0.5, height=0.34)+
    draw_plot(tp, x=0.5, y=0.66, width = 0.5, height =0.34)+
    draw_plot(tntp, x=0.0, y=0.33, width = 0.5, height =0.34)+
    draw_plot(chl, x=0.5, y=0.33, width=0.5, height=0.34)+
    draw_plot(nh4, x=0.0, y=0, width = 0.5, height =0.34)+
    draw_plot(don, x=0.5, y=0, width = 0.5, height =0.34))

(Fig3<-ggdraw()+
    draw_plot(sal, x=0.0, y=-0.02, width=0.5, height=1.02)+
    draw_plot(suc, x=0.5, y=-0.02, width=0.5, height=1.02))

ggsave(plot = Fig3, filename = "2panel_station_nodots.pdf",  width = 190, height = 50, units = 'mm' )

ggsave(plot = ent1, filename = "ENTshaded_nodots.pdf",  width = 90, height = 50, units = 'mm' )


###############################


##TN
tn<-ggplot(data = dat_c,
           aes(x = site_acronym,
               y = TN2,
               color = wbid)) +
  #geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA, aes(ymin=quantile(na.omit(TN2), 0.05), ymax=quantile(na.omit(TN2), 0.95))) +
  theme_classic() +
  theme(legend.position = c(0.85, 0.83), legend.title = element_blank(),
        axis.text = element_text(color = "black"), axis.title = element_text(size = 9), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) +
  labs(x = "",
       y = "Total N, mg/L")+
  scale_color_manual(values = c("blue", "darkorange"))+
  coord_cartesian(ylim = c(0, 4))
length(dat_c$TN[which(dat_c$TN>4)])

##CHL
chl<-ggplot(data = dat_c,
            aes(x = site_acronym,
                y = CHLa_C,
                color = wbid)) +
  #geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA, aes(ymin=quantile(na.omit(CHLa_C), 0.05), ymax=quantile(na.omit(CHLa_C), 0.95))) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 9), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) +
  labs(x = "",
       y = "Chlorophyll a, μg/L")+
  scale_color_manual(values = c("blue", "darkorange"))+
  coord_cartesian(ylim = c(0, 82))
length(dat_c$CHLa_C[which(dat_c$CHLa_C>82)])


##TP
tp<-ggplot(data = dat_c,
           aes(x = site_acronym,
               y = TP,
               color = wbid)) +
  #geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA, aes(ymin=quantile(na.omit(TP), 0.05), ymax=quantile(na.omit(TP), 0.95))) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 9), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) +
  labs(x = "",
       y = "Total Phos., mg/L")+
  scale_color_manual(values = c("blue", "darkorange"))+
  coord_cartesian(ylim = c(0, 0.43))
length(dat_c$TP[which(dat_c$TP>0.43)])



##NH4
nh4<-ggplot(data = dat_c,
            aes(x = site_acronym,
                y = NH4F,
                color = wbid)) +
  #geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA, aes(ymin=quantile(na.omit(NH4F), 0.05), ymax=quantile(na.omit(NH4F), 0.95))) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 9), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) +
  labs(x = "",
       y = "Ammonium, mg/L")+
  scale_color_manual(values =c("blue", "darkorange"))  +
  coord_cartesian(ylim = c(0, 0.21))
length(dat_c$NH4F[which(dat_c$NH4F>0.21)])

#TNTP
tntp<-ggplot(data = dat_c,
             aes(x = site_acronym,
                 y = TNTP,
                 color=wbid)) +
  #geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA, aes(ymin=quantile(na.omit(TNTP), 0.05), ymax=quantile(na.omit(TNTP), 0.95))) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 9), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) +
  labs(x = "",
       y = "TN to TP Ratio ")+
  scale_color_manual(values = c("blue", "darkorange"))+
  coord_cartesian(ylim = c(0, 52))

length(dat_c$TNTP[which(dat_c$TNTP>52)])

#DON
don<-ggplot(data = dat_d,
            aes(x = site_acronym,
                y = DON,
                color=wbid)) +
  #geom_jitter(width = 0.2, alpha = 0.8, color="grey30") +
  geom_boxplot(alpha=0.5, outlier.shape = NA, aes(ymin=quantile(na.omit(DON), 0.05), ymax=quantile(na.omit(DON), 0.95))) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"), axis.title = element_text(size = 9), axis.text.x= element_text(size=9), axis.text.y= element_text(size=9)) +
  labs(x = "",
       y = "Dissolved Organic N, mg/L")+
  scale_color_manual(values = c("blue", "darkorange"))+
  coord_cartesian(ylim = c(0, 3.01))
length(dat_d$DON[which(dat_d$DON>3.01)])/length(dat_d$DON)


(Fig2<-ggdraw()+
    draw_plot(tn, x=0.0, y=0.645, width=0.5, height=0.36)+
    draw_plot(tp, x=0.5, y=0.645, width = 0.5, height =0.36)+
    draw_plot(tntp, x=0.0, y=0.32, width = 0.5, height =0.36)+
    draw_plot(chl, x=0.5, y=0.32, width=0.5, height=0.36)+
    draw_plot(nh4, x=0.0, y=-0.02, width = 0.5, height =0.36)+
    draw_plot(don, x=0.5, y=-0.02, width = 0.5, height =0.36))

ggsave(plot = Fig2, filename = "6panel_station.pdf",  width = 190, height = 100, units = 'mm' )


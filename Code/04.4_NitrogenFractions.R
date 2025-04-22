library(here)
source(here('R', '01_load-data.R'))
source(here('R', '02_wrangle-data.R'))
source(here('R', '00_vis_custom.R'))


#TN_breakdown

dat_d<-dat_c%>% #calculate N components
  rowwise()%>%
  mutate(DIN = NH4F + NO23F,
       TN2 = TKN + NO23F,
       DON = TKNF - NH4F,
       PN = TN2 - (DIN + DON))%>%
  ungroup()

N<-dat_d%>%
  select(1:4, TN, NH4F, NO23F, TKN, DIN, DON, PN, TN2) #look at just N forms



#Without year
N_long2<-N%>%
  drop_na(c(DIN, DON, PN))%>% #remove rows without all three measurements
  pivot_longer(cols = c(DIN, DON, PN), names_to = "Variable", values_to = "Value")%>%
  mutate(Year=year(sample_date))%>%
  group_by(site_acronym, Variable, wbid)%>%
  mutate(Value=if_else(Value<0, 0, Value))%>%
  summarise(avg=mean(Value)) ##NOT USING GEOMETRIC MEAN BECAUSE ZEROS

(Nfrac<-ggplot(data=N_long2,aes(x=site_acronym, y=avg, fill=Variable))+
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  xlab("")+
  theme(legend.position = c(0.9, 0.7), legend.title = element_blank(), axis.text = element_text(color = "black"), axis.title = element_text(size = 9), axis.text.y= element_text(size=9), axis.text.x = element_text(size=9), legend.text = element_text(size=8), legend.background = element_rect(color = NA), legend.key.size = unit(0.3, "cm"), axis.title.y= element_text(size=9), axis.title.x = element_text(size=9))+
  ylab("Nitrogen (mg/L)")+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name="Nitrogen Form")+
  facet_grid(~wbid, scales = "free_x", space = "free"))




N%>%
  drop_na(c(DIN, DON, PN))%>% #remove rows without all three measurements
  pivot_longer(cols = c(DIN, DON, PN), names_to = "Variable", values_to = "Value")%>%
  mutate(Year=year(sample_date))%>%
  group_by(Variable, wbid)%>%
  mutate(Value=if_else(Value<0, 0, Value))%>%
  summarise(avg=mean(Value), se= sd(Value)/sqrt(length(Value)))


ggsave(plot = Nfrac, filename = "Nfrac.pdf",  width = 90, height = 50, units = 'mm' )


library(tidyverse)
library(lubridate)
library(ggplot2)
co<-read_csv("https://www.epa.gov/sites/production/files/2016-08/ghg-concentrations_fig-1.csv",skip = 6)
me<-read_csv("https://www.epa.gov/sites/production/files/2016-08/ghg-concentrations_fig-2.csv",skip = 6)
no<-read_csv("https://www.epa.gov/sites/production/files/2016-08/ghg-concentrations_fig-3.csv",skip = 6)

co<-gather(co,location,co2,2:11)
me<-gather(me,location,nh4,2:6)
no<-gather(no,location,no2,2:7)
head(co)
head(me)
head(no)
a<-bind_rows(co,me,no) %>% rename(Year=`Year (negative values = BC)`) %>% mutate(Year=as.numeric(Year))
b<-gather(a,type,val,3:5)
head(b )

bb<-filter(b,Year>1699, !is.na(val), type=="co2") %>% mutate(val=as.numeric(val))
ggplot(bb,aes(x=Year,y=val),colour="blue")+
  geom_smooth(se = F,size=1.5)+
  geom_vline(xintercept = c(1951),size=1.1,colour="red")+
  ylab(~CO[2]~Concentration~(ppm))+
  theme_bw(base_size = 18)+
  theme(panel.grid = element_blank())+
  scale_x_continuous(breaks = c(1700,1800,1850,1900,1925,1950,1975,2000,2015))+
  # geom_text(angle=90,label="Bowhead Whale Born",x=1879,y=325,vjust=0, hjust=0,size=10)+
  geom_text(angle=90,label="Wisdom Hatches",x=1950,y=325,vjust=0, hjust=0,size=10)
ggsave("CO2_1800-2015.jpg",width = 12,height = 8,dpi=120)

bb %>% 
  group_by(Year) %>% 
  filter(Year%in%c(1700,1880,1950,2015))


# Pop
pop<-data.frame(Year=c(1923,1956,1962,1982,2005,2016),Pairs=c(30000,100000,72000,235000,408000,457451))

ggplot(pop,aes(x=Year,y=Pairs),colour="blue")+
  geom_smooth(se = F,size=1.5)+
  geom_vline(xintercept = c(1951),size=1.1,colour="red")+
  ylab("Laysan Albatross Pairs")+
  theme_bw(base_size = 18)+theme(panel.grid = element_blank())+
  scale_x_continuous(breaks = c(1920,1950,1975,2000,2015))+ 
  scale_y_continuous(labels = c("100,000","200,000","300,000","400,000","500,000"),breaks = c(100000,200000,300000,400000,500000))+ 
  geom_text(angle=90,label="Wisdom Hatches",x=1950,y=150000,vjust=0, hjust=0,size=10)
ggsave("LAAL_Pop1920-2015.jpg",width = 12,height = 8,dpi=120)
# PLastic
plast<-data.frame(Year=c(1930,1950,1976,1989,2002,2005,2014),Mton=c(0,1.3,50,100,200,230,311))

ggplot(plast,aes(x=Year,y=Mton),colour="blue")+
  geom_smooth(se = F,size=1.5)+
  geom_vline(xintercept = c(1951),size=1.1,colour="red")+
  ylab("Global Plastic Production (tons in millions)")+
  theme_bw(base_size = 18)+theme(panel.grid = element_blank())+
  scale_x_continuous(breaks = c(1950,1975,2000,2015),limits = c(1940,2020))+ 
  # scale_y_continuous(labels = c("10,000","100,000","200,000","300,000","400,000","500,000"),breaks = c(10000,100000,200000,300000,400000,500000))+ 
  geom_text(angle=90,label="Wisdom Hatches",x=1950,y=50,vjust=0, hjust=0,size=10)
ggsave("LAAL_Pplastic1920-2015.jpg",width = 12,height = 8,dpi=120)

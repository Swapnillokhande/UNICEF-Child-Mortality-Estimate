# Read xlsx into R
library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)
#create country U5MR dataset
U5MR_mortality_rate_2018_country <- read_excel("D:/Github/UNICEF/U5MR_mortality_rate_2018.xlsx", 
                                       sheet = "Three Countries")
U5MR_mortality_rate_2018_country <- as.data.frame(U5MR_mortality_rate_2018_country)
#U5MR_mortality_rate_2018_country$

#create region U5MR dataset
U5MR_world_and_region<- read_excel("D:/Github/UNICEF/U5MR_mortality_rate_2018.xlsx", 
                                   sheet = "Region and World")
U5MR_world_and_region<-as.data.frame(U5MR_world_and_region)

#create SDG U5MR dataset
U5MR_sdg<- read_excel("D:/Github/UNICEF/U5MR_mortality_rate_2018.xlsx", 
                                   sheet = "SDG")
U5MR_sdg<-as.data.frame(U5MR_sdg)

#create cause death country dataset for age under 5 months
under5death_country<- read_excel("D:/Github/UNICEF/Cause-of-Death-2017.xlsx", 
                         sheet = "Death under 5")
under5death_country<-as.data.frame(under5death_country)
under5death_country<-under5death_country[-c(14,28,42,56,70,84),]
under5death_country <- under5death_country %>% mutate(id = row_number())
under5death_country$id<-NULL

#create cause death region dataset for age under 5 months
under5death_region<- read_excel("D:/Github/UNICEF/Cause-of-Death-2017.xlsx", 
                               sheet = "Region Under 5")
under5death_region<-as.data.frame(under5death_region)

#create cause death country dataset for age under 1 month
under1death_country<- read_excel("D:/Github/UNICEF/Cause-of-Death-2017.xlsx", 
                         sheet = "Death under 1")
under1death_country<-as.data.frame(under1death_country)
under1death_country<-under1death_country[-c(9,18,27,36,45,54),]
under1death_country <- under1death_country %>% mutate(id = row_number())
under1death_country$id<-NULL


#create cause death dataset by region for age under 1 month 
under1death_region<- read_excel("D:/Github/UNICEF/Cause-of-Death-2017.xlsx", 
                               sheet = "Region under 1")
under1death_region<-as.data.frame(under1death_region)

#plot a line graph comparing the countries based on mortality rate
U5MR_mortality_china<-U5MR_mortality_rate_2018_country[U5MR_mortality_rate_2018_country$`Country Name`=="China",]
U5MR_mortality_India<-U5MR_mortality_rate_2018_country[U5MR_mortality_rate_2018_country$`Country Name`=="India",]
U5MR_mortality_US<-U5MR_mortality_rate_2018_country[U5MR_mortality_rate_2018_country$`Country Name`=="United States",]
U5MR_world<-U5MR_world_and_region[U5MR_world_and_region$Region=="World",]

library("reshape2")
library("ggplot2")
U5MR_greater_than_1990<-U5MR_mortality_rate_2018_country[U5MR_mortality_rate_2018_country$Year>=1990.00,]

ggplot(U5MR_greater_than_1990, aes(Year,`Mortality Rate`,colour= `Country Name`)) +
  geom_line(size=1)+
  geom_point()+
  xlim(min(U5MR_greater_than_1990$Year),max(U5MR_greater_than_1990$Year))+
  scale_color_manual(values=c('light blue','light green','pink'))+
  #guides(fill=guide_legend(title="New Legend Title"))+
  ggtitle("Change in the mortality rate in last two decades in\nthree highly populated countries") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank())

#compare the mortality rate with the world
U5MR_world_and_region$Uncertanity<-NULL
U5MR_greater_than_1990$`ISO Code`<-NULL
U5MR_greater_than_1990$Uncertanity<-NULL
colnames(U5MR_world_and_region)<-c("Country Name","Year","Mortality Rate")

U5MR_world<-U5MR_world_and_region[U5MR_world_and_region$`Country Name`=="World",]


U5MR_merge<-rbind(U5MR_greater_than_1990,U5MR_world)

ggplot(U5MR_merge, aes(Year,`Mortality Rate`,colour= `Country Name`)) +
  geom_line(size=1)+
  geom_point()+
  xlim(min(U5MR_greater_than_1990$Year),max(U5MR_greater_than_1990$Year))+
  #guides(fill=guide_legend(title="New Legend Title"))+
  ggtitle("Change in the mortality rate in selected countries\nin comparison to the World") +
  scale_color_manual(values=c('light blue','light green','pink','red'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.8,0.85),
        legend.title = element_blank())

#compare each country corresponding to its region
china_region<-rbind(U5MR_greater_than_1990[U5MR_greater_than_1990$`Country Name`=="China",],U5MR_world_and_region[U5MR_world_and_region$`Country Name`=="East Asia and Pacific",])
united_region<-rbind(U5MR_greater_than_1990[U5MR_greater_than_1990$`Country Name`=="United States",],U5MR_world_and_region[U5MR_world_and_region$`Country Name`=="North America",])
india_region<-rbind(U5MR_greater_than_1990[U5MR_greater_than_1990$`Country Name`=="India",],U5MR_world_and_region[U5MR_world_and_region$`Country Name`=="South Asia",])

#compare China with its region
ggplot(china_region, aes(Year,`Mortality Rate`,colour= `Country Name`)) +
  geom_line(size=1)+
  geom_point()+
  xlim(min(china_region$Year),max(china_region$Year))+
  ggtitle("Change in the mortality rate in China in comparison\nto East Asia and Pacific") +
  scale_color_manual(values=c('light blue','dark blue'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.85,0.9),
        legend.title = element_blank())

#Compare india with its region
ggplot(india_region, aes(Year,`Mortality Rate`,colour= `Country Name`)) +
  geom_line(size=1)+
  geom_point()+
  xlim(min(india_region$Year),max(india_region$Year))+
  ggtitle("Change in the mortality rate in India in comparison\nto South Asia") +
  scale_color_manual(values=c('light green','dark green'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.9,0.9),
        legend.title = element_blank())

#compare united state to its region
ggplot(united_region, aes(Year,`Mortality Rate`,colour= `Country Name`)) +
  geom_line(size=1)+
  geom_point()+
  xlim(min(united_region$Year),max(united_region$Year))+
  ggtitle("Change in the mortality rate in the United States\n in comparison to North America") +
  scale_color_manual(values=c('deeppink3','pink'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.9,0.9),
        legend.title = element_blank())

#compare countries with their SDG goals
U5MR_sdg$Uncertanity<-NULL
colnames(U5MR_sdg)<-c("Country Name","Year","Mortality Rate")

china_sdg<-rbind(U5MR_greater_than_1990[U5MR_greater_than_1990$`Country Name`=="China",],U5MR_sdg[U5MR_sdg$`Country Name`=="Eastern Asia",])
united_sdg<-rbind(U5MR_greater_than_1990[U5MR_greater_than_1990$`Country Name`=="United States",],U5MR_sdg[U5MR_sdg$`Country Name`=="Northern America and Europe",])
india_sdg<-rbind(U5MR_greater_than_1990[U5MR_greater_than_1990$`Country Name`=="India",],U5MR_sdg[U5MR_sdg$`Country Name`=="Southern Asia",])


#compare China with its SDG goal region
ggplot(china_sdg, aes(Year,`Mortality Rate`,colour= `Country Name`)) +
  geom_line(size=1)+
  geom_point()+
  xlim(min(china_sdg$Year),max(china_sdg$Year))+
  ggtitle("Change in the mortality rate in China in comparison\n to SDG development goal of that region") +
  scale_color_manual(values=c('light blue','dark blue'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.9,0.9),
        legend.title = element_blank())

#Compare india with its SDG region
ggplot(india_sdg, aes(Year,`Mortality Rate`,colour= `Country Name`)) +
  geom_line(size=1)+
  geom_point()+
  xlim(min(india_sdg$Year),max(india_sdg$Year))+
  ggtitle("Change in the mortality rate in India in comparison\n to the SDG development goal of that region") +
  scale_color_manual(values=c('light green','dark green'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.9,0.9),
        legend.title = element_blank())

#compare united states with SDG 
ggplot(united_sdg, aes(Year,`Mortality Rate`,colour= `Country Name`)) +
  geom_line(size=1)+
  geom_point()+
  xlim(min(united_sdg$Year),max(united_sdg$Year))+
  ggtitle("Change in the mortality rate in the United States in comparison\nto SDG development goal of that region") +
  scale_color_manual(values=c('deeppink3','pink'))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank())

#under 5 mortality rate by cause of death in the year 2016
under5death_country<-under5death_country[under5death_country$Year==2016,]
under5death_country$`ISO Code`<-NULL
under5death_region<-under5death_region[under5death_region$Year==2016,]
colnames(under5death_region)<-c("Country Name","Year","Disease","Death %")


#Under 5 mortality rate by cause of death of each country
under5death_china<-under5death_country[under5death_country$`Country Name`=="China",]
under5death_india<-under5death_country[under5death_country$`Country Name`=="India",]
under5death_us<-under5death_country[under5death_country$`Country Name`=="United States of America",]


#cause of death in USA
under5death_us <- under5death_us %>% mutate( ToHighlight = ifelse( under5death_us$`Death %`>= 20, "yes", "no" ) )
ggplot(under5death_us, aes(x=reorder(Disease,under5death_us$`Death %`),y=`Death %`,fill=ToHighlight)) +
  geom_bar(stat = "identity",show.legend=FALSE)+
  coord_flip()+
  ggtitle("Percentage of deaths of children under the age of 5 in\nthe United States by diseases in 2016") +
  scale_fill_manual( values = c( "yes"="deeppink", "no"="gray" ), guide = FALSE )+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.9,0.9),
        legend.title = element_blank())

#cause of death in India
under5death_india <- under5death_india %>% mutate( ToHighlight = ifelse( under5death_india$`Death %`>= 11, "yes", "no" ) )
ggplot(under5death_india, aes(x=reorder(Disease,under5death_india$`Death %`),y=`Death %`,fill=ToHighlight)) +
  geom_bar(stat = "identity",show.legend=FALSE)+
  coord_flip()+
  ggtitle("Percentage of deaths in children under the age of 5 in\nIndia by diseases in 2016") +
  scale_fill_manual( values = c( "yes"="green", "no"="gray" ), guide = FALSE )+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.9,0.9),
        legend.title = element_blank())

#cause of death in China
under5death_china <- under5death_china %>% mutate( ToHighlight = ifelse( under5death_china$`Death %`>= 15, "yes", "no" ) )
ggplot(under5death_china, aes(x=reorder(Disease,under5death_china$`Death %`),y=`Death %`,fill=ToHighlight)) +
  geom_bar(stat = "identity",show.legend=FALSE)+
  coord_flip()+
  ggtitle("Percentage of deaths in children under the age of 5 in\nChina by diseases in 2016") +
  scale_fill_manual( values = c( "yes"="blue", "no"="gray" ), guide = FALSE )+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.9,0.9),
        legend.title = element_blank())

#comparison of death between countries and region
under5death_region$ToHighlight<-""
under5death_china$ToHighlight<-""
under5death_india$ToHighlight<-""
under5death_us$ToHighlight<-""
under_region_india<-rbind(under5death_india[which (under5death_india$Disease=="Preterm"| under5death_india$Disease=="Pneumonia"| under5death_india$Disease=="Intrapartum"),],under5death_region[which(under5death_region$`Country Name`=="South Asia"& (under5death_region$Disease=="Preterm"| under5death_region$Disease=="Pneumonia"| under5death_region$Disease=="Intrapartum")),])
under_region_china<-rbind(under5death_china[which (under5death_china$Disease=="Preterm"| under5death_china$Disease=="Congenital"| under5death_china$Disease=="Other"),],under5death_region[which (under5death_region$`Country Name`=="East Asia and Pacific" & (under5death_region$Disease=="Preterm"| under5death_region$Disease=="Congenital"| under5death_region$Disease=="Other")),])
under_region_us<-rbind(under5death_us[which (under5death_us$Disease=="Preterm"| under5death_us$Disease=="Congenital"| under5death_us$Disease=="Other"),],under5death_region[which (under5death_region$`Country Name`=="North America" & (under5death_region$Disease=="Preterm"| under5death_region$Disease=="Congenital"| under5death_region$Disease=="Other")),])

#compare india and region
ggplot(under_region_india, aes(x=reorder(Disease,-under_region_india$`Death %`),y=`Death %`,fill=under_region_india$`Country Name`)) +
  geom_bar(stat = "identity",position="dodge")+
  #coord_flip()+
  ggtitle("Comparison of top three causes of deaths in children under the\nage of 5 in India and South Asia in the year 2016") +
  scale_fill_manual(values = c("light green", "dark green"))+
  #guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.9,0.9),
        legend.title = element_blank())

#compare china and region
ggplot(under_region_china, aes(x=reorder(Disease,-under_region_china$`Death %`),y=`Death %`,fill=under_region_china$`Country Name`)) +
  geom_bar(stat = "identity",position="dodge")+
  #coord_flip()+
  ggtitle("Comparison of top three causes of deaths in children under the\nage of 5 in China and East Asia and Pacific in the year 2016") +
  scale_fill_manual(values = c("light blue", "dark blue"))+
  #guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.9,0.9),
        legend.title = element_blank())

#compare Us and region
ggplot(under_region_us, aes(x=reorder(Disease,-under_region_us$`Death %`),y=`Death %`,fill=under_region_us$`Country Name`)) +
  geom_bar(stat = "identity",position="dodge")+
  #coord_flip()+
  ggtitle("Comparison of top three causes of deaths in children under the\nage of 5 in the United States and North America in the year 2016") +
  scale_fill_manual(values = c("deeppink3", "pink"))+
  #guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "Black"),
        legend.position = c(0.88,0.92),
        legend.title = element_blank())
#circular barplot
#install.packages("hrbrthemes")
#install.packages("kableExtra")
#install.packages("viridis","ggplot2","ggthemes","dplyr","knitr")
#install.packages("tidyverse")

library(tidyverse)
#library(hrbrthemes)
#library(kableExtra)
#options(knitr.table.format = "html")
#library(viridis)
library(ggplot2)
#library(ggthemes)
library(dplyr)
#library(waffle)

dat <- read.csv("selected_txt_data.csv") #on RStudio online need to change encoding
tmp <- dat[,c(1,2,10,11,14,15,17,18,24,33,35)]
str(tmp)

#change some long values
levels(tmp$country_txt)[levels(tmp$country_txt)=='West Bank and Gaza Strip'] <- 'Gaza Strip'
levels(tmp$country_txt)[levels(tmp$country_txt)=='Democratic Republic of the Congo'] <- 'Congo'
levels(tmp$country_txt)[levels(tmp$country_txt)=='West Germany (FRG)'] <- 'W Germany'
levels(tmp$country_txt)[levels(tmp$country_txt)=='United Kingdom'] <- 'UK'
levels(tmp$country_txt)[levels(tmp$country_txt)=='United States'] <- 'US'
levels(tmp$attacktype1_txt)[levels(tmp$attacktype1_txt)=='Hostage Taking (Barricade Incident)'] <- 'Barricade'
levels(tmp$attacktype1_txt)[levels(tmp$attacktype1_txt)=='Facility/Infrastructure Attack'] <- 'Facility Attk'
levels(tmp$attacktype1_txt)[levels(tmp$attacktype1_txt)=='Hostage Taking (Kidnapping)'] <- 'Kidnapping'

#use this in R for character class
#tmp[tmp == "West Bank and Gaza Strip" ] <- "Gaza Strip"
#tmp[tmp == "Democratic Republic of the Congo" ] <- "Congo"
#tmp[tmp == "West Germany (FRG)" ] <- "W Germany"
#tmp[tmp == "United Kingdom" ] <- "UK"
#tmp[tmp == "United States" ] <- "US"
#tmp[tmp == "Hostage Taking (Barricade Incident)" ] <- "Barricade"
#tmp[tmp == "Facility/Infrastructure Attack" ] <- "Facility Attk"
#tmp[tmp == "Hostage Taking (Kidnapping)" ] <- "Kidnapping"



#calc number of attacks
tmp2 <- tmp %>% group_by(iyear, country_txt) %>% 
  summarise(attack=n()) %>%
  merge(tmp, by = c("iyear","country_txt"))%>% 
  group_by(iyear, country_txt) %>% 
  mutate(successful=sum(success))%>%
  mutate(failed=attack-successful) %>%
  dplyr::filter(!is.na(nwound)) %>%
  dplyr::filter(!is.na(nkill)) %>%
  group_by(iyear, country_txt)%>% 
  mutate(wound=sum(nwound)) %>% 
  mutate(kill=sum(nkill)) %>%
  mutate(victim=sum(nwound,nkill))

#plot stacked barplot of attacks overtime
byyr <- tmp2 %>% 
  select(iyear, country_txt, region_txt, successful, failed) %>% 
  distinct() %>% #remove duplicate rows 
  gather(key = "observation", value="value", -c(1:3)) #transfer format

#stacked barplot of number attacks overtime
ggplot(byyr, aes(x=iyear,y=value,fill=observation))+
  geom_bar(stat = "identity") +
  labs(x= 'Year', y= 'Number of attacks')

#plot stacked barplot of victims overtime
byvt <- tmp2 %>% 
  select(iyear, country_txt, wound, kill) %>% 
  distinct() %>% #remove duplicate rows 
  gather(key = "observation", value="value", -c(1:2)) #transfer format

ggplot(byvt, aes(x=iyear,y=value,fill=observation))+
  geom_bar(stat = "identity") +
  labs(x= 'Year', y= 'Number of victims')

#plot barplot by regions, stacked by the successful and failed
byrg <- byyr %>% group_by(region_txt, observation) %>% 
  mutate(value = sum(value)) %>%
  mutate(percent = value / sum(value) * 100) %>%
  select(region_txt,observation,value,percent) %>%
  distinct()

ggplot(byrg, aes(x=reorder(region_txt,value),y=value,fill=observation))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=1)) +
  labs(x= 'Attacked Regions', y= 'Number of Attacks') +
  coord_flip()


#plot pie chart of percent attacks in diff regions
byregion <- dat %>% group_by(region_txt) %>% 
  summarise(number=n()) %>%
  mutate(percent = number / sum(number) * 100) 
  
ggplot(byregion, aes(x="", y=percent,fill=region_txt)) +
  geom_bar(width = 1, stat="identity")+ 
  scale_fill_manual(values=c("#3c3f5e", "#3c46ad", "#607fd6","#b5c1e5","#8ab1d8","#bccfe2","#70b9d8","#75d0d8","#87e5e2","#9fccc0","#bad1ca","#13c48e"))+
  theme_minimal()+
  coord_polar("y", start=0)


#stacked circular plot of attacks by countries
#data of attacks by countries
byct <- byyr %>% group_by(country_txt, observation) %>% 
  mutate(value = sum(value)) %>%
  group_by(country_txt, observation) %>% 
  mutate(total = sum(value)) %>%
  select(country_txt,region_txt,observation,value) %>%
  distinct()


byct <- byct %>% group_by(country_txt) %>% 
  mutate(total = sum(value)) %>%
  arrange(desc(total)) %>% head(100)

byct <- with(byct, byct[order(region_txt,country_txt),]) #order data by region and country

#plot attacks by countries, separate colors by regions
ggplot(byct) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x = reorder(country_txt,value), y=value, fill=region_txt), stat="identity", alpha=0.5) +
  #scale_fill_viridis(discrete=TRUE) +
  ylim(-0,22300) +
  theme(axis.text.x = element_text(angle = 90,size=11),
        legend.position="bottom") +
  labs(x = "Countries", y = "Number of attacks")
  

#plot attacks by attacktypes and subweapons

bytype <- tmp %>%
  group_by(iyear,attacktype1_txt, weapsubtype1_txt)%>% 
  dplyr::filter(!is.na(weapsubtype1_txt)) %>%
  summarise(attack=n()) 

levels(bytype$weapsubtype1_txt)[levels(bytype$weapsubtype1_txt)==""] <- "unknown"
#bytype[bytype == "" ] <- "Unknown"

ggplot(bytype, aes(x=attacktype1_txt,y=attack, fill=weapsubtype1_txt))+
  geom_bar(stat = "identity") +
  labs(x= 'Attack types', y= 'Number of Attacks') +
  theme(axis.text.x = element_text(angle = 90), legend.position="bottom")


#pie chart of attack type percents
bytype2 <- tmp %>% group_by(attacktype1_txt) %>% 
  summarise(number=n()) %>% mutate(percent = number/sum(number) *100)

ggplot(bytype2, aes(x="", y=percent,fill=attacktype1_txt)) +
  geom_bar(width=1, stat="identity")+  
  scale_fill_brewer(palette="Blues")+
  theme_minimal() +
  coord_polar("y", start=0) +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=1))




library(maps)
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(plyr)

data_effect<- read_excel("Data_effects_complete.xlsx")

data_effect <- data_effect  %>% 
  mutate(country = strsplit(as.character(country), ",")) %>% 
  unnest(country) %>%
  mutate(country = str_trim(as.character(country)))

data_effect$z <- 1

data_effect <- data_effect %>%
  mutate(effect = case_when(effect == 1 ~ "increase", effect == -1 ~ "decrease", effect == 0 ~ "no change"))

countries <- data_effect%>%
  mutate(country = ifelse(country == "United States", "USA",country))%>%
  filter(outcome_clean == "trust" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "participation" | outcome_clean == "expression") %>%
  complete(country, effect, fill = list(z = 0)) %>% 
  dplyr::group_by(country, effect) %>% 
  dplyr::summarise(n = sum(z)) %>%
  na.omit()
#dplyr::group_by(country) %>%
#dplyr::summarise(m = mean(effect, na.rm=TRUE))
#dplyr::summarise(m = mean(effect, na.rm=TRUE))

countries$region <- as.character(countries$country)

map.world <- map_data("world")
map.review <- left_join(map.world, y=countries, by.x="region", by.y="country")

p <- ggplot(map.review, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgrey")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )

p 

centres <- ddply(map.world,.(region),summarize,long=mean(long),lat=mean(lat))


my_plot_fun <- function(data){
  ggplot(data, aes(x="", y=n, fill=effect)) +
    geom_bar(alpha=0.8, position='stack', stat='identity') +
    theme_void()+
    scale_fill_manual("effect", values=c("#EE847D", "grey", "#7EAB55"), limits=c("decrease", "no change", "increase")) +
    coord_polar("y", start=0) +
    theme(legend.position="none")
}

newtable <- merge(countries ,centres, by  = "region") 

annotation_fun <- function(data, lat,  long, plot_fun) {
  subplot = plot_fun(data)
  sub_grob <- annotation_custom(ggplotGrob(subplot), 
                                x = long-12, y = lat-12, 
                                xmax = long+12, ymax = lat+12)
}

subgrobs <- newtable %>% 
  nest(-long,-lat)  %>%
  pmap(annotation_fun,plot_fun = my_plot_fun)

p + subgrobs 


#####################################################################

data_effect<- read_excel("Data_effects_complete.xlsx")

data_effect <- data_effect  %>% 
  mutate(country = strsplit(as.character(country), ",")) %>% 
  unnest(country) %>%
  mutate(country = str_trim(as.character(country)))

data_effect$z <- 1

data_effect <- data_effect %>%
  mutate(effect = case_when(effect == 1 ~ "increase", effect == -1 ~ "decrease", effect == 0 ~ "no change"))

countries <- data_effect%>%
  mutate(country = ifelse(country == "United States", "USA",country))%>%
  filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate" | outcome_clean == "misinformation")%>%
  complete(country, effect, fill = list(z = 0)) %>% 
  dplyr::group_by(country, effect) %>% 
  dplyr::summarise(n = sum(z)) %>%
  na.omit()
#dplyr::group_by(country) %>%
#dplyr::summarise(m = mean(effect, na.rm=TRUE))
#dplyr::summarise(m = mean(effect, na.rm=TRUE))

countries$region <- as.character(countries$country)

map.world <- map_data("world")
map.review <- left_join(map.world, y=countries, by.x="region", by.y="country")

p <- ggplot(map.review, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgrey")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )

p 

centres <- ddply(world_map,.(region),summarize,long=mean(long),lat=mean(lat))

my_plot_fun <- function(data){
  ggplot(data, aes(x="", y=n, fill=effect)) +
    geom_bar(alpha=0.8, position='stack', stat='identity') +
    theme_void()+
    scale_fill_manual("effect", values=c("#7EAB55", "grey", "#EE847D"), limits=c("decrease", "no change", "increase")) +
    coord_polar("y", start=0) +
    theme(legend.position="none")
}

newtable <- merge(countries ,centres, by  = "region") 

annotation_fun <- function(data, lat,  long, plot_fun) {
  subplot = plot_fun(data)
  sub_grob <- annotation_custom(ggplotGrob(subplot), 
                                x = long-12, y = lat-12, 
                                xmax = long+12, ymax = lat+12)
}

subgrobs <- newtable %>% 
  nest(-long,-lat)  %>%
  pmap(annotation_fun,plot_fun = my_plot_fun)

p + subgrobs 


#####################################################################################

data_effect<- read_excel("Data_effects_complete.xlsx")

data_heterogeneity <- data_effect

par(mar = c(4, 4, .1, .1))

outcomes <- data_heterogeneity %>%
  filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate" | outcome_clean == "misinformation")%>%
  #filter(outcome_clean == "participation" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "trust")%>%
  dplyr::count(heterogeneity)%>%
  arrange(desc(n))%>%
  na.omit()%>%
  head(10)

ggplot(outcomes, aes(x = reorder(heterogeneity, n), y = n)) + 
  geom_bar(stat = "identity", fill = "grey")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))+
  coord_flip()+
  ylab("count")+
  xlab("Heterogeneity")

#####################################################################################

data_effect<- read_excel("Data_effects_complete.xlsx")

data_heterogeneity <- data_effect

par(mar = c(4, 4, .1, .1))

outcomes <- data_heterogeneity %>%
  #filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate")%>%
  filter(outcome_clean == "participation" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "trust" | outcome_clean == "expression")%>%
  dplyr::count(heterogeneity)%>%
  arrange(desc(n))%>%
  na.omit()%>%
  head(10)

ggplot(outcomes, aes(x = reorder(heterogeneity, n), y = n)) + 
  geom_bar(stat = "identity", fill = "grey")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))+
  coord_flip()+
  ylab("count")+
  xlab("Heterogeneity")

#####################################################################################

data_effect<- read_excel("Data_effects_complete.xlsx")

data_trust<- data_effect

data_trust <- data_trust %>% 
  mutate(country = strsplit(as.character(country), ",")) %>% 
  unnest(country) %>%
  mutate(country = str_trim(as.character(country)))


# N=selected papers, streamlined categories
trust_effects <- data_trust %>%
  mutate(country = ifelse(country == "United States", "USA",country))%>%
  filter(outcome_clean == "trust" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "participation")%>%
  #filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate")%>%
  dplyr::count(effect, country) %>%
  na.omit()%>%
  arrange(desc(n))

trust_effects <- trust_effects %>%
  mutate(effect = case_when(effect == 1 ~ "positive", effect == -1 ~ "negative", effect == 0 ~ "no change"))

p <- ggplot(trust_effects, aes(fill=effect, y=n, x=country)) + 
  #geom_bar(position='stack', stat='identity', color="black") +
  geom_bar(position='stack', stat='identity') +
  theme_minimal()+
  coord_flip()+
  scale_fill_manual(values = c("#EE847D", "grey", "#7EAB55")) +
  #scale_x_discrete('Country', limits=c("Sweden", "Finland", "Germany", "Netherlands", "Belgium", "France", "UK", "Spain", "Italy", "Europe", "Austria", "South Korea", "Japan", "USA", "Israel", "Brazil", "Poland", "Hungary", "Macedonia", "Kenya", "Malaysia", "Jordan", "Ethiopia", "Russia", "Korea"), drop=FALSE) +
  scale_x_discrete('Country', limits=c("Denmark", "Norway", "Sweden", "Switzerland", "Finland", "Germany", "Belgium",  "France", "Netherlands", "Spain", "Italy", "Europe", "Australia", "Austria", "Chile", "Canada", "Japan", "USA", "Israel", "South Korea", "Taiwan", "Croatia", "Ghana", "Brazil", "Poland", "Hungary", "Ecuador", "Colombia", "Indonesia", "Macedonia", "Mexico", "Nigeria", "India", "Lebanon", "Philippines", "Kenya", "Malaysia", "Jordan", "Pakistan", "Hong Kong", "Ethiopia", "North Africa", "Iran", "Middle East", "Kazakhstan", "Egypt", "Turkey", "Russia", "China"), drop=FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))
p


################################################################

data_effect<- read_excel("Data_effects_complete.xlsx")

data_trust<- data_effect

data_trust <- data_trust %>% 
  mutate(country = strsplit(as.character(country), ",")) %>% 
  unnest(country) %>%
  mutate(country = str_trim(as.character(country)))


# N=selected papers, streamlined categories
trust_effects <- data_trust %>%
  mutate(country = ifelse(country == "United States", "USA",country))%>%
  #filter(outcome_clean == "trust" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "participation")%>%
  filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate")%>%
  dplyr::count(effect, country) %>%
  na.omit()%>%
  arrange(desc(n))

trust_effects <- trust_effects %>%
  mutate(effect = case_when(effect == 1 ~ "positive", effect == -1 ~ "negative", effect == 0 ~ "no change"))

p <- ggplot(trust_effects, aes(fill=effect, y=n, x=country)) + 
  #geom_bar(position='stack', stat='identity', color="black") +
  geom_bar(position='stack', stat='identity') +
  theme_minimal()+
  coord_flip()+
  scale_fill_manual(values = c("#7EAB55","grey" ,"#EE847D")) +
  #scale_x_discrete('Country', limits=c("Sweden", "Finland", "Germany", "Netherlands", "Belgium", "France", "UK", "Spain", "Italy", "Europe", "Austria", "South Korea", "Japan", "USA", "Israel", "Brazil", "Poland", "Hungary", "Macedonia", "Kenya", "Malaysia", "Jordan", "Ethiopia", "Russia", "Korea"), drop=FALSE) +
  scale_x_discrete('Country', limits=c("Denmark", "Norway", "Sweden", "Switzerland", "Finland", "Germany", "Belgium",  "France", "Netherlands", "Spain", "Italy", "Europe", "Australia", "Austria", "Chile", "Canada", "Japan", "USA", "Israel", "South Korea", "Taiwan", "Croatia", "Ghana", "Brazil", "Poland", "Hungary", "Ecuador", "Colombia", "Indonesia", "Macedonia", "Mexico", "Nigeria", "India", "Lebanon", "Philippines", "Kenya", "Malaysia", "Jordan", "Pakistan", "Hong Kong", "Ethiopia", "North Africa", "Iran", "Middle East", "Kazakhstan", "Egypt", "Turkey", "Russia", "China"), drop=FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))
p

  
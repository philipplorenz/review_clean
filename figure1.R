# packages
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(writexl)
library(dplyr)

# data (preprocessing in quality_label_preprocessing.R)
data<- read_excel("data_final.xlsx")

######################################################
# count variable combinations
sort(table(data$variables_clean))

######################################################
# methods and outcome measures


### split up the outcome measures in separate rows

data_final <- data %>% 
  mutate(outcome_rich = strsplit(as.character(outcome_rich), ",")) %>% 
  unnest(outcome_rich) %>%
  mutate(outcome_rich = str_trim(as.character(outcome_rich)))

# streamlining outcome categories Lisa
data_final <- data_final%>%
  mutate(outcome_clean = case_when(grepl('populis|right|white|AfD|partisan|group|tolerance|equalitarian', outcome_rich) ~ "populism",
                                   grepl('fragmentation|fragementation|consensus|polariz|polaris', outcome_rich) ~ "polarization",
                                   grepl('radical|extremis|terror|ISIS|islamist', outcome_rich) ~ "extremism",
                                   grepl('network|homophily|echo', outcome_rich) ~ "network/echo chamber",
                                   grepl('trust|hesistancy',outcome_rich) ~ "trust",
                                   grepl('misinfo|troll|bot', outcome_rich) ~ "misinformation",
                                   grepl('hate|crime|violence|incivil|tone|racis|intolerance|muslim|prejudice|anti|harassment|language', outcome_rich) ~ "hate",
                                   grepl('expression|sharing|censor|present|commenting|discuss|discours|communication',outcome_rich) ~ "expression",
                                   grepl('consumption|news|selective|exposure|search|avoidance|diet|homogen|heterogen|homophil|follow',outcome_rich) ~ "exposure",
                                   grepl('knowledge|learning|salience|interest|efficacy|seek|internet|information|issue',outcome_rich) ~ "knowledge",
                                   grepl('risk',outcome_rich) ~ "risk perception",
                                   grepl('health|covid-19|ebola|vaccine',outcome_rich) ~ "health information",
                                   grepl('attitude|opinion|orient|environment',outcome_rich) ~ "attitude",
                                   grepl('China|china|regime|system|chinese',outcome_rich) ~ "autocracy",
                                   grepl('democra|satisfaction|support|election|politicization|government',outcome_rich) ~ "democracy",
                                   grepl('moderation|restrict',outcome_rich) ~ "moderation",
                                   grepl('partici|civic|civil|protest|voting|vote|participaton|membership|political engagement|
                                      engagement|social engagement|petition|turnout|behavior|activism|behaviour',outcome_rich) ~ "participation",
                                   grepl('campaign|propaganda|editing|mobilization|agenda|foreign|strategies',outcome_rich) ~ "strategy",
                                   grepl('algo',outcome_rich) ~ "algorithm",
                                   TRUE ~ "other"))
#TRUE ~ outcome_rich))

par(mar = c(4, 4, .1, .1))

# N=selected papers, streamlined categories
methods_simple <- data_final %>%
  dplyr::count(method_clean) %>%
  na.omit()%>%
  arrange(desc(n))

print(sum(methods_simple$n))

ggplot(methods_simple, aes(x = reorder(method_clean, n), y = n)) + 
  geom_bar(stat = "identity", fill = "indianred")+
  theme_minimal()+
  coord_flip()+
  ylab("count")+
  xlab("Methods")

# appearances of methods (search+count in method_rich)
# add if we like

# N=selected papers, streamlined categories
outcomes_simple <- data_final %>%
  dplyr::count(outcome_clean) %>%
  na.omit()%>%
  arrange(desc(n))

ggplot(outcomes_simple, aes(x = reorder(outcome_clean, n), y = n)) + 
  geom_bar(stat = "identity", fill = "goldenrod")+
  theme_minimal()+
  coord_flip()+
  ylab("count")+
  xlab("Outcome Variables")


#################################################

# N=selected papers, streamlined categories
outcomes_rich <- data_final %>%
  filter(outcome_clean=="expression") %>%
  mutate(outcome_rich = str_trim(as.character(outcome_rich))) %>%
  dplyr::count(outcome_rich) %>%
  na.omit() %>%
  arrange(desc(n)) 

######################################################
# methods and outcome matrix

combinations_simple <- data_final %>%
  dplyr::count(outcome_clean, method_clean) %>%
  #filter(!(method_clean=="survey" & outcome_clean=="participation"))%>%
  na.omit()%>%
  arrange(desc(n))

o <- data_final %>% 
  dplyr::count(outcome_clean) %>%
  #filter(!(method_clean=="survey" & outcome_clean=="participation"))%>%
  na.omit()%>%
  arrange(desc(-n))

o <- unique(o$outcome_clean)

my_breaks = c(1, 3, 10, 30, 80)


p <- ggplot(combinations_simple, aes(x = reorder(method_clean, -n), y = outcome_clean, fill = n)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15))+
  scale_y_discrete(limits=o) +
  scale_fill_viridis_c(direction = -1, trans = "log", breaks=my_breaks, na.value="white", name = "Number of Measurements")

p

#########################################################
# map

data_effect <- read_excel("Data_effects_complete.xlsx") %>%
  distinct(Key, .keep_all = TRUE)

data <- read_excel("data_final.xlsx")

data_all <- merge(x = data, y = data_effect, by = "Key", all.x = TRUE)


countries <- data_all %>%
  mutate(country.y = ifelse(country.y == "United States", "USA",country.y))%>%
  dplyr::count(country.y) %>%
  na.omit()%>%
  arrange(desc(n))

countries$region <- as.character(countries$country)

map.world <- map_data("world")
map.review <- left_join(map.world, y=countries, by.x="region", by.y="country")

my_breaks = c(1, 3, 10, 30, 80)

p <- ggplot(map.review, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = n))+
  theme_bw()+
  theme(legend.position = c(0.15, 0.25),
        legend.key.size = unit(0.5, 'cm'),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )+
  scale_fill_viridis_c(direction = -1, trans = "log", breaks=my_breaks, na.value="lightgrey", name = "Number of Papers")
p

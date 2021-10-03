# packages
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)

# data (preprocessing in quality_label_preprocessing.R)
data<- read_excel("data_final.xlsx")

######################################################
# count variable combinations
sort(table(data$variables_clean))

######################################################
# methods and outcome measures

par(mar = c(4, 4, .1, .1))

# N=selected papers, streamlined categories
methods_simple <- data %>%
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
outcomes_simple <- data %>%
  dplyr::count(outcome_clean) %>%
  na.omit()%>%
  arrange(desc(n))

ggplot(outcomes_simple, aes(x = reorder(outcome_clean, n), y = n)) + 
  geom_bar(stat = "identity", fill = "goldenrod")+
  theme_minimal()+
  coord_flip()+
  ylab("count")+
  xlab("Outcome Variables")

######################################################
# methods and outcome matrix

combinations_simple <- data %>%
  dplyr::count(outcome_clean, method_clean) %>%
  #filter(!(method_clean=="survey" & outcome_clean=="participation"))%>%
  na.omit()%>%
  arrange(desc(n))

o <- data %>% 
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
  scale_fill_viridis_c(direction = -1, trans = "log", breaks=my_breaks, na.value="white", name = "Number of Studies")

p

#########################################################
# map

data_effect<- read_excel("Data_effects_complete_clean.xlsx")

countries <- data_effect%>%
  mutate(country = ifelse(country == "United States", "USA",country))%>%
  dplyr::count(country) %>%
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

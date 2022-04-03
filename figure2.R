
# packages
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)

data_effect<- read_excel("data_effects.xlsx")
data_review <- read_excel("data_review.xlsx")
  
data_merged <- left_join(data_effect, data_review, by = "Key")

######################################################################
# effect distribution, change outcome for different plots

data_trust<- data_effect %>%
  filter(outcome_clean == "trust")

gg_b <- ggplot_build(
  ggplot() + geom_histogram(aes(x = data_trust$effect), binwidth=1., stat="count")
)

nu_bins <- dim(gg_b$data[[1]])[1]

p <- ggplot() + geom_histogram(aes(x = data_trust$effect), binwidth=1.,  fill = c("#7EAB55", "grey", "#EE847D"), stat="count") + 
  theme_minimal() + 
  geom_bar(stat='identity', color="black") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))+
  ylab("")+
  xlab("")+
  scale_x_discrete(labels = c("-1" = "decrease","0" ="no effect","1" ="increase"))+
  guides(fill = FALSE)  
p


######################################################################
# effect distribution split via digital media, change outcome for different plots

data_trust <- data_merged %>%
  filter(outcome_clean.x == "misinformation" & dm_category == "social media") %>%
  mutate(effect = factor(effect))

gg_b <- ggplot_build(
  ggplot() + geom_histogram(aes(x = data_trust$effect), binwidth=1., stat="count")
)

nu_bins <- 3

p <- ggplot() + geom_histogram(aes(x = data_trust$effect), binwidth=1., fill = c("#7DBFA7", "grey", "#ED936B"), stat="count") + 
  theme_minimal() + 
  geom_bar(stat='identity', color="black", position = position_dodge2()) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))+
  ylab("")+
  xlab("")+
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  scale_x_discrete(limits = c("-1","0","1"), drop = FALSE)+
  guides(fill = FALSE) 
p

######################################################################
# distrubution above split up via methods, change outcome for different plots

data_trust<- data_effect %>%
  filter(outcome_clean == "network/echo chamber")

trust_effects <- data_trust %>%
  dplyr::count(effect, method_clean) %>%
  na.omit()%>%
  arrange(desc(n))

trust_effects <- trust_effects %>%
  mutate(effect = case_when(effect == 1 ~ "positive", effect == -1 ~ "negative", effect == 0 ~ "no change"))

p <- ggplot(trust_effects, aes(fill=effect, y=n, x=method_clean)) + 
  geom_bar(position='stack', stat='identity', color="black") +
  theme_minimal()+
  scale_fill_manual(values = c("#7EAB55", "grey", "#EE847D")) +
  scale_x_discrete('Method', limits=c("descriptive", "experiment", "network", "panel", "qualitative", "social_media", "social_media_combined", "survey", "survey_combined", "tracking"), drop=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))
p

######################################################################
# distrubution above split up via digital media type, change outcome for different plots

data_trust<- data_effect %>%
  filter(outcome_clean == "exposure")

trust_effects <- data_trust %>%
  dplyr::count(effect, dm_clean) %>%
  na.omit()%>%
  arrange(desc(n))

trust_effects <- trust_effects %>%
  mutate(effect = case_when(effect == 1 ~ "positive", effect == -1 ~ "negative", effect == 0 ~ "no change"))

p <- ggplot(trust_effects, aes(fill=effect, y=n, x=dm_clean)) + 
  geom_bar(position='stack', stat='identity', color="black") +
  theme_minimal()+
  #scale_fill_manual(values = c("#7DBFA7", "grey", "#ED936B")) +
  scale_fill_manual(values = c("#ED936B", "grey", "#7DBFA7")) +
  #scale_x_discrete('Method', limits=c("descriptive", "experiment", "network", "panel", "qualitative", "social_media", "social_media_combined", "survey", "survey_combined", "tracking"), drop=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))

p + theme(plot.margin = margin(0, 0, 0, 3, "cm"))



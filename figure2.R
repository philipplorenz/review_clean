
# packages
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)

data_effect<- read_excel("Data_effects_complete.xlsx")


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

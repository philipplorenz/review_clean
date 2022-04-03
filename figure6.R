# packages
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(readxl)

data_effect<- read_excel("data_effects.xlsx")

data_trust <- data_effect %>% 
  mutate(sampling = strsplit(as.character(sampling), "+", fixed = TRUE)) %>% 
  unnest(sampling) %>%
  mutate(sampling = str_trim(as.character(sampling))) 

##########################################################
# count risk of bias

##########################################################
# open data

sort(table(data_trust$open_data))

data_trust<- data_trust %>%
  #filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate") %>%
  filter(outcome_clean == "trust" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "participation") %>%
  filter(open_data == "No" )

sort(table(data_trust$effect))

##########################################################
# competing interest

data_trust <- data_effect %>% 
  mutate(sampling = strsplit(as.character(sampling), "+", fixed = TRUE)) %>% 
  unnest(sampling) %>%
  mutate(sampling = str_trim(as.character(sampling))) 

sort(table(data_trust$competing_interest))

data_trust<- data_trust %>%
  #filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate") %>%
  filter(outcome_clean == "trust" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "participation") %>%
  filter(competing_interest == "No")

sort(table(data_trust$effect))

##########################################################
# pre registration

data_trust <- data_effect %>% 
  mutate(sampling = strsplit(as.character(sampling), "+", fixed = TRUE)) %>% 
  unnest(sampling) %>%
  mutate(sampling = str_trim(as.character(sampling))) 

sort(table(data_trust$pre_registered))

data_trust<- data_trust %>%
  filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate") %>%
  #filter(outcome_clean == "trust" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "participation") %>%
  filter(pre_registered == "Yes")

sort(table(data_trust$effect))


######################################################################
# distrubution above split up via digital media type, change outcome for different plots

data_trust<- data_trust %>%
  filter(outcome_clean == "trust" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "participation") 

data_trust <- data_trust[data_trust$sampling != "NA",]

trust_effects <- data_trust %>%
  dplyr::count(effect, sampling) %>%
  na.omit()%>%
  arrange(desc(n))

trust_effects <- trust_effects %>%
  mutate(effect = case_when(effect == 1 ~ "positive", effect == -1 ~ "negative", effect == 0 ~ "no change"))

p <- ggplot(trust_effects, aes(fill=effect, y=n, x=sampling)) + 
  geom_bar(position='stack', stat='identity', color="black") +
  theme_minimal()+
  scale_fill_manual(values = c("#ED936B", "grey", "#7DBFA7")) +
  #scale_x_discrete('Method', limits=c("descriptive", "experiment", "network", "panel", "qualitative", "social_media", "social_media_combined", "survey", "survey_combined", "tracking"), drop=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))

p + theme(plot.margin = margin(0, 0, 0, 2, "cm"))



data_effect<- read_excel("data_effects.xlsx")

data_trust <- data_effect %>% 
  mutate(sampling = strsplit(as.character(sampling), "+", fixed = TRUE)) %>% 
  unnest(sampling) %>%
  mutate(sampling = str_trim(as.character(sampling))) 


######################################################################
# distrubution above split up via digital media type, change outcome for different plots

data_trust<- data_trust %>%
  filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate")

data_trust <- data_trust[data_trust$sampling != "NA",]

trust_effects <- data_trust %>%
  dplyr::count(effect, sampling) %>%
  na.omit()%>%
  arrange(desc(n))

trust_effects <- trust_effects %>%
  mutate(effect = case_when(effect == 1 ~ "positive", effect == -1 ~ "negative", effect == 0 ~ "no change"))

p <- ggplot(trust_effects, aes(fill=effect, y=n, x=sampling)) + 
  geom_bar(position='stack', stat='identity', color="black") +
  theme_minimal()+
  scale_fill_manual(values = c("#7DBFA7", "grey", "#ED936B")) +
  #scale_x_discrete('Method', limits=c("descriptive", "experiment", "network", "panel", "qualitative", "social_media", "social_media_combined", "survey", "survey_combined", "tracking"), drop=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))

p + theme(plot.margin = margin(0, 0, 0, 2, "cm"))

#######################################################################
# scatter plot with sample size


######################################################################
# distrubution above split up via digital media type, change outcome for different plots


data_trust <- data_effect %>% 
  mutate(sampling = strsplit(as.character(sampling), "+", fixed = TRUE)) %>% 
  unnest(sampling) %>%
  mutate(sampling = str_trim(as.character(sampling))) 

data_trust <- data_trust %>% 
  mutate(sampling =  sub(",.*", "", sampling))
  

data_trust <- data_trust %>% 
  mutate(size = strsplit(as.character(size), "+", fixed = TRUE)) %>% 
  unnest(size) %>%
  mutate(size = str_trim(as.character(size))) 

data_trust$size <- as.numeric(data_trust$size)

data_trust<- data_trust %>%
  #filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate")
  filter(outcome_clean == "trust" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "participation") 

data_trust <- data_trust[data_trust$sampling != "NA",]

trust_effects <- data_trust %>%
  dplyr::count(effect, sampling, size) %>%
  na.omit()%>%
  arrange(desc(n))

trust_effects <- trust_effects %>%
  mutate(effect = case_when(effect == 1 ~ "positive", effect == -1 ~ "negative", effect == 0 ~ "no change"))

p <- ggplot(trust_effects, aes(y=size, x=sampling, color=effect)) + 
  geom_jitter(width=0.2, alpha=0.8, aes(size=2.)) +
  theme_minimal()+
  scale_color_manual(values=c("#ED936B", "grey", "#7DBFA7"))+
  #scale_color_manual(values=c("#7DBFA7", "grey", "#ED936B"))+
  #scale_x_discrete('Method', limits=c("descriptive", "experiment", "network", "panel", "qualitative", "social_media", "social_media_combined", "survey", "survey_combined", "tracking"), drop=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) + scale_y_log10()
p



##########################################################
# co-author network


data_effect<- read_excel("data_effects.xlsx")

data_author <- data_effect %>% 
  mutate(Author= strsplit(as.character(Author), ";"))


data_effect<- data_effect %>%
  filter(outcome_clean == "polarization" | outcome_clean == "populism" | outcome_clean == "network/echo chamber" | outcome_clean == "hate")
  #filter(outcome_clean == "trust" | outcome_clean == "knowledge" | outcome_clean == "exposure" | outcome_clean == "participation") 


edge_list <- data_effect$Author %>% 
  # First remove everything from date onwards:
  gsub("\\(.*","",.) %>% 
  # Remove asterix:
  gsub("\\*","",.) %>%
  # Make lowercase:
  tolower %>%
  # Remove spaces:
  gsub("\\s*","",.) %>%
  # Remove &
  gsub("\\&","",.) %>%
  # Split on .,:
  strsplit(split=";") %>%
  # Remove periods and everything following:
  lapply(function(x)gsub("\\..*","",x)) %>%
  lapply(function(x)gsub(",.*","", x))

effect_list <- data_effect %>%
  mutate(effect = case_when(effect == 1 ~ "#EE847D", effect == -1 ~ "#7EAB55", effect == 0 ~ "grey"))

effect_list <- effect_list$effect
# length(data_author$Author[3])
# data_author$Author[3]
# 
# edge_list <- lapply(data_author$Author, function(x)
#           if(length(x)>1) {
#             combn(x, 2, simplify=FALSE)
#            }
#            else x)
# 
# 
# edge_list <- unlist(edge_list,recursive=FALSE)

#edge_list <- data_author$Author %>%
#  lapply(tolower)

## how many authors are there?
allAuthors <- unique(unlist(edge_list))

# Create list 
SymXDis <- do.call(cbind,lapply(edge_list,function(x){
  1*(allAuthors %in% x)
}))

# Make adjacency:
adj <- SymXDis %*% t(SymXDis)

# Labels:
labs <- gsub(",.*","",allAuthors)
substr(labs,0,1) <- toupper(substr(labs,0,1))

library(qgraph)

qgraph(adj, labels = labs, color = "#eeeeee",  vsize = 1.5, diag = FALSE,
       layout = "spring", edge.color = effect_list, repulsion = 0.85, 
       border.width=0.2, border.color='#444444', label.color="#555555",filetype='pdf',height=40,width=50)


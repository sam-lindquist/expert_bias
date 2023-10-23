# the purpose of this code is to create a graph that shows trends in confidence in institutions
# specifically the press, science, and medicine

library(ggrepel)
library(tidyverse)

path_to_dir <- "/Users/samuellindquist/Library/CloudStorage/Dropbox/expert_bias/code_data" # sams mac
#path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

df <- read_csv("input/norc_general_social_survey/confidence_combined.csv")

df %>% 
  mutate(topic_end = if_else(year == max(year), as.character(topic), NA)) %>% 
  ggplot(aes(x = year, y = value, color = topic)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Percent of Americans with a Great Deal of Confidence in...",
       x = "Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = function(x) {paste0(as.character(x), "%")}) + 
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title = element_blank(),
        axis.text = element_text(size = 10)) +
  geom_label_repel(aes(label = topic_end),
                   nudge_x = 1,
                   nudge_y = 1,
                   size = 4,
                   na.rm = TRUE)

# exporting
path_to_output <- c("output/graphs/misc/")

ggsave(paste0(path_to_output, "gss_confidence_institutions.jpeg"))


# the purpose of this code is to make graphs that show the type of bias (or lack thereof)
# that senders have in the experiment. The graphs are used in the survey to explain the
# experimental design.
# the code is horrible


library(tidyverse)

path_to_dir <- "/Users/samuellindquist/Library/CloudStorage/Dropbox/expert_bias/code_data" # sams mac
#path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

df_len <- 5
line_pos <- 1

df <- tibble(line = c(0:4, (0:4)),
             x_axis = c(rep(1, df_len), rep(-1, df_len)))

df$x_axis_str <- as.character(df$x_axis)

green <- "#009E73"
pink <- "#CC79A7"

#### unbiased sender inflation ####
ggplot(df, aes(x = x_axis, y = line, group = x_axis_str)) + 
  geom_line(alpha = 0) +
  annotate("rect", xmin = -1.2, xmax = 1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = green) + 
  annotate("rect", xmin = 1.2, xmax = Inf, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = pink) + 
  annotate("rect", xmin = -Inf, xmax = -1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = pink) + 
  coord_cartesian(xlim = c(-3, 3), ylim = c(1.5,2.5)) + 
  annotate("segment", x = -3, xend = -1.2, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "first"), color = "black") + 
  annotate("segment", x = -1.2, xend = 1.2, y = 2, yend = 2, color = "black") +
  annotate("segment", x = 1.2, xend = 3, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "last"), color = "black") +
  annotate("label", x = 0, y = 2.1, label = "Incentivized", color = green, size = 6) +
  annotate("label", x = -2.2, y = 2.1, label = "No Incentive", color = "red", size = 6) +
  annotate("label", x = 2.2, y = 2.1, label = "No Incentive", color = "red", size = 6) +
  geom_point(x = 0, y = 2 , size = 2, color = "black") + 
  annotate("text", x = 0, y = 1.94, label = "True Inflation", size = 6) +
  annotate("text", x = 1.2, y = 1.94, label = "+ 1%", size = 6) +
  annotate("text", x = -1.2, y = 1.94, label = "- 1%", size = 6) +
  annotate("text", x = 2.4, y = 1.94, label = "+ 2%", size = 6) +
  annotate("text", x = -2.4, y = 1.94, label = "- 2%", size = 6) +
  geom_segment(x = 1.2, xend = 1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = -1.2, xend = -1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = -2.4, xend = -2.4, y = 1.98, yend = 2.02) +
  geom_segment(x = 2.4, xend = 2.4, y = 1.98, yend = 2.02) + 
  labs(title = "Other Participant's Estimate") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, size = 25))
ggsave("output/graphs/misc/sender_incentive_graphs/unbiased_sender_inflation_incentives.png", width = 10, height = 4)

#### ambiguous biased sender inflation ####
ggplot(df, aes(x = x_axis, y = line, group = x_axis_str)) + 
  geom_line(alpha = 0) +
  annotate("rect", xmin = -1.2, xmax = 1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = pink) + 
  annotate("rect", xmin = 1.2, xmax = Inf, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = green) + 
  annotate("rect", xmin = -Inf, xmax = -1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = green) + 
  coord_cartesian(xlim = c(-3, 3), ylim = c(1.5,2.5)) + 
  annotate("segment", x = -3, xend = -1.2, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "first"), color = "black") + 
  annotate("segment", x = -1.2, xend = 1.2, y = 2, yend = 2, color = "black") +
  annotate("segment", x = 1.2, xend = 3, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "last"), color = "black") +
  annotate("label", x = 0, y = 2.1, label = "No Incentive", color = 'red', size = 6) +
  annotate("label", x = -2.2, y = 2.1, label = "Incentivized", color = green, size = 6) +
  annotate("label", x = 2.2, y = 2.1, label = "Incentivized", color = green, size = 6) +
  geom_point(x = 0, y = 2 , size = 2, color = "black") + 
  annotate("text", x = 0, y = 1.94, label = "True Inflation", size = 6) +
  annotate("text", x = 1.2, y = 1.94, label = "+ 1%", size = 6) +
  annotate("text", x = -1.2, y = 1.94, label = "- 1%", size = 6) +
  annotate("text", x = 2.4, y = 1.94, label = "+ 2%", size = 6) +
  annotate("text", x = -2.4, y = 1.94, label = "- 2%", size = 6) +
  geom_segment(x = 1.2, xend = 1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = -1.2, xend = -1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = -2.4, xend = -2.4, y = 1.98, yend = 2.02) +
  geom_segment(x = 2.4, xend = 2.4, y = 1.98, yend = 2.02) + 
  labs(title = "Other Participant's Estimate") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, size = 25))
ggsave("output/graphs/misc/sender_incentive_graphs/ambiguous_bias_sender_inflation_incentives.png", width = 10, height = 4)



#### biased up sender inflation ####
ggplot(df, aes(x = x_axis, y = line, group = x_axis_str)) + 
  geom_line(alpha = 0) +
  annotate("rect", xmin = -1.2, xmax = 1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = pink) + 
  annotate("rect", xmin = 1.2, xmax = Inf, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = green) + 
  annotate("rect", xmin = -Inf, xmax = -1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = pink) + 
  coord_cartesian(xlim = c(-3, 3), ylim = c(1.5,2.5)) + 
  annotate("segment", x = -3, xend = -1.2, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "first"), color = "black") + 
  annotate("segment", x = -1.2, xend = 1.2, y = 2, yend = 2, color = "black") +
  annotate("segment", x = 1.2, xend = 3, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "last"), color = "black") +
  annotate("label", x = -1.2, y = 2.1, label = "No Incentive", color = 'red', size = 6) +
  annotate("label", x = 2.2, y = 2.1, label = "Incentivized", color = green, size = 6) +
  geom_point(x = 0, y = 2 , size = 2, color = "black") + 
  annotate("text", x = 0, y = 1.94, label = "True Inflation", size = 6) +
  annotate("text", x = 1.2, y = 1.94, label = "+ 1%", size = 6) +
  annotate("text", x = -1.2, y = 1.94, label = "- 1%", size = 6) +
  annotate("text", x = 2.4, y = 1.94, label = "+ 2%", size = 6) +
  annotate("text", x = -2.4, y = 1.94, label = "- 2%", size = 6) +
  geom_segment(x = 1.2, xend = 1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = -1.2, xend = -1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = -2.4, xend = -2.4, y = 1.98, yend = 2.02) +
  geom_segment(x = 2.4, xend = 2.4, y = 1.98, yend = 2.02) + 
  labs(title = "Other Participant's Estimate") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, size = 25))
ggsave("output/graphs/misc/sender_incentive_graphs/biased_up_sender_inflation_incentives.png", width = 10, height = 4)

#### unbiased sender ucsd rank ####
ggplot(df, aes(x = x_axis, y = line, group = x_axis_str)) + 
  geom_line(alpha = 0) +
  annotate("rect", xmin = -1.2, xmax = 1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = green) + 
  annotate("rect", xmin = 1.2, xmax = Inf, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = pink) + 
  annotate("rect", xmin = -Inf, xmax = -1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = pink) + 
  coord_cartesian(xlim = c(-3, 3), ylim = c(1.5,2.5)) + 
  annotate("segment", x = -3, xend = -1.2, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "first"), color = "black") + 
  annotate("segment", x = -1.2, xend = 1.2, y = 2, yend = 2, color = "black") +
  annotate("segment", x = 1.2, xend = 3, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "last"), color = "black") +
  annotate("label", x = 0, y = 2.1, label = "Incentivized", color = green, size = 6) +
  annotate("label", x = -2.2, y = 2.1, label = "No Incentive", color = "red", size = 6) +
  annotate("label", x = 2.2, y = 2.1, label = "No Incentive", color = "red", size = 6) +
  geom_point(x = 0, y = 2 , size = 2, color = "black") + 
  annotate("text", x = 0, y = 1.94, label = "True Rank", size = 6) +
  annotate("text", x = 1.2, y = 1.94, label = "+ 2", size = 6) +
  annotate("text", x = -1.2, y = 1.94, label = "- 2", size = 6) +
  annotate("text", x = 2.4, y = 1.94, label = "+ 4", size = 6) +
  annotate("text", x = -2.4, y = 1.94, label = "- 4", size = 6) +
  geom_segment(x = 1.2, xend = 1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = .6, xend = .6, y = 1.98, yend = 2.02) +
  geom_segment(x = -1.2, xend = -1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = -.6, xend = -.6, y = 1.98, yend = 2.02) +
  geom_segment(x = -1.8, xend = -1.8, y = 1.98, yend = 2.02) +
  geom_segment(x = -2.4, xend = -2.4, y = 1.98, yend = 2.02) +
  geom_segment(x = 1.8, xend = 1.8, y = 1.98, yend = 2.02) +
  geom_segment(x = 2.4, xend = 2.4, y = 1.98, yend = 2.02) + 
  labs(title = "Other Participant's Estimate") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, size = 25))
ggsave("output/graphs/misc/sender_incentive_graphs/unbiased_sender_rank_incentives.png", width = 10, height = 4)


#### ambiguous biased sender rank ####
ggplot(df, aes(x = x_axis, y = line, group = x_axis_str)) + 
  geom_line(alpha = 0) +
  annotate("rect", xmin = -1.2, xmax = 1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = pink) + 
  annotate("rect", xmin = 1.2, xmax = Inf, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = green) + 
  annotate("rect", xmin = -Inf, xmax = -1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = green) + 
  coord_cartesian(xlim = c(-3, 3), ylim = c(1.5,2.5)) + 
  annotate("segment", x = -3, xend = -1.2, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "first"), color = "black") + 
  annotate("segment", x = -1.2, xend = 1.2, y = 2, yend = 2, color = "black") +
  annotate("segment", x = 1.2, xend = 3, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "last"), color = "black") +
  annotate("label", x = 0, y = 2.1, label = "No Incentive", color = 'red', size = 6) +
  annotate("label", x = -2.2, y = 2.1, label = "Incentivized", color = green, size = 6) +
  annotate("label", x = 2.2, y = 2.1, label = "Incentivized", color = green, size = 6) +
  geom_point(x = 0, y = 2 , size = 2, color = "black") + 
  annotate("text", x = 0, y = 1.94, label = "True Rank", size = 6) +
  annotate("text", x = 1.2, y = 1.94, label = "+ 2", size = 6) +
  annotate("text", x = -1.2, y = 1.94, label = "- 2", size = 6) +
  annotate("text", x = 2.4, y = 1.94, label = "+ 4", size = 6) +
  annotate("text", x = -2.4, y = 1.94, label = "- 4", size = 6) +
  geom_segment(x = 1.2, xend = 1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = .6, xend = .6, y = 1.98, yend = 2.02) +
  geom_segment(x = -1.2, xend = -1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = -.6, xend = -.6, y = 1.98, yend = 2.02) +
  geom_segment(x = -1.8, xend = -1.8, y = 1.98, yend = 2.02) +
  geom_segment(x = -2.4, xend = -2.4, y = 1.98, yend = 2.02) +
  geom_segment(x = 1.8, xend = 1.8, y = 1.98, yend = 2.02) +
  geom_segment(x = 2.4, xend = 2.4, y = 1.98, yend = 2.02) + 
  labs(title = "Other Participant's Estimate") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, size = 25))
ggsave("output/graphs/misc/sender_incentive_graphs/ambiguous_bias_sender_rank_incentives.png", width = 10, height = 4)

#### biased up sender rank ####
ggplot(df, aes(x = x_axis, y = line, group = x_axis_str)) + 
  geom_line(alpha = 0) +
  annotate("rect", xmin = -1.2, xmax = 1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = pink) + 
  annotate("rect", xmin = 1.2, xmax = Inf, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = green) + 
  annotate("rect", xmin = -Inf, xmax = -1.2, ymin = -Inf, ymax = Inf,
           alpha = .3,
           fill = pink) + 
  coord_cartesian(xlim = c(-3, 3), ylim = c(1.5,2.5)) + 
  annotate("segment", x = -3, xend = -1.2, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "first"), color = "black") + 
  annotate("segment", x = -1.2, xend = 1.2, y = 2, yend = 2, color = "black") +
  annotate("segment", x = 1.2, xend = 3, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "last"), color = "black") +
  annotate("label", x = -1.2, y = 2.1, label = "No Incentive", color = 'red', size = 6) +
  annotate("label", x = 2.2, y = 2.1, label = "Incentivized", color = green, size = 6) +
  geom_point(x = 0, y = 2 , size = 2, color = "black") + 
  annotate("text", x = 0, y = 1.94, label = "True Rank", size = 6) +
  annotate("text", x = 1.2, y = 1.94, label = "+ 2", size = 6) +
  annotate("text", x = -1.2, y = 1.94, label = "- 2", size = 6) +
  annotate("text", x = 2.4, y = 1.94, label = "+ 4", size = 6) +
  annotate("text", x = -2.4, y = 1.94, label = "- 4", size = 6) +
  geom_segment(x = 1.2, xend = 1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = .6, xend = .6, y = 1.98, yend = 2.02) +
  geom_segment(x = -1.2, xend = -1.2, y = 1.98, yend = 2.02) +
  geom_segment(x = -.6, xend = -.6, y = 1.98, yend = 2.02) +
  geom_segment(x = -1.8, xend = -1.8, y = 1.98, yend = 2.02) +
  geom_segment(x = -2.4, xend = -2.4, y = 1.98, yend = 2.02) +
  geom_segment(x = 1.8, xend = 1.8, y = 1.98, yend = 2.02) +
  geom_segment(x = 2.4, xend = 2.4, y = 1.98, yend = 2.02) + 
  labs(title = "Other Participant's Estimate") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, size = 25))
ggsave("output/graphs/misc/sender_incentive_graphs/biased_up_sender_rank_incentives.png", width = 10, height = 4)





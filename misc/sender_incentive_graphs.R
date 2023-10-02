# the purpose of this code is to make graphs with show how senders are biased

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

# biased up
ggplot(df, aes(x = x_axis, y = line, group = x_axis_str)) + 
  geom_line(alpha = 0)+
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
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank()) + 
  annotate("segment", x = -3, xend = -1.2, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "first"), color = "red") + 
  annotate("segment", x = -1.2, xend = 1.2, y = 2, yend = 2, color = green) +
  annotate("segment", x = 1.2, xend = 3, y = 2, yend = 2, 
           arrow = arrow(length = unit(0.5, "cm"), ends = "last"), color = "red") +
  annotate("label", x = 0, y = 2.1, label = "Incentivized", color = green, size = 6) +
  annotate("label", x = -2.2, y = 2.1, label = "No Incentive", color = "red", size = 6) +
  annotate("label", x = 2.2, y = 2.1, label = "No Incentive", color = "red", size = 6) +
  geom_point(x = 0, y = 2 , size = 2, color = "black")









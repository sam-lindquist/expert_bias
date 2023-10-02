library(lubridate)
library(tidyverse)
library(zoo)
library(scales)

path_to_dir <- "/Users/samuellindquist/Library/CloudStorage/Dropbox/expert_bias/code_data" # sams mac
#path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

dates <- seq.Date(
  from = as.Date("2023-06-01"),
  to = as.Date("2025-06-01"),
  by = "1 month"
)

df <- tibble(dates)

df$inf <- NA

df[df$dates == "2023-06-01", ]$inf <- 3.2
df[df$dates == "2024-06-01", ]$inf <- 2.5
df[df$dates == "2025-06-01", ]$inf <- 2.1

df$inf_app <- approx(df$inf, n = nrow(df))$y

date_labels <- as.Date(c("2023-06-01", "2024-06-01", "2025-06-01"), format = "%Y-%m-%d")
names(date_labels) <- c("June 2023", "June 2024", "June 2025")

x_limits <- as.Date(c("2023-01-01", "2026-01-01"))

our_estimate <- df[df$dates == "2023-12-01", ]$inf_app

df %>% 
  filter(!is.na(inf)) %>% 
  ggplot(aes(dates, inf)) + geom_point() + geom_line() +
  coord_cartesian(xlim = x_limits) +
  scale_y_continuous(labels = function (x) {paste0(x, "%")}) + 
  scale_x_date(breaks = date_labels,
               labels = names(date_labels)) + 
  geom_point(x = as.Date("2023-12-01"), y = our_estimate, color = "steelblue") + 
  annotate("label", x = as.Date("2024-05-01"), y = our_estimate, label = paste0("Correct Answer = ", our_esimate), color = "steelblue") +
  labs(title = "FOMC Inflation Projections",
       y = "Inflation", 
       x = "") + 
  theme(plot.title = element_text(hjust = 0.5))
 
# exporting
path_to_output <- c("output/graphs/misc/")

ggsave(paste0(path_to_output, "inf_fomc_projections.jpeg"))


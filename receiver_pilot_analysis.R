library(tidyverse)
library(Hmisc)
library(DescTools)
library(tidylog, warn.conflicts = FALSE)
library(lubridate)
library(fixest)
library(tidymodels)

path_to_dir <- "/Users/samuellindquist/Library/CloudStorage/Dropbox/expert_bias/code_data" # sams mac
#path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

df1 <- read_csv("output/receiver_pilot_101323_clean.csv")
df2 <- read_csv("output/receiver_pilot_101523_clean.csv")

df1$pilot <- 1
df2$pilot <- 2

df <- bind_rows(df1, df2)

#### CREATING VARS ####
df$post_inflation_point_win <- Winsorize(df$post_inflation_point, na.rm = TRUE)
df$prior_inflation_point_win <- Winsorize(df$prior_inflation_point, na.rm = TRUE)
df$update_win <- df$post_inflation_point_win - df$prior_inflation_point_win

df$prior_signal_diff_win <- 2 - df$prior_inflation_point_win 

#### BASICS ####

df %>% group_by(treatment) %>% 
  summarise(prior_mean = mean(prior_inflation_point, na.rm = TRUE),
            prior_median = median(prior_inflation_point, na.rm = TRUE),
            post_mean = mean(post_inflation_point, na.rm = TRUE), 
            post_median = median(post_inflation_point, na.rm = TRUE),
            update = mean(update, na.rm = TRUE),
            update_win = mean(update_win, na.rm = TRUE),
            prior_mean_win = mean(prior_inflation_point_win, na.rm = TRUE))

df %>% summarise(mean(prior_inflation_density_mean, na.rm = TRUE),
                 mean(prior_inflation_point, na.rm = TRUE),
                 mean(post_inflation_density_mean, na.rm = TRUE),
                 mean(post_inflation_point, na.rm = TRUE),
                 mean(prior_inflation_density_sd, na.rm = TRUE),
                 mean(post_inflation_density_sd, na.rm = TRUE),
                 median(prior_inflation_density_sd, na.rm = TRUE),
                 median(post_inflation_density_sd, na.rm = TRUE))

# looking into distributions
df %>% ggplot(aes(x = prior_inflation_point)) + geom_histogram(bins = 50)
df %>% ggplot(aes(x = prior_inflation_point_win)) + geom_histogram(bins = 50)

df %>% ggplot(aes(x = post_inflation_point)) + geom_histogram(bins = 50)
df %>% ggplot(aes(x = post_inflation_point_win)) + geom_histogram(bins = 50)

df %>% ggplot(aes(x = update)) + geom_histogram(bins = 50)
df %>% ggplot(aes(x = update_win)) + geom_histogram(bins = 50)

describe(df$update)

#### REGRESSIONS ####

# updating towards the signal, reduction of difference between belief and signal




df$treatment <- factor(df$treatment, levels = c("nbe", "nbl", "ae"))

feols(post_inflation_point ~ treatment + prior_inflation_point*treatment + prior_inflation_point, df)
feols(post_inflation_point_win ~ treatment + prior_inflation_point_win*treatment + prior_inflation_point_win, df)


feols(update ~ treatment + prior_signal_diff*treatment + prior_signal_diff, df %>% filter(pilot == 1))
feols(update ~ treatment + prior_signal_diff*treatment + prior_signal_diff, df %>% filter(pilot == 2))

feols(update_win ~ treatment + prior_signal_diff_win*treatment + prior_signal_diff_win, df %>% filter(pilot == 1))

feols(update_win ~ treatment + prior_signal_diff_win*treatment + prior_signal_diff_win, df %>% filter(pilot == 2))


feols(update_win ~ treatment + prior_signal_diff_win*treatment + prior_signal_diff_win, df)
feols(update ~ treatment + prior_signal_diff*treatment + prior_signal_diff, df)


feols(post_inflation_point_win ~ treatment*prior_inflation_point_win, 
      df %>% filter(pilot == 2))







#### POWER ####






# reduction in belief
feols(post_inflation_point ~ treatment + prior_inflation_point*treatment + prior_signal_diff, df)

# uncertainty reduction
feols(post_inflation_density_sd ~ treatment + prior_signal_diff*treatment + prior_inflation_density_sd, df)

#### POWER STUFF ####
power = function(group1.pilot, group2.pilot, reps=1000, size=10) {
  results  <- sapply(1:reps, function(r) {
    group1.resample <- sample(group1.pilot, size=size, replace=TRUE) 
    group2.resample <- sample(group2.pilot, size=size, replace=TRUE) 
    test <- wilcox.test(group1.resample, group2.resample, paired=FALSE)
    test$p.value
  })
  sum(results<0.05)/reps
}

set.seed(123)
df_boot <- bootstraps(df2,
                      times = 1000,
                      apparent = TRUE)

df_models <- df_boot %>% 
  mutate(model = map(splits, ~lm(post_signal_diff ~ treatment + prior_signal_diff*treatment + prior_signal_diff,
                                 data = .)),
         coef_inf = map(model, tidy))

df_coefs <- df_models %>% 
  unnest(coef_inf)

df_coefs %>% 
  filter(term == "treatmentnbl:prior_signal_diff") %>% 
  ggplot(aes(x = estimate)) +
  geom_histogram()

df_coefs %>% 
  filter(term == "treatmentnbl:prior_signal_diff") %>% 
  summarise(mean(estimate),
            median(estimate),
            sd(estimate))

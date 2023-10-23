library(tidyverse)
library(Hmisc)
library(DescTools)
library(tidylog, warn.conflicts = FALSE)
library(lubridate)
library(fixest)
library(tidymodels)

#path_to_dir <- "/Users/samuellindquist/Library/CloudStorage/Dropbox/expert_bias/code_data" # sams mac
path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

df <- read_csv("input/receiver_pilot_101523_raw.csv")

#### CLEANING, RENAMING, ETC ####

# deleting first two rows: qualtrics metadata
df <- df %>% 
  slice(3:nrow(df))

# deleting observations from before pilot started
df <- df %>% 
  slice(3:nrow(df))

# renaming vars
df <- df %>% 
  rename(prior_inflation_point = prior_inflation_poin,
         id = ResponseId)

# deleting those that didnt solve attention check on second try
df <- df %>% 
  filter((attention_check_2_1 == 100 | is.na(attention_check_2_1)) & (attention_check_2_2 == 50 | is.na(attention_check_2_2)))

# converting some vars to numeric
df <- df %>% 
  mutate(across(attention_check_1:post_inflation_point, as.numeric))

df <- df %>% mutate(trust_likert_num = as.numeric(str_remove_all(trust_likert, "(\\(Very Much\\)|\\(Very little\\)|\\(Moderately\\))")))

# reordering treatments, first element treated as control in treatments
df$treatment <- factor(df$treatment, levels = c("nbe", "nbl"))

# creating signal updating variables
inflation_signal <- 2

df$prior_signal_diff <- inflation_signal - df$prior_inflation_point 
df$post_signal_diff <- inflation_signal - df$post_inflation_point
df$update <- df$post_inflation_point - df$prior_inflation_point

# exporting
write_csv(df, "output/receiver_pilot_101523_clean.csv")


#### REGRESSIONS ####

t.test(update ~ treatment, df)

feols(post_inflation_point ~ treatment + prior_inflation_point*treatment + prior_inflation_point, df)

feols(update ~ treatment + prior_signal_diff*treatment + prior_signal_diff, df)


df %>% group_by(treatment) %>% 
  summarise(prior_mean = mean(prior_inflation_point),
            prior_median = median(prior_inflation_point),
            post_mean = mean(post_inflation_point), 
                 post_median = median(post_inflation_point))
                 

#### POWER STUFF ####
power_t_test = function(group1.pilot, group2.pilot, reps, size) {
  results  <- sapply(1:reps, function(r) {
    group1.resample <- sample(group1.pilot, size=size, replace=TRUE) 
    group2.resample <- sample(group2.pilot, size=size, replace=TRUE) 
    test <- wilcox.test(group1.resample, group2.resample, paired=FALSE)
    test$p.value
  })
  sum(results<0.05)/reps
}

power_t_test(df %>% filter(treatment == "nbe") %>% pull(update),
      df %>% filter(treatment == "nbl") %>% pull(update),
      1000,
      1000)

power_reg = function(reg) {
  results  <- sapply(1:reps, function(r) {
    df_sample <- sample(df, size=nrow(df), replace=TRUE) 
    
    
    
    test$p.value
  })
  sum(results<0.05)/reps
}

reg <- feols(update ~ treatment + prior_signal_diff*treatment +  prior_signal_diff, df_sample)

power_t_test(df %>% filter(treatment == "nbe") %>% pull(update),
             df %>% filter(treatment == "nbl") %>% pull(update),
             1000,
             1000)

df_sample <- sample(df, size=nrow(df), replace=TRUE) 

set.seed(123)
df_boot <- bootstraps(df,
                      times = 1000,
                      apparent = TRUE)

df_models <- df_boot %>% 
  mutate(model = map(splits, ~lm(update ~ treatment + prior_signal_diff*treatment + prior_signal_diff,
                                 data = .)),
         coef_inf = map(model, tidy))

df_coefs <- df_models %>% 
  unnest(coef_inf)

df_coefs %>% 
  filter(term == "treatmentnbl:prior_signal_diff") %>% 
  ggplot(aes(x = statistic)) +
  geom_histogram()

df_coefs %>% 
  filter(term == "treatmentnbl:prior_signal_diff") %>% 
  summarise(mean(estimate),
            median(estimate),
            sd(estimate))

# #### VARIABLES ON DISTRIBUTIONS ####
# 
# distribution_vars_prefixes <- c("prior_inflation_dist", "post_inflation_dist")
# 
# # inflation
# lower_bounds <- rev(c(-Inf, -12,  -8,  -4,  -2, 0,  2, 4, 8, 12))
# upper_bounds <- rev(c(-12, -8, -4, -2, 0, 2, 4, 8, 12, Inf))
# 
# inflation_buckets <- tibble(lower_bound = rep(lower_bounds, length(distribution_vars_prefixes)),
#                             upper_bound = rep(upper_bounds, length(distribution_vars_prefixes)))
# 
# inflation_buckets$variable <- names(df)[grepl(paste(distribution_vars_prefixes, collapse = "|"), names(df))]
# 
# inflation_buckets$variable_name <- str_split_fixed(inflation_buckets$variable, "_dist_", 2)[, 1]
# inflation_buckets$variable_num <- as.numeric(str_split_fixed(inflation_buckets$variable, "_dist_", 2)[, 2])
# 
# inflation_buckets <- inflation_buckets %>% 
#   mutate(bucket_avg = ifelse(is.infinite(lower_bound), upper_bound + (upper_bound - lag(upper_bound))/2, 
#                              ifelse(is.infinite(upper_bound), lower_bound + (lower_bound - lead(lower_bound))/2,
#                                     (lower_bound + upper_bound)/2)))
# 
# df_dist <- df %>% 
#   select(id, 
#          starts_with(distribution_vars_prefixes)) %>% 
#   pivot_longer(cols = !id,
#                names_to = "variable",
#                values_to = "probability") %>% 
#   mutate(probability = probability)
# 
# df_dist <- left_join(df_dist, inflation_buckets, by = "variable") %>% 
#   arrange(id, variable_name, variable_num)
# 
# # making sure all probabilities sum to 100
# df_dist <- df_dist %>% 
#   group_by(id, variable_name) %>% 
#   mutate(prob_sum = round(sum(probability))) %>% 
#   ungroup() %>% 
#   mutate(prob_clean = ifelse(prob_sum == 100, probability/100, NA))
# 
# # dropping responses from anybody who gave negative responses
# df_dist <- df_dist %>% 
#   group_by(id, variable_name) %>% 
#   mutate(any_prob_less_zero = ifelse(any(probability < 0), TRUE, FALSE)) %>% 
#   ungroup() %>% 
#   mutate(prob_clean = ifelse(any_prob_less_zero == TRUE, NA, prob_clean)) %>% 
#   ungroup()
# 
# # vars no longer needed
# df_dist <- df_dist %>% 
#   select(-c(prob_sum, any_prob_less_zero))
# 
# df_dist_agg <- df_dist %>% 
#   group_by(id, variable_name) %>% 
#   summarise(density_mean = sum(bucket_avg*prob_clean),
#             density_sd = sqrt(sum(prob_clean*(bucket_avg - density_mean)^2))) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = "variable_name",
#               values_from = c("density_mean", "density_sd"),
#               names_glue = "{variable_name}_{.value}")
# 
# # merging back into dataset
# df <- df %>% 
#   left_join(df_dist_agg, by = "id")
# 
# #### EXPORTING ####
# write_csv(df, "output/receiver_pilot_101323_clean.csv")


#### PAYOUT BONUSES ####
# payouts <- df %>% filter(post_inflation_point >= 1.5 & post_inflation_point <= 4.3) %>% 
#   select(PROLIFIC_PID) %>% 
#   mutate(pay = 1)
# 
# write_csv(payouts, "payouts.csv")
# 
# for (i in  1:nrow(payouts)) {
#   print(paste0(payouts$PROLIFIC_PID[i], ", ", payouts$pay[i]))
# }
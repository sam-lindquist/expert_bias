library(tidyverse)
library(tidylog, warn.conflicts = FALSE)
library(DescTools)
library(fixest)

path_to_dir <- "/Users/samuellindquist/Library/CloudStorage/Dropbox/expert_bias/code_data" # sams mac
#path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

df <- read_csv("input/receiver_rady_lab_102023.csv")

#### CLEANING, RENAMING, ETC ####

# deleting first two rows of qualtrics metadata
df <- df %>% 
  filter(!StartDate %in% c("Start Date", "{\"ImportId\":\"startDate\",\"timeZone\":\"America/Denver\"}"))

# deleting those that didn't consent
df <- df %>% 
  filter(consent %in% "I have read this consent form and agree to take part in this study")

# deleting those that didnt pass attention check
df <- df %>% 
  filter((attention_check_2_1 == 100 | is.na(attention_check_2_1)) & (attention_check_2_1 == 50 | is.na(attention_check_2_1)))

# renaming vars

# delete the "ucsd_" prefix from all vars
df <- df %>% 
  rename_with(~str_replace(.,"ucsd_" , ""), starts_with("ucsd_"))

# changing the prefix "rank_prior_" to "prior_rank_"
df <- df %>% 
  rename_with(~str_replace(.,"rank_prior_" , "prior_rank_"), starts_with("rank_prior_"))

df <- df %>% 
  rename(prior_inflation_point = prior_inflation_poin,
         prior_rank_point = prior_rank_poin,
         id = ResponseId,
         post_rank_dist_1 = Q60_1,
         post_rank_dist_2 = Q60_2,
         post_rank_dist_3 = Q60_3,
         post_rank_dist_4 = Q60_4,
         post_rank_dist_5 = Q60_5,
         post_rank_dist_6 = Q60_6,
         post_rank_dist_7 = Q60_7,
         post_rank_dist_8 = Q60_8,
         post_rank_dist_9 = Q60_9,
         post_rank_dist_10 = Q60_10,
         post_rank_dist_11 = Q60_11,
         post_rank_dist_12 = Q60_12,
         post_rank_point = post_rank_point,
         trust_open = Q1,
         trust_likert = Q2)

# converting some vars to numeric
df <- df %>% 
  mutate(across(c(attention_check_1_1:post_inflation_dist_10, prior_rank_point:post_rank_dist_12), as.numeric))

# creating datetime variable
df$date_time <- as.POSIXct(df$StartDate, format = "%Y-%m-%d %H:%M:%OS")

#### CREATING NEW RELEVANT VARIABLES ####

inflation_signal <- 2
rank_signal <- 32

# inflation-related
df$inf_update <- df$post_inflation_point - df$prior_inflation_point
df$prior_inf_signal_diff <- inflation_signal - df$prior_inflation_point

# rank related
df$rank_update <- df$post_rank_point - df$prior_rank_point
df$prior_rank_signal_diff <- rank_signal - df$prior_rank_point

# everything created above winsorized

# inflation stuff
df$post_inflation_point_win <- Winsorize(df$post_inflation_point, na.rm = TRUE)
df$prior_inflation_point_win <- Winsorize(df$prior_inflation_point, na.rm = TRUE)

df$inf_update_win <- df$post_inflation_point_win - df$prior_inflation_point_win

df$prior_inf_signal_diff_win <- inflation_signal - df$prior_inflation_point_win 

# ucsd rank stuff
df$post_rank_point_win <- Winsorize(df$post_rank_point, na.rm = TRUE)
df$prior_rank_point_win <- Winsorize(df$prior_rank_point, na.rm = TRUE)

df$rank_update_win <- df$post_rank_point_win - df$prior_rank_point_win

df$prior_rank_rank_signal_diff_win <- rank_signal - df$prior_rank_point_win

#### VARIABLES ON DISTRIBUTIONS ####

# vars to be used from dataframe
distribution_vars_prefixes <- c("prior_inflation_dist", "post_inflation_dist", "prior_rank_dist", "post_rank_dist")

# upper and lower bounds of distributions
inf_lower_bounds <- rev(c(-Inf, -12,  -8,  -4,  -2, 0,  2, 4, 8, 12)) #writing them out in reverse from how they were elicited
inf_upper_bounds <- rev(c(-12, -8, -4, -2, 0, 2, 4, 8, 12, Inf))

rank_lower_bounds <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60)
rank_upper_bounds <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, Inf)

# we duplicate above lists because we elicit each belief twice (prior and post)
belief_elicitation_count <- 2

lower_bounds <- c(rep(inf_lower_bounds, belief_elicitation_count), rep(rank_lower_bounds, belief_elicitation_count))
upper_bounds <- c(rep(inf_upper_bounds, belief_elicitation_count), rep(rank_upper_bounds, belief_elicitation_count))

belief_buckets <- tibble(lower_bound = lower_bounds,
                            upper_bound = upper_bounds)

belief_buckets$variable <- names(df)[grepl(paste(distribution_vars_prefixes, collapse = "|"), names(df))]

belief_buckets$variable_name <- str_split_fixed(belief_buckets$variable, "_dist_", 2)[, 1]
belief_buckets$variable_num <- as.numeric(str_split_fixed(belief_buckets$variable, "_dist_", 2)[, 2])


# adding a variable with the average of each bucket
belief_buckets <- belief_buckets %>%
  mutate(bucket_avg = (if_else(grepl("(inflation_dist_1)\\b", variable), lower_bound + lead(upper_bound - lower_bound)/2, 
                               if_else(grepl("(inflation_dist_10)\\b", variable), upper_bound - lag(upper_bound - lower_bound)/2,
                                       if_else(grepl("(rank_dist_12)\\b", variable), lower_bound + lag(upper_bound - lower_bound)/2,
                                              (lower_bound + upper_bound)/2)))))

df_dist <- df %>% 
  select(id, 
         starts_with(distribution_vars_prefixes)) %>% 
  pivot_longer(cols = !id,
               names_to = "variable",
               values_to = "probability") %>% 
  mutate(probability = probability)

df_dist <- left_join(df_dist, belief_buckets, by = "variable") %>% 
  arrange(id, variable_name, variable_num)

# making sure all probabilities sum to 100
df_dist <- df_dist %>% 
  group_by(id, variable_name) %>% 
  mutate(prob_sum = round(sum(probability))) %>% 
  ungroup() %>% 
  mutate(prob_clean = ifelse(prob_sum == 100, probability/100, NA))

# dropping responses from anybody who gave negative responses
df_dist <- df_dist %>% 
  group_by(id, variable_name) %>% 
  mutate(any_prob_less_zero = ifelse(any(probability < 0), TRUE, FALSE)) %>% 
  ungroup() %>% 
  mutate(prob_clean = ifelse(any_prob_less_zero == TRUE, NA, prob_clean)) %>% 
  ungroup()

# vars no longer needed
df_dist <- df_dist %>% 
  select(-c(prob_sum, any_prob_less_zero))

# calculating summary variables for each distribution
df_dist_agg <- df_dist %>% 
  group_by(id, variable_name) %>% 
  summarise(dist_mean = sum(bucket_avg*prob_clean), # mean
            dist_sd = sqrt(sum(prob_clean*(bucket_avg - dist_mean)^2)), # standard deviation
            dist_mad = sum(prob_clean*abs(bucket_avg - dist_mean))) %>% # mean absolute deviation
  ungroup() %>% 
  pivot_wider(names_from = "variable_name",
              values_from = c("dist_mean", "dist_sd", "dist_mad"), 
              names_glue = "{variable_name}_{.value}")

# merging back into dataset
df <- df %>% 
  left_join(df_dist_agg, by = "id")

#### REGRESSIONS ####

df$treatment <- factor(df$treatment, levels = c("control", "nbl", "nbe", "ae", "bue"))

feols(inf_update ~ treatment + prior_inf_signal_diff + prior_inf_signal_diff*treatment, df) %>% 
  summary()

feols(post_inflation_point ~ treatment + prior_inf_signal_diff + prior_inf_signal_diff*treatment, df) %>% 
  summary()

feols(post_inflation_point_win ~ treatment + prior_inf_signal_diff_win + prior_inf_signal_diff_win*treatment, df) %>% 
  summary()





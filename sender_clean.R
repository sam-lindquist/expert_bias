library(tidyverse)
library(Hmisc)
library(DescTools)
library(tidylog, warn.conflicts = FALSE)

path_to_dir <- "/Users/samuellindquist/Library/CloudStorage/Dropbox/expert_bias/code_data" # sams mac
#path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

df <- read_csv("input/sender_raw.csv")

# deleting first two rows: qualtrics metadata
df <- df %>% 
  slice(3:nrow(df))

# deleting attention check failures (also deletes tests)
df <- df %>%
  filter(attention_check_1 == 100 & attention_check_2 == 50)

# deleting those who didn't finish
df <- df %>% 
  filter(Finished %in% "True")

# deleting second responses of people that did survey twice
df$sona_id <- ifelse(is.na(df$sona_id), df$Q89, df$sona_id)

df <- df %>% 
  rename(id = Q89) %>% 
  mutate(id = ifelse(is.na(id), sona_id, id))

df <- df %>% 
  filter(!is.na(id)) %>% 
  distinct(id, .keep_all = TRUE)

# using 'id' variable as identifier because question forced individuals to give a five digit response so its more regular

# renaming
df <- df %>% 
  rename(prior_inflation_point = prior_inflation_poin,
         ucsd_rank_prior_point = ucsd_rank_prior_poin)

# converting some vars to numeric
df <- df %>% 
  mutate(across(attention_check_1:post_ucsd_pop, as.numeric))

# trying with data winsorized
df$post_inflation_point <- Winsorize(df$post_inflation_point, probs = c(0.05, 0.95))
df$post_ucsd_pop <- Winsorize(df$post_inflation_point, probs = c(0.05, 0.95))
df$post_ucsd_rank <- Winsorize(df$post_inflation_point, probs = c(0.05, 0.95))

# breaking up treatments along expert/layperson and accurate/upbiased/downbiased dims
df <- df %>% 
  mutate(info_treat = case_when(grepl("(nbl|bul|bdl)", treatment) ~ "Layperson",
                                grepl("(bde|bue|nbe)", treatment) ~ "Expert"),
         bias_treat = case_when(grepl("(bue|bul)", treatment) ~ "Biased Up",
                                grepl("(nbe|nbl)", treatment) ~ "Non-biased",
                                grepl("(bde|bdl)", treatment) ~ "Biased Down"))

# looking at revision of information after the treatment
df$inflation_update <- df$post_inflation_point - df$prior_inflation_point
df$ucsd_pop_update <- df$post_ucsd_pop - df$prior_ucsd_pop_point
df$ucsd_rank_update <- df$post_ucsd_rank - df$ucsd_rank_prior_point

# accuracy outcomes 
correct_inflation_forecast_march_23 <- 3.2 # "correct" inflation rate (as forecasted by the fed)
correct_inflation_forecast_june_23 <- 3.3

# correct ucsd population 
correct_ucsd_pop <- 42968 # for fall of academic year 2022-2023

#correct ucsd rank
correct_ucsd_rank <- 34 # for academic year 2022-2023

# accuracy measures
df$post_inflation_accuracy <- df$post_inflation_point - correct_inflation_forecast_march_23
df$post_ucsd_pop_accuracy <- df$post_ucsd_pop - correct_ucsd_pop
df$post_ucsd_rank_accuracy <- df$post_ucsd_rank - correct_ucsd_rank

df$prior_inflation_accuracy <- df$prior_inflation_point - correct_inflation_forecast_march_23
df$prior_ucsd_pop_accuracy <- df$prior_ucsd_pop_point - correct_ucsd_pop
df$prior_ucsd_rank_accuracy <- df$ucsd_rank_prior_point - correct_ucsd_rank

# distribution of probabilities for each measure (inflation, pop, rank)

# inflation
inflation_buckets <- tibble(lower_bound = c(-Inf, -12,  -8,  -4,  -2, 0,  2, 4, 8, 12),
                            upper_bound = c(-12, -8, -4, -2, 0, 2, 4, 8, 12, Inf))

inflation_buckets$variable <- names(df)[grepl("prior_inflation_dist", names(df))]


# ucsd ranking
rank_buckets <- tibble(lower_bound = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60),
                       upper_bound = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, Inf))

rank_buckets$variable <- names(df)[grepl("ucsd_rank_prior_dist", names(df))]

# ucsd pop
pop_buckets <- tibble(lower_bound = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000),
                      upper_bound = c(5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, Inf))

pop_buckets$variable <- names(df)[grepl("prior_ucsd_pop_dist", names(df))]

# appending these three variable types together
buckets <- rbind(inflation_buckets, pop_buckets, rank_buckets)

buckets$variable_name <- str_split_fixed(buckets$variable, "_dist_", 2)[, 1]
buckets$variable_num <- as.numeric(str_split_fixed(buckets$variable, "_dist_", 2)[, 2])

buckets <- buckets %>% 
  mutate(bucket_avg = ifelse(is.infinite(lower_bound), upper_bound + (upper_bound - lead(upper_bound))/2, 
                                       ifelse(is.infinite(upper_bound), lower_bound + (lower_bound - lag(lower_bound))/2,
                                              (lower_bound + upper_bound)/2)))

df_dist <- df %>% 
  select(id, 
         starts_with("prior_inflation_dist"), 
         starts_with("ucsd_rank_prior_dist"), 
         starts_with("prior_ucsd_pop_dist")) %>% 
  pivot_longer(cols = !id,
               names_to = "variable",
               values_to = "probability") %>% 
  mutate(probability = probability)

df_dist <- left_join(df_dist, buckets, by = "variable") %>% 
  arrange(id, variable_name, variable_num)

# making sure all probabilities sum to 100
df_dist <- df_dist %>% 
  group_by(id, variable_name) %>% 
  mutate(prob_sum = round(sum(probability))) %>% 
  ungroup() %>% 
  mutate(prob_clean = ifelse(prob_sum == 100, probability, NA))

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

df_dist_agg <- df_dist %>% 
  group_by(id, variable_name) %>% 
  summarise(density_mean = sum(bucket_avg*prob_clean),
         density_sd = sqrt(sum(prob_clean*(bucket_avg - density_mean)^2))) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "variable_name",
              values_from = c("density_mean", "density_sd"),
              names_glue = "{variable_name}_{.value}")

# merging back into dataset
df <- df %>% 
  left_join(df_dist_agg, by = "id")

# exporting            
write_csv(df, "output/sender_clean.csv")





library(tidyverse)

# path_to_dir <- "/Users/samuellindquist/Dropbox/expert_bias/code_data" # sams mac
path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

df <- read_csv("input/sender_08232023.csv")

# deleting first two rows: qualtrics metadata
df <- df %>% 
  slice(3:nrow(df))

# deleting attention check failures (also deletes tests)
df <- df %>%
  filter(attention_check_1 == 100 & attention_check_2 == 50)

# deleting those who didn't finish
df <- df %>% 
  filter(Finished %in% "True")

# renaming
df <- df %>% 
  rename(prior_inflation_point = prior_inflation_poin,
         ucsd_rank_prior_point = ucsd_rank_prior_poin)

# converting some vars to numeric
df <- df %>% 
  mutate(across(attention_check_1:post_ucsd_pop, as.numeric))

# breaking up treatments along expert/layperson and accurate/upbiased/downbiased dims
df <- df %>% 
  mutate(info_treat = case_when(grepl("(nbl|bul|bdl)", treatment) ~ "Layperson",
                                grepl("(bde|bue|nbe)", treatment) ~ "Expert"),
         bias_treat = case_when(grepl("(bue|bul)", treatment) ~ "Biased Up",
                                grepl("(nbe|nbl)", treatment) ~ "Non-biased",
                                grepl("(bde|bdl)", treatment) ~ "Biased Down"))

# looking at revision of information after the treatment
df$inflation_update <- df$prior_inflation_point - df$post_inflation_point
df$ucsd_pop_update <- df$prior_ucsd_pop_point - df$post_ucsd_pop
df$ucsd_rank_update <- df$ucsd_rank_prior_point - df$post_ucsd_rank

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

# mean bar chart
df %>% 
  group_by(treatment) %>% 
  mutate(post_inflation_point_sd = sd(post_inflation_point),
         post_inflation_point_mean = mean(post_inflation_point)) %>% 
  ggplot(color = treatment) +
  geom_bar(aes(x = treatment, 
               y = post_inflation_point),
           stat = 'summary', 
           fun = 'mean') + 
  geom_errorbar(aes(x = treatment,
                    ymin = post_inflation_point_mean - post_inflation_point_sd,
                    ymax = post_inflation_point_mean + post_inflation_point_sd))

# median bar chart
df %>% 
  group_by(treatment) %>% 
  mutate(post_inflation_point_sd = sd(post_inflation_point),
         post_inflation_point_median = median(post_inflation_point)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(aes(x = treatment, 
               y = post_inflation_point,
               fill = treatment),
           stat = 'summary', 
           fun = 'median') + 
  geom_errorbar(aes(x = treatment,
                    ymin = post_inflation_point_median - post_inflation_point_sd,
                    ymax = post_inflation_point_median + post_inflation_point_sd))

# median inflation updating 
df %>% 
  group_by(treatment) %>% 
  mutate(inflation_update_sd = sd(inflation_update),
         inflation_update_median = median(inflation_update)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(aes(x = treatment, 
               y = inflation_update_median,
               fill = treatment),
           stat = 'summary', 
           fun = 'median') + 
  geom_errorbar(aes(x = treatment,
                    ymin = inflation_update_median - inflation_update_sd,
                    ymax = inflation_update_median + inflation_update_sd))

# mean inflation updating 
df %>% 
  group_by(treatment) %>% 
  mutate(inflation_update_sd = sd(inflation_update),
         inflation_update_mean = mean(inflation_update)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(aes(x = treatment, 
               y = inflation_update_mean,
               fill = treatment),
           stat = 'summary', 
           fun = 'mean') + 
  geom_errorbar(aes(x = treatment,
                    ymin = inflation_update_mean - inflation_update_sd,
                    ymax = inflation_update_mean + inflation_update_sd))

# regressions
reg <- function(x) {
  summary(lm(x, df))
}



reg(prior_inflation_point ~ info_treat)
reg(prior_inflation_point ~ bias_treat)

# layperson has significantly higher inflation prior than expert
reg(post_inflation_point ~ info_treat)

# incentives insignificant
reg(post_inflation_point ~ bias_treat)

reg(inflation_update ~ info_treat)
reg(inflation_update ~ bias_treat)

# adding controls
# layperson still has higher inflation priors with controls
reg(post_inflation_point ~ info_treat + gender + political_party + employed + family_yearly_income)
reg(post_inflation_point ~ bias_treat + gender + political_party + employed + family_yearly_income)

reg(inflation_update ~ info_treat + gender + political_party + employed + family_yearly_income)

# biased up treatment marginally significant downwards
reg(inflation_update ~ bias_treat + gender + political_party + employed + family_yearly_income)

# looking at the other variables, ucsd stats and population
reg(ucsd_pop_update ~ info_treat + gender + political_party + employed + family_yearly_income)
reg(ucsd_pop_update ~ bias_treat + gender + political_party + employed + family_yearly_income)

reg(ucsd_rank_update ~ info_treat + gender + political_party + employed + family_yearly_income)
reg(ucsd_rank_update ~ bias_treat + gender + political_party + employed + family_yearly_income)

# looking at accuracy
reg(post_inflation_accuracy ~ bias_treat)
reg(post_ucsd_pop_accuracy ~ bias_treat)
reg(post_ucsd_rank_accuracy ~ bias_treat)

reg(post_inflation_accuracy ~ info_treat) # info makes people more accurate
reg(post_ucsd_pop_accuracy ~ info_treat)
reg(post_ucsd_rank_accuracy ~ info_treat) # info makes people more accurate

df %>% group_by(post_inflation_point) %>% distinct(treatment) %>% count() %>% view()

reg(post_inflation_accuracy ~ treatment + gender + political_party + employed + family_yearly_income)


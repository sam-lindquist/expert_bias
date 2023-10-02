# this code does some exploratory analysis on the sender data

library(tidyverse)
library(Hmisc)
library(DescTools)
library(stargazer)

path_to_dir <- "/Users/samuellindquist/Library/CloudStorage/Dropbox/expert_bias/code_data" # sams mac
#path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

df <- read_csv("output/sender_clean.csv")

# getting data ready for analysis
treatments <- c(
  "Biased Down Expert" = "bde", 
  "Non-biased Expert" = "nbe",
  "Biased Up Expert" = "bue",
  "Biased Down Layperson" = "bdl",
  "Non-biased Layperson" = "nbl",
  "Biased Up Layperson" = "bul"
)

df %>% filter(treatment %in% c("nbl", "bdl", "bul")) %>% 
  filter(abs(inflation_update) < 10) %>% 
  ggplot(aes(x = inflation_update, fill = treatment)) +
  geom_histogram()

#### GRAPHS ####
# mean inflation posterior
df %>% 
  group_by(treatment) %>% 
  mutate(post_inflation_point_sd = sd(post_inflation_point, na.rm = TRUE),
         post_inflation_point_mean = mean(post_inflation_point, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(color = treatment) +
  geom_bar(aes(x = treatment, 
               y = post_inflation_point),
           stat = 'summary', 
           fun = 'mean') + 
  scale_x_discrete(breaks = treatments,
                   labels = names(treatments)) +
  geom_errorbar(aes(x = treatment,
                    ymin = post_inflation_point_mean - post_inflation_point_sd,
                    ymax = post_inflation_point_mean + post_inflation_point_sd)) +
  theme(axis.text.x = element_text(angle = 45))

# median inflation posterior
df %>% 
  group_by(treatment) %>% 
  mutate(post_inflation_point_sd = sd(post_inflation_point, na.rm = TRUE),
         post_inflation_point_median = median(post_inflation_point, na.rm = TRUE)) %>% 
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
  mutate(inflation_update_sd = sd(inflation_update, na.rm = TRUE),
         inflation_update_median = median(inflation_update, na.rm = TRUE)) %>% 
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
  mutate(inflation_update_sd = sd(inflation_update, na.rm = TRUE),
         inflation_update_mean = mean(inflation_update, na.rm = TRUE)) %>% 
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

# median inflation accuracy 
df %>% 
  group_by(treatment) %>% 
  mutate(inflation_accuracy_sd = sd(post_inflation_accuracy, na.rm = TRUE),
         inflation_accuracy_median = median(post_inflation_accuracy, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(aes(x = treatment, 
               y = inflation_accuracy_median,
               fill = treatment),
           stat = 'summary', 
           fun = 'median') + 
  geom_errorbar(aes(x = treatment,
                    ymin = inflation_accuracy_median - inflation_accuracy_sd,
                    ymax = inflation_accuracy_median + inflation_accuracy_sd))

# mean inflation accuracy 
df %>% 
  group_by(treatment) %>% 
  mutate(inflation_accuracy_sd = sd(post_inflation_accuracy, na.rm = TRUE),
         inflation_accuracy_mean = mean(post_inflation_accuracy, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(aes(x = treatment, 
               y = inflation_accuracy_mean,
               fill = treatment),
           stat = 'summary', 
           fun = 'mean') + 
  geom_errorbar(aes(x = treatment,
                    ymin = inflation_accuracy_mean - inflation_accuracy_sd,
                    ymax = inflation_accuracy_mean + inflation_accuracy_sd))


#### REGRESSIONS ####
reg <- function(x) {
  summary(lm(x, df))
}
reg(inflation_update ~ prior_inflation_accuracy*treatment + treatment + prior_inflation_accuracy)


# compared to biased down expert, everybody else has a lower inflation update
reg(inflation_update ~ treatment + prior_inflation_density_sd + prior_inflation_point + gender + political_party + employed + family_yearly_income)
reg(inflation_update ~ treatment + prior_inflation_density_sd + prior_inflation_point)


reg(post_inflation_accuracy ~ treatment + prior_inflation_accuracy + gender + political_party + employed + family_yearly_income)
reg(post_inflation_accuracy ~ treatment + prior_inflation_density_sd + prior_inflation_point)

reg(post_inflation_accuracy ~ relevel(as.factor(bias_treat), ref = "Non-biased") + prior_inflation_density_sd + prior_inflation_point)
reg(post_inflation_accuracy ~ relevel(as.factor(info_treat), ref = "Layperson") + prior_inflation_density_sd + prior_inflation_point)

reg(inflation_update ~ relevel(as.factor(bias_treat), ref = "Non-biased") + prior_inflation_density_sd + prior_inflation_point)
reg(inflation_update ~ relevel(as.factor(info_treat), ref = "Layperson") + prior_inflation_density_sd + prior_inflation_point)


for (i in seq_along(treatments)) {
  for (j in seq_along(treatments)) {
    t_test <- t.test(df[df$treatment == treatments[i], ]$inflation_update, df[df$treatment == treatments[j], ]$inflation_update)
    p_value <- t_test[3]
    if (p_value < .1) {
      print(paste(names(treatments)[i], names(treatments)[j]))
      print(t_test)
    }
  }
}

df %>% group_by(treatment) %>% 
  summarise(mean())


# diff between Non-biased Expert Biased Up Layperson

t.test(inflation_update ~ info_treat, data = df)
t.test(inflation_update ~ bias_treat, data = df %>% filter(bias_treat != "Non-biased"))
t.test(inflation_update ~ bias_treat, data = df %>% filter(bias_treat != "Biased Down"))
t.test(inflation_update ~ bias_treat, data = df %>% filter(bias_treat != "Biased Up"))

t.test(inflation_update ~ bias_treat, data = df %>% filter(bias_treat != "Biased Up"))

# fixing bias level, looking at 
t.test(inflation_update ~ bias_treat, data = df)

# it appears overall that the bias treatment moved people more than the information treatment

# one prediction: holding bias fixed, experts will be more accurate than nonexperts
t.test(post_inflation_accuracy ~ info_treat, data = df %>% filter(bias_treat == "Non-biased"))
t.test(post_inflation_accuracy ~ info_treat, data = df %>% filter(bias_treat == "Biased Up"))
t.test(post_inflation_accuracy ~ info_treat, data = df %>% filter(bias_treat == "Biased Down"))

# another prediction: holding expertise fixed, non-biased people will be the most accurate, biased down people will underreport, biased up people will over report
t.test(post_inflation_accuracy ~ bias_treat, data = df %>% filter(info_treat == "Expert" & bias_treat != "Non-biased"))
t.test(post_inflation_accuracy ~ bias_treat, data = df %>% filter(info_treat == "Expert" & bias_treat != "Biased Up"))
t.test(post_inflation_accuracy ~ bias_treat, data = df %>% filter(info_treat == "Expert" & bias_treat != "Biased Down"))


# separating out info and bias 
df$info_treat <- relevel(as.factor(df$info_treat), ref = "Expert")
df$bias_treat <- relevel(as.factor(df$bias_treat), ref = "Biased Down")

#inflation
reg(post_inflation_accuracy ~ info_treat + bias_treat + info_treat*bias_treat + prior_inflation_accuracy + prior_inflation_density_sd + gender + political_party + employed + family_yearly_income)
reg(inflation_update ~ info_treat+ bias_treat + info_treat*bias_treat + prior_inflation_density_sd + gender + political_party + employed + family_yearly_income)

#pop
reg(post_ucsd_pop_accuracy ~ info_treat + bias_treat + info_treat*bias_treat + prior_ucsd_pop_accuracy + prior_ucsd_pop_density_sd + gender + political_party + employed + family_yearly_income)
reg(ucsd_pop_update ~ info_treat + bias_treat + info_treat*bias_treat + prior_ucsd_pop_density_sd + gender + political_party + employed + family_yearly_income)

reg(post_ucsd_pop_accuracy ~ treatment + prior_ucsd_pop_accuracy + prior_ucsd_pop_density_sd + gender + political_party + employed + family_yearly_income)
reg(ucsd_pop_update ~ treatment + prior_ucsd_pop_density_sd + gender + political_party + employed + family_yearly_income)

# ucsd rank
reg(post_ucsd_rank_accuracy ~ info_treat + bias_treat + info_treat*bias_treat + prior_ucsd_rank_accuracy + ucsd_rank_prior_density_sd + gender + political_party + employed + family_yearly_income)
reg(ucsd_rank_update ~ info_treat + bias_treat + info_treat*bias_treat + ucsd_rank_prior_density_sd + gender + political_party + employed + family_yearly_income)

reg(post_ucsd_rank_accuracy ~ treatment + prior_ucsd_rank_accuracy + ucsd_rank_prior_density_sd + gender + political_party + employed + family_yearly_income)
reg(ucsd_rank_update ~ treatment + ucsd_rank_prior_density_sd + gender + political_party + employed + family_yearly_income)


reg(inflation_update ~ info_treat+ bias_treat + info_treat*bias_treat + prior_inflation_density_sd + gender + political_party + employed + family_yearly_income)
reg(inflation_update ~ info_treat + bias_treat + info_treat*bias_treat + gender + political_party + employed + family_yearly_income)

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

reg(post_inflation_accuracy ~ info_treat)
reg(post_ucsd_pop_accuracy ~ info_treat)
reg(post_ucsd_rank_accuracy ~ info_treat) # info makes people more accurate

reg(post_inflation_accuracy ~ treatment + gender + political_party + employed + family_yearly_income)
reg(post_inflation_accuracy_abs ~ treatment + gender + political_party + employed + family_yearly_income)




#### SUMMARY STATS ####
df %>% group_by(treatment) %>% 
  summarise(mean = mean(post_inflation_accuracy, na.rm = T),
            sd = sd(post_inflation_accuracy, na.rm = T)) %>% 
  arrange(mean)

df %>% group_by(treatment) %>% 
  summarise(mean = mean(post_inflation_point, na.rm = T),
            sd = sd(post_inflation_point, na.rm = T)) %>% 
  arrange(mean)

df %>% group_by(info_treat) %>% 
  summarise(mean = mean(post_inflation_point, na.rm = T),
            sd = sd(post_inflation_point, na.rm = T)) %>% 
  arrange(mean)

# for the experts, we have the bd < nb < bu for inflation posterior
df %>% 
  filter(info_treat == "Expert") %>% 
  group_by(bias_treat) %>% 
  summarise(mean = mean(post_inflation_point, na.rm = T),
            sd = sd(post_inflation_point, na.rm = T)) %>% 
  arrange(mean)

# this pattern does not hold for laypeople, we get nb < bd < bu
df %>% 
  filter(info_treat == "Layperson") %>% 
  group_by(bias_treat) %>% 
  summarise(mean = mean(post_inflation_point, na.rm = T),
            sd = sd(post_inflation_point, na.rm = T)) %>% 
  arrange(mean)

# comparing two non-biased people, the expert is more accurate
df %>% 
  filter(bias_treat == "Non-biased") %>% 
  group_by(info_treat) %>% 
  summarise(mean = mean(post_inflation_point, na.rm = T),
            sd = sd(post_inflation_point, na.rm = T)) %>% 
  arrange(mean)

# comparing two biased up people, the expert has a higher estimate
df %>% 
  filter(bias_treat == "Biased Up") %>% 
  group_by(info_treat) %>% 
  summarise(mean = mean(post_inflation_point, na.rm = T),
            sd = sd(post_inflation_point, na.rm = T)) %>% 
  arrange(mean)

# comparing two biased down people, the expert has a lower estimate
df %>% 
  filter(bias_treat == "Biased Down") %>% 
  group_by(info_treat) %>% 
  summarise(mean = mean(post_inflation_point, na.rm = T),
            sd = sd(post_inflation_point, na.rm = T)) %>% 
  arrange(mean)



post_vars <- c("post_inflation_point", "post_ucsd_rank", "post_ucsd_pop")
post_vars <- c("post_inflation_accuracy", "post_ucsd_rank_accuracy", "post_ucsd_pop_accuracy")


for (i in seq_along(post_vars)) {
  print(paste("-----------", post_vars[i], "------------"))
  
  print("#### For experts ####")
  df %>% 
    filter(info_treat == "Expert") %>% 
    group_by(bias_treat) %>% 
    summarise(median = median(get(post_vars[i]), na.rm = T),
              sd = sd(get(post_vars[i]), na.rm = T)) %>% 
    arrange(median) %>% 
    print()
  
  print("#### For lay people ####")
  df %>% 
    filter(info_treat == "Layperson") %>% 
    group_by(bias_treat) %>% 
    summarise(median = median(get(post_vars[i]), na.rm = T),
              sd = sd(get(post_vars[i]), na.rm = T)) %>% 
    arrange(median) %>% 
    print()
  
  print("#### non biased people ####")
  df %>% 
    filter(bias_treat == "Non-biased") %>% 
    group_by(info_treat) %>% 
    summarise(median = median(get(post_vars[i]), na.rm = T),
              sd = sd(get(post_vars[i]), na.rm = T)) %>% 
    arrange(median) %>% 
    print()
  
  print("#### biased up ####")
  df %>% 
    filter(bias_treat == "Biased Up") %>% 
    group_by(info_treat) %>% 
    summarise(median = median(get(post_vars[i]), na.rm = T),
              sd = sd(get(post_vars[i]), na.rm = T)) %>% 
    arrange(median) %>% 
    print()
  
  print("#### biased down ####")
  df %>% 
    filter(bias_treat == "Biased Down") %>% 
    group_by(info_treat) %>% 
    summarise(median = median(get(post_vars[i]), na.rm = T),
              sd = sd(get(post_vars[i]), na.rm = T)) %>% 
    arrange(median) %>% 
    print()
}








reg(inflation_update ~ treatment + gender + political_party + employed + family_yearly_income)
reg(inflation_update ~ treatment)

describe(df$prior_inflation_point)
sd(df$prior_inflation_point, na.rm = T)

describe(df$post_inflation_point)
sd(df$post_inflation_point, na.rm = T)

df %>% group_by(info_treat) %>% 
  summarise(sd(prior_inflation_point, na.rm= T),
            sd(post_inflation_point, na.rm= T)) %>% 
  arrange()



## please comment on this script 

# looking at what values we can send to recipients
# inflation: 1, 2, 4, 5, 10, 25
df %>% group_by(post_inflation_point) %>% 
  distinct(treatment) %>% 
  count() %>% 
  filter(n == 6)

# ucsd rank: 32, 33, 34, 35
df %>% group_by(post_ucsd_rank) %>% 
  distinct(treatment) %>% 
  count() %>% 
  filter(n == 6)

# ucsd pop: 30,000, 40,000, 45,000
df %>% group_by(post_ucsd_pop) %>% 
  distinct(treatment) %>% 
  count() %>% 
  filter(n == 6)


#### OLD STUFF, KEEPING AROUND FOR NOW ####

# # trying measure of accuracy which is inverse distance: the higher the more accurate
# df$inverse_post_inflation_accuracy <- ifelse(df$post_inflation_accuracy != 0, abs(1/df$post_inflation_accuracy), NA)
# df$inverse_post_inflation_accuracy <- 1/(1+df$post_inflation_accuracy^2)
# df$inverse_prior_inflation_accuracy <- 1/(1+df$prior_inflation_accuracy^2)
# 
# df$prior_inflation_accuracy <- abs(df$prior_inflation_accuracy)




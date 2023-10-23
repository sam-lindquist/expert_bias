library(tidyverse)
library(tidylog, warn.conflicts = FALSE)

path_to_dir <- "/Users/samuellindquist/Library/CloudStorage/Dropbox/expert_bias/code_data" # sams mac
#path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

df_102023 <- read_csv("input/receiver_rady_lab_102023.csv")

#### CLEANING, RENAMING, ETC ####

# deleting first two rows of qualtrics metadata
df_102023 <- df_102023 %>% 
  filter(!StartDate %in% c("Start Date", "{\"ImportId\":\"startDate\",\"timeZone\":\"America/Denver\"}"))

# deleting those that didn't consent
df_102023 <- df_102023 %>% 
  filter(consent %in% "I have read this consent form and agree to take part in this study")

# deleting those that didnt pass attention check
df_102023 <- df_102023 %>% 
  filter((attention_check_2_1 == 100 | is.na(attention_check_2_1)) & (attention_check_2_1 == 50 | is.na(attention_check_2_1)))

# renaming vars
df_102023 <- df_102023 %>% 
  rename(prior_inflation_point = prior_inflation_poin,
         ucsd_rank_prior_point = ucsd_rank_prior_poin,
         id = ResponseId,
         ucsd_rank_post_dist_1 = Q60_1,
         ucsd_rank_post_dist_2 = Q60_2,
         ucsd_rank_post_dist_3 = Q60_3,
         ucsd_rank_post_dist_4 = Q60_4,
         ucsd_rank_post_dist_5 = Q60_5,
         ucsd_rank_post_dist_6 = Q60_6,
         ucsd_rank_post_dist_7 = Q60_7,
         ucsd_rank_post_dist_8 = Q60_8,
         ucsd_rank_post_dist_9 = Q60_9,
         ucsd_rank_post_dist_10 = Q60_10,
         ucsd_rank_post_dist_11 = Q60_11,
         ucsd_rank_post_dist_12 = Q60_12)

# creating datetime variable
df_102023$date_time <- as.POSIXct(df_102023$StartDate, format = "%Y-%m-%d %H:%M:%OS")

# converting some vars to numeric
df_102023 <- df_102023 %>% 
  mutate(across(c(attention_check_1_1:post_inflation_dist_10, ucsd_rank_prior_point:ucsd_rank_post_dist_12), as.numeric))

#### CORRECT ANSWER BOUNDS ####

# correct answer is 126 for majors_num
majors_num_lower <- 116
majors_num_upper <- 136

# range is 2.5 to 4.3 for inflation
inflation_lower <- 1.5
inflation_upper <- 4.3

# correct rank is 28
rank_lower <- 26
rank_upper <- 30

#### ADDING UP POINTS ####
payments <- df_102023 %>% 
  mutate(payments = 1 + 
           if_else(majors_num >= majors_num_lower & majors_num <= majors_num_upper, 1, 0) + 
           if_else(post_inflation_point >= inflation_lower & post_inflation_point <= inflation_upper, 1, 0) + 
           if_else(post_rank_point >= rank_lower & post_rank_point <= rank_upper, 1, 0)) %>% 
  select(id, date_time, venmo_id_1, venmo_id_2, payments, majors_num, post_inflation_point, post_rank_point)

completed_payments <- payments %>% 
  arrange(payments, date_time) %>% 
  filter(!is.na(venmo_id_1)) %>% 
  mutate(venmo_ids_equal = if_else(venmo_id_1 == venmo_id_2, 1, 0)) %>% 
  select(id, date_time, venmo_id_1, venmo_id_2, payments, venmo_ids_equal, majors_num, post_inflation_point, post_rank_point)

#### BRINGING IN HAND CHECKED VENMO IDS ####
id <- c(
  "R_1IK930Ot9t6VO5m",
  "R_1mwP5eJizPD9B9e",
  "R_1qeWA87EDYepcUE",
  "R_28ZHZuaisGQoJvZ",
  "R_2CDHGvnfOFU9x86",
  "R_2TEMWYHUJ7InEe5",
  "R_2aer9fJwMBGve2N",
  "R_2azSkWELCtZ9a32",
  "R_2b1dFHwcHgOKHzK",
  "R_2qg749CQ0mGcZmt",
  "R_3IPRivClHcR87eh",
  "R_3kqcM1zbTg5VAlk",
  "R_bC2llznaLZEDPu9",
  "R_sX0G0nLIJh8pXbj",
  "R_tDpxyqbSkg2NE9r",
  "R_vfuanAwGAdtz39L",
  "R_1CjxEtwB3RK4nP8",
  "R_1FCy8EJt7r2AECE",
  "R_1FPwpsLwkMUhq29",
  "R_1ML4naHAdmOcYjP",
  "R_1lcaRBXr83mDKfh",
  "R_1mC2KWW60onV84c",
  "R_27EvJfGVnWJBBLt",
  "R_28TIxiW8JhoW9MU",
  "R_2CfDy8fcoRc3q0y",
  "R_2PaESZqXdiuSGuM",
  "R_2SqvNTDrUFYMxmi",
  "R_2WHe5OB0HJ4uMWB",
  "R_2YJa6cxNZ50mWl3",
  "R_2ck2LEAfYuLQx2D",
  "R_2eRM4n8TBtnML1S",
  "R_2xXFVuqwQcFVsab",
  "R_3Eb9jm4p4dM94gH",
  "R_3NIedMeGYDZxc9z",
  "R_3fE4BXgUmYK9DfM",
  "R_3fTzsd68dFIQis0",
  "R_3paobuuATXaw5vt",
  "R_72SsKhByBn28ZdD",
  "R_BPu3Np8KsQTiJLb",
  "R_RraYN2lCDg4D1YJ",
  "R_Ug2C9mbFtBSEKDn",
  "R_d0BVX7GtMB7rKdH",
  "R_qImWmeh2pUTXytr",
  "R_u7BeTAgDTRQ3ylr",
  "R_wLfDyy2Acjft4w9",
  "R_xydEhn9X9dSXgaJ",
  "R_yHG00KRrsEDtdGp",
  "R_1fZrFkzwEtvgbog",
  "R_1hEeBpCLDggEWRe",
  "R_2B36ZFoRzRS7dk8",
  "R_2VF5BZKahRUhjOe",
  "R_3CT4JQ8QMW3PwL6",
  "R_6VGzpPxis50rDCF",
  "R_e9voNduOBUQx1oR",
  "R_snc5y3nj69F6DDP",
  "R_Q5JA7C8zEM5qpgZ"
  
)

venmo_is_valid <- c(
  1,
  1,
  1,
  0,
  1,
  1,
  1,
  1,
  0,
  1,
  1,
  0,
  1,
  1,
  1,
  0,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  1,
  0,
  1,
  1,
  0,
  1,
  0,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  1,
  1,
  1,
  1,
  1,
  0,
  1,
  1,
  0,
  1,
  1,
  1,
  1,
  0,
  1
)

valid_venmo <- c(
  "ChristianLe9",
  "shado_bronmg",
  "anshisingh",
  NA,
  "adri_monti6",
  "JoshEllison-Puray",
  "Emilyannho",
  "Dhairya-Rastogi",
  NA,
  "terri-loew",
  "eerinmae",
  NA,
  "josephpark2",
  "brandysepulveda11",
  "Asiya2",
  NA,
  "Cmagni",
  "Liane-Barkhordar",
  "annievn19",
  "bsekhon",
  "david-s-oh",
  "randy-chen-12",
  "aaron.t10ng",
  "chasesgore",
  NA,
  "korina.harrison",
  NA,
  "KimmyDat",
  "setayesh-zakerani",
  NA,
  "cherbearr",
  NA,
  "Aashita-Nath",
  "Jonathan-Javier-4",
  "Sean-W134",
  "nailsbysstephh",
  "Nathan-Fang-2",
  "khalicel",
  "karamssodhi",
  "angiehvo",
  NA,
  "Calseo",
  "Dante-Hernandez-08",
  "Spenceryi96",
  "ninosbagpipe",
  "paulina-laba",
  "zoeludena",
  "miaming",
  "helen3579",
  NA,
  "luiza-gava",
  "Hope-Min",
  "alyson-k-yi",
  "Hoa-Phan-47",
  NA,
  "axblif"
)

valid_venmos <- tibble(id, valid_venmo, venmo_is_valid)

completed_payments <- completed_payments %>% 
  left_join(valid_venmos, by = "id")


#### CODE TO QUOTE  ####
payments_valid_venmos <- completed_payments %>% 
  filter(!is.na(valid_venmo))
  
paste(paste0("@", payments_valid_venmos[payments_valid_venmos$payments == 1, ]$valid_venmo), collapse = ", ")
paste(paste0("@", payments_valid_venmos[payments_valid_venmos$payments == 2, ]$valid_venmo), collapse = ", ")
paste(paste0("@", payments_valid_venmos[payments_valid_venmos$payments == 3, ]$valid_venmo), collapse = ", ")
paste(paste0("@", payments_valid_venmos[payments_valid_venmos$payments == 4, ]$valid_venmo), collapse = ", ")

#### DATASET FOR INDIVIDUALS WHO HAVE ALREADY BEEN PAID ####
id <- c(
  "R_1IK930Ot9t6VO5m",
  "R_1mwP5eJizPD9B9e",
  "R_1qeWA87EDYepcUE",
  "R_28ZHZuaisGQoJvZ",
  "R_2CDHGvnfOFU9x86",
  "R_2TEMWYHUJ7InEe5",
  "R_2aer9fJwMBGve2N",
  "R_2azSkWELCtZ9a32",
  "R_2b1dFHwcHgOKHzK",
  "R_2qg749CQ0mGcZmt",
  "R_3IPRivClHcR87eh",
  "R_3kqcM1zbTg5VAlk",
  "R_bC2llznaLZEDPu9",
  "R_sX0G0nLIJh8pXbj",
  "R_tDpxyqbSkg2NE9r",
  "R_vfuanAwGAdtz39L",
  "R_1CjxEtwB3RK4nP8",
  "R_1FCy8EJt7r2AECE",
  "R_1FPwpsLwkMUhq29",
  "R_1ML4naHAdmOcYjP",
  "R_1lcaRBXr83mDKfh",
  "R_1mC2KWW60onV84c",
  "R_27EvJfGVnWJBBLt",
  "R_28TIxiW8JhoW9MU",
  "R_2CfDy8fcoRc3q0y",
  "R_2PaESZqXdiuSGuM",
  "R_2SqvNTDrUFYMxmi",
  "R_2WHe5OB0HJ4uMWB",
  "R_2YJa6cxNZ50mWl3",
  "R_2ck2LEAfYuLQx2D",
  "R_2eRM4n8TBtnML1S",
  "R_2xXFVuqwQcFVsab",
  "R_3Eb9jm4p4dM94gH",
  "R_3NIedMeGYDZxc9z",
  "R_3fE4BXgUmYK9DfM",
  "R_3fTzsd68dFIQis0",
  "R_3paobuuATXaw5vt",
  "R_72SsKhByBn28ZdD",
  "R_BPu3Np8KsQTiJLb",
  "R_RraYN2lCDg4D1YJ",
  "R_Ug2C9mbFtBSEKDn",
  "R_d0BVX7GtMB7rKdH",
  "R_qImWmeh2pUTXytr",
  "R_u7BeTAgDTRQ3ylr",
  "R_wLfDyy2Acjft4w9",
  "R_xydEhn9X9dSXgaJ",
  "R_yHG00KRrsEDtdGp",
  "R_1fZrFkzwEtvgbog",
  "R_1hEeBpCLDggEWRe",
  "R_2B36ZFoRzRS7dk8",
  "R_2VF5BZKahRUhjOe",
  "R_3CT4JQ8QMW3PwL6",
  "R_6VGzpPxis50rDCF",
  "R_e9voNduOBUQx1oR",
  "R_snc5y3nj69F6DDP",
  "R_Q5JA7C8zEM5qpgZ"
)

subject_has_been_paid <- c(
  1,
  1,
  1,
  0,
  1,
  1,
  1,
  1,
  0,
  1,
  1,
  0,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  1,
  0,
  0,
  0,
  1,
  1,
  0,
  1,
  0,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  1,
  1,
  1,
  1,
  0,
  1
)

paid_subjects <- tibble(id, subject_has_been_paid)

write_csv(paid_subjects, "output/misc/receiver_payments/paid_subjects.csv")






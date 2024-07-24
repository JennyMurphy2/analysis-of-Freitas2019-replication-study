# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)
library(janitor)

set.seed(21)

# Load data

data <- read_csv("replication_data.csv") %>%
  clean_names()

# Prepare data ----------------

rep_data <- data %>%
  select(id, atp_kg_total, placebo_kg_total) %>%
  mutate(difference = atp_kg_total - placebo_kg_total) 

paired_data <- rep_data %>% 
  select(-c("difference")) %>%
  pivot_longer(cols = c("atp_kg_total", "placebo_kg_total"),
               names_to = "condition",
               values_to = "total_weight") 

## Calculate descriptives for data -------------------------------------

# Replication descriptives

summary_rep <- paired_data %>%
  group_by(condition) %>%
  summarise(count = n(),
            mean = mean(total_weight),
            sd = sd(total_weight)) %>%
  mutate(mean_diff = mean(rep_data$difference), 
         sd_diff = sd(rep_data$difference)
  )
summary_rep

## Resolving assumptions  ------------------------------------
### Checking distribution ---------------------------------------

ggplot(paired_data, aes(total_weight)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10) +
  facet_wrap(~ condition,
             labeller = label_both)

ggplot(paired_data, aes(condition, total_weight, color = condition)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

ggplot(paired_data, aes(condition, total_weight, color = condition)) +  
  geom_violin(fill = "light gray") +
  geom_boxplot(width = .07,
               fill = "white") +
  geom_jitter(position = position_jitter(0.21)) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 12,
               color = "black",
               size = 5) +
  theme_bw()

### Checking for outliers on difference score -----------------------------------

rep_data %>%
  identify_outliers(difference)

### Checking normality ----------------------------------------------------------
rep_data %>% shapiro_test(difference) 

# T test ---------------------------------------------------

paired_data$condition <- as.factor(paired_data$condition)

# R compares conditions alphabetically, I am reordering here to match the original study

paired_data$condition <- forcats::fct_relevel(paired_data$condition, "atp_kg_total", "placebo_kg_total")

results <- t.test(total_weight ~ condition, paired_data, 
                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
results

# Analyse the replication ------

## Calculate Original ES --------

#Original descriptives

orig_values <- data.frame(
  ori_pval = 0.005,
  N = 11,
  atp_mean = 4967.4,
  atp_sd = 1497.9,
  placebo_mean = 3995.7,
  placebo_sd = 1137.8
)

# Estimating the t-value

quantile = 1 - orig_values$ori_pval/2 # for two-tailed

ori_tval <- qt(quantile, df = 10)

# Calculating effect size

ori_dz <- d.dep.t.diff.t(t = ori_tval, n = 11, a = 0.05)
ori_dz

## Calculate replication ES ------

rep_dz <- d.dep.t.diff(mdiff = summary_rep$mean_diff[1], sddiff = summary_rep$sd_diff[1], 
                       n = summary_rep$count[1], a = 0.05)
rep_dz

## Z-test  --------

rep_test <- compare_smd(
  smd1 = ori_dz$d,
  n1 = orig_values$N,
  smd2 = rep_dz$d,
  n2 = summary_rep$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test

## Z-test reported --------

rep_test <- compare_smd(
  smd1 = 0.73,
  n1 = orig_values$N,
  smd2 = rep_dz$d,
  n2 = summary_rep$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test

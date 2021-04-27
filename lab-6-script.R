# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")
crabs <- read_csv("chap15q27FiddlerCrabFans.csv")


# put data in tidy format ------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()


# Question A t-test -------------------------------------------------------
 

ttest_results <- t.test(formula = species ~ location, data = fish_long)
ttest_results

# Question B difference in means -----------------------------------------
Mean_difference = 16.41667-14.58333
Mean_difference

# 95% confidence interval of Mean length difference

Upper_limit = -4.587031

Lower_limit = 8.253697

# Question C histograms --------------------------------------------------

fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 8, 
    alpha = 0.5, 
    position = "identity"
  ) +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal()


# Question D distributions ------------------------------------------------

  crabs %>% 
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType), 
    bins = 15, 
    alpha = 0.5, 
    position = "identity",
    na.rm= TRUE
  ) +
  scale_fill_manual(values = c("darkorange","cyan4","blue","red")) +
  theme_minimal()


# Question E  -------------------------------------------------------------

aov_bodyTemperature_crabType <-
  aov(bodyTemperature ~ crabType, data = crabs)
aov_bodyTemperature_crabType
summary(aov_bodyTemperature_crabType)


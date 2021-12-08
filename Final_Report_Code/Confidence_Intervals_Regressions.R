#### Elise Rust
#### ANLY 511
#### Final Project - Confidence Intervals
## December 2021

# Load in necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

######## 1. Colleges
# Load in data
colleges_full <- read.csv("~/Desktop/Papers/Georgetown/Fall 2021/ANLY 511/Final Project/colleges_cleaned2.csv")

# Examine
head(colleges_full)
str(colleges_full)

###### EDA
boxplot(colleges_full$cases, main = "Colleges Total Cases")

boxplot(colleges_full$case_rate,
        main = "Colleges Normalized Case Rate")

### Create Size Subgroups
summary(colleges_full$enroll2020)
small_schools <- colleges_full$case_rate[colleges_full$enroll2020 <= 18000]
big_schools <- colleges_full$case_rate[colleges_full$enroll2020 > 18000]

boxplot(small_schools, big_schools,
        main = "College COVID-19 Case Rates For Small vs. Large Schools",
        names = c("Small Schools", "Large Schools"))

### Create Mask Wearing Subgroups
summary(colleges_full$ALWAYS)
high_mask_usage <- colleges_full$case_rate[colleges_full$ALWAYS > 0.5]
low_mask_usage <- colleges_full$case_rate[colleges_full$ALWAYS <= 0.5]
# Remove NAs
high_mask_usage<-high_mask_usage[!is.na(high_mask_usage)]
low_mask_usage<-low_mask_usage[!is.na(low_mask_usage)]


boxplot(low_mask_usage, high_mask_usage,
        main = "College COVID-19 Case Rates For Schools in Counties with Low vs. High Mask Usage",
        names = c("Low Mask Usage", "High Mask Usage"))


### Create Voting Subgroups
red_counties <- colleges_full$case_rate[colleges_full$winner == "Democrat"]
blue_counties <- colleges_full$case_rate[colleges_full$winner == "Republican"]
# Remove NAs
red_counties<-red_counties[!is.na(red_counties)]
blue_counties<-blue_counties[!is.na(blue_counties)]



boxplot(red_counties, blue_counties,
        main = "College COVID-19 Case Rates For Schools Based on 2020 Election Results",
        names = c("Republican-Voting Counties", "Democrat-Voting Counties"))



######### Confidence Intervals
# Difference between small vs. large schools
school_size_bootstrap <- rep(NA,100000) 

for (j in 1:100000) {
  boot.large <- mean(sample(big_schools, length(big_schools), replace = T)) 
  boot.small <- mean(sample(small_schools, length(small_schools), replace = T)) 
  school_size_bootstrap[j] <- boot.large - boot.small #the difference
}

head(school_size_bootstrap)
mean(school_size_bootstrap) #bootstrap mean difference

CI <- quantile(school_size_bootstrap, c(.05, .95))
CI



# Difference between schools in counties that wear masks and those that don't 
mask_bootstrap <- rep(NA,100000) 

for (j in 1:100000) {
  mask_high <- mean(sample(high_mask_usage, length(high_mask_usage), replace = T)) 
  mask_low <- mean(sample(low_mask_usage, length(low_mask_usage), replace = T)) 
  mask_bootstrap[j] <- mask_high - mask_low #the difference
}

head(mask_bootstrap)
mean(mask_bootstrap) #bootstrap mean difference

CI_mask <- quantile(mask_bootstrap, c(.05, .95))
CI_mask


# Difference between schools in counties that wear masks and those that don't 
election_bootstrap <- rep(NA,100000) 

for (j in 1:100000) {
  election_red <- mean(sample(red_counties, length(red_counties), replace = T)) 
  election_blue <- mean(sample(blue_counties, length(blue_counties), replace = T)) 
  election_bootstrap[j] <- election_red - election_blue #the difference
}

head(election_bootstrap)
mean(election_bootstrap) #bootstrap mean difference

CI_election <- quantile(election_bootstrap, c(.05, .95))
CI_election




#########################
##### Prison Data

# Load in data
systems <- read.csv("~/Desktop/Papers/Georgetown/Fall 2021/ANLY 511/Final Project/systems_cleaned.csv")
facilities <- read.csv("~/Desktop/Papers/Georgetown/Fall 2021/ANLY 511/Final Project/facilities_edited.csv")

head(systems)
head(facilities)

str(systems)
str(facilities)
facilities$facility_type = as.factor(facilities$facility_type)
facilities <- facilities %>%
  mutate(case_rate = total_inmate_cases/latest_inmate_population) %>%
  filter(case_rate < 1)

### Confidence Intervals
levels(facilities$facility_type)

# Break up data into subgroups 
state_prisons <- facilities$case_rate[facilities$facility_type == c("State halfway house", "State work camp",
                                               "State juvenile detention", "State prison", "State facility",
                                               "State rehabilitation center")]
federal_prisons <- facilities$case_rate[facilities$facility_type == c("Detention center", "Juvenile detention at jail",
                                                 "Federal halfway house", "Low-security work release", "Federal prison",
                                                 "Reservation jail", "Jail")]
state_prisons<-state_prisons[!is.na(state_prisons)]
federal_prisons<-federal_prisons[!is.na(federal_prisons)]



small_prisons <- facilities$case_rate[facilities$latest_inmate_population < 580]
big_prisons <- facilities$case_rate[facilities$latest_inmate_population >= 580]
# Remove NAs
small_prisons<-small_prisons[!is.na(small_prisons)]
big_prisons<-big_prisons[!is.na(big_prisons)]



red_prisons <- facilities$case_rate[facilities$political_party == "Republican"]
blue_prisons <- facilities$case_rate[facilities$political_party == "Democrat"]
red_prisons<-red_prisons[!is.na(red_prisons)]
blue_prisons<-blue_prisons[!is.na(blue_prisons)]

red_prisons_systems <- systems$case_rate[systems$political_party == "R"]
blue_prisons_systems <- systems$case_rate[systems$political_party == "D"]

# 1. Mean case numbers between different types of prisons
prison_type_bootstrap <- rep(NA,100000) 

for (j in 1:100000) {
  boot.state <- mean(sample(state_prisons, length(state_prisons), replace = T)) 
  boot.federal <- mean(sample(federal_prisons, length(federal_prisons), replace = T)) 
  prison_type_bootstrap[j] <- boot.state - boot.federal #the difference
}

head(prison_type_bootstrap)
mean(prison_type_bootstrap) #bootstrap mean difference

CI_prison1 <- quantile(prison_type_bootstrap, c(.05, .95))
CI_prison1

# 2. Mean case numbers between different sized prisons
prison_type_bootstrap2 <- rep(NA,100000) 

for (j in 1:100000) {
  boot.small <- mean(sample(small_prisons, length(small_prisons), replace = T)) 
  boot.big <- mean(sample(big_prisons, length(big_prisons), replace = T)) 
  prison_type_bootstrap2[j] <- boot.big - boot.small #the difference
}

head(prison_type_bootstrap2)
mean(prison_type_bootstrap2) #bootstrap mean difference

CI_prison2 <- quantile(prison_type_bootstrap2, c(.05, .95))
CI_prison2

# 3. Mean case numbers between prisons in Red vs. Blue states - facilities.csv
prison_type_bootstrap3 <- rep(NA,100000) 

for (j in 1:100000) {
  boot.red <- mean(sample(red_prisons, length(red_prisons), replace = T)) 
  boot.blue <- mean(sample(blue_prisons, length(blue_prisons), replace = T)) 
  prison_type_bootstrap3[j] <- boot.red - boot.blue #the difference
}

head(prison_type_bootstrap3)
mean(prison_type_bootstrap3) #bootstrap mean difference

CI_prison3 <- quantile(prison_type_bootstrap3, c(.05, .95))
CI_prison3

# 4. Mean case numbers between prisons in Red vs. Blue states - systems.csv
prison_type_bootstrap4 <- rep(NA,100000) 

for (j in 1:100000) {
  boot.red.systems <- mean(sample(red_prisons_systems, length(red_prisons_systems), replace = T)) 
  boot.blue.systems <- mean(sample(blue_prisons_systems, length(blue_prisons_systems), replace = T)) 
  prison_type_bootstrap4[j] <- boot.red.systems - boot.blue.systems#the difference
}

head(prison_type_bootstrap4)
mean(prison_type_bootstrap4) #bootstrap mean difference

CI_prison4 <- quantile(prison_type_bootstrap4, c(.05, .95))
CI_prison4



##########################
## Visualize

school_results <- data.frame(school = c(1, 2, 3),
                   zero    = c(0, 0, 0),
                   center   = c(0.005595, -0.007355, -0.034185),
                   CI       = c(0.010125, 0.010615, 0.025195))
print(school_results)

CI_schools <- ggplot(school_results, aes(school, center)) +
  geom_point(color = "red") +
  geom_point(aes(x = school, y = zero)) + 
  geom_errorbar(aes(ymin = center - CI, ymax = center + CI)) +
  labs(x = "",
       y = "Difference of Mean COVID-19 Case Rates",
       title = "Confidence Intervals of Diff. of Means across School Subgroups") +
  theme_classic() 

print(CI_schools)


# Prison data
prison_results <- data.frame(tests = c(1, 2, 3),
                             zero    = c(0, 0, 0),
                             center   = c(0.005595, 0.05542, 0.04157),
                             CI       = c(-0.017795, 0.02133, 0.02111))
print(school_results)

CI_prisons <- ggplot(prison_results, aes(tests, center)) +
  geom_point(color = "red") +
  geom_point(aes(x = tests, y = zero)) + 
  geom_errorbar(aes(ymin = center - CI, ymax = center + CI)) +
  labs(x = "",
       y = "Difference of Mean COVID-19 Case Rates",
       title = "Confidence Intervals of Diff. of Means across Prison Subgroups") +
  theme_classic() 

print(CI_prisons)


#################################
########## Regressions
# Colleges Data

# Correlations Matrix of quantitative variables
colleges_small <- colleges_full %>%
select(case_rate, enroll2020, NEVER, RARELY, SOMETIMES, FREQUENTLY, ALWAYS) %>%
drop_na()
cor(colleges_small)

# Regression of colleges
regression_colleges <- lm(case_rate ~ state+rank2020+enroll2020+NEVER+winner, data = colleges_full)
stats_colleges <- summary(regression_colleges)
print(stats_colleges)


### Prison Data
# Correlations Matrix of quantitative variables
prison_small <- facilities %>%
  select(case_rate, latest_inmate_population, total_inmate_cases, total_inmate_deaths, county_population) %>%
  drop_na()
cor(prison_small)

# Regression of colleges
regression_prisons <- lm(case_rate ~ facility_type+state+latest_inmate_population+political_party+county_population, data = facilities)
stats_prisons <- summary(regression_prisons)
print(stats_prisons)

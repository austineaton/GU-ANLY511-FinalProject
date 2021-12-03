# title: "Colleges_cleaning"
# author: "Elise Rust"
# date: "12/1/2021"

# Load in packages
library(tidyverse)
library(ggplot2)


# Load in data
colleges <- read.csv("~/Desktop/Papers/Georgetown/Fall 2021/ANLY 511/Final Project/colleges.csv")
attendance <- read.csv("~/Desktop/Papers/Georgetown/Fall 2021/ANLY 511/Final Project/college_attendance.csv", na.strings=c("","NA"))
mask_use <- read.csv("~/Desktop/Papers/Georgetown/Fall 2021/ANLY 511/Final Project/MaskUse.csv")

head(colleges)
head(attendance)
head(mask_use)

# Basic summary statistics and structure
summary(colleges)
summary(attendance)
summary(mask_use)

## # 1. Fix column names
names(attendance) <- c("rank2020", "rank2015", "college", "enroll2020", "enroll2015")

## 2. Remove unnecessary columns
# colleges --> remove cases2021 (only interested in total cases) and notes (extraneous), date (all on 5/26/2021)
colleges <- colleges %>%
  select(-c(cases_2021, notes, date))

# mask_use --> remove fips code (not used for join), abr (not useful)
mask_use <- mask_use %>%
  select(-c(fips, abbr))

## 4. Missing/incorrect values
# Colleges dataframe
sum(is.na(colleges$cases) == TRUE)  
sum(is.na(colleges$college) == TRUE)
colleges$ipeds_id[colleges$ipeds_id=="laccd"] <- NA
# No NULL values in colleges dataframe

# Attendance dataframe
sum(is.na(attendance)) # 31 NAs
attendance$rank2015[attendance$rank2015=="N/A"] <- NA
attendance$enroll2015[attendance$enroll2015=="Not Available"] <- NA
attendance$enroll2015[attendance$enroll2015=="No Change"] <- NA

sum(is.na(attendance)) # 20 NAs

# Mask Use dataframe
sum(is.na(mask_use))

# FIX NAs
sum(is.na(attendance$enroll2020))
# Southeastern Louisiana had 2020 enrollment of 13,490
attendance$enroll2020[attendance$college == "Southeastern Louisiana University"] = 13490
# Drop entirely blank rows at bottom
attendance <- attendance %>%
  drop_na(rank2020)

# Remove extra whitespace so type conversion can happen
attendance$enroll2020 = str_trim(attendance$enroll2020, side = c("both"))
attendance$enroll2015 = str_trim(attendance$enroll2015, side = c("both"))
attendance$rank2020 = str_trim(attendance$rank2020, side = c("both"))
attendance$rank2015 = str_trim(attendance$rank2015, side = c("both"))

# Remove commas from numbers
attendance$enroll2020 <- as.numeric(gsub(",","",attendance$enroll2020))
attendance$enroll2015 <- as.numeric(gsub(",","", attendance$enroll2015))

## 3. Correct datatypes before median imputation
str(colleges)
str(attendance)

colleges <- colleges %>%
  mutate(ipeds_id = as.numeric(colleges$ipeds_id)) %>%
  mutate(cases = as.numeric(cases))

attendance <- attendance %>%
  mutate(rank2020 = as.numeric(rank2020)) %>%
  mutate(rank2015 = as.numeric(rank2015))


# Median imputation for enroll_2015
attendance$enroll2015[is.na(attendance$enroll2015)] <- median(attendance$enroll2015, na.rm = TRUE)

sum(is.na(attendance$enroll2015))



##################
# Join
colleges_full <- inner_join(colleges, attendance, by = "college" )
colleges_full <- left_join(colleges_full, mask_use, by = c("county", "state"))
head(colleges_full)
str(colleges_full)


## Calculate "rate" parameter and percent change in enrollment
colleges_full <- colleges_full %>%
  mutate(perc_enroll_change = (enroll2020 - enroll2015)/enroll2015) %>%
  mutate(case_rate = cases/enroll2020)




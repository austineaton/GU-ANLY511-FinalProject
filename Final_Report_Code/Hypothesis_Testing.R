library(tidyverse)

#Uploading data, cleaning county census data population estimates & mask data
#-------------------------------------------------------------------------------------
countyDeaths <- read.csv('/Users/christopherdelaney/Desktop/GU-ANLY511-FinalProject-main/live/us-counties.csv')#deaths by county dataset
countyPopulations <- read.csv('/Users/christopherdelaney/Desktop/PopulationEstimatesCSV.csv')#populations by county dataset
masks <- read.csv('/Users/christopherdelaney/Desktop/GU-ANLY511-FinalProject-main/mask-use/mask-use-by-county.csv')#mask usage dataset
names <- countyPopulations[1, ]
names[1] <- "fips" #Changing name for merging
names[1,8] <- "Population"
countyPopulations <- countyPopulations[-c(1:2), ]
names(countyPopulations) <- names
countyPopulations$fips <- as.numeric(countyPopulations$fips) #converting fips codes to numeric for converting
maskNames <- names(masks)
maskNames[1] <- "fips" #Changing name for merging
names(masks) <- maskNames

#Merging based on fips code, selecting key variables, cleaning resulting dataset
#-------------------------------------------------------------------------------------
df <- merge(countyPopulations, countyDeaths, by = "fips" ) %>% 
  select ('fips', 'state', 'Area name', 'Population', 'confirmed_cases', 'confirmed_deaths') 
df$Population <- gsub("\\,", "", df$Population) #removing commas from population data
df$Population <- as.numeric(df$Population)
df$confirmed_deaths <- as.numeric(df$confirmed_deaths)
df$confirmed_cases <- as.numeric(df$confirmed_cases)
df <- df %>% mutate(case_rate = confirmed_cases/Population, death_rate = confirmed_deaths/Population) #calcualting case rate
newdf <- merge(df, masks, by = "fips")
noMaskCounties <- newdf %>% arrange(desc(NEVER, RARELY)) %>% drop_na() %>% slice(1:30) %>% mutate(label = "Low Mask Usage") #Selecting 30 lowest mask using counties with sufficient data
maskCounties <- newdf %>% arrange(desc(ALWAYS, FREQUENTLY)) %>% drop_na() %>% slice(1:30) %>% mutate(label = "High Mask Usage")#Selecting 30 highest mask using counties with sufficient data
finalDF <- rbind(noMaskCounties, maskCounties)
View(finalDF)

#Creating visualizations
#------------------------------------------------------------------------------------
finalDF %>% ggplot(aes(x = label, y = death_rate, fill = label)) + geom_boxplot() +
  labs(title = "Death Rates by Counties with Differing Mask Usage", x = "Mask Usage", y = "Death Rate") +
  scale_y_continuous(labels = scales::percent)
finalDF %>% ggplot(aes(x = label, y = case_rate, fill = label)) + geom_boxplot() +
  labs(title = "Case Rates by Counties with Differing Mask Usage", x = "Mask Usage", y = "Case Rate") +
  scale_y_continuous(labels = scales::percent)


#T-Test
#-------------------------------------------------------------------------------------
freqMaskDR <- subset(finalDF, select=death_rate, subset=label=="High Mask Usage", drop=T)
rarelyMaskDR <- subset(finalDF, select=death_rate, subset=label=="Low Mask Usage", drop=T)
t.test(rarelyMaskDR, freqMaskDR, alt="greater")

freqMaskCR <- subset(finalDF, select=case_rate, subset=label=="High Mask Usage", drop=T)
rarelyMaskCR <- subset(finalDF, select=case_rate, subset=label=="Low Mask Usage", drop=T)
t.test(rarelyMaskCR, freqMaskCR, alt="greater")


#CHI-SQR Test
#-------------------------------------------------------------------------------------
#DEATH RATE:
chisqDF <- finalDF %>% select(label, death_rate)

diffMean = function(chisqDF) { 
  agg = aggregate(death_rate ~ label, data = chisqDF, FUN = mean) 
  return(agg$death_rate[1] - agg$death_rate[2]) #xbar_c - xbar_t 
}
myPerm <- function(){
  dfCopy = chisqDF
  dfCopy$death_rate <- dfCopy$death_rate[sample(60,60,replace=F)]
  diffMean(dfCopy)
}
stat = diffMean(chisqDF)
test = replicate(1000, myPerm())
mean(test < stat) #p-value

#Frequency distribution visualization
hist(test, main = "Null Distribution, Difference of Means", prob = T, col = "cadetblue")
abline(v = stat,col = 2, lwd = 2)

#----

#CASE RATE:
chisqDF <- finalDF %>% select(label, case_rate)

diffMean = function(chisqDF) { 
  agg = aggregate(case_rate ~ label, data = chisqDF, FUN = mean) 
  return(agg$case_rate[1] - agg$case_rate[2]) #xbar_c - xbar_t 
}
myPerm <- function(){
  dfCopy = chisqDF
  dfCopy$case_rate <- dfCopy$case_rate[sample(60,60,replace=F)]
  diffMean(dfCopy)
}
stat = diffMean(chisqDF)
test = replicate(1000, myPerm())
mean(test < stat) #p-value

#Frequency distribution visualization
range <- c(-0.04, 0.03)
hist(test, main = "Null Distribution, Difference of Means", prob = T, col = "cadetblue", xlim = range)
abline(v = stat,col = 2, lwd = 2)



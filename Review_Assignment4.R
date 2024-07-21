#Assignment 4
#Neha Kodali
# REVIEW BY CRYSTAL LEE - COMMENTS MARKED BY "CL"

library(lubridate)
library(tidyverse)
install.packages("tidyverse") # SUGGESTION: Make sure to install packages before loading "library()" them - CL

# Read the data into a dataframe
# SUGGESTION: Make sure your ufo_subset.csv file is loaded into your repo - CL
## ufod <- read.csv("/Users/nehakodali/Desktop/Mbiotech/Coding_in_R/ufo_subset.csv")
ufod <- read.csv("ufo_subset.csv")

# issues
str(ufod)
summary(ufod)

# country, shape and duration seconds columns
table(ufod$country)
table(ufod$shape)
summary(ufod$duration.seconds)

# missing values
ufod <- ufod %>%
  mutate(
    # COMMENT: Good! But upon inspecting the country and shape column, we see there are no NAs,
    # instead, there are a bunch of blank entries. Try replacing those blank entries
    # with "NA" or "unknown" or whatever you see fit - CL
    country = ifelse(is.na(country), "Unknown", country), 
    shape = ifelse(is.na(shape), "Unknown", shape),
  
  )

# Additional things to consider cleaning - CL
## Since you will eventually compare two different dates per entry, it may be good
# to separate the date and time in datetime
## duration.hours.min lacks consistency in its entries and is in characters, it may be a good idea
# to create two columns for both duration hours and duration mins. You can utilize
# duration.seconds to do that
## Some of the city entries are inconsistent, where there is a bracket containing 
# additional info after the city name. This can also be redundant since that info 
# may already be in another column. What can you do with this info inside the brackets?
## The date_posted entries are formatted differently from datetime. Is there a way to
# make the formatting more consistent across the data frame?
## Always check for duplicates!


#Remove outliers
# COMMENT: Why did you decide on this number as the threshold? - CL
# For a more objective method, you can try utilizing quantiles (think of boxplots) when
# identifying outliers - CL
ufod <- ufod[ufod$duration.seconds < 604800,]

# Remove hoax appearances
# SUGGESTION: Keep in mind some comments might say "is not a hoax", so think of ways
# to take those comments into account - CL
ufod <- ufod %>% 
  filter(!str_detect(tolower(comments), "hoax"))

# report_delay column
ufod$datetime <- ymd_hm(ufod$datetime)

#date_posted column

ufod$date_posted<- dmy(ufod$date_posted, tz = "America/Toronto")
  

#report delay column
# COMMENT: Try to find a way to calculate report_delay in days. You can try 
# using "difftime()" from base R or "interval()"- CL
ufod$report_delay <- ufod$date_posted - ufod$datetime
  
# Removing rows where appearance was reported before it occurred
# Well done! - CL
ufod <- ufod %>% 
  filter(report_delay >= 0)

# Country - average report delay
# SUGGESTION: Consider labeling the first blank variable under country. Is it NA? Is it unknown? - CL
avg_report_delay <- ufod %>% 
  group_by(country) %>% 
  summarise(avg_report_delay = mean(report_delay, na.rm = TRUE))


# Display the average report delay per country
print(avg_report_delay)

# SUGGESTION: Explain why you chose the log of duration.seconds - CL
# SUGGESTION: Re-label the x-axis - CL
# A histogram of duration seconds
hist(log(ufod$duration.seconds))

#### CODE REVIEW SUMMARY - CL
# FUNCTIONALITY - The code runs very well! Good job utilizing the packages for your cleaning and analysis.

# STRUCTURE/LOGIC - Good structure and logic! Started with loading packages, inspecting structure 
# of the data, moved on to cleaning, and then analysis
# I would just make sure the installation of packages is always before the loading of them

# DESIGN - Consider ways to deal with blank entries and make sure there are NAs before
# dealing with NAs. Check the data frame to see if there's any inconsistencies with 
# entries and formatting and try different ways to deal with those.

# READABILITY - Well-organized code and comments generally explain the code. I suggest including
# more details into why you decided on certain things, such as using the log scale in your histogram
# or how you decided on the outliers threshold. 

   
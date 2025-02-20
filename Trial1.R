install.packages("tidyverse")
library("tidyverse")

data <- read.csv("Kelp_morphology_rawdata.csv")

#clean up headings
library(dplyr)
library(stringr)

# Clean column names: Convert to lowercase and replace spaces & special characters
data <- data %>%
  rename_with(~ str_replace_all(__ "[ ()]", "_") %>% tolower())

colnames(data)  # Check cleaned names

#Removing missing data 
clean_data <- na.omit(data)

#change variables to factors 
clean_data <- clean_data %>%
  mutate(
    sample_site = as.factor(sample_site),
    harvested_non_harvested = as.factor(harvested_non_harvested)
  )
  
#save clean data as csv
write.csv(clean_data, "tidydata.csv", row.names = FALSE)

# Read the dataset
data <- read_csv("tidydata.csv")

# Convert categorical variables to factors
data <- data %>%
  mutate(
    sample_site = as.factor(sample_site),  
    harvested_non_harvested = as.factor(harvested_non_harvested)
  )

### ANOVA: Effect of Sample Site on Frond Length**
anova1 <- aov(frond_length__cm_ ~ sample_site, data = data)
summary(anova1)

### ANOVA: Effect of Sample Site on Holdfast Volume**
anova2 <- aov(holdfast_volume__cm3_ ~ sample_site, data = data)
summary(anova2)

### t-Test: Effect of Harvesting on Frond Mass**
t_test <- t.test(frond_mass__g_ ~ harvested_non_harvested, data = data)
t_test

# Load necessary libraries
library(ggplot2)
library(tidyverse)

# Read the dataset
data <- read_csv("tidydata.csv")

# Convert categorical variables to factors
data <- data %>%
  mutate(
    sample_site = as.factor(sample_site),  
    harvested_non_harvested = as.factor(harvested_non_harvested)
  )

###Boxplot: Frond Length by Sample Site
ggplot(data, aes(x = sample_site, y = frond_length__cm_, fill = sample_site)) +
  geom_boxplot() +
  labs(title = "Frond Length Across Sample Sites",
       x = "Sample Site",
       y = "Frond Length (cm)") +
  theme_minimal()

###Boxplot: Frond Mass by Harvesting Status
ggplot(data, aes(x = harvested_non_harvested, y = frond_mass__g_, fill = harvested_non_harvested)) +
  geom_boxplot() +
  labs(title = "Frond Mass by Harvesting Status",
       x = "Harvesting Status",
       y = "Frond Mass (g)") +
  theme_minimal()

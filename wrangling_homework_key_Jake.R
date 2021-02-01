#PSYC 259 Homework 2 - Data Wrangling
#This assignment should be completed in RStudioCloud
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
library(vroom)
library(janitor)

#Load the 4 data files and fix the y scaling
files <- c("data_raw/101.txt", "data_raw/102.txt", "data_raw/103.txt", "data_raw/104.txt")
ds <- vroom(files, id = "file", delim = " ", skip = 6) 
ds <- ds %>% mutate(porY = porY * 1.33) # scaling x and y to the same coordinates
ds <- ds %>% separate(file, into = c(NA,NA,"id",NA))

### Question 1 ---------- 

#Check what kind of variable id is. 
#Then, convert it to a numeric

#ANSWER
typeof(ds$id)
ds <-  ds %>% mutate(id = as.numeric(id)) 

### Question 2 ---------- 

#Before we do anything else, let's make the variable names easier to type
#Rename sceneQTtime(d:h:m:s.tv/ts) and porQTtime(d:h:m:s.tv/ts) to scene_time and por_time
#Hint: you will need to use backticks around the messy, original names
#Then, use make_clean_names from the janitor package to convert everything to snake_case

#ANSWER
ds <- ds %>% rename(scene_time = `sceneQTtime(d:h:m:s.tv/ts)`, 
                    por_time = `porQTtime(d:h:m:s.tv/ts)`)
ds <- ds %>% rename_with(make_clean_names)

### Question 3 ----------

#There are 4 rows with missing values for record_scene_count
#Use filter to remove them from the dataset

# I noticed a fair amount of people did...
test<-ds %>% filter(record_frame_count > 0)

# this does work since there are no values below 0 but I think it's generally safer to explicitly call to remove NAs
ds <- ds %>% filter(!is.na(record_frame_count))

### JAKE
# drop_na could work too
ds <- ds %>% drop_na(record_frame_count)

### Question 4 ----------

#record_frame_counts is a measure of time (30 frames = 1 second).
#The first 30 seconds (900 frames) should be marked as "calibration" time, and the remaining time
#should be referred to as "test" time. Add a new factor called "period" to mark calibration vs test time
#use fct_count to check that the correct # of frames were counted as calibration 
#4 participants x 900 = 3600 calibration frames

#ANSWER
ds <- ds %>% mutate(
  period = ifelse(record_frame_count <= 900, "calibration","test"),
  period = factor(period))

ds <- ds %>% mutate(
  period = as.factor(ifelse(record_frame_count <= 900, "calibration","test")))

fct_count(ds$period)

### Question 5 ----------

#por_x and por_y values should be between 0 and 640
#set por_x and por_y values outside that range to be NA 
#I've given you an "out_of_bounds" function to make this easier, 
#but you can do it without it if you prefer

out_of_bounds <- function(x) {ifelse(x < 0 | x > 640, NA, x)}

#ANSWER
ds <- ds %>% mutate(across(por_x:por_y, out_of_bounds))

### Question 6 ----------

#scene_frame_count, corneal_ref_x, corneal_ref_y, diameter_h, and diameter_w are variables we don't need
#use 'select' save the dataset without those columns

#ANSWER
# keep everything but these
ds <- ds %>% select(-scene_frame_count) %>% select(-(corneal_ref_x:diameter_h))

# another way is keep only these

### Question 7 ----------

### Use summarize to find the mean of avg_fps for each participant in each period

ds %>% group_by(id, period) %>% summarize(avg_fps = mean(avg_fps, na.rm = T))

### Question 8 ---------- 

###Find the median, sd, min, and max for por_x and por_y for each participant during test only
###Save the summary to a tibble named results
###Hint: it might be easier to handle NA's before summarizing

#ANSWER
fx <- list(median = median, sd = sd, min = min, max = max)
results <- 
  ds %>% filter(!is.na(por_x) & !is.na(por_y)) %>% 
  filter(period == "test") %>% 
  group_by(id) %>% 
  summarize(across(por_x:por_y, fx))

## JAKE example

ds %>% drop_na(por_x:por_y) %>% group_by(id) %>% filter(period == "test") %>% 
  summarise(mean = across(por_x:por_y, mean),
            median = across(por_x:por_y, median),
            sd  = across(por_x:por_y, sd),
            min = across(por_x:por_y, min),
            max = across(por_x:por_y, max)
  )

## ANTONIO example

results <- ds %>% 
  group_by(id, period) %>% filter(period == "test") %>%
  summarise(xmedian = median(por_x, na.rm = TRUE), xsd = sd(por_x, na.rm = TRUE),
            xmin = min (por_x, na.rm = TRUE), xmax = max (por_x, na.rm = TRUE),
            ymedian = median(por_y, na.rm = TRUE), ysd = sd(por_y, na.rm = TRUE),
            ymin = min (por_y, na.rm = TRUE), ymax = max (por_y, na.rm = TRUE))

## DEJA example

results <- ds %>% filter(period == "test") %>% 
  group_by(id) %>% 
  summarise(across(c("por_x", "por_y"), list(median = median, sd = sd, min = min, max = max)))

### Question 9 ---------

#Select just the summary statistics for the y values and id
#and sort the values by median por_y value (ascending)
#Save this to results_y

#Answer
results_y <- results %>% select(id, contains("_y_")) %>% arrange(por_y_median)

### Question 10 --------

#Extract the value (not a tibble) of the smallest por_y_median value
#Bonus: look up the dplyr "slice" function to help (but there are plenty of other good ways to do it)

#ANSWER (a few options)
results_y %>% slice_head() %>% pull(por_y_median)
min(results$por_y_median)

  
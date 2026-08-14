# Process & clean the raw data
library(dplyr)

### dissection data (cb count under miscroscope after bee dissection)
# load data
cb_count <- read.csv("../data/dissection_data.csv")
str(cb_count)
# rename columns
colnames(cb_count)[c(2,3,15,16)] <- c("sugar_conc", "group", "meanCBcount", "CBLoadPerB")
# Transform multiple specific columns to factors
cb_count <- cb_count %>%
  mutate(across(c(date_d_m, sugar_conc, group, box, sex, PBS_ul), as.factor))
# remove columns of raw grid counts
cb_count <- cb_count %>% select(-c(grid1_CBcount, grid2, grid3, grid4, grid5))
str(cb_count)

### experimental data
# filter to leave group 4~7 only. the prior groups were pilot study with slightly different experimental settings
experiment_data <- cb_count %>%
  filter(group %in% c("4", "5", "6", "7"))
# drop extra factor levels
experiment_data <- droplevels(experiment_data)
str(experiment_data)    # PBS_ul will have 2 levels because one mistakely added 600ul, all others used 300ul
# add colony column (group 4, 6 = colony 1; group 5, 7 = colony 2)
experiment_data$colony <- ifelse(experiment_data$group %in% c("4", "6"), "1", "2")

### end day data (live dissection, ensure the same development time)
# end day for groups: 4(24-May), 5(28-May), 6(8-Jun), 7(9-Jun)
# do each group seperately
group4end <- cb_count %>%
  filter(group == "4" & date_d_m == "24-5")
group5end <- cb_count %>%
  filter(group == "5" & date_d_m == "28-5")
group6end <- cb_count %>%
  filter(group == "6" & date_d_m == "8-6")
group7end <- cb_count %>%
  filter(group == "7" & date_d_m == "9-6")

# combine all groups
EndDay_data <- rbind(group4end, group5end, group6end, group7end)
EndDay_data <- droplevels(EndDay_data)
str(EndDay_data)

# calculate standardized cb concentration, unit = cells / microliter (standardise the 600ul -> multiply it by 2)
# calculation equation comes from KOVA manual
EndDay_data$conc <- (EndDay_data$meanCBcount)*90*(EndDay_data$dilution)
# check the PBS volume (!= 300ul due to an experimental mistake)
PBS_abnorm <- EndDay_data %>%
  filter(PBS_ul != 300)
str(PBS_abnorm)
# standardize the 600ul as 300ul PBS for: mean grid coun, cb load per bee, and cb concentration
EndDay_data$meanCBcount[EndDay_data$PBS_ul == 600] <- EndDay_data$meanCBcount[EndDay_data$PBS_ul == 600]*2
EndDay_data$CBLoadPerB[EndDay_data$PBS_ul == 600] <- EndDay_data$CBLoadPerB[EndDay_data$PBS_ul == 600]*2
EndDay_data$conc[EndDay_data$PBS_ul == 600] <- EndDay_data$conc[EndDay_data$PBS_ul == 600]*2
# remove PBS volume column
EndDay_data <- EndDay_data %>% select(-c(PBS_ul))
# add colony column (group 4, 6 = colony 1; group 5, 7 = colony 2)
EndDay_data$colony <- ifelse(EndDay_data$group %in% c("4", "6"), "1", "2")
str(EndDay_data)

# save the processed data
write.csv(experiment_data, "../data/ExDissection.csv", row.names = FALSE)
write.csv(EndDay_data, "../data/EndDayDissection.csv", row.names = FALSE)

# extract survival data from dissection data, seperate for box A & B
# into columns of: experiment day, group, sugar conc., number of live bees at the day
# note: 
# -each group has different train start day (experiment day 6): 4(17-May), 5(21-May), 6(1-Jun), 7(2-Jun)
# and end day (experiment day 13): 4(24-May), 5(28-May), 6(8-Jun), 7(9-Jun)
# -any sample dissected before the end day are dead bees
# (-dead bees at the end day are noted (db) in the sample_name column, e.g. "10_5A db4" 
    # ??? for now, ignore the db at the end day? treat them as live till the last time?
### bees in box A of 0% for group 6, was realocated to other 10% & 60% due to their high mortality
    # so no control train for group 6, and the CBinput data (ExSurvival_copy.csv) of its 10A and 60A boxes were manually modified

# call packages
library(dplyr)
library(lubridate)

# load data
ExData <- read.csv("../data/ExDissection.csv")
# Transform multiple specific columns to factors
ExData <- ExData %>%
  mutate(across(c(date_d_m, sugar_conc, group, box, sex), as.factor))
str(ExData)


# ── 1. Define group metadata ──────────────────────────────────────────────────
group_meta <- data.frame(
  group = factor(c("4",    "5",    "6",   "7")),
  start = as.Date(c("2026-05-17", "2026-05-21", "2026-06-01", "2026-06-02")),
  end   = as.Date(c("2026-05-24", "2026-05-28", "2026-06-08", "2026-06-09")),
  stringsAsFactors = FALSE
)

# ── 2. Parse date_d_m to real Date ─────────────────────────────────────────────
ExData <- ExData %>%
  mutate(
    date_parsed = as.Date(
      paste0(as.character(date_d_m), "-2026"),
      format = "%d-%m-%Y"
    )
  )

# ── 3. Join group metadata ────────────────────────────────────────────────────
ExData <- ExData %>%
  left_join(group_meta, by = "group")

# ── 4. Add experimental day + binary death status ─────────────────────────────
ExData <- ExData %>%
  mutate(
    # Experimental day: start = day 6, end = day 13
    exp_day = as.integer(date_parsed - start) + 6,

    # Death status:
    # 1 (dead) if:
    #   (a) recorded BEFORE the end day (intermediate dead)
    #   (b) recorded ON the end day BUT sample_name ends in "db#"
    # 0 (alive) if:
    #   recorded on end day AND sample_name does NOT end in "db#"
    status = case_when(
      date_parsed < end                                    ~ 1L,   # (a) pre-end dead
      date_parsed == end & grepl("db\\d*$", sample_name,
                                  ignore.case = TRUE)     ~ 1L,   # (b) end-day dead
      date_parsed == end                                   ~ 0L,   # alive at end
      TRUE                                                 ~ NA_integer_  # safety catch
    )
  )

# drop unnecessary columns
ExData <- ExData %>%
  select(-c(date_d_m, start, end, date_parsed,
                note, PBS_ul))
str(ExData)

# check & save
str(ExData)
write.csv(ExData, "../data/ExStatus.csv", row.names = FALSE)

# later analysis may need to remove group6 sugar0 data, no use
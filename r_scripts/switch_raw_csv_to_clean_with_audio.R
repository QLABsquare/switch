# This document converts raw Gorilla csv file to clean csv file for online eyemovement analysis
# First written on May 13,2026 by Heesu Yun

library(dplyr)
library(tidyr)
library(readr)

################################################################################
#1. Set path to where all csv files are located (now it's set to Qlab NAS switch folder)
setwd("/Volumes/data/projects/switch/gorilla_csv/raw_csv")

#2. Load data
df <- read_csv("switch_064.csv", show_col_types = FALSE)

#3. Define target screens
target_screens <- c("baseline_test_choice", 
                    "phase1_training","phase1_tests_choice", 
                    "phase2_training","phase2_test_choice")

#4. Filter, pick the right audio column per screen, and deduplicate
# test screens  → "Spreadsheet: test_audio"
# training screens → "Spreadsheet: training_audio"

training_screens <- c("phase1_training", "phase2_training")
result <- df |>
  filter(Screen %in% target_screens) |>
  mutate(
    audio = if_else(
      Screen %in% training_screens,
      `Spreadsheet: training_audio`,
      `Spreadsheet: test_audio`
    )
  ) |>
  select(
    participant = `Participant Public ID`,
    screen      = Screen,
    trial       = `Trial Number`,
    audio
  ) |>
  filter(!is.na(audio)) |>
  distinct(participant, screen, trial, audio) |>
  arrange(participant, screen, trial)

# 5. Create combined column with renamed screen labels (to match with eyecoding results)
screen_rename <- c(
  "baseline_test_choice"  = "baseline_test",
  "phase1_tests_choice"   = "phase_one_test",
  "phase1_training"       = "phase_one_training",
  "phase2_training"       = "phase_two_training",
  "phase2_test_choice"    = "phase_two_test"
)
result_combined <- result |>
  mutate(
    screen_renamed = screen_rename[screen],
    File = paste(participant, screen_renamed, trial, sep = "_")
  ) |>
  select(File, audio)

# 6. Load word onsets / image location lookup table
word_onsets <- read_csv("/Volumes/data/projects/switch/info_csv/word_onsets_image_location.csv", show_col_types = FALSE)

# 7. Join result_combined with word_onsets on audio == file_name
result_final <- result_combined |>
  left_join(word_onsets, by = c("audio" = "file_name"))

# 8. Build a per-participant verb -> cueType lookup from phase_one_training (except baseline test)
verb_cuetype_lookup <- result_final |>
  filter(grepl("phase_one_training", File)) |>
  filter(!is.na(cueType)) |>
  mutate(participant = sub("_.*", "", File)) |>   # extract participant ID from File
  distinct(participant, verb, cueType)

# 9. Fill in cueType for test and phase_two_training rows using the lookup
result_final <- result_final |>
  mutate(participant = sub("_.*", "", File)) |>
  left_join(verb_cuetype_lookup, by = c("participant", "verb"), suffix = c("", "_filled")) |>
  mutate(
    cueType = if_else(
      is.na(cueType) & grepl("phase_one_test|phase_two_test|phase_two_training", File),
      cueType_filled,
      cueType
    )
  ) |>
  select(-cueType_filled, -participant)   # drop the helper column

write.csv(result_final, "/Volumes/data/projects/switch/gorilla_csv/clean_csv_with_audio/switch_064_clean_w_audio.csv", row.names = FALSE)
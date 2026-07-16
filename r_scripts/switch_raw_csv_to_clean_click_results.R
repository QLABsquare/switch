# This document converts raw Gorilla csv file to clean csv file for offline behavior (clicking) analysis
# First written on January 7,2026 and edited on February 24, 2026 by Heesu Yun

library(dplyr)

################################################################################
#1. Set path to where all csv files are located (now it's set to Qlab NAS switch folder)
setwd("/Volumes/data/projects/switch/gorilla_csv/raw_csv")

#2. Read Gorilla csv and only keep columns needed
                   ## change participant ID ##
raw_csv <- read.csv("switch_a_211.csv")
raw_csv <- raw_csv %>% select(Participant.Public.ID,Trial.Number,
                              Screen,Response.Type,Response,Spreadsheet..test_audio,
                              Spreadsheet..top_right_image,Spreadsheet..top_left_image,
                              Spreadsheet..bottom_left_image, Spreadsheet..bottom_right_image)

#3. Only keep rows with click response for test trials (remove practices and fillers) 
raw_csv <- raw_csv %>% filter(Response.Type == "response")
raw_csv <- raw_csv %>% filter(!Screen %in% c("test_practice_choice", "phase1_practice", 
                       "phase1_training", "phase2_training"))
raw_csv <- raw_csv %>% filter(!grepl("^Fillers_", Spreadsheet..test_audio))

#4. Remove unnecessary columns, rename the columns
raw_csv <- raw_csv[, -4]
names(raw_csv) <- c("participant_ID", "trial_number", "phase", "click", "test_audio",
                    "top_right_image", "top_left_image","bottom_left_image", "bottom_right_image")

#5. Changed click values from position to real png files
raw_csv$click <- trimws(as.character(raw_csv$click))
raw_csv$click <- raw_csv[cbind(seq_len(nrow(raw_csv)),
                               match(raw_csv$click, names(raw_csv)))]

#6. Add test time points per each phase and test trials
raw_csv <- raw_csv %>%
  group_by(participant_ID) %>%
  mutate(
    phase_order = ave(trial_number, phase, FUN = function(x)
      rank(x, ties.method = "first")),
    phase = case_when(
      phase == "baseline_test_choice" ~ 0L,
      phase == "phase1_tests_choice" & phase_order <= 8 ~ 1L,
      phase == "phase1_tests_choice" & phase_order >  8 ~ 2L,
      phase == "phase2_test_choice"  & phase_order <= 8 ~ 3L,
      phase == "phase2_test_choice"  & phase_order >  8 ~ 4L,
      TRUE ~ NA_integer_
    )
  ) %>%
  ungroup() %>%
  select(-phase_order)

raw_csv <- raw_csv %>%
  group_by(participant_ID, phase) %>%
  arrange(trial_number, .by_group = TRUE) %>%
  mutate(trial_number = row_number()) %>%
  ungroup()

raw_csv <- raw_csv %>%
  relocate(phase, .before = trial_number)

################################################################################

#7. Read and merge target csv with the raw_csv
                       ## change answer spreadsheet based on the participant's spreadsheet
target_csv <- read.csv("with_point_to_knock_on_w_target.csv")
target_csv <- target_csv %>% select(test_audio, target_animal, target_instrument, verb, cueType)
target_csv <- target_csv[rowSums(is.na(target_csv) | target_csv == "") == 0, ]

raw_csv <- raw_csv %>%
  left_join(
    target_csv,
    by = "test_audio"
  )
raw_csv$age <- 18.803 ## change this for each participant

#8. Reorder the columns 
clean_csv <- raw_csv[, c("participant_ID","age","phase","trial_number","test_audio","verb","cueType",
                         "top_right_image","top_left_image","bottom_left_image","bottom_right_image",
                         "target_animal","target_instrument","click")]
clean_csv <- clean_csv[, -5] #removing test audio; was needed to compare with target csv
#for children
#clean_csv$participant_ID <- sprintf("switch_%03d", clean_csv$participant_ID)

#9. Assign click to TA (target animal) / TI (target instrument) / DA (distractor animal) / DI (distractor instrument)
clean_csv$clickToTA <- as.integer(clean_csv$click == clean_csv$target_animal)
clean_csv$clickToTI <- as.integer(clean_csv$click == clean_csv$target_instrument)
clean_csv$clickToDA <- as.integer(grepl("_", clean_csv$click) & clean_csv$click != clean_csv$target_animal)
clean_csv$clickToDI <- as.integer(!grepl("_", clean_csv$click) & clean_csv$click != clean_csv$target_instrument)
################################################################################

#10. Save it to new path with new name
                                                                          ## rename the ID
write.csv(clean_csv, "/Volumes/data/projects/switch/gorilla_csv/clean_csv_for_click_analysis/switch_a_211_clean_with_click.csv", row.names = FALSE)


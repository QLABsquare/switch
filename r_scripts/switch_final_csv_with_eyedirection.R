# This document combines gorilla csv with handed coded eye movement data
# First written on May 19,2026 by Heesu Yun

library(tidyverse)

################################################################################
#1. Set directories
audio_dir <- "/Volumes/data/projects/switch/gorilla_csv/clean_csv_with_audio"
eye_dir   <- "/Volumes/data/projects/switch/zoom_recording/coded_results"
out_dir   <- "/Volumes/data/projects/switch/gorilla_csv/final_csv_for_eyemovement_analysis"

# remove leading zeros from audio files (e.g. "037" -> "37") to match with coded results csvs 
strip_zero <- function(x) sub("^0+", "", x)

# create final csv file that combines gorilla csv and eyecoding csv for
# 9-10 yo participants, whose eyecoding is complete

ids <- c("024")
audio_files <- file.path(audio_dir, paste0("switch_", ids, "_clean_w_audio.csv"))

for (af in audio_files) {
  id <- gsub(".*switch_(.+)_clean_w_audio\\.csv", "\\1", basename(af))
  eye_file <- file.path(eye_dir, paste0("switch_", id),
                        paste0("switch_", id, "_eyedirection.csv"))
  
  if (!file.exists(eye_file)) { message("Skipping ", id); next }
  
  audio <- read_csv(af, show_col_types = FALSE) %>%
    filter(!grepl("^Filler", audio, ignore.case = TRUE)) %>%
    mutate(File = strip_zero(File))
  
  eye <- read_csv(eye_file, show_col_types = FALSE) %>%
    mutate(File = strip_zero(File),
           Time_sec = Frame / 25)
  
  merged <- left_join(eye, audio, by = "File") %>%
    filter(!is.na(audio)) %>%
    mutate(
      LookTarget = case_when(
        LookCode == "Right" & grepl("right", target_animal_loc, ignore.case = TRUE) ~ "TA",
        LookCode == "Right" & grepl("right", target_inst_loc,   ignore.case = TRUE) ~ "TI",
        LookCode == "Left"  & grepl("left",  target_animal_loc, ignore.case = TRUE) ~ "TA",
        LookCode == "Left"  & grepl("left",  target_inst_loc,   ignore.case = TRUE) ~ "TI",
        TRUE ~ LookCode
      ),
      Subject     = id,
      TestingTime = case_when(
        grepl("baseline_test",      File, ignore.case = TRUE) ~ "baseline_test",
        grepl("phase_one_test",     File, ignore.case = TRUE) ~ "phase_one_test",
        grepl("phase_one_training", File, ignore.case = TRUE) ~ "phase_one_training",
        grepl("phase_two_test",     File, ignore.case = TRUE) ~ "phase_two_test",
        grepl("phase_two_training", File, ignore.case = TRUE) ~ "phase_two_training",
        TRUE ~ NA_character_
      ),
      TrialOrder  = as.integer(gsub(".*_(\\d+)$", "\\1", File)),
      Time        = Time_sec - verb_onset,
      Is_start    = !duplicated(File)
    ) %>%
    select(Subject, TestingTime, TrialOrder,
           everything(),
           -File, -Frame, -Coder)
  
  write_csv(merged, file.path(out_dir, paste0("switch_", id, "_final_w_eyemovement.csv")))
  message("Done: ", id, " (", nrow(merged), " rows)")
}

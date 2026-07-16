library(tidyverse)
library(irr)

# set directory to from NAS/switch folder and read the data (ex. switch_019 by LM and SL)

setwd("/Volumes/data/projects/switch/zoom_recording/coded_results/switch_019")

primary <- read_csv("switch_019_eyedirection.csv") #by LM
reliability <- read_csv("switch_019_eyedirection_reliability.csv") #by SL

# merge two files 
merged <- inner_join(
  primary %>% select(File, Frame, LookCode) %>% rename(Code_Primary = LookCode),
  reliability %>% select(File, Frame, LookCode) %>% rename(Code_Reliability = LookCode),
  by = c("File", "Frame")
)

merged <- merged %>%
  filter(
    Code_Primary %in% c("Left", "Right"),
    Code_Reliability %in% c("Left", "Right")
  )


# ---------------------------------------------------
# check overall % agreement 
#(didn't use Kappa, since having Left and Right only is not uneven, but the only choice in the eyecoding)
pct_agree <- mean(merged$Code_Primary == merged$Code_Reliability) * 100
cat(sprintf("Overall %% Agreement: %.2f%%\n", pct_agree))
  ## Overall % Agreement: 72.14%

# ---------------------------------------------------
# Kappa by phase and trial type
merged <- merged %>%
  mutate(
    Phase     = str_extract(File, "baseline|phase_one|phase_two"),
    TrialType = str_extract(File, "training|test")
  )

merged %>%
  group_by(Phase, TrialType) %>%
  summarise(
    N         = n(),
    Pct_Agree = mean(Code_Primary == Code_Reliability) * 100,
    .groups   = "drop"
  )

#   Phase     TrialType     N Pct_Agree
#   <chr>     <chr>     <int>     <dbl>
# 1 baseline  test        880      85.1
# 2 phase_one test       2925      76.3
# 3 phase_one training   2444      67.4
# 4 phase_two test       3939      73.1
# 5 phase_two training   3395      67.5

## test trials are more reliable across two participants than training trials

# ---------------------------------------------------
# ONLY Left/Right direction agreement across two coders 
merged_lr <- merged %>%
  filter(Code_Primary %in% c("Left", "Right") | Code_Reliability %in% c("Left", "Right"))

pct_agree_lr <- mean(merged_lr$Code_Primary == merged_lr$Code_Reliability) * 100
cat(sprintf("Left/Right Overall %% Agreement: %.2f%%\n", pct_agree_lr))
  ## Left/Right Overall % Agreement: 68.13%
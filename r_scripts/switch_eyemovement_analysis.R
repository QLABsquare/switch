# This document analyze eyemovement from hand coded data by timepoints
# First written on June 11,2026 by Heesu Yun

###############################################################################
## Aim 1 — Eye-tracking analysis (phase_one_test)
## Model: LookToTargetInst ~ baselineBias * CueType * TrialIndex + Subject and Verb as random-effect controls.
##        Sample-level (40 ms) data with AR1 correction for within-trial autocorrelation (resets per trial via Is_start).
##
## Outcome: LookToTargetInst = 1 if looking at Target Instrument (TI),
##                             0 if looking anywhere else identifiable
##                             (TA, Shifting, Left, Right, Center).
##          Dropped as unusable/non-engaged: Cannot_Tell, Blink, Away (+ strays).
##
## baselineBias = per-subject proportion of TI looks (TI vs all) during
##                baseline_test, centered. Carried into phase_one as a covariate.
## TrialIndex   = trial position 1...16 within phase_one (filler trials removed).
##
###############################################################################

## set paths and parameters here:
data_dir    <- "/Volumes/data/projects/switch/gorilla_csv/final_csv_for_eyemovement_analysis"
file_glob   <- "switch_*_final_w_eyemovement.csv"
age_csv     <- "/Volumes/data/projects/switch/info_csv/switch_child_participant_info.csv"
out_dir     <- "/Volumes/data/projects/switch/r_outputs/eye_movement_analysis"

###############################################################################
## import libraries

library(mgcv)
library(itsadug)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

###############################################################################
## load and stack all subjects

files <- list.files(data_dir, pattern = utils::glob2rx(file_glob), full.names = TRUE)
stopifnot(length(files) > 0)

## Subject in eye-movement files is an integer (e.g. 31) -> pad to "031".
data_raw <- files |>
  lapply(function(f) readr::read_csv(f, show_col_types = FALSE)) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Subject = stringr::str_pad(as.character(Subject), 3, pad = "0"))

## Age file uses participant_id "switch_###", age_at_test (years), sex.
ages <- readr::read_csv(age_csv, show_col_types = FALSE) |>
  dplyr::mutate(Subject = stringr::str_pad(
    stringr::str_remove(stringr::str_trim(participant_id), "switch_"),
    3, pad = "0"),
    Age = age_at_test) |>
  dplyr::select(Subject, Age, sex)

data_raw <- dplyr::left_join(data_raw, ages, by = "Subject")

###############################################################################
## define outcome (Look to Target Instrument (TI) vs Look to Target Animal (TA))

## Keep the test phases and trim each trial to frames up to noun2_onset + 2 s.
data <- data_raw |>
  dplyr::filter(TestingTime %in% c("baseline_test", "phase_one_test", "phase_two_test"),
                !is.na(noun2_onset)) |>
  dplyr::filter(Time <= noun2_onset + 2)

## Re-index trial position within each Subject x TestingTime.
## Filler trials were removed, so raw TrialOrder is non-consecutive.
## TrialIndex renumbers the surviving test trials 1..N in TrialOrder order,restarting at 1 for each phase.

data <- data |>
  dplyr::group_by(Subject, TestingTime) |>
  dplyr::mutate(TrialIndex   = dplyr::dense_rank(TrialOrder),
                TrialIndex_c = TrialIndex - mean(TrialIndex)) |>
  dplyr::ungroup()

## Outcome: TI = 1; any other identifiable look (TA, Shifting, Center) = 0.
## Dropped as unusable: Cannot_Tell, Blink, Away (plus stim_offset and stray codes: good/n/NA). 
## (Left/Right occur only in training trials, which are already filtered out.)

## NOTE: dropping mid-trial frames makes the within-trial time grid irregular, so the
## AR1 rho is approximate (gaps are treated as one time-step). This is minimized, not
## eliminated, by keeping deletions few; binning to a fixed grid would remove it fully.
missing_codes <- c("Cannot_Tell", "Blink", "Away", "stim_offset", "good", "n", NA)
data <- data |>
  dplyr::filter(!LookTarget %in% missing_codes, !is.na(LookTarget)) |>
  dplyr::mutate(LookToTargetInst = as.integer(LookTarget == "TI"))

###############################################################################
## calculating baselineBias  (per-subject TI preference at baseline, centered)
### TI is 1 and TA, Shifting, Center is 0; positive = more instrument-biased at baseline; negative = less instrument-biased at baseline

bias_tbl <- data |>
  dplyr::filter(TestingTime == "baseline_test") |>
  dplyr::group_by(Subject) |>
  dplyr::summarise(baselineBias_raw = mean(LookToTargetInst), .groups = "drop") |>
  dplyr::mutate(baselineBias = baselineBias_raw - mean(baselineBias_raw)) # center

data <- dplyr::left_join(data, bias_tbl, by = "Subject")

###############################################################################
## factors, ordering, and AR.start #since this is eyetracking data, each row is dependent to each other

prep <- function(dat_in) {
  ## Sort into trial time order so AR1 reads rows as consecutive time steps.
  ## NOTE: the raw Is_start marks the first row of the ORIGINAL trial, but after
  ## dropping frames that row is often gone (~38% of trials here). So we recompute
  ## the marker on the filtered data with start_event() — which flags the first
  ## SURVIVING row per trial — and overwrite Is_start with it.
  d <- as.data.frame(dat_in)   # start_event() errors on tibbles (xtfrm.data.frame)
  d <- d[order(d$Subject, d$TestingTime, d$TrialOrder, d$Time), ]
  d$Event <- interaction(d$Subject, d$TestingTime, d$TrialOrder, drop = TRUE)
  d <- itsadug::start_event(d, column = "Time", event = "Event")  # makes start.event
  d$Is_start <- d$start.event   # overwrite existing column
  d$start.event <- NULL         # drop the temporary column
  d$Subject <- factor(d$Subject)
  d$Verb    <- factor(d$verb)
  d
}

###############################################################################
## baseline_test vs phase_one_test (frequenti)

mdat <- data |> dplyr::filter(TestingTime == "phase_one_test")
mdat$CueType <- factor(mdat$cueType, levels = c("event", "structure_event"))
mdat <- prep(mdat)

## Three-way interaction of baselineBias x CueType x TrialIndex (TrialIndex linear:
## only 16 positions, so a smooth has too little to bend). Subject & Verb are random
## intercepts (control only). Time is not a predictor, but AR1 still corrects the
## within-trial sample-to-sample correlation and resets at each trial via Is_start.
m_form <- LookToTargetInst ~ baselineBias * CueType * TrialIndex_c + s(Subject, bs = "re") + s(Verb, bs = "re")

m_noAR <- bam(m_form, data = mdat, family = binomial, discrete = TRUE)
rho    <- itsadug::start_value_rho(m_noAR)
m      <- bam(m_form, data = mdat, family = binomial, discrete = TRUE, rho = rho, AR.start = mdat$Is_start)

cat("AR1 rho =", rho, "\n\n")
  #AR1 rho = 0.8807926 | ~88% are correlated from one time step to the next within trials, so AR1 correction is important.
print(summary(m))
  #                                                  Estimate Std. Error z value Pr(>|z|)    
  # (Intercept)                                      -0.87950    0.26350  -3.338 0.000845 ***
  # baselineBias                                      1.00322    2.06865   0.485 0.627703    
  # CueTypestructure_event                           -0.05774    0.13715  -0.421 0.673786    
  # TrialIndex_c                                      0.01544    0.02041   0.756 0.449493    
  # baselineBias:CueTypestructure_event              -1.72463    1.30720  -1.319 0.187060    
  # baselineBias:TrialIndex_c                        -0.06865    0.15462  -0.444 0.657066    
  # CueTypestructure_event:TrialIndex_c               0.02431    0.03025   0.803 0.421699    
  # baselineBias:CueTypestructure_event:TrialIndex_c  0.03018    0.29905   0.101 0.919623    
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Approximate significance of smooth terms:
  #                edf Ref.df  Chi.sq p-value
  # s(Subject) 10.1554     12 131.430   0.635
  # s(Verb)     0.6912      3   1.497   0.163
  # 
  # R-sq.(adj) =   0.14   Deviance explained = 11.2%
  # fREML =  -2884  Scale est. = 1         n = 15107

###############################################################################
## mean TI-looking across the 16 trials, by cueType (save as csv)

descr <- mdat |>
  dplyr::group_by(CueType, TrialIndex) |>
  dplyr::summarise(p_TI = mean(LookToTargetInst), n = dplyr::n(), .groups = "drop")
readr::write_csv(descr, file.path(out_dir, "descriptive_TI_by_trial.csv"))

## plot mean TI-looking across the 16 trials, by cueType 
p_descr <- ggplot() +
  geom_point(data = descr, aes(TrialIndex, p_TI, colour = CueType)) +
  geom_smooth(data = mdat,
              aes(TrialIndex, LookToTargetInst, colour = CueType, fill = CueType),
              method = "gam", method.args = list(family = "binomial"),
              formula = y ~ s(x, k = 5), alpha = 0.15) +
  labs(x = "Trial (1-16, phase one)", y = "P(look to TI)",
       colour = NULL, fill = NULL) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal()

print(p_descr)      

ggsave(file.path(out_dir, "descriptive_TI_by_trial.png"),plot = p_descr, width = 8, height = 5, dpi = 130)   


###############################################################################
## Aim 2 — Eye-tracking analysis (phase_two_test)
## Model: Look2TA ~ TrialIndex * Age_centered + Subject and Verb as random-effect controls.
##        Sample-level (40 ms) data with AR1 correction for within-trial autocorrelation (resets per trial via Is_start).
##
## Goal: does looking to the Target Animal (modifier interpretation) change across the 16 phase-two test trials (learning), 
##      and does that change differ with Age?
##
## Outcome: Look2TA = 1 if looking at Target Animal (TA),
##                    0 if looking anywhere else identifiable
##                    (TI, Shifting, Center).
##          Dropped as unusable/non-engaged: Cannot_Tell, Blink, Away (+ strays).
##
## TrialIndex   = test-trial position 1...16 within phase_two_test (filler trials removed),centered; 
## Age_centered = Age - mean(Age) across subjects.
###############################################################################

## Phase-two data from data (already trimmed & gaze-filtered upstream)  (frequentist)
m2dat <- data |> dplyr::filter(TestingTime == "phase_two_test")

## outcome = looks to target animal
m2dat$Look2TA <- as.integer(m2dat$LookTarget == "TA")

## age centered across subjects
m2dat$Age_centered <- m2dat$Age - mean(m2dat$Age, na.rm = TRUE)

## order rows, rebuild per-trial AR1 start marker, set factors.
m2dat <- prep(m2dat)

## Learning across trials: TrialIndex_c x Age_centered, Subject & Verb random intercepts. 
m2_form <- Look2TA ~ TrialIndex_c * Age_centered + s(Subject, bs = "re") + s(Verb,    bs = "re")

m2_noAR <- bam(m2_form, data = m2dat, family = binomial, discrete = TRUE)
rho2    <- itsadug::start_value_rho(m2_noAR)
cat("MODEL 2 — AR1 rho =", rho2, "\n\n")
  # MODEL 2 — AR1 rho = 0.9044925 

m2      <- bam(m2_form, data = m2dat, family = binomial, discrete = TRUE, rho = rho2, AR.start = m2dat$Is_start)
print(summary(m2))

  #                            Estimate Std. Error z value Pr(>|z|)  
  # (Intercept)                0.429941   0.220227   1.952   0.0509 .
  # TrialIndex_c              -0.005721   0.016558  -0.345   0.7297  
  # Age_centered              -0.408143   0.579397  -0.704   0.4812  
  # TrialIndex_c:Age_centered  0.017453   0.041916   0.416   0.6771  
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Approximate significance of smooth terms:
  #                  edf Ref.df Chi.sq p-value    
  # s(Subject) 8.9452997     12 73.973  <2e-16 ***
  # s(Verb)    0.0001524      4  0.006   0.116    
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # R-sq.(adj) =  0.114   Deviance explained = 8.74%
  # fREML = -3046.2  Scale est. = 1         n = 13422

###############################################################################
## animal-looking across the 16 phase-two trials (sanity check)
## points = raw per-trial means. Curve = smoothed trend from sample-level data.

descr2 <- m2dat |>
  dplyr::group_by(TrialIndex) |>
  dplyr::summarise(p_TA = mean(Look2TA), n = dplyr::n(), .groups = "drop")
readr::write_csv(descr2, file.path(out_dir, "model2_animal_by_trial.csv"))

p_m2 <- ggplot() +
  geom_point(data = descr2, aes(TrialIndex, p_TA)) +
  geom_smooth(data = m2dat, aes(TrialIndex, Look2TA),
              method = "gam", method.args = list(family = "binomial"),
              formula = y ~ s(x, k = 5), alpha = 0.15) +
  labs(x = "Phase-two test trial (1-16)", y = "P(look to target animal)") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal()
print(p_m2)
ggsave(file.path(out_dir, "model2_animal_by_trial.png"),
       plot = p_m2, width = 8, height = 5, dpi = 130)

###############################################################################
## VISUALIZATION of time-course look to TI phase_one_test part 1 & 2 & phase_tw_test part 1 & 2

tc <- data |>
  dplyr::filter(TestingTime %in% c("phase_one_test", "phase_two_test")) |>
  dplyr::mutate(
    TI      = as.integer(LookTarget == "TI"),
    CueType = factor(cueType, levels = c("event", "structure_event")),
    half    = ifelse(TrialIndex <= 8, "trials 1-8", "trials 9-16"),
    phase   = ifelse(TestingTime == "phase_one_test", "Phase 1", "Phase 2"),
    Segment = factor(paste(phase, half),
                     levels = c("Phase 1 trials 1-8", "Phase 1 trials 9-16",
                                "Phase 2 trials 1-8", "Phase 2 trials 9-16")))

## Global average onsets across ALL test trials (one value each, same in every panel).
onsets <- data |>
  dplyr::filter(TestingTime %in% c("phase_one_test", "phase_two_test")) |>
  dplyr::group_by(Subject, TestingTime, TrialOrder) |>
  dplyr::summarise(noun1 = dplyr::first(noun1_onset),
                   with  = dplyr::first(with_onset),
                   noun2 = dplyr::first(noun2_onset), .groups = "drop") |>
  dplyr::summarise(noun1 = mean(noun1, na.rm = TRUE),
                   with  = mean(with,  na.rm = TRUE),
                   noun2 = mean(noun2, na.rm = TRUE))
onset_lines <- data.frame(onset = c("noun1", "with", "noun2"),
                          x = c(onsets$noun1, onsets$with, onsets$noun2))

## Binned points (50 ms) per Segment x CueType.
tc_bins <- tc |>
  dplyr::mutate(tbin = floor(Time / 0.05) * 0.05 + 0.025) |>
  dplyr::group_by(Segment, CueType, tbin) |>
  dplyr::summarise(p_TI = mean(TI), n = dplyr::n(), .groups = "drop")
readr::write_csv(tc_bins, file.path(out_dir, "timecourse_TI_4panel.csv"))

p_tc <- ggplot() +
  geom_vline(data = onset_lines, aes(xintercept = x),
             linetype = "dotted", colour = "grey40") +
  geom_text(data = onset_lines, aes(x = x, y = 1.02, label = onset),
            angle = 90, hjust = 0, vjust = -0.2, size = 3, colour = "grey40") +
  geom_point(data = tc_bins, aes(tbin, p_TI, colour = CueType), alpha = 0.4, size = 0.9) +
  geom_smooth(data = tc, aes(Time, TI, colour = CueType, fill = CueType),
              method = "gam", method.args = list(family = "binomial"),
              formula = y ~ s(x), alpha = 0.15) +
  facet_wrap(~ Segment, nrow = 1) +
  labs(x = "Time from sentence onset (s)", y = "P(look to target instrument)",
       colour = NULL, fill = NULL,
       title = "TI-looking time course by CueType across test segments (descriptive)") +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_tc)

ggsave(file.path(out_dir, "timecourse_TI_4panel.png"),
       plot = p_tc, width = 16, height = 4.5, dpi = 130)


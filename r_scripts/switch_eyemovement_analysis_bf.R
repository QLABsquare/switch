###############################################################################
## BF STOPPING RULE - written on June 18, 2026
##
##   After each new child is collected, it recomputes the Bayes Factor that says how strong the evidence is for 
##  "children adapt to new sentence format," and tells you whether that evidence is strong enough to STOP collecting.
##
###############################################################################

## values to decide before analyzing the data
data_dir  <- "/Volumes/data/projects/switch/gorilla_csv/final_csv_for_eyemovement_analysis"
file_glob <- "switch_*_final_w_eyemovement.csv"
out_dir   <- "/Volumes/data/projects/switch/r_outputs/eye_movement_analysis"

FOCAL        <- "interaction"
BF_THRESHOLD <- 6           
N_MIN        <- 12          
N_MAX        <- Inf           
DIRECTIONAL  <- FALSE    
FOCAL_PRIOR_SD <- 0.5   

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

library(dplyr)
library(readr)
library(stringr)
library(brms)
library(ggplot2)

## Load Data
files <- list.files(data_dir, pattern = utils::glob2rx(file_glob), full.names = TRUE)

data_raw <- files |>
  lapply(function(f) readr::read_csv(f, show_col_types = FALSE)) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Subject = stringr::str_pad(as.character(Subject), 3, pad = "0"))

###############################################################################
## Keep phase_one_test, trim each trial to noun2_onset + 2 s, drop unusable frames,mark TI looks. 
missing_codes <- c("Cannot_Tell", "Blink", "Away", "stim_offset", "good", "n", NA)

trials <- data_raw |>
  dplyr::filter(TestingTime == "phase_one_test", !is.na(noun2_onset)) |>
  dplyr::filter(Time <= noun2_onset + 2) |>
  dplyr::filter(!LookTarget %in% missing_codes, !is.na(LookTarget)) |>
  dplyr::mutate(TI = as.integer(LookTarget == "TI")) |>
  dplyr::group_by(Subject) |>
  dplyr::mutate(TrialIndex = dplyr::dense_rank(TrialOrder)) |>   # renumber 1..16
  dplyr::ungroup()

###############################################################################
## Collapse each trial to a count:
## One row per real trial: how many frames looked at TI, out of how many usable.

agg <- trials |>
  dplyr::group_by(Subject, TrialOrder) |>
  dplyr::summarise(
    n_TI       = sum(TI),
    n_tot      = dplyr::n(),
    CueType    = dplyr::first(cueType),
    Verb       = dplyr::first(verb),
    TrialIndex = dplyr::first(TrialIndex),
    .groups = "drop") |>
  dplyr::filter(n_tot > 0) |>
  dplyr::mutate(
    CueType      = factor(CueType, levels = c("event", "structure_event")),
    Verb         = factor(Verb),
    TrialIndex_c = TrialIndex - 8.5)

###############################################################################
## model for BF stopping rule
## focal coefficient name (brms builds it from the factor level + the term)
focal_coef <- if (FOCAL == "interaction")
  "CueTypestructure_event:TrialIndex_c" else "CueTypestructure_event"

## A Bayes Factor REQUIRES a real prior on the focal effect (a flat prior makes
## it undefined). Normal(0, 0.5) on the log-odds scale is a modest, sensible default.
priors <- c(
  prior(normal(0, 1.5), class = "Intercept"),
  prior(normal(0, 1),   class = "b"),                                  # other terms
  set_prior(paste0("normal(0,", FOCAL_PRIOR_SD, ")"),
            class = "b", coef = focal_coef),                           # the focal one
  prior(exponential(1), class = "sd"))

model_form <- bf(n_TI | trials(n_tot) ~ CueType * TrialIndex_c +
                   (1 | Subject) + (1 | Verb))
## To mirror Model 1 more closely you could add baselineBias as a covariate here
## (compute it once from baseline_test, freeze its centering, then "+ baselineBias").

hyp_str <- if (DIRECTIONAL) paste(focal_coef, "> 0") else paste(focal_coef, "= 0")

## We compile the Stan model ONCE, then reuse it via update() for every child
## count — otherwise it recompiles each time and is painfully slow.
fit_template <- NULL

bf10_for <- function(d_k) {
  if (is.null(fit_template)) {
    fit_template <<- brm(
      model_form, data = d_k, family = beta_binomial(),
      prior = priors, sample_prior = "yes",        # "yes" is REQUIRED for the BF
      chains = 4, iter = 4000, warmup = 1000,
      control = list(adapt_delta = 0.95), refresh = 0, seed = 1)
    fit <- fit_template
  } else {
    fit <- update(fit_template, newdata = d_k, recompile = FALSE, refresh = 0)
  }
  er <- hypothesis(fit, hyp_str)$hypothesis$Evid.Ratio
  ## For "= 0", Evid.Ratio is BF in favor of NO effect (BF01), so flip it.
  ## For "> 0", Evid.Ratio is already the directional BF10.
  if (DIRECTIONAL) er else 1 / er
}

###############################################################################
## Accrue children in the order their eyecoding was completed
subj_order <- sort(unique(agg$Subject))
N <- length(subj_order)

res <- data.frame(); decision <- "continue collecting"
for (k in seq(N_MIN, min(N, N_MAX))) {
  d_k  <- dplyr::filter(agg, Subject %in% subj_order[1:k])
  bf10 <- tryCatch(bf10_for(d_k), error = function(e) NA_real_)
  res  <- rbind(res, data.frame(n_children = k, BF10 = bf10))
  cat(sprintf("n = %2d   BF10 = %8.3f\n", k, bf10))
  
  if (!is.na(bf10) && bf10 >= BF_THRESHOLD)     { decision <- "STOP — evidence FOR adaptation"; break }
  if (!is.na(bf10) && bf10 <= 1 / BF_THRESHOLD) { decision <- "STOP — evidence AGAINST adaptation"; break }
}
cat("\n>>> Decision:", decision, "at n =", tail(res$n_children, 1), "children\n")
readr::write_csv(res, file.path(out_dir, "bf_trajectory.csv"))

## plot the BF as children accrue, with the two stopping lines
p <- ggplot(res, aes(n_children, BF10)) +
  geom_hline(yintercept = c(1/BF_THRESHOLD, 1, BF_THRESHOLD),
             linetype = c("dashed","solid","dashed"), colour = "grey50") +
  geom_line() + geom_point() + scale_y_log10() +
  labs(x = "Number of children", y = "BF10 (log scale)",
       title = paste0("Sequential BF — ", FOCAL, " term (threshold ", BF_THRESHOLD, ")")) +
  theme_minimal()
print(p)
ggsave(file.path(out_dir, "bf_trajectory.png"), p, width = 8, height = 5, dpi = 130)

## After all the data collection, check the PRIOR ROBUSTNESS
## The BF depends on the prior width. Run this on your current full sample and
## report the range — a conclusion that holds across all three is trustworthy.
# for (sd in c(0.25, 0.5, 1.0)) {
#   pr <- priors
#   pr$prior[pr$coef == focal_coef] <- paste0("normal(0,", sd, ")")
#   f  <- brm(model_form, data = agg, family = beta_binomial(),
#             prior = pr, sample_prior = "yes", refresh = 0, seed = 1)
#   er <- hypothesis(f, paste(focal_coef, "= 0"))$hypothesis$Evid.Ratio
#   cat(sprintf("prior SD %.2f  ->  BF10 = %.3f\n", sd, 1/er))
# }
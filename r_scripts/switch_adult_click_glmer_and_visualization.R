# This document visualizes the clicking behaviors from gorilla clean csv files 
# Written on January 8, 2026 by Heesu Yun 
# Saved as "adult_click_analysis" for switch adult clicking data analyses

library(tidyverse)
library(ggplot2)
library(dplyr)
library(lme4)

#1. Set path and merge all participants' clean_csv into one csv file
# Participants to include
ids <- c(
  "switch_a_200","switch_a_201","switch_a_206","switch_a_207","switch_a_209","switch_a_211","switch_a_0213",
  "swtich_a_214")

# Path and file list
data_dir <- "/Volumes/data/projects/switch/gorilla_csv/clean_csv_for_click_analysis"
csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

# Keep only the csv that match one of the IDs in the file name
files <- csv_files[str_detect(basename(csv_files), str_c(ids, collapse = "|"))]

# Read + merge only those participants
csv <- map_dfr(files, ~ read_csv(.x, show_col_types = FALSE)) %>%
  filter(participant_ID %in% ids) 

csv <- csv %>%
  rename(testing_time_point = phase)

#2.1 Save the csv to NAS switch folder
write_csv(csv, "/Volumes/data/projects/switch/r_outputs/adult_clean.csv")

#2.2. Check mean and range of participants' age
mean_age <- mean(csv$age)
range_age <- range(csv$age)
sd_age <- sd(csv$age)

#3. Define output path and standard figure size
output_dir <- "/Volumes/data/projects/switch/r_outputs"

#3.1 Create another csv with mean click to target instrument
click_TI <- csv %>%
  group_by(testing_time_point) %>%
  summarise(
    mean_click_TI = mean(clickToTI, na.rm = TRUE),
    .groups = "drop"
  )

#4. Visualize participant's click to instrument
click_TI_fig <- ggplot(click_TI, aes(x = factor(testing_time_point), y = mean_click_TI)) +
                  geom_col(width = 0.6) +
                  coord_cartesian(ylim = c(0, 1)) +
                  labs(
                    x = "Testing Time Point",
                    y = "Mean Proportion Click to Target Instrument",
                    title = "Average Click to Target Instrument by Testing Time Point"
                  ) +
                  theme_classic() +
                  theme(
                    plot.title  = element_text(size = 18, hjust = 0.5),
                    axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    axis.title  = element_text(size = 16)
                  )

print(click_TI_fig)
ggsave(file.path(output_dir, "proportion_clickToTI_adult.png"), plot = click_TI_fig, width = 10, height = 8, dpi = 300)

################################################################################
#5. Visualize participant's click to instrument by "cueType"

click_TI_cueType <- csv %>%
  group_by(testing_time_point, cueType) %>%
  summarise(
    mean_click_TI = mean(clickToTI, na.rm = TRUE),
    .groups = "drop"
  )

# Visualize participant's click to instrument
click_TI_cueType_fig <- ggplot(click_TI_cueType, aes(x = factor(testing_time_point), y = mean_click_TI)) +
                          geom_col(width = 0.6) +
                          facet_wrap(~ cueType) +
                          coord_cartesian(ylim = c(0, 1)) +
                          labs(
                            x = "Testing Time Point",
                            y = "Mean Proportion Click to Target Instrument",
                            title = "Average Click to Instrument by Testing Time Point and Training Cue Type"
                          ) +
                          theme_classic() +
                          theme(
                            plot.title  = element_text(size = 18, hjust = 0.5),
                            axis.text.x = element_text(size = 14),
                            axis.text.y = element_text(size = 14),
                            axis.title  = element_text(size = 16)
                          )

print(click_TI_cueType_fig)
ggsave(file.path(output_dir, "proportion_clickToTI_by_cueType_adult.png"), plot = click_TI_cueType_fig, width = 10, height = 6, dpi = 300)

################################################################################
#5.1 Bonus for master's talk (only draw structure_event)

click_TI_structure_event <- csv %>%
  filter(cueType == "structure_event", testing_time_point %in% c(0, 1, 2, 3, 4)) %>%
  group_by(testing_time_point) %>%
  summarise(
    mean_click_TI = mean(clickToTI, na.rm = TRUE),
    se_click_TI = sd(clickToTI, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

click_TI_structure_event_fig <- ggplot(click_TI_structure_event, aes(x = factor(testing_time_point), y = mean_click_TI, group = 1)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 4, color = "steelblue") +
  geom_errorbar(aes(ymin = mean_click_TI - se_click_TI,
                    ymax = mean_click_TI + se_click_TI),
                width = 0.1, color = "steelblue") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Testing Time Point",
    y = "Mean Proportion Click to Target Instrument"
  ) +
  theme_classic() +
  theme(
    plot.title  = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title  = element_text(size = 16)
  )

print(click_TI_structure_event_fig)
ggsave(file.path(output_dir, "proportion_clickToTI_structure_event_line_adult.png"),
       plot = click_TI_structure_event_fig, width = 10, height = 6, dpi = 300)

################################################################################
#6. glmer analysis from baseline (testing time point == 0) to instrument training (phase 1, testing time points 1,2)

baseline_to_phase_1 <- csv %>%
  filter(testing_time_point %in% c(0,1,2)) %>%
  mutate(
    testing_time_point = factor(testing_time_point, levels = c(0,1,2)),
    cueType = factor(cueType)
  )

#sum coding for cueType
baseline_to_phase_1 <- baseline_to_phase_1 %>%
  mutate(cueType_sum = ifelse(cueType == "event", -0.5, 0.5))

glmer_inst_training <- glmer(clickToTI ~ cueType_sum * testing_time_point + (1 | participant_ID),family = binomial, 
                        data = baseline_to_phase_1, control = glmerControl(optimizer = "bobyqa")) 
            ## removed (1 | verbs) as it's variance is zero (isSingular)

summary(glmer_inst_training)

#                                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -3.355638   0.002174 -1543.6  < 2e-16 ***
# cueType_sum                     -0.600960   0.002245  -267.7  < 2e-16 ***
# testing_time_point1              0.436237   0.002174   200.7  < 2e-16 ***
# testing_time_point2              2.180100   0.589218     3.7 0.000216 ***
# cueType_sum:testing_time_point1 -0.974051   0.002173  -448.2  < 2e-16 ***
# cueType_sum:testing_time_point2  0.237739   0.002245   105.9  < 2e-16 ***     

## Calculate the BF value for testing_time_point2

baseline_to_phase_1_bf <- baseline_to_phase_1 %>%
  mutate(time2_vs_baseline =
           case_when(
             testing_time_point == 2 ~ 1,
             testing_time_point == 0 ~ 0,
             TRUE ~ NA_real_
           )) %>%
  filter(!is.na(time2_vs_baseline))

m0 <- glmer(clickToTI ~ cueType_sum +
              (1 | participant_ID) + (1 | verb),
            family = binomial,
            data = baseline_to_phase_1_bf)

m1 <- glmer(clickToTI ~ cueType_sum + time2_vs_baseline +
              (1 | participant_ID) + (1 | verb),
            family = binomial,
            data = baseline_to_phase_1_bf)

bf_time2 <- exp((BIC(m0) - BIC(m1)) / 2)
bf_time2 ## 11.91909

################################################################################
#6. glmer analysis from end of inst training (testing time point == 2) to modifier training (phase 2, testing time points 3,4)

phase1_to_phase2 <- csv %>%
  filter(testing_time_point %in% c(2,3,4)) %>%
  mutate(
    testing_time_point = factor(testing_time_point, levels = c(2,3,4))
    )

phase1_to_phase2 <- phase1_to_phase2 %>%
  mutate(age_c = age - mean(age, na.rm = TRUE))

glmer_mod_training <- glmer(clickToTA ~ testing_time_point * age_c + (1 | participant_ID) + (1 | verb),
    family = binomial, data = phase1_to_phase2, control = glmerControl(optimizer = "bobyqa"))

summary(glmer_mod_training)

#                             Estimate Std. Error z value Pr(>|z|)   
# (Intercept)                   1.5353     1.0064   1.526  0.12710   
# testing_time_point3           1.5363     0.7822   1.964  0.04952 * 
# testing_time_point4          16.9980  5844.4975   0.003  0.99768   
# age_c                        -7.0139     2.6838  -2.613  0.00896 **
# testing_time_point3:age_c     3.4584     2.0183   1.714  0.08661 . 
# testing_time_point4:age_c   -49.3890 21930.5713  -0.002  0.99820  

################################################################################
#8. the relationship between % click to Target Animal and change in click to Target Instrument 
#   from baseline to phase 1 test 2

# Calculate proportions for each participant at each phase (phase0 = baseline, phase1&2 = inst training, phase3&4 = mod training)
phase_proportions <- csv %>%
  group_by(participant_ID, testing_time_point) %>% 
  summarize(
    prop_clickTA = mean(clickToTA, na.rm = TRUE),
    prop_clickTI = mean(clickToTI, na.rm = TRUE),
    prop_clickDA = mean(clickToDA, na.rm = TRUE),
    prop_clickDI = mean(clickToDI, na.rm = TRUE),
    .groups = 'drop'
  )

# Get testing time point == 0 (baseline) data
testing_time_point_0_data <- phase_proportions %>%
        filter(testing_time_point == 0) %>%
        select(participant_ID, 
               baseline_clickTA = prop_clickTA,
               baseline_clickTI = prop_clickTI)

# Get testing time point 2 data (instrument training test 2)
testing_time_point_2_data <- phase_proportions %>%
      filter(testing_time_point == 2) %>%
      select(participant_ID, 
             phase2_clickTI = prop_clickTI)

# Merge and Calculate change
graph1_data <- testing_time_point_0_data %>%
  inner_join(testing_time_point_2_data, by = "participant_ID") %>%
  mutate(change_clickTI = phase2_clickTI - baseline_clickTI)

# Statistical tests for Graph 1
cor_test1 <- cor.test(graph1_data$baseline_clickTA, graph1_data$change_clickTI, method = "spearman")

# Create Graph 1
p1 <- ggplot(graph1_data, aes(x = baseline_clickTA, y = change_clickTI)) +
  geom_jitter(size = 3, alpha = 0.6, color = "steelblue", width = 0.02, height = 0.02) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  annotate("text", x = min(graph1_data$baseline_clickTA) + 0.05, 
           y = max(graph1_data$change_clickTI) - 0.05,
           label = sprintf("rho = %.3f\np = %.3f", cor_test1$estimate, cor_test1$p.value),
           hjust = 0, vjust = 1, size = 4.5, 
           fontface = "bold") +
  labs(
    title = "Effect of Instrument Training on Click to Target Instrument",
    x = "Proportion Click to Target Animal at Baseline (time point 0)",
    y = "Change in Click to Target Instrument\n(time point 2 - time point 0)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    axis.text = element_text(size = 17),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    axis.title = element_text(size = 17),
    panel.grid.minor = element_blank()
  )

print(p1)
ggsave(file.path(output_dir, "baseline_to_inst_training_adult.png"), plot = p1, width = 8, height = 6, dpi = 300)

################################################################################
library(ggrepel)

#9. Draw p1 by differentiating the cuetypes (event vs. event_structure)
phase_proportions_cue <- csv %>%
  group_by(participant_ID, testing_time_point, cueType) %>%
  summarize(
    prop_clickTA = mean(clickToTA, na.rm = TRUE),
    prop_clickTI = mean(clickToTI, na.rm = TRUE),
    prop_clickDA = mean(clickToDA, na.rm = TRUE),
    prop_clickDI = mean(clickToDI, na.rm = TRUE),
    .groups = 'drop'
  )

testing_time_point_0_data_cue <- phase_proportions_cue %>%
  filter(testing_time_point == 0) %>%
  select(participant_ID, cueType,
         baseline_clickTA = prop_clickTA,
         baseline_clickTI = prop_clickTI)

# Phase 2 by cueType
testing_time_point_2_data_cue <- phase_proportions_cue %>%
  filter(testing_time_point == 2) %>%
  select(participant_ID, cueType,
         phase2_clickTI = prop_clickTI,
         phase2_clickTA = prop_clickTA)

# Graph 1 split by cueType (baseline clickTA → change in clickTI)

graph1_cue <- testing_time_point_0_data_cue %>%
  inner_join(testing_time_point_2_data_cue %>% select(participant_ID, cueType, phase2_clickTI),
             by = c("participant_ID", "cueType")) %>%
  mutate(change_clickTI = phase2_clickTI - baseline_clickTI)

plot_p1_by_cue <- function(data, cue_label, point_color = "steelblue") {
  df <- data %>% filter(cueType == cue_label)
  cor_r <- cor.test(df$baseline_clickTA, df$change_clickTI, method = "spearman", exact = FALSE)
  
  ggplot(df, aes(x = baseline_clickTA, y = change_clickTI)) +
    geom_jitter(size = 3, alpha = 0.6, color = point_color, width = 0.02, height = 0.02) +
    geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    annotate("text",
             x = min(df$baseline_clickTA) + 0.05,
             y = max(df$change_clickTI) - 0.05,
             label = sprintf("rho = %.3f\np = %.3f", cor_r$estimate, cor_r$p.value),
             hjust = 0, vjust = 1, size = 7, fontface = "bold") +
    labs(
      title = sprintf("Effect of Instrument Training on Click to Target Instrument \n[cueType: %s]", cue_label),
      x = "Proportion Click to Target Animal at Baseline \n(Time Point 0)",
      y = "Change in Click to Target Instrument\n(Time Point 2 - Time Point 0)"
    ) +
    coord_cartesian(ylim = c(-1, 1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 17),
      axis.text = element_text(size = 17),
      plot.subtitle = element_text(size = 13, color = "gray40"),
      axis.title = element_text(size = 17),
      panel.grid.minor = element_blank()
    )
}
set.seed(42)
p1_event <- plot_p1_by_cue(graph1_cue, "event", point_color = "steelblue")
p1_structure_event <- plot_p1_by_cue(graph1_cue, "structure_event", point_color = "darkorange")

print(p1_event)
print(p1_structure_event)

ggsave(file.path(output_dir, "baseline_to_inst_training_event_cue_adult.png"), plot = p1_event, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_dir, "baseline_to_inst_training_structure_event_cue_adult.png"), plot = p1_structure_event, width = 8, height = 6, dpi = 300)

################################################################################
#10. the relationship between % of click to TI at phase 1 test 2 and change in click to TA 
#   from phase 1 test 2 to phase 2 test 2

# Get phase 4 (mod training test 2) data 
testing_time_point_4_data <- phase_proportions %>%
  filter(testing_time_point == 4) %>%
  select(participant_ID, 
         phase4_clickTA = prop_clickTA)


# Merge and calculate change
graph2_data <- testing_time_point_2_data %>% 
  inner_join((phase_proportions %>% filter(testing_time_point ==2) %>%
                select(participant_ID, phase2_clickTA = prop_clickTA)), by = "participant_ID") %>%
  inner_join(testing_time_point_4_data, by = "participant_ID") %>%
  mutate(change_clickTA = phase4_clickTA - phase2_clickTA)

# Statistical test
cor_test2 <- cor.test(graph2_data$phase2_clickTI, graph2_data$change_clickTA, method = "spearman")

# Create Graph 2
p2 <- ggplot(graph2_data, aes(x = phase2_clickTI, y = change_clickTA)) +
  geom_jitter(size = 3, alpha = 0.6, color = "darkgreen", width = 0.02, height = 0.02) +
  geom_smooth(method = "lm", se = TRUE, color = "purple", linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  annotate("text", x = min(graph2_data$phase2_clickTI) + 0.05, 
           y = max(graph2_data$change_clickTA) - 0.05,
           label = sprintf("rho = %.3f\np = %.3f", cor_test2$estimate, cor_test2$p.value),
           hjust = 0, vjust = 1, size = 7, 
           fontface = "bold") +
  labs(
    title = "Effect of Modifier Training on Click to Target Animal",
    x = "Proportion Click to Target Instrument \n(Post-Instrument Training, Time Point 2)",
    y = "Change in Click to Target Animal\n(Time Point 4 - Time Point 2)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    axis.text = element_text(size = 17),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.title = element_text(size = 17),
    panel.grid.minor = element_blank()
  )

print(p2)
ggsave(file.path(output_dir, "inst_to_mod_training_adult.png"), plot = p2, width = 7, height = 6, dpi = 300)

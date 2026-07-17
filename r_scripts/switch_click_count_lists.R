## This code counts the number of study version participants completed
## Written on February 24, 2026 by Heesu Yun

library(dplyr)

data_folder <- "/Volumes/data/projects/switch/gorilla_csv/raw_csv"

participant_ids <- c("017", "018", "019", "021", "024", "035", "027", "022",
                     "029", "031", "026", "042", "025", "028", "030", "033",
                     "046", "044", "037", "040", "034", "038", "047", "050",
                     "051", "049", "052", "053", "041", "054", "048", "055",
                     "061", "062", "064")

file_paths <- file.path(data_folder, paste0("switch_", participant_ids, ".csv"))

participant_lists <- lapply(file_paths, function(fp) {
  read.csv(fp, stringsAsFactors = FALSE) %>%
    select(Participant.Public.ID, randomiser.vsgr) %>%
    distinct() %>%
    slice(1)
}) %>%
  bind_rows()

list_num <- table(participant_lists$randomiser.vsgr)
print(list_num)

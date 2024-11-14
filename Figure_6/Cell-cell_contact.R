library(readxl)
library(dplyr)
library(writexl)

folder_path <- " "

#file_list <- list.files(folder_path, pattern = "JSQH.*_Gen.*_contacts.xlsx", full.names = TRUE)

results <- data.frame(File = character(), Duration = numeric(), Name = character(), stringsAsFactors = FALSE)

for (file in file_list) {
  df <- read_excel(file)
  df_grouped <- df %>%
    group_by(File) %>%
    summarize(
      true_count = sum(Highlight == TRUE),
      total_count = n(),
      Duration = (true_count / total_count) * 100
    )
  results <- rbind(results, data.frame(File = df_grouped$File, Duration = df_grouped$Duration, Name = basename(file)))
}
output_file <- "Durations_16hrs.xlsx"
write_xlsx(results, output_file)

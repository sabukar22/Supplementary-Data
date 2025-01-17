library(celltrackR)
library(dplyr)
library(ggplot2)
library(purrr)
library(ggbeeswarm)
library(rstatix)
library(ggpubr)
library(svglite)
library(writexl)

process_fate <- function(data, ids, label) {
  filtered_data <- data %>% filter(Group_ID %in% ids)
  csv_name <- paste0(label, ".csv")
  write.csv(filtered_data, csv_name, row.names = FALSE)
  
  t <- read.tracks.csv(csv_name,
                       header = TRUE, sep = ',',
                       id.column = "Group_ID",    
                       time.column = "Corrected_Minutes",  
                       pos.columns = c("POSITION_X", "POSITION_Y", "POSITION_Z"), 
                       scale.t = 1)
  
  list_data <- as.list(t)
  label_rep <- rep(label, length(list_data))
  group_ids <- filtered_data$Group_ID  
  
  list(list_data = list_data, label_rep = label_rep, group_ids = group_ids)
}

P <- read_excel('ALL_tracks.xlsx')  %>%
  filter(Movie =='JSQH176')
#  filter(Fate =='LV')

P <- P %>%
  mutate(Group_ID = paste(Fate, LABEL, TRACK_ID, sep = "_"))

df <- P %>%
  select(Group_ID, Fate, Potency, Movie)

fate_groups <- df %>%
  group_by(Fate) %>%
  summarise(Group = list(unique(Group_ID)))

groups <- split(fate_groups$Group, fate_groups$Fate)
groups <- lapply(groups, unlist, recursive = FALSE)

all_tracks <- list()
real_celltype <- c()
group_ids <- c()  
for (label in names(groups)) {
  result <- process_fate(P, groups[[label]], label)
  all_tracks <- c(all_tracks, result$list_data)
  real_celltype <- c(real_celltype, result$label_rep)
  group_ids <- c(group_ids, result$group_ids)  
}

unique_group_ids <- unique(group_ids) 

all_speeds <- sapply(all_tracks, speed)


D <- data.frame(
  Track_Name = names(all_tracks),  
  Average_Speed = all_speeds
)

print(D)

write_xlsx(D, "JSQH176_speed.xlsx")

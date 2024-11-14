#setwd("D:/00_1st_Year_PhD/00_Manuscript_2/Data/JSQH176/DISPERSION/Dispersion_Migration")
#setwd("D:/00_1st_Year_PhD/00_Manuscript_2/Data/JSQH176/DISPERSION/Dispersion_Migration")
setwd("D:/00_1st_Year_PhD/00_Manuscript_2/Plots/DISPERSION")

library(readxl)
library(writexl)

input_file <- "Control_HomHet_data.xlsx"
sheet_names <- excel_sheets(input_file)
filtered_data <- data.frame()

for (sheet in sheet_names) {
  data <- read_excel(input_file, sheet = sheet)
  
  if (all(c("POSITION_T", "LABEL", "POSITION_X", "POSITION_Y", "POSITION_Z") %in% names(data))) {
    filtered_rows <- data[data$POSITION_T == 440, ]
    
    filtered_data <- rbind(filtered_data, filtered_rows)
  }
}

output_file <- "filtered_data.xlsx"
write_xlsx(filtered_data, output_file)
data_D1 <- filtered_data[grepl("^D1", filtered_data$LABEL), ]
data_D2 <- filtered_data[grepl("^D2", filtered_data$LABEL), ]

coords_D1 <- data_D1[, c("POSITION_X", "POSITION_Y", "POSITION_Z")]
labels_D1 <- data_D1$LABEL
coords_D2 <- data_D2[, c("POSITION_X", "POSITION_Y", "POSITION_Z")]
labels_D2 <- data_D2$LABEL

distances_D1 <- as.matrix(dist(coords_D1, method = "euclidean"))
distances_D2 <- as.matrix(dist(coords_D2, method = "euclidean"))
distances_all <- as.matrix(dist(filtered_data[, c("POSITION_X", "POSITION_Y", "POSITION_Z")], method = 'euclidean'))

distances_D1 <- distances_D1 * 0.347
distances_D2 <- distances_D2 * 0.347
distances_all <- distances_all * 0.347

labels_all <- filtered_data$LABEL

format_distances <- function(distance_matrix, labels) {
  num_points <- length(labels)
  if (num_points < 2) {
    return(data.frame(Pair = character(), Distance = numeric()))
  }
  
  distance_list <- list()
  
  for (i in 1:(num_points - 1)) {
    for (j in (i + 1):num_points) {
      distance_list[[length(distance_list) + 1]] <- c(
        paste(labels[i], "-", labels[j]),
        distance_matrix[i, j]
      )
    }
  }
  
  result <- do.call(rbind, distance_list)
  return(as.data.frame(result, stringsAsFactors = FALSE, row.names = NULL))
}

formatted_distances_D1 <- format_distances(distances_D1, labels_D1)
formatted_distances_D2 <- format_distances(distances_D2, labels_D2)
formatted_distances_all <- format_distances(distances_all, labels_all)

colnames(formatted_distances_D1) <- c("Pair", "Distance")
colnames(formatted_distances_D2) <- c("Pair", "Distance")
colnames(formatted_distances_all) <- c("Pair", "Distance")

output_distances_file <- "HomHet_distances.xlsx"
write_xlsx(list(
  "Distances_D1" = formatted_distances_D1,
  "Distances_D2" = formatted_distances_D2,
  "Distances_All" = formatted_distances_all
), output_distances_file)

print(distances_all)


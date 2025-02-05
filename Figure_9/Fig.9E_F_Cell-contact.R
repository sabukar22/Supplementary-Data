library(openxlsx)
library(readxl)

folder_path <- " "
file_list <- list.files(path = folder_path, pattern = "^Track_\\d+\\.xlsx$", full.names = TRUE)

combined_distances <- data.frame(Time=numeric(), Index_Sheet1=integer(), Index_Sheet2=integer(), 
                                 Distance=numeric(), Highlight=logical(), File=character(), stringsAsFactors=FALSE)

threshold <- 13

for (file in file_list) {
  sheet_names <- excel_sheets(file)
  if ("D11" %in% sheet_names & "D12" %in% sheet_names) {
    data_sheet1 <- read_excel(file, sheet = "D11")
    data_sheet2 <- read_excel(file, sheet = "D12")

    data_sheet1 <- data_sheet1[data_sheet1$POSITION_T <= 480, ]
    data_sheet2 <- data_sheet2[data_sheet2$POSITION_T <= 480, ]
    
    if ((max(data_sheet1$POSITION_T) - min(data_sheet1$POSITION_T)) >= 120 && 
        (max(data_sheet2$POSITION_T) - min(data_sheet2$POSITION_T)) >= 120) {
      
      distances <- data.frame(Time=numeric(), Index_Sheet1=integer(), Index_Sheet2=integer(), 
                              Distance=numeric(), Highlight=logical(), stringsAsFactors=FALSE)
      for (i in 1:nrow(data_sheet1)) {
        for (j in 1:nrow(data_sheet2)) {
          if (data_sheet1$POSITION_T[i] == data_sheet2$POSITION_T[j]) {
            distance <- (sqrt((data_sheet1$POSITION_X[i] - data_sheet2$POSITION_X[j])^2 + 
                                (data_sheet1$POSITION_Y[i] - data_sheet2$POSITION_Y[j])^2 + 
                                (data_sheet1$POSITION_Z[i] - data_sheet2$POSITION_Z[j])^2)) * 0.347
            highlight <- distance < threshold
            new_row <- data.frame(Time=data_sheet1$POSITION_T[i], Index_Sheet1=i, Index_Sheet2=j, 
                                  Distance=distance, Highlight=highlight, File=basename(file), stringsAsFactors=FALSE)
            distances <- rbind(distances, new_row)
          }
        }
      }
      
      combined_distances <- rbind(combined_distances, distances)
    } else {
      message(paste("Skipping file:", basename(file), "- Time range less than 120"))
    }
  } else {
    message(paste("Skipping file:", basename(file), "- Sheets not found"))
  }
}

write.xlsx(combined_distances, "combined_distances.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Combined Distances")

writeData(wb, sheet = 1, combined_distances)

conditionalFormatting(wb, 1, cols = 4, rows = 1:(nrow(combined_distances) + 1), rule = paste0("<", threshold), style = createStyle(fontColour = "#FF0000", bgFill = "#FFFF00"))

saveWorkbook(wb, "test.xlsx", overwrite = TRUE)

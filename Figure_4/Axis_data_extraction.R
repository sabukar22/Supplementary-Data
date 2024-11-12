setwd("D:/00_1st_Year_PhD/00_Manuscript_2/Live-imaging-cell-analysis/AP axis/")

library(readxl) 
library(dplyr)  

#this is the 'Track_info' excel that tells us which tips are GFP+ve and which aren't
df1 <- read_excel("D:/00_1st_Year_PhD/00_Manuscript_2/Track_info_FINAL.xlsx", sheet = 5)

#this is the excel with all the XYZT info for each cell
df2 <- read.csv("D:/00_1st_Year_PhD/00_Manuscript_2/Live-imaging-cell-analysis/Raw_track_data_processing/JSQH173_tracks_processed.csv")

df1_filtered <- df1 %>% filter(PLUS == "YES")

merged_df <- merge(df1_filtered, df2, by = c("TRACK_ID", "LABEL"))

final_df <- merged_df %>% filter(POSITION_T == 700)


print(final_df)

write.xlsx(final_df, "JSQH173_GFP+ve_TP700.xlsx")

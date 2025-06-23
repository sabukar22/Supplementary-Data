setwd("D:/00_1st_Year_PhD/00_Manuscript_2/Live-imaging-cell-analysis/Lineage Tree/Lineage_tree_code_UPDATE")

library(celltrackR)
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(openxlsx)
library(ggtree)
library(ape)
library(phytools)
library(treeio)
library(tidytree)
library(gridExtra)
library(svglite)

#Input files/directories
input_file <- "path/to/Movie1.xlsx"
input_directory <- "path/to/Input"
output_directory <- "path/to/Output"
base_name <- tools::file_path_sans_ext(basename(input_file))
processed_file <- paste0(base_name, "_processed.csv")

##Outputs the processed excel file
P <- read_excel(input_file)
P <- P[c('LABEL', 'TRACK_ID', 'POSITION_X', 'POSITION_Y', 'POSITION_Z', 'POSITION_T', 'MEAN_INTENSITY_CH1')]
P$MEAN_INTENSITY_CH1 <- as.numeric(as.character(P$MEAN_INTENSITY_CH1))
P <- na.omit(P)
max_intensity <- max(P$MEAN_INTENSITY_CH1, na.rm = TRUE) 
P$MEAN_INTENSITY_CH1 <- (P$MEAN_INTENSITY_CH1 / max_intensity) * 100 
write.csv(P, processed_file, row.names = FALSE)

##Individual files for each track
data <- read_csv(processed_file, show_col_types = FALSE)
data_split <- split(data, data$TRACK_ID)
for (track_id in names(data_split)) {
  file_name <- file.path(input_directory, paste0("Track_", track_id, ".xlsx"))
  write.xlsx(data_split[[track_id]], file_name)
}

file_list <- list.files(path = input_directory, pattern = "^Track_\\d+\\.xlsx$", full.names = TRUE)

process_file <- function(file_name) {
  data <- read_excel(file_name)
  filtered_data <- data %>% 
    filter(POSITION_T %% 5 == 0)
  
  data_groups <- filtered_data %>% 
    group_by(LABEL) %>%
    group_split()

  wb <- createWorkbook()
  for (i in seq_along(data_groups)) {
    sheet_name <- ifelse(is.na(data_groups[[i]]$LABEL[1]) | data_groups[[i]]$LABEL[1] == "", 
                         paste("Sheet", i), 
                         as.character(data_groups[[i]]$LABEL[1]))
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, x = data_groups[[i]])
  }

  saveWorkbook(wb, file_name, overwrite = TRUE)
}

lapply(file_list, process_file)

cat("All files processed successfully.\n")



##LINEAGE TREE PROCESSING
process_track_file <- function(input_excel_path, output_excel_path1, output_excel_path2) {
  sheets <- getSheetNames(input_excel_path)
  
  data_gfp <- data.frame(Cell_Name = character(), Highest_GFP_Signal = numeric(), Timepoint = numeric(), stringsAsFactors = FALSE)
  data_lifetime <- data.frame(Cell_Name = character(), Lifetime = numeric(), stringsAsFactors = FALSE)
  
  origin_value <- NA
  
  for (sheet_name in sheets) {
    df <- read.xlsx(input_excel_path, sheet = sheet_name)
    
    max_gfp_row <- df[which.max(df$MEAN_INTENSITY_CH1),]
    max_gfp_value <- max_gfp_row$MEAN_INTENSITY_CH1
    timepoint <- max_gfp_row$POSITION_T
    
    data_gfp <- rbind(data_gfp, data.frame(Cell_Name = sheet_name, Highest_GFP_Signal = max_gfp_value, Timepoint = timepoint))
    
    lifespan <- max(df$POSITION_T) - min(df$POSITION_T)
    lifespan_minutes <- lifespan * 2
    
    data_lifetime <- rbind(data_lifetime, data.frame(Cell_Name = sheet_name, Lifetime = lifespan_minutes))
    
    if (sheet_name == 'Mother') {
      origin_value <- min(df$POSITION_T) * 2
    }
  }
  
  if (!is.na(origin_value)) {
    data_lifetime <- rbind(data_lifetime, data.frame(Cell_Name = 'Origin', Lifetime = origin_value))
  } else {
    data_lifetime <- rbind(data_lifetime, data.frame(Cell_Name = 'Origin', Lifetime = NA))
  }
  
  write.xlsx(data_gfp, output_excel_path1, rowNames = FALSE)
  write.xlsx(data_lifetime, output_excel_path2, rowNames = FALSE)
  
  cat(sprintf('Done! The summarized data has been written to %s\n', output_excel_path1))
  cat(sprintf('Summarized data has been saved to %s\n', output_excel_path2))
}


input_files <- list.files(path = input_directory, pattern = 'Track_.*\\.xlsx', full.names = TRUE)
cat(sprintf('Found %d files.\n', length(input_files)))

for (input_file in input_files) {
  base_name <- tools::file_path_sans_ext(basename(input_file))
  
  output_excel_path1 <- file.path(output_directory, paste0(base_name, "_GFP.xlsx"))
  output_excel_path2 <- file.path(output_directory, paste0(base_name, "_tree_length.xlsx"))
  
  process_track_file(input_file, output_excel_path1, output_excel_path2)
}

cat("All lineage tree processing completed.\n")



##NEWICK FORMATTER
log_file <- "error_log.txt"

process_file <- function(file_path, output_directory) {
  tryCatch({
    df <- read_excel(file_path)
    branch_lengths <- setNames(df[[2]], df[[1]])
    if (any(is.na(branch_lengths))) {
      stop("Some branch lengths are missing. Please check the data.")
    }
    
    create_branch <- function(node, children) {
      if (!node %in% names(branch_lengths)) {
        return(NULL)
      }
      if (is.null(children) || length(children) == 0) {
        return(paste0(node, ":", branch_lengths[node]))
      }
      children_strings <- lapply(children, function(child) {
        subchildren <- sub_nodes[[child]]
        create_branch(child, subchildren)
      })
      
      children_strings <- Filter(Negate(is.null), children_strings)
      
      if (length(children_strings) == 0) {
        return(paste0(node, ":", branch_lengths[node]))
      }
      
      combined_children <- paste(children_strings, collapse = ",")
      
      return(paste0("(", combined_children, ")", node, ":", branch_lengths[node]))
    }
    
    root_node <- "Origin"
    sub_nodes <- list(
      Origin = c("Mother"),
      Mother = c("D1", "D2"),
      D1 = c("D11", "D12"),
      D2 = c("D21", "D22"),
      D11 = c("D111", "D112"),
      D12 = c("D121", "D122"),
      D21 = c("D211", "D212"),
      D22 = c("D221", "D222"),
      D111 = c("D1111", "D1112"),
      D112 = c("D1121", "D1122"),
      D121 = c("D1211", "D1212"),
      D122 = c("D1221", "D1222"),
      D211 = c("D2111", "D2112"),
      D212 = c("D2121", "D2122"),
      D221 = c("D2211", "D2212"),
      D222 = c("D2221", "D2222"),
      D1111 = c("D11111", "D11112"),
      D1112 = c("D11121", "D11122"),
      D1121 = c("D11211", "D11212"),
      D1122 = c("D11221", "D11222"),
      D1211 = c("D12111", "D12112"),
      D1212 = c("D12121", "D12122"),
      D1221 = c("D12211", "D12212"),
      D1222 = c("D12221", "D12222"),
      D2111 = c("D21111", "D21112"),
      D2112 = c("D21121", "D21122"),
      D2121 = c("D21211", "D21212"),
      D2122 = c("D21221", "D21222"),
      D2211 = c("D22111", "D22112"),
      D2212 = c("D22121", "D22122"),
      D2221 = c("D22211", "D22212"),
      D2222 = c("D22221", "D22222")
    )
    
    newick_string <- create_branch(root_node, sub_nodes[[root_node]])
    if (is.null(newick_string)) {
      stop("Invalid tree structure. Missing required nodes.")
    }
    newick_string <- paste0("(", newick_string, ");")
    
    file_number <- gsub("Track_|_tree_length.xlsx", "", basename(file_path))
    output_file <- file.path(output_directory, paste0("Tree_", file_number, ".tre"))
    
    cat("Newick string:\n", newick_string, "\n")
    write.table(newick_string, file = output_file, sep = "\t",
                row.names = FALSE, quote = FALSE, col.names = FALSE)
    
    tree <- read.tree(text = newick_string)
    print(newick_string)
    print(tree)
    
    plot(tree, show.tip.label = TRUE)
    
    ggtree(tree) +
      geom_tiplab() +
      theme_tree2() +
      xlim(0, NA)
  }, error = function(e) {
    message <- paste0("Error in file ", file_path, ": ", e$message, "\n")
    cat(message, file = log_file, append = TRUE)
    cat(message)
  })
}

file_list <- list.files(output_directory, pattern = "Track_\\d+_tree_length\\.xlsx", full.names = TRUE)

for (file_path in file_list) {
  process_file(file_path, output_directory)
}

##Tips and nodes generation
tree_files <- list.files(path = output_directory, pattern = "Tree_.*\\.tre$", full.names = TRUE)
gfp_files <- list.files(path = output_directory, pattern = "Track_.*_GFP\\.xlsx$")

print(tree_files)
print(gfp_files)

for (i in seq_along(tree_files)) {
  track_number <- sub(".*_(\\d+).*", "\\1", tree_files[i])
  gfp_file <- file.path(output_directory, paste0("Track_", track_number, "_GFP.xlsx"))
  print(gfp_file)
  print(tree_file)
  
  if (file.exists(tree_files[i]) && file.exists(gfp_file)) {
    newick_tree <- read.tree(tree_files[i])
    data <- read.xlsx(gfp_file, sheet = 1)
    
    tips <- newick_tree$tip.label
    all_nodes <- unique(c(newick_tree$node.label, newick_tree$edge[, 1]))
    nodes <- setdiff(all_nodes, tips)
    nodes <- nodes[nodes != ""]  
    
    species_df <- data %>% 
      filter(Cell_Name %in% tips) %>%
      mutate(Species = 1:n(), svl = Highest_GFP_Signal) %>%
      select(Species, svl)  
    
    next_species_number <- max(species_df$Species) + 1
    
    new_rows <- data.frame(Species = c(next_species_number, next_species_number + 1), svl = c(100, 0))
    nodes_df <- data %>% 
      filter(Cell_Name %in% nodes) %>%
      mutate(Species = (next_species_number + 2):(next_species_number + 2 + n() - 1), 
             svl = Highest_GFP_Signal) %>%
      select(Species, svl)
    nodes_df <- bind_rows(new_rows, nodes_df)
    
    write.csv(species_df, file = file.path(output_directory, paste0(track_number, "_T.csv")), row.names = FALSE)  
    write.csv(nodes_df, file = file.path(output_directory, paste0(track_number, "_N.csv")), row.names = FALSE)  
    print(paste("T and N files for", paste0("Track_", track_number), "were generated!"))
  } else print(paste("T and N files for", paste0("Track_", track_number), "were not generated :("))
}



##PLOTS THE LINEAGE TREES WITH GFP INFO
tree_trait_pairs <- list()
for (tree_file in tree_files) {
  track_number <- sub(".*_(\\d+).*", "\\1", tree_file)
  trait_T_file <- file.path(output_directory, paste0(track_number, "_T.csv"))
  trait_N_file <- file.path(output_directory, paste0(track_number, "_N.csv"))
  
  if (file.exists(trait_T_file) && file.exists(trait_N_file)) {
    tree_trait_pairs[[length(tree_trait_pairs) + 1]] <- list(
      tree_file = tree_file,
      trait_T_file = trait_T_file,
      trait_N_file = trait_N_file
    )
  } else print(paste("Error with", paste0("Track_", track_number)))
}


process_tree <- function(tree_file, trait_T_file, trait_N_file) {
  tree <- read.tree(tree_file)
  svl_T <- read.csv(trait_T_file, row.name = 1)
  svl_T <- as.matrix(svl_T)[, 1]
  
  svl_N <- read.csv(trait_N_file, row.names = 1)
  svl_N <- as.matrix(svl_N)[, 1]
  
  td <- data.frame(node = names(svl_T), trait = svl_T)
  nd <- data.frame(node = names(svl_N), trait = svl_N)
  d <- rbind(td, nd)
  d$node <- as.numeric(d$node)
  
  tree_F <- full_join(tree, d, by = 'node')
  
  p <- ggtree(tree_F, aes(color = trait), layout = 'rectangular', 
              ladderize = FALSE, continuous = 'colour', size = 0.5) +
    scale_color_gradientn(colours = c('#3a2f6b', '#36669c', '#41a0ae', '#3ec995', '#77f07f'), limits = c(0, 100)) + 
    xlim(0, 3000) + 
    geom_tiplab(hjust = -1, size = 5, color = '#bddf26') +
    theme(legend.position = c(5, .85)) +
    theme (
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent')
    ) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  return(p)
}

plots <- lapply(tree_trait_pairs, function(pair) {
  process_tree(pair$tree_file, pair$trait_T_file, pair$trait_N_file)
})

combined_plot <- do.call(grid.arrange, c(plots, ncol = 1))
ggsave('Lineage_tree.svg', combined_plot, bg = 'transparent', width = 25, height = 5 * length(plots), limitsize = FALSE)

cat("Processing complete. Combined plot saved as 'Lineage_tree.svg'.\n")



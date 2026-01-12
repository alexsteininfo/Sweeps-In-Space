Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

# Initialize summary dataframe
summary_df <- data.frame(
  index = character(),
  sweep_complete_time = numeric(),
  sweep_complete_size=character(),
  sweep_complete_ID = numeric(),
  stringsAsFactors = FALSE
)

# Function to check if a sweep is present
sweep <- function(df, x) ifelse(max(df$Frequency) >= x, TRUE, FALSE)

# Recursive function to get all descendants of a genotype
get_descendants <- function(genotype_id, tree) {
  descendants <- tree$Identity[tree$Parent == genotype_id]
  all_descendants <- descendants
  for (desc in descendants) {
    all_descendants <- c(all_descendants, get_descendants(desc, tree))
  }
  return(unique(all_descendants))
}

# Function to find the first sweep completion time
find_first_sweep_time <- function(df) {
  time_steps <- sort(unique(df$Generation))
  
  for (t in time_steps) {
    current_df <- df[df$Generation == t, ]
    total_pop <- sum(current_df$Population)
    genotypes <- unique(current_df$Identity)
    
    for (g in genotypes) {
      if (g == 0) next
      
      descendants <- get_descendants(g, current_df)
      relevant_ids <- c(g, descendants)
      pop_sum <- sum(current_df$Population[current_df$Identity %in% relevant_ids])
      
      if (pop_sum == total_pop) {
        return(list(time = t, size = total_pop, genotype = g))
      }
    }
  }
  
  return(NULL)
}

# Function to find all sweeps sequentially
find_all_sweeps <- function(df, max_sweeps = Inf) {
  remaining_df <- df
  sweep_results <- list()
  swept_ids <- c()
  n <- 0
  max_repeats <- 10

  repeat{
    if (length(sweep_results) >= max_sweeps) break
    if (n >= max_repeats) break 
   result <- find_first_sweep_time(remaining_df)
    print(result)

    if (is.null(result)) break
    
    sweep_results[[length(sweep_results) + 1]] <- result
    
    swept_ids <- c(swept_ids, result$genotype)
    remaining_df <- remaining_df[!remaining_df$Identity %in% swept_ids, ]
    n <- n + 1 
print(n)
 }
  
  return(sweep_results)
}

# Function to process a single simulation folder
get_sweep_complete_times <- function(configid, path){
  driver_phylo <- read_delim(
    paste(path, "/driver_phylo.dat", sep=""),
    "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols()
  )
  
  output_driver_genotype_properties <- read_delim(
    paste(path, "/output_driver_genotype_properties.dat", sep=""),
    "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols()
  )
  
  output_driver_genotype_properties <- mutate(
    output_driver_genotype_properties,
    Frequency = Descendants / max(Descendants)
  )

  output_driver_genotype_properties_f <- filter(output_driver_genotype_properties, Identity > 0)
  first_row_driver_identity <- output_driver_genotype_properties_f$DriverIdentity[1]
  output_driver_genotype_properties_ff <-filter(output_driver_genotype_properties_f, output_driver_genotype_properties_f$Frequency >= 1.0)
  output_driver_genotype_properties_fff <-filter(output_driver_genotype_properties_ff, output_driver_genotype_properties_ff$DriverIdentity >= first_row_driver_identity)
  sweeps_present <- nrow(output_driver_genotype_properties_fff)
  
  if (sweeps_present > 0) {
    sweep_list <- find_all_sweeps(driver_phylo, max_sweeps = sweeps_present)
  } else {
    sweep_list <- list(list(time = 0, size = 0, genotype = 0))
  }
    
  print(configid)
 
  return(sweep_list)
}

# List simulation folders
simulation_paths <- Sys.glob(file.path(
  "/users/adhh339/sharedscratch/warlock/outputsrevised1000/simulations",
  "*"
))
simulation_ids <- sapply(strsplit(simulation_paths, "/"), tail, 1)

# Run analysis for each simulation
for (configid in simulation_ids){
  sweep_list <- get_sweep_complete_times(
    configid,
    paste(
      "/users/adhh339/sharedscratch/warlock/outputsrevised1000/simulations/",
      configid,
      sep=""
    )
  )
  
  for (i in seq_along(sweep_list)) {
    sweep_complete_time <- sweep_list[[i]]$time
    sweep_complete_size <- sweep_list[[i]]$sweep_complete_size
    sweep_complete_ID <- sweep_list[[i]]$genotype
    

    summary_df[nrow(summary_df) + 1, ] <- list(
	paste(configid, "_sweep", i, sep=""),
	sweep_list[[i]]$time,
	sweep_list[[i]]$size,
	sweep_list[[i]]$genotype
)
  }
  
  if(nrow(summary_df) %% 100 ==0){
    write.csv(
      summary_df, "/users/adhh339/sharedscratch/warlock/outputsrevised1000/all_sweep_complete_times_Umuts_main_temp.csv"
    )
  }
}

# Write results to CSV
write.csv(
  summary_df,
  "/users/adhh339/sharedscratch/warlock/outputsrevised1000/all_sweep_complete_times_Umuts_main.csv",
  row.names = FALSE
)

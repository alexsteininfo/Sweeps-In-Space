Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

sweep_complete_times_df <- read_csv("/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/Figure 3/005_data/unlimited/sweep_complete_times005_sweep90.csv")

setwd("/Volumes/Seagate/005_data/unlimited/simulations")

summary_df <- data.frame(
  id=character(),
  sweep_complete_time=character(),
  num_cell=character(),
  stringsAsFactors=FALSE
) 

sweep <- function(df, x) ifelse(max(df$Frequency) >= x, TRUE, FALSE)

get_pop_at_sweep_completion <- function(configid, path){

  # To test whether more than one mutant is present at the end of the simulation
  # (in which case a sweep would be unlikely to occur if we kept the simulation running):
  output <- read_delim(
    paste(
      path,
      "/output.dat",
      sep=""
    ),
    "\t", escape_double = FALSE,
    trim_ws = TRUE,
    col_types = cols()
  )

  output_driver_genotype_properties <- read_delim(
    paste(
      path,
      "/output_driver_genotype_properties.dat",
      sep=""
    ),
    "\t", escape_double = FALSE,
    trim_ws = TRUE,
    col_types = cols()
  )
  
 
                            
  sweep_complete_time_df_f <- sweep_complete_times_df %>% filter(index1 == configid)

  sweep_complete_time <- as.numeric(sweep_complete_time_df_f$sweep_complete_time)
  
  num_cells <- output %>% 
    filter(abs(Generation - sweep_complete_time) == min(abs(Generation - sweep_complete_time))) %>%
    select(NumCells) %>% slice(1)
  # The first line ends with slice(1) just in case two rows have the same Generation value
  # (though I don't think this will ever happen). ~Rob

  return(
    list(
      "sweep_complete_time"=sweep_complete_time,
      "num_cells"=num_cells
    )
  )
}

simulation_paths <- Sys.glob(file.path("/Volumes/Seagate/005_data/unlimited/simulations", "*"))
simulation_ids <- sapply(strsplit(simulation_paths, "/"), tail, 1)

for (configid in simulation_ids){
  
  print(configid)
  
  returnlist = get_pop_at_sweep_completion(
    configid,
    paste(
      "",
      configid,
      sep=""
    )
  )
    
  sweep_complete_time = returnlist$sweep_complete_time
  num_cells = returnlist$num_cells
  summary_df[nrow(summary_df) + 1,] <- c(
    configid, sweep_complete_time, num_cells
  )
}

write.csv(
  summary_df,
  "/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/Figure 3/005_data/unlimited/sweeps-complete-cells_005_sweep90.csv"
)

setwd("/Volumes/Seagate/005_data/unlimited/")
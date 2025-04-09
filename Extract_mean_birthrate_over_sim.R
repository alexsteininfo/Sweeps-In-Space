#### Extract average birth rate of all mutated cells ####


Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

summary_df <- data.frame(
  index=character(),
  birth_rate_mean=character(),
  stringsAsFactors=FALSE
) 

#setwd("/Volumes/Seagate Backup Plus Drive/0075_unlimited/simulations")

get_birth_rate <- function(configid, path){
  
  # To extract summary of birth rates from a batch of simulation outputs
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
  
  return(
    list("birth_rate_mean"= mean(output$MeanBirthRate))
  )
}

simulation_paths <- Sys.glob(file.path("/Volumes/Seagate Backup Plus Drive/005_data/1_mut/simulations", "*"))
simulation_ids <- sapply(strsplit(simulation_paths, "/"), tail, 1)

for (configid in simulation_ids){
  
  returnlist = get_birth_rate(
    configid,
    paste(
      "",
      configid,
      sep=""
    )
  )
  
  
  birth_rate_mean = returnlist$birth_rate_mean
  
  
  summary_df[nrow(summary_df) + 1,] <- c(
    configid, birth_rate_mean 
  )
}

write.csv(
  summary_df,
  "/Volumes/Seagate Backup Plus Drive/005_data/1_mut/birth_rates_mean_time.csv"
)


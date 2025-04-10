Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

summary_df <- data.frame(
  index=character(),
  birth_rate_end_mean=character(),
  stringsAsFactors=FALSE
) 

setwd("/Volumes/Seagate Backup Plus Drive/0075_unlimited/simulations")

get_birth_rate <- function(configid, path){
  
  # To extract summary of birth rates from a batch of simulation outputs
  output_driver_genotype <- read_delim(
    paste(
      path,
      "/output_driver_genotype_properties.dat",
      sep=""
    ),
    "\t", escape_double = FALSE,
    trim_ws = TRUE,
    col_types = cols()
  )
  
 output_driver_genotype <- mutate(output_driver_genotype, Pop_x_br = Population * BirthRate)
  
  return(
    list(
      "birth_rate_end_mean"=sum(output_driver_genotype$Pop_x_br)/sum(output_driver_genotype$Population)
    )
  )
}

simulation_paths <- Sys.glob(file.path("/Volumes/Seagate Backup Plus Drive/0075_unlimited/simulations", "*"))
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
  
  
  birth_rate_end_mean = returnlist$birth_rate_end_mean
  
  
  summary_df[nrow(summary_df) + 1,] <- c(
    configid, birth_rate_end_mean 
  )
}

write.csv(
  summary_df,
  "birth_rates_end_mean_all.csv"
)


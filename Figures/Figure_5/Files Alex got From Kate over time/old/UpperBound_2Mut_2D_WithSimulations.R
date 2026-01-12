# title: Plotting sweep probabilities versus exact fitness of sweep ancestor.
# author: Alexander Stein, Kate Bostock

#################
### libraries ###
#################

library("tidyverse")
library("ggtext")
library("RColorBrewer")
library("ggplot2")
library("tidyverse")
library("ggtext")
library("RColorBrewer")
library("gridExtra")
library("cowplot")
library(gridExtra)
library(grid)

library(ggpubr)

### choose graph settings ###
sweep_cutoff <- "x100" # options are: x070, x075, x080, x085, x090, x095, x099, x100

### assumptions ###

m <- 10 # maximum birth rate
b1 <- 1 # base birth rate

###################
### Import data ###
###################

### Simulation data

setwd("/Users/alexanderstein/Documents/Sweeps-project/Sweeps_Kate/")

### Original data - 1 fixed mutation
params <- read_csv("simulation_data_original/params.csv") 
probs <- read_csv("simulation_data_original/sweeps-prob.csv") 

params_high_s <- read_csv("Archive_simulations/simulation_data_high_birth_rates2/params.csv") # extension to single fixed mutation data for s = 5, 10
probs_high_s <- read_csv("Archive_simulations/simulation_data_high_birth_rates2/sweeps-prob-descendants.csv")  # extension to single fixed mutation data for s = 5, 10

param_wt <- read_csv("simulation_data_original/params_wt.csv") 
meas_wt <- read_csv("simulation_data_original/wt_speeds.csv") 

param_mut <- read_csv("simulation_data_original/params_mut_expanded.csv") 
meas_mut <- read_csv("simulation_data_original/mut_speeds_expanded.csv")

### Revised data - unlimited random mutations
params_revised <- read_csv("Revised_sims_used/simulation_data_revised/1000_sims/params.csv") 
probs_revised <- read_csv("Revised_sims_used/simulation_data_revised/1000_sims/sweeps-prob-descendants.csv") 
birth_rates_mean <- read_csv("Revised_sims_used/simulation_data_revised/1000_sims/birth_rates_end_mean.csv") 

params_revised2 <- read_csv("Revised_sims_used/simulation_data_s015/params.csv") 
probs_revised2 <- read_csv("Revised_sims_used/simulation_data_s015/sweeps-prob-descendants.csv") 
birth_rates_mean2 <- read_csv("Revised_sims_used/simulation_data_s015/birth_rates_end_mean_all.csv") 

params_revised3 <- read_csv("Revised_sims_used/005_data/unlimited/params.csv") 
probs_revised3 <- read_csv("Revised_sims_used/005_data/unlimited/sweeps-prob-descendants.csv") 
birth_rates_mean3 <- read_csv("Revised_sims_used/005_data/unlimited/birth_rates_end_mean_all.csv") 

params_revised4 <- read_csv("Revised_sims_used/0075_data/0075_unlimited/params.csv") 
probs_revised4 <- read_csv("Revised_sims_used/0075_data/0075_unlimited/sweeps-prob-descendants.csv") 
birth_rates_mean4 <- read_csv("Revised_sims_used/0075_data/0075_unlimited/birth_rates_end_mean_all.csv") 

### Revised data - 3 random mutations
params_revised_3muts <- read_csv("Revised_sims_used/simulation_data_3muts/params.csv") 
probs_revised_3muts <- read_csv("Revised_sims_used/simulation_data_3muts/sweeps-prob-descendants.csv") 
birth_rates_mean_3muts <- read_csv("Revised_sims_used/simulation_data_3muts/birth_rates_end_mean_all.csv") 

params_revised_3muts2 <- read_csv("Revised_sims_used/005_data/3_mut/params.csv") 
probs_revised_3muts2 <- read_csv("Revised_sims_used/005_data/3_mut/sweeps-prob-descendants.csv") 
birth_rates_mean_3muts2 <- read_csv("Revised_sims_used/005_data/3_mut/birth_rates_end_mean_all.csv") 

params_revised_3muts3 <- read_csv("Revised_sims_used/0075_data/0075_3/params.csv") 
probs_revised_3muts3 <- read_csv("Revised_sims_used/0075_data/0075_3/sweeps-prob-descendants.csv") 
birth_rates_mean_3muts3 <- read_csv("Revised_sims_used/0075_data/0075_3/birth_rates_end_mean_all.csv") 

### Revised data - 2 random mutations
params_revised_2muts <- read_csv("Revised_sims_used/simulation_data_2muts/params.csv") 
probs_revised_2muts <- read_csv("Revised_sims_used/simulation_data_2muts/sweeps-prob-descendants.csv") 
birth_rates_mean_2muts <- read_csv("Revised_sims_used/simulation_data_2muts/birth_rates_end_mean_all.csv") 

params_revised_2muts2 <- read_csv("Revised_sims_used/005_data/2_mut/params.csv") 
probs_revised_2muts2 <- read_csv("Revised_sims_used/005_data/2_mut/sweeps-prob-descendants.csv") 
birth_rates_mean_2muts2 <- read_csv("Revised_sims_used/005_data/2_mut/birth_rates_end_mean_all.csv") 

params_revised_2muts3 <- read_csv("Revised_sims_used/0075_data/0075_2/params.csv") 
probs_revised_2muts3 <- read_csv("Revised_sims_used/0075_data/0075_2/sweeps-prob-descendants.csv") 
birth_rates_mean_2muts3 <- read_csv("Revised_sims_used/0075_data/0075_2/birth_rates_end_mean_all.csv") 

### Revised data - 1 random mutation
params_revised_1mut <- read_csv("Revised_sims_used/simulation_data_1mut/params.csv") 
probs_revised_1mut <- read_csv("Revised_sims_used/simulation_data_1mut/sweeps-prob-descendants.csv") 
birth_rates_mean_1mut <- read_csv("Revised_sims_used/simulation_data_1mut/birth_rates_end_mean_all.csv") 

params_revised_1mut2 <- read_csv("Revised_sims_used/005_data/1_mut/params.csv") 
probs_revised_1mut2 <- read_csv("Revised_sims_used/005_data/1_mut/sweeps-prob-descendants.csv") 
birth_rates_mean_1mut2 <- read_csv("Revised_sims_used/005_data/1_mut/birth_rates_end_mean_all.csv") 

params_revised_1mut3 <- read_csv("Revised_sims_used/0075_data/0075_1/params.csv") 
probs_revised_1mut3 <- read_csv("Revised_sims_used/0075_data/0075_1/sweeps-prob-descendants.csv") 
birth_rates_mean_1mut3 <- read_csv("Revised_sims_used/0075_data/0075_1/birth_rates_end_mean_all.csv") 

# join the data sets
# revised - unlimited mutations
params_revised <- rbind(params_revised, params_revised2)
params_revised <- rbind(params_revised, params_revised3)
params_revised <- rbind(params_revised, params_revised4)
probs_revised <- rbind(probs_revised, probs_revised2)
probs_revised <- rbind(probs_revised, probs_revised3)
probs_revised <- rbind(probs_revised, probs_revised4)
birth_rates_mean <- rbind(birth_rates_mean, birth_rates_mean2)
birth_rates_mean <- rbind(birth_rates_mean, birth_rates_mean3)
birth_rates_mean <- rbind(birth_rates_mean, birth_rates_mean4)

# revised - 3 mutations
params_revised_3muts <- rbind(params_revised_3muts, params_revised_3muts2)
probs_revised_3muts <- rbind(probs_revised_3muts, probs_revised_3muts2)
birth_rates_mean_3muts <- rbind(birth_rates_mean_3muts, birth_rates_mean_3muts2) 

params_revised_3muts <- rbind(params_revised_3muts, params_revised_3muts3)
probs_revised_3muts <- rbind(probs_revised_3muts, probs_revised_3muts3)
birth_rates_mean_3muts <- rbind(birth_rates_mean_3muts, birth_rates_mean_3muts3) 

# revised - 2 mutations
params_revised_2muts <- rbind(params_revised_2muts, params_revised_2muts2)
probs_revised_2muts <- rbind(probs_revised_2muts, probs_revised_2muts2)
birth_rates_mean_2muts <- rbind(birth_rates_mean_2muts, birth_rates_mean_2muts2) 

params_revised_2muts <- rbind(params_revised_2muts, params_revised_2muts3)
probs_revised_2muts <- rbind(probs_revised_2muts, probs_revised_2muts3)
birth_rates_mean_2muts <- rbind(birth_rates_mean_2muts, birth_rates_mean_2muts3) 

# revised - 1 mutation
params_revised_1mut <- rbind(params_revised_1mut, params_revised_1mut2)
probs_revised_1mut <- rbind(probs_revised_1mut, probs_revised_1mut2)
birth_rates_mean_1mut <- rbind(birth_rates_mean_1mut, birth_rates_mean_1mut2) 

params_revised_1mut <- rbind(params_revised_1mut, params_revised_1mut3)
probs_revised_1mut <- rbind(probs_revised_1mut, probs_revised_1mut3)
birth_rates_mean_1mut <- rbind(birth_rates_mean_1mut, birth_rates_mean_1mut3) 

# Numerical solutions
df_num <- read_csv("numerical_data/sweep probability vs speed ratio c_wt 0.152 mutation rate 2.34e-06 simplifying assumption 0.csv")
df_num_expanded <- read_csv("Numerical_results_revised/numerical_results_revised_2.csv")
df_num <- rbind(df_num, df_num_expanded )
df_num <- rename(df_num, c("ratio"="Speed ratio", "twodim"="2D", "threedim"="3D"))

# Some converting things
df_num$twodim <- as.complex(df_num$twodim)
df_num$twodim <- Re(df_num$twodim)

df_num$threedim <- as.complex(df_num$threedim)
df_num$threedim <- Re(df_num$threedim)

# Find the equivalent 's' value given that there is a random element applied to generate the birth rate b2 = b1(1 +s(1 - b1/m)*r)  therefore sr = (b2/b1 - 1)/(1 - b1/m)
birth_rates_mean <- mutate(birth_rates_mean, sr1 = (birth_rate_end_mean/b1 - 1)/(1 - b1/m))
birth_rates_mean_3muts <- mutate(birth_rates_mean_3muts, sr1 = (birth_rate_end_mean/b1 - 1)/(1 - b1/m))
birth_rates_mean_2muts <- mutate(birth_rates_mean_2muts, sr1 = (birth_rate_end_mean/b1 - 1)/(1 - b1/m))
birth_rates_mean_1mut <- mutate(birth_rates_mean_1mut, sr1 = (birth_rate_end_mean/b1 - 1)/(1 - b1/m))

# Merge the parameters with the data dataframe (orginal)
df <- merge(probs, params, by.x = "id" , by.y = "index" )
df_high_s <- merge(probs_high_s, params_high_s, by.x = "id" , by.y = "index" )
df_high_s <- filter(df_high_s, (s_driver_birth == 10 |s_driver_birth == 5))
df <- rbind(df, df_high_s)

df_wt <- merge(meas_wt, param_wt, by.x = "id" , by.y = "index" )
df_mut <- merge(meas_mut, param_mut, by.x = "id" , by.y = "index" )

# Merge the parameters with the data dataframe (revised)
df_revised <- merge(probs_revised, params_revised, by.x = "id" , by.y = "index" )
df_revised <- merge(df_revised, birth_rates_mean, by.x = "id" , by.y = "index" )
df_revised <- df_revised[, !duplicated(as.list(df_revised))]

df_revised_3muts <- merge(probs_revised_3muts, params_revised_3muts, by.x = "id" , by.y = "index" )
df_revised_3muts <- merge(df_revised_3muts, birth_rates_mean_3muts, by.x = "id" , by.y = "index" )
df_revised_3muts <- df_revised_3muts[, !duplicated(as.list(df_revised_3muts))]

df_revised_2muts <- merge(probs_revised_2muts, params_revised_2muts, by.x = "id" , by.y = "index" )
df_revised_2muts <- merge(df_revised_2muts, birth_rates_mean_2muts, by.x = "id" , by.y = "index" )
df_revised_2muts <- df_revised_2muts[, !duplicated(as.list(df_revised_2muts))]

df_revised_1mut <- merge(probs_revised_1mut, params_revised_1mut, by.x = "id" , by.y = "index" )
df_revised_1mut <- merge(df_revised_1mut, birth_rates_mean_1mut, by.x = "id" , by.y = "index" )
df_revised_1mut <- df_revised_1mut[, !duplicated(as.list(df_revised_1mut))]

###########################
### Read out the speeds ###
###########################

# Filter data of interest
df_wt_f <- df_wt %>% filter(log2_deme==4, migration_rate==0.05, 
                            normal_birth_rate>=0.9, normal_birth_rate<=0.95) 
df_mut_f <- df_mut %>% filter(log2_deme==4, migration_rate==0.05) 

c_wt <- mean(df_wt_f$speed_late) 

# Prepare the speed-selection correspondence
s_xaxis = c(0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8,  0.9,  1.0,
            1.1,  1.2,  1.3,  1.4,  1.5,  1.6,  1.7,  1.8,  1.9,  2.0,
            2.1,  2.2,  2.3,  2.4,  2.5,  2.6,  2.7,  2.8,  2.9,  3.0,
            3.1,  3.2,  3.3,  3.4,  3.5,  3.6,  3.7,  3.8,  3.9,  4.0,
            4.1,  4.2,  4.3,  4.4,  4.5,  4.6,  4.7,  4.8,  4.9,  5.0,  
            5.1,  5.2,  5.3,  5.4,  5.5,  5.6,  5.7,  5.8,  5.9,  6.0,
            6.1,  6.2,  6.3,  6.4,  6.5,  6.6,  6.7,  6.8,  6.9,  7.0,
            7.1,  7.2,  7.3,  7.4,  7.5,  7.6,  7.7,  7.8,  7.9,  8.0,
            8.1,  8.2,  8.3,  8.4,  8.5,  8.6,  8.7,  8.8,  8.9,  9.0,
            9.1,  9.2,  9.3,  9.4,  9.5,  9.6,  9.7,  9.8,  9.9, 10.0) # the list of s_driver_birth values in the params_mut file
c_xaxis = c(0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8,  0.9,  1.0,
            1.1,  1.2,  1.3,  1.4,  1.5,  1.6,  1.7,  1.8,  1.9,  2.0,
            2.1,  2.2,  2.3,  2.4,  2.5,  2.6,  2.7,  2.8,  2.9,  3.0,
            3.1,  3.2,  3.3,  3.4,  3.5,  3.6,  3.7,  3.8,  3.9,  4.0,
            4.1,  4.2,  4.3,  4.4,  4.5,  4.6,  4.7,  4.8,  4.9,  5.0,  
            5.1,  5.2,  5.3,  5.4,  5.5,  5.6,  5.7,  5.8,  5.9,  6.0,
            6.1,  6.2,  6.3,  6.4,  6.5,  6.6,  6.7,  6.8,  6.9,  7.0,
            7.1,  7.2,  7.3,  7.4,  7.5,  7.6,  7.7,  7.8,  7.9,  8.0,
            8.1,  8.2,  8.3,  8.4,  8.5,  8.6,  8.7,  8.8,  8.9,  9.0,
            9.1,  9.2,  9.3,  9.4,  9.5,  9.6,  9.7,  9.8,  9.9, 10.0)  # the list of s_driver_birth values in the params_mut file
j=1
for (s in s_xaxis) {
  df_mut_ff <- df_mut_f %>% filter(s_driver_birth==s) 
  c_xaxis[j] <- mean(df_mut_ff$speed_late) 
  j <- j+1
}

####################
### Computations ###
####################

### Define parameters of the simulation
s_wt <- 0.1 

s_vector = c(0.1,0.2,0.3,0.4,0.5,1.0,1.5,2.0,5.0,10.0) 

### Filter data of interest
df_f2 <- df %>% filter(mu_driver_birth == 1e-05) 

xaxis <- s_xaxis/s_wt 
sweep_ana <- ((c_xaxis-c_wt)/c_xaxis)^2

correction <- 1/sqrt(2+s_xaxis)
sweep_ana2 <- (1 - c_wt/c_xaxis*correction )^2


# Compute the sweep probabilities from data

# Original data
y_data2 <- c(0,0,0,0,0,0,0,0,0,0)
x_data2 <- s_vector/s_wt 
i2=1
for (s in s_vector) {
  df_ff <- df_f2 %>% filter(s_driver_birth == s)  
  logical <- select(df_ff,sweep_cutoff)                          
  prob <- sum(logical)/1000                      
  y_data2[i2] <- prob                             
  i2 <- i2+1
}

# Revised data
y_data3 <- c(0,0,0,0,0,0,0,0) 
s_mean_end_sim_unlimited <-c()
i3=1

b_bins <- c(0.05, 0.075, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2)
b_bins_plotting <- b_bins / s_wt

for (b in b_bins) {
  df_ff <- df_revised %>% filter(s_driver_birth == b) #filter()
  logical <- select(df_ff,sweep_cutoff)
  prob <- sum(logical)/nrow(df_ff)
  y_data3[i3] <- prob
  s_mean_end_sim_unlimited[i3] <- mean(df_ff$sr1)
  i3 <- i3+1
}

# Revised data - 3 mutations only
y_data4 <- c(0,0,0,0,0,0,0,0)
s_mean_end_sim_3muts <-c()
i4=1

for (b in b_bins) {
  df_ff <- df_revised_3muts %>% filter(s_driver_birth == b)
  logical <- select(df_ff,sweep_cutoff)
  prob <- sum(logical)/nrow(df_ff)
  y_data4[i4] <- prob
  s_mean_end_sim_3muts[i4] <- mean(df_ff$sr1)
  i4 <- i4+1
}

# Revised data - 2 mutations only
y_data5 <- c(0,0,0,0,0,0,0,0)   
s_mean_end_sim_2muts <-c()
i5=1

for (b in b_bins) {
  df_ff <- df_revised_2muts %>% filter(s_driver_birth == b)
  logical <- select(df_ff,sweep_cutoff)
  prob <- sum(logical)/nrow(df_ff)
  y_data5[i5] <- prob
  s_mean_end_sim_2muts[i5] <- mean(df_ff$sr1)
  i5 <- i5+1
}

# Revised data - 1 mutation only
y_data6 <- c(0,0,0,0,0,0,0,0)    
s_mean_end_sim_1muts <-c()
i6=1

for (b in b_bins) {
  df_ff <- df_revised_1mut %>% filter(s_driver_birth == b)
  logical <- select(df_ff,sweep_cutoff)
  prob <- sum(logical)/nrow(df_ff)
  y_data6[i6] <- prob
  s_mean_end_sim_1muts[i6] <- mean(df_ff$sr1)
  i6 <- i6+1
}


mean_s <- c()
for (s in b_bins){
  df_temp <- df_revised %>% filter(s_driver_birth == s)
  mean_s <- c(mean_s, mean(df_temp$sr1))
}



##############
## Analysis ##
##############

s_driver_list <- c(0.05, 0.075, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2)
median_sr1_1mut_list <- c()
mean_sr1_1mut_list <- c()

for (s in s_driver_list){
  df_ff <- df_revised_1mut %>% filter(s_driver_birth == s)
  median_sr1 <- median(df_ff$sr1)
  mean_sr1 <- mean(df_ff$sr1)
  median_sr1_1mut_list <- c(median_sr1_1mut_list,median_sr1)
  mean_sr1_1mut_list <- c(mean_sr1_1mut_list,mean_sr1)
}

median_sr1_1mut_list_plotting <- median_sr1_1mut_list/s_wt
mean_sr1_1mut_list_plotting <- mean_sr1_1mut_list/s_wt

#Count data
median_count <- c(0,0,0,0,0,0,0,0) 
i3=1

bar_width_2 <-0.05 #0.05
filter_s_driver <- 0.1
b_bins_2 <- seq(bar_width_2,10, bar_width_2) 
b_bins_bottom_2 <- c(0.0,head(b_bins_2, -1))
b_bins_middle_2 <- (b_bins_2 + b_bins_bottom_2 ) / 2
b_bins_plotting_2 <- b_bins_middle_2 / s_wt
b_bins_width_2 <- (b_bins_2 - b_bins_bottom_2)/s_wt
previous_b_2 <- 0

for (b in b_bins_2) {
  df_ff <- df_revised_1mut %>% filter( s_driver_birth == filter_s_driver, sr1 > previous_b_2, sr1 <= b) #filter() s_driver_birth == filter_s_driver, 
  median_count[i3] <-nrow(df_ff)
  i3 <- i3+1
  previous_b_2 <- b
}

#############
### Plots ###
#############
# Define colors and legends
cols <- brewer.pal(12, "Paired")
legend_1 <- "1 fixed"
legend_2 <- "pred (exact, 1 fixed)"
legend_3 <- "pred (approx., 1 fixed)"
legend_8 <- "pred (approx., 2 fixed)"
legend_4 <- "unlimited"
legend_5 <- "3 random"
legend_6 <- "2 random"
legend_7 <- "1 random"

# Define colors, linetypes, and shapes
colours <- c(cols[7], cols[8],"darkblue")
names(colours) <- c(legend_2, legend_3, legend_8)

lty <- c("solid", "22", "11")
names(lty) <- c(legend_2, legend_3, legend_8)

bar_fills <- c(
  "darkslategray2",  # Unlimited mutations
  "thistle",         # Three mutations
  "orange",          # Two mutations
  "red"              # One mutation
)
names(bar_fills) <- c(legend_4, legend_5, legend_6, legend_7)

shapes <- c(17)  # Shape for simulation points (triangle)
names(shapes) <- c(legend_1)

# Plot by input s-value

# adjust for missing sweeps

F_fixed_effect <- c(0.74, 1,    1,    1,    1,    1,    1, 1, 1, 1)
F_max1 <-         c(0.85, 0.93, 0.97, 0.98, 0.99, 0.99, 1, 1, 1, 1, 1)
F_max2 <-         c(0.59, 0.94, 0.97, 0.99, 1,    1,    1, 1, 1, 1, 1)
F_max3 <-         c(0.58, 0.94, 0.95, 0.99, 1,    1,    1, 1, 1, 1, 1)
F_unlimited <-    c(0.51, 0.73, 0.83, 0.95, 0.98, 1,    1, 1, 1, 1, 1)

print(y_data3)
y_data2 <- y_data2 / F_fixed_effect
y_data6 <- y_data6 / F_max1
y_data5 <- y_data5 / F_max2
y_data4 <- y_data4 / F_max3
y_data3 <- y_data3 / F_unlimited
print(y_data3)


# Plot by mean effective s value
g3<-ggplot() +

  # Lines for predictions
  #geom_line(data = df_num, aes(x = c(2:100)*s_wt, y = twodim, linetype = legend_2, color = legend_2), size = 1.5) +
  #geom_line(aes(x = xaxis*s_wt, y = sweep_ana, linetype = legend_3, color = legend_3), linewidth = 1.5) +
  #geom_line(aes(x = xaxis*s_wt, y = sweep_ana2, linetype = legend_8, color = legend_8), linewidth = 1.5) +
  #geom_line(aes(x = xaxis*s_wt, y = sweep_ana) linewidth = 1.5) +
  #geom_line(aes(x = xaxis*s_wt, y = sweep_ana, linetype = legend_3, color = legend_3), linewidth = 1.5) +
  
  geom_line(data = df_num, aes(x = c(2:100)*s_wt, y = twodim, linetype = legend_2, color = legend_2), linewidth = 1.5) +
  geom_line(aes(x = xaxis*s_wt, y = sweep_ana, linetype = legend_3, color = legend_3), linewidth = 1.5) +
  geom_line(aes(x = xaxis*s_wt, y = sweep_ana2, linetype = legend_8, color = legend_8), linewidth = 1.5) +
  

  # Points for simulation
  geom_point(aes(x = x_data2*s_wt, y = y_data2, shape = legend_1), color = "black", size = 4.0) +

  # Bar plots with legends
  geom_point(aes(x = mean_sr1_1mut_list_plotting*s_wt, y = y_data6), stat = 'identity', colour =  "red", shape = 18, size = 4) +  # one mutations
  geom_point(aes(x = mean_sr1_1mut_list_plotting*s_wt, y = y_data5), stat = 'identity', colour =  "orange", shape = 18, size = 4) +  # two mutations
  #geom_point(aes(x = mean_sr1_1mut_list_plotting*s_wt, y = y_data4), stat = 'identity', colour =  "thistle", shape = 18, size = 4) +  # three mutations
  #geom_point(aes(x = mean_sr1_1mut_list_plotting*s_wt, y = y_data3), stat = 'identity', colour = "darkslategray2", shape = 18, size = 4) + # unlimited mutations
  
  # annotation_custom(
  #   grob = grid::rectGrob(gp = grid::gpar(fill = "white", alpha = 1, col = NA)),
  #   xmin = 2, xmax = 20, ymin = 0.7, ymax = 1.1) +

  # Labels and themes
  labs(
    x = expression("mean effect of surviving mutations, " * italic(tilde(s))),
    y = "Pr(sweep)"
  ) +
  xlim(0, 20.56*s_wt) + ylim(0, 1) +
  scale_color_manual(values = colours, name = "Legend") +
  scale_linetype_manual(values = lty, name = "Legend") +
  scale_fill_manual(values = bar_fills, name = "Legend") +
  scale_shape_manual(values = shapes, name = "Legend") +
  theme_bw(base_size = 25) +
  theme(
    axis.title.x = element_text(),
    panel.grid = element_blank(),
    axis.title.y = element_text(),
    legend.position = c(0.65, 0.2),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = alpha("white", 0),  # White background
    ),
    legend.spacing.y = unit(-0.95, "cm"),
    legend.box.margin = margin(0, 0, 0, 0),
    #    legend.key.width = unit(0.5, "cm"),  # Adjust width of legend keys
    #    legend.key.height = unit(0.5, "cm"),  # Adjust height of legend keys
    legend.text = element_text(margin = margin(l = 5), size= 18)  # Adjust space between legend icon and text
  )+
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(linetype = 0, shape = NA),
      ncol = 1,
      byrow = TRUE
    ),
    color = guide_legend(
      order = 2,
      override.aes = list(fill = NA, shape = NA),
      ncol = 1,
      byrow = TRUE
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(fill = NA, shape = NA),
      ncol = 1,
      byrow = TRUE
    ),
    shape = guide_legend(
      order = 1,
      override.aes = list(fill = NA, linetype = 0),
      ncol = 1,
      byrow = TRUE
    )
  )

print(g3)

ggsave("Figure_S30.png", g3, width = 4^1.5, height = 3^1.5)



# title: Plotting mean sweep origin times versus fitness.
# author: Alexander Stein, Rob Noble, Kate Bostock

#################
### libraries ###
#################

library("tidyverse")
library("ggtext")
library("RColorBrewer")
library("gridExtra")
library("cowplot")

### assumptions ###

m <- 10 # maximum birth rate
b1 <- 1 # base birth rate

###################
### Import data ###
###################

### Simulation data
params <- read_csv("simulation_data_original/params.csv") 
probs <- read_csv("simulation_data_original/sweeps-prob.csv") 

params_high_s <- read_csv("simulation_data_high_birth_rates2/params.csv") # extension to single fixed mutation data for s = 5, 10
probs_high_s <- read_csv("simulation_data_high_birth_rates2/sweeps-prob-descendants.csv")  # extension to single fixed mutation data for s = 5, 10

params_revised <- read_csv("simulation_data_revised/1000_sims/params.csv") 
probs_revised <- read_csv("simulation_data_revised/1000_sims/sweeps-prob-descendants.csv") 
birth_rates_mean <- read_csv("simulation_data_revised/1000_sims/birth_rates_end_mean.csv") 
all_sweeps <- read_csv("simulation_data_revised/1000_sims/sweeps-all.csv") 

params_revised2 <- read_csv("simulation_data_s015/params.csv") 
probs_revised2 <- read_csv("simulation_data_s015/sweeps-prob-descendants.csv") 
birth_rates_mean2 <- read_csv("simulation_data_s015/birth_rates_end_mean_all.csv") 
all_sweeps2 <- read_csv("simulation_data_s015/sweeps-all.csv") 

params_revised3 <- read_csv("005_data/unlimited/params.csv") 
probs_revised3 <- read_csv("005_data/unlimited/sweeps-prob-descendants.csv") 
birth_rates_mean3 <- read_csv("005_data/unlimited/birth_rates_end_mean_all.csv") 
all_sweeps3 <- read_csv("005_data/unlimited/sweeps-all.csv") 

params_revised4 <- read_csv("0075_data/0075_unlimited/params.csv") 
probs_revised4 <- read_csv("0075_data/0075_unlimited/sweeps-prob-descendants.csv") 
birth_rates_mean4 <- read_csv("0075_data/0075_unlimited/birth_rates_end_mean_all.csv") 
all_sweeps4 <- read_csv("0075_data/0075_unlimited/sweeps-all.csv") 

### Revised data - 3 random mutations
params_revised_3muts <- read_csv("simulation_data_3muts/params.csv") 
probs_revised_3muts <- read_csv("simulation_data_3muts/sweeps-prob-descendants.csv") 
birth_rates_mean_3muts <- read_csv("simulation_data_3muts/birth_rates_end_mean_all.csv") 
all_sweeps_3muts <- read_csv("simulation_data_3muts/sweeps-all.csv") 

params_revised_3muts2 <- read_csv("005_data/3_mut/params.csv") 
probs_revised_3muts2 <- read_csv("005_data/3_mut/sweeps-prob-descendants.csv") 
birth_rates_mean_3muts2 <- read_csv("005_data/3_mut/birth_rates_end_mean_all.csv") 
all_sweeps_3muts2 <- read_csv("005_data/3_mut/sweeps-all.csv") 

params_revised_3muts3 <- read_csv("0075_data/0075_3/params.csv") 
probs_revised_3muts3 <- read_csv("0075_data/0075_3/sweeps-prob-descendants.csv") 
birth_rates_mean_3muts3 <- read_csv("0075_data/0075_3/birth_rates_end_mean_all.csv") 
all_sweeps_3muts3 <- read_csv("0075_data/0075_3/sweeps-all.csv") 

### Revised data - 2 random mutations
params_revised_2muts <- read_csv("simulation_data_2muts/params.csv") 
probs_revised_2muts <- read_csv("simulation_data_2muts/sweeps-prob-descendants.csv") 
birth_rates_mean_2muts <- read_csv("simulation_data_2muts/birth_rates_end_mean_all.csv") 
all_sweeps_2muts <- read_csv("simulation_data_2muts/sweeps-all.csv") 

params_revised_2muts2 <- read_csv("005_data/2_mut/params.csv") 
probs_revised_2muts2 <- read_csv("005_data/2_mut/sweeps-prob-descendants.csv") 
birth_rates_mean_2muts2 <- read_csv("005_data/2_mut/birth_rates_end_mean_all.csv") 
all_sweeps_2muts2 <- read_csv("005_data/2_mut/sweeps-all.csv") 

params_revised_2muts3 <- read_csv("0075_data/0075_2/params.csv") 
probs_revised_2muts3 <- read_csv("0075_data/0075_2/sweeps-prob-descendants.csv") 
birth_rates_mean_2muts3 <- read_csv("0075_data/0075_2/birth_rates_end_mean_all.csv") 
all_sweeps_2muts3 <- read_csv("0075_data/0075_2/sweeps-all.csv") 

### Revised data - 1 random mutations
params_revised_1mut <- read_csv("simulation_data_1mut/params.csv") 
probs_revised_1mut <- read_csv("simulation_data_1mut/sweeps-prob-descendants.csv") 
birth_rates_mean_1mut <- read_csv("simulation_data_1mut/birth_rates_end_mean_all.csv") 
all_sweeps_1mut <- read_csv("simulation_data_1mut/sweeps-all.csv") 

params_revised_1mut2 <- read_csv("005_data/1_mut/params.csv") 
probs_revised_1mut2 <- read_csv("005_data/1_mut/sweeps-prob-descendants.csv") 
birth_rates_mean_1mut2 <- read_csv("005_data/1_mut/birth_rates_end_mean_all.csv") 
all_sweeps_1mut2 <- read_csv("005_data/1_mut/sweeps-all.csv") 

params_revised_1mut3 <- read_csv("0075_data/0075_1/params.csv") 
probs_revised_1mut3 <- read_csv("0075_data/0075_1/sweeps-prob-descendants.csv") 
birth_rates_mean_1mut3 <- read_csv("0075_data/0075_1/birth_rates_end_mean_all.csv") 
all_sweeps_1mut3 <- read_csv("0075_data/0075_1/sweeps-all.csv") 

# join the data sets
params_revised <- rbind(params_revised, params_revised2)
params_revised <- rbind(params_revised, params_revised3)
params_revised <- rbind(params_revised, params_revised4)
probs_revised <- rbind(probs_revised, probs_revised2)
probs_revised <- rbind(probs_revised, probs_revised3)
probs_revised <- rbind(probs_revised, probs_revised4)
birth_rates_mean <- rbind(birth_rates_mean, birth_rates_mean2)
birth_rates_mean <- rbind(birth_rates_mean, birth_rates_mean3)
birth_rates_mean <- rbind(birth_rates_mean, birth_rates_mean4)
all_sweeps <- rbind(all_sweeps, all_sweeps2)
all_sweeps <- rbind(all_sweeps, all_sweeps3)
all_sweeps <- rbind(all_sweeps, all_sweeps4)

# revised - 3 mutations
params_revised_3muts <- rbind(params_revised_3muts, params_revised_3muts2)
probs_revised_3muts <- rbind(probs_revised_3muts, probs_revised_3muts2)
birth_rates_mean_3muts <- rbind(birth_rates_mean_3muts, birth_rates_mean_3muts2) 
all_sweeps_3muts <- rbind(all_sweeps_3muts, all_sweeps_3muts2)

params_revised_3muts <- rbind(params_revised_3muts, params_revised_3muts3)
probs_revised_3muts <- rbind(probs_revised_3muts, probs_revised_3muts3)
birth_rates_mean_3muts <- rbind(birth_rates_mean_3muts, birth_rates_mean_3muts3) 
all_sweeps_3muts <- rbind(all_sweeps_3muts, all_sweeps_3muts3)

# revised - 2 mutations
params_revised_2muts <- rbind(params_revised_2muts, params_revised_2muts2)
probs_revised_2muts <- rbind(probs_revised_2muts, probs_revised_2muts2)
birth_rates_mean_2muts <- rbind(birth_rates_mean_2muts, birth_rates_mean_2muts2)
all_sweeps_2muts <- rbind(all_sweeps_2muts, all_sweeps_2muts2)

params_revised_2muts <- rbind(params_revised_2muts, params_revised_2muts3)
probs_revised_2muts <- rbind(probs_revised_2muts, probs_revised_2muts3)
birth_rates_mean_2muts <- rbind(birth_rates_mean_2muts, birth_rates_mean_2muts3) 
all_sweeps_2muts <- rbind(all_sweeps_2muts, all_sweeps_2muts3)

# revised - 1 mutation
params_revised_1mut <- rbind(params_revised_1mut, params_revised_1mut2)
probs_revised_1mut <- rbind(probs_revised_1mut, probs_revised_1mut2)
birth_rates_mean_1mut <- rbind(birth_rates_mean_1mut, birth_rates_mean_1mut2)
all_sweeps_1mut <- rbind(all_sweeps_1mut, all_sweeps_1mut2)

params_revised_1mut <- rbind(params_revised_1mut, params_revised_1mut3)
probs_revised_1mut <- rbind(probs_revised_1mut, probs_revised_1mut3)
birth_rates_mean_1mut <- rbind(birth_rates_mean_1mut, birth_rates_mean_1mut3) 
all_sweeps_1mut <- rbind(all_sweeps_1mut, all_sweeps_1mut3)

# get rid of zeroes for wild type from the sweeps data
all_sweeps_f <- filter(all_sweeps, sweep_init_time != 0)
all_sweeps_1mut_f <- filter(all_sweeps_1mut, sweep_init_time != 0)
all_sweeps_2muts_f <- filter(all_sweeps_2muts, sweep_init_time != 0)
all_sweeps_3muts_f <- filter(all_sweeps_3muts, sweep_init_time != 0)

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

# Merge the parameters with the data dataframe (revised)
df_revised <- merge(probs_revised, params_revised, by.x = "id" , by.y = "index" )
df_revised <- merge(df_revised, birth_rates_mean, by.x = "id" , by.y = "index" )
df_revised <- df_revised[, !duplicated(as.list(df_revised))]
df_revised_all_sweeps <- merge(all_sweeps_f, df_revised, by.x = "id")

df_revised_3muts <- merge(probs_revised_3muts, params_revised_3muts, by.x = "id" , by.y = "index" )
df_revised_3muts <- merge(df_revised_3muts, birth_rates_mean_3muts, by.x = "id" , by.y = "index" )
df_revised_3muts <- df_revised_3muts[, !duplicated(as.list(df_revised_3muts))]
df_revised_3muts_all_sweeps <- merge(all_sweeps_3muts_f, df_revised_3muts, by.x = "id")

df_revised_2muts <- merge(probs_revised_2muts, params_revised_2muts, by.x = "id" , by.y = "index" )
df_revised_2muts <- merge(df_revised_2muts, birth_rates_mean_2muts, by.x = "id" , by.y = "index" )
df_revised_2muts <- df_revised_2muts[, !duplicated(as.list(df_revised_2muts))]
df_revised_2muts_all_sweeps <- merge(all_sweeps_2muts_f, df_revised_2muts, by.x = "id")

df_revised_1mut <- merge(probs_revised_1mut, params_revised_1mut, by.x = "id" , by.y = "index" )
df_revised_1mut <- merge(df_revised_1mut, birth_rates_mean_1mut, by.x = "id" , by.y = "index" )
df_revised_1mut <- df_revised_1mut[, !duplicated(as.list(df_revised_1mut))]
df_revised_1mut_all_sweeps <- merge(all_sweeps_1mut_f, df_revised_1mut, by.x = "id")



####################
### Simulations  ###
####################

s_wt <- 0.1 

#Original data
df_f2 <- df %>% filter(mu_driver_birth == 1e-05)
s_vector = c(0.1,0.2,0.3,0.4,0.5,1.0,1.5,2.0,5,10)
y_data2 <- c(0,0,0,0,0,0,0,0)
x_data2 <- s_vector/s_wt 
i2=1
for (s in s_vector) {
  df_ff <- df_f2 %>% filter(s_driver_birth == s, x100 == TRUE)  
  mean_origin_time <- mean(df_ff$first_mutant_init_time)
  y_data2[i2] <- mean_origin_time                     
  i2 <- i2+1
}

# Revised data
y_data3 <- c(0,0,0,0,0,0,0,0)                    
i3=1

b_bins <- seq(0.2,10, 0.2) 
b_bins_bottom <- c(0.0,head(b_bins, -1))
b_bins_middle <- (b_bins + b_bins_bottom ) / 2
b_bins_plotting <- b_bins_middle / s_wt
b_bins_width <- (b_bins - b_bins_bottom)/s_wt
previous_b <- 0

for (b in b_bins) {
  df_ff <- df_revised_all_sweeps %>% filter(sr1 > previous_b, sr1 <= b, x100 == TRUE)#, s_driver_birth == s_choice)
  mean_origin_time <- mean(df_ff$sweep_init_time)
  y_data3[i3] <- mean_origin_time
  i3 <- i3+1
  previous_b <- b
}

# Revised data - 3 mutations only
y_data4 <- c(0,0,0,0,0,0,0,0)                   
i4=1

previous_b <- 0

for (b in b_bins) {
  df_ff <- df_revised_3muts_all_sweeps %>% filter(sr1 > previous_b, sr1 <= b, x100 == TRUE)#, s_driver_birth == s_choice)
  mean_origin_time <- mean(df_ff$sweep_init_time)
  y_data4[i4] <- mean_origin_time
  i4 <- i4+1
  previous_b <- b
}

# Revised data - 2 mutations only
y_data5 <- c(0,0,0,0,0,0,0,0)                   
i5=1

previous_b <- 0

for (b in b_bins) {
  df_ff <- df_revised_2muts_all_sweeps %>% filter(sr1 > previous_b, sr1 <= b, x100 == TRUE)#, s_driver_birth == s_choice)
  mean_origin_time <- mean(df_ff$sweep_init_time)
  y_data5[i5] <- mean_origin_time
  i5 <- i5+1
  previous_b <- b
}

# Revised data - 1 mutation only
y_data6 <- c(0,0,0,0,0,0,0,0)                   
i6=1

previous_b <- 0

for (b in b_bins) {
  df_ff <- df_revised_1mut_all_sweeps %>% filter(sr1 > previous_b, sr1 <= b, x100 == TRUE)#, s_driver_birth == s_choice)
  mean_origin_time <- mean(df_ff$sweep_init_time)
  y_data6[i6] <- mean_origin_time
  i6 <- i6+1
  previous_b <- b
}

#############
### Plots ###
#############

# Combine data into a single data frame
data_combined <- data.frame(
  b_bins_plotting = rep(b_bins_plotting, 4),
  y_data = c(y_data3, y_data4, y_data5, y_data6),
  group = rep(c("unlimited", "3 random", "2 random", "1 random"), each = length(b_bins_plotting))
)

# Define colors and shapes for each group
colours <- c("unlimited" = "#FF7F00", "3 random" = "#33A02C", "2 random" = "#1F78B4", "1 random" = "#6A3D9A")
shapes <- c("unlimited" = 16, "3 random" = 17, "2 random" = 18, "1 random" = 15)

g_combo <- ggplot(data_combined, aes(x = b_bins_plotting, y = y_data, color = group, shape = group)) +
  geom_point(size = 3) +
  labs(x = "ratio of fitness difference, *a<sub>m*</sub> / *a<sub>wt*</sub>", y = "Mean sweep origin time") +
  xlim(0, 100) + ylim(0, 400) +
  scale_color_manual(values = colours) +
  scale_shape_manual(values = shapes) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.7, 0.15), legend.title = element_blank(), legend.text = element_markdown())

print(g_combo)

##############################

# get parameters theta_2D and beta:
get_theta <- function(cwt, mu) (3*cwt / (pi*mu))^(1/3)
get_beta <- function(cwt, cm) (cm - cwt) / cm

# survival probability:
get_rho <- function(r1, r2, K) {
  r <- r1 / r2
  return((1 - 1/r) / (1 - 1/r^K))
}

# multi-purpose function, called by three functions that follow:
master_function <- function(x, am, cwt, mu, K, type) {
  cm <- lookup_speed(am)
  rm <- am + 1
  mu <- mu * get_rho(rm, 1, K)
  theta <- get_theta(cwt, mu)
  beta <- get_beta(cwt, cm)
  shape <- 3
  scale <- theta * beta^(2/3)

  if(type == 1) return(dweibull(x, shape, scale))
  if(type == 2) return(scale * gamma(1 + 1/shape))
  q <- c(0.05, 0.5, 0.95)
  return(qweibull(q, shape, scale))
}

# approximate f_X(X = x | sweep) in two dimensions,
# which follows a Weibull distribution,
# like the dashed green curve in our Figure 2F:
dist_x_given_sweep <- function(x, am, cwt, mu, K) {
  master_function(x, am, cwt, mu, K, 1)
}

# approximate 5%, 50% and 95% quantiles of (X | sweep) in two dimensions,
# based on the Weibull distribution:
Q_x_given_sweep <- function(am, cwt, mu, K) {
  master_function(NA, am, cwt, mu, K, 3)
}

# mutant radial expansion speeds from our Table 1
lookup_speed <- function(am) {
  cm <- 1e-2 * c(14, 23, 31, 38, 46,
                 53, 60, 68, 75, 82,
                 89, 96, 103, 111, 117,
                 124, 131, 139, 146, 152, 
                 159, 167, 174, 181, 188, #speeds are almost an exact straight line C_m = 0.7202 * a_m + 0.0928. Therefore extrapolated for extra a_m values.
                 196, 203, 210, 217, 225, 
                 232, 239, 246, 254, 261, 
                 268, 275, 283, 290, 297, 
                 305, 312, 319, 326, 334, 
                 341, 348, 355, 363, 370, 
                 377, 384, 392, 399, 406, 
                 413, 421, 428, 435, 443, 
                 450, 457, 464, 472, 479, 
                 486, 493, 501, 508, 515, 
                 522, 530, 537, 544, 551, 
                 559, 566, 573, 581, 588, 
                 595, 602, 610, 617, 624, 
                 631, 639, 646, 653, 660, 
                 668, 675, 682, 689, 697, 
                 704, 711, 719, 726, 733) #speeds are almost an exact straight line C_m = 0.7202 * a_m + 0.0928. Therefore extrapolated for extra a_m values.
  return(cm[10*am])
}



########
am <- 0.1 * 1:100 # absolute difference in birth rates
am_over_awt <- am / (1/11)
am <- 0.1 * 1:100

quantiles_x <- sapply(am, Q_x_given_sweep, cwt = 0.15, mu = 1e-5, K = 16)
# convert mean radius x into mean origin time:
ot_05 <- quantiles_x[1, ] / 0.15
ot_5 <- quantiles_x[2, ] / 0.15
ot_95 <- quantiles_x[3, ] / 0.15

g9 <- ggplot() +
  geom_line(aes(x = am_over_awt, y = ot_5, color = "Analytical result"), linetype = 2, linewidth = 1.5) +  # Dashed green line
  geom_point(aes(x = x_data2, y = y_data2, shape = "Simulation"), color = "black", size = 4) +  # Black triangles
  labs(x = "ratio of fitness difference, *a<sub>m* / *a<sub>wt*", y = "Mean sweep origin time") +
  xlim(0, 20) + ylim(0, 500) +
  scale_color_manual(values = c("Analytical result" = "#33A02C")) +
  scale_shape_manual(values = c("Simulation" = 17)) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(),
        legend.position = c(0.5, 0.7), legend.title = element_blank(), legend.text = element_markdown(),
        legend.spacing = unit(0.1, "cm"),  # Adjust legend spacing
        legend.key.size = unit(0.5, "cm"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0))  # Reduce margin )  # Adjust legend key size

g9

##################

# Define colors and shapes for each group
colours <- c("unlimited" = "darkslategray2", "3 random" = "thistle", "2 random" = "orange", "1 random" = "red")
shapes <- c("unlimited" = 16, "3 random" = 17, "2 random" = 18, "1 random" = 15)

g10 <- ggplot() +
  geom_point(aes(x = data_combined$b_bins_plotting, y = data_combined$y_data, color = data_combined$group), alpha = 0.7, size = 3) + # coloured points
  geom_line(aes(x = am_over_awt, y = ot_5, color = "analytical"), linetype = 2, linewidth = 1.5) +  # Dashed green line
  geom_point(aes(x = x_data2, y = y_data2, shape = "1 fixed"), color = "black", size = 4) +  # Black triangles
  annotation_custom(
    grob = grid::rectGrob(gp = grid::gpar(fill = "white", alpha = 1, col = NA)),
    xmin = 11, xmax = 50, ymin = -4, ymax = 110) + 
  
    labs(x = "ratio of fitness difference, *a<sub>m* / *a<sub>wt* <br> (mean at end of simulation)", y = "mean sweep origin time") +
  xlim(0, 50) + ylim(0, 500) +
  scale_color_manual(values = c("analytical" = "#33A02C", "1 fixed" = "black", colours)) +
  scale_shape_manual(values = c("1 fixed" = 17, shapes)) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(),
        legend.position = c(0.59, 0.12), legend.title = element_blank(),
        legend.text = element_markdown(size = 18),  # Reduce legend text size
        legend.spacing = unit(0.1, "cm"),  # Adjust legend spacing
        legend.key.size = unit(0.5, "cm"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0), # Reduce margin 
        legend.background = element_rect(fill = "white")) +  # White background with black border
  guides(color = guide_legend(ncol = 2), shape = guide_legend(ncol = 2))  # Split legend into 2 columns


g10
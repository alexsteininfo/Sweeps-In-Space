#################
### libraries ###
#################
library("tidyverse")
library("RColorBrewer")
library(ggplot2)
library(dplyr)

setwd("~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_5/data/Revised_sims_used/")

###################
### Import data ###
###################

### Simulation data

#Unlimited case
params <- read_csv("simulation_data_revised/1000_sims/params.csv")
probs <- read_csv("simulation_data_revised/1000_sims/sweeps-prob-descendants.csv")
sweeps <- read_csv("simulation_data_revised/1000_sims/sweeps-all.csv")

params_revised2 <- read_csv("simulation_data_s015/params.csv")
probs_revised2 <- read_csv("simulation_data_s015/sweeps-prob-descendants.csv")
sweeps2 <- read_csv("simulation_data_s015/sweeps-all.csv")

params_revised3 <- read_csv("005_data/unlimited/params.csv") 
probs_revised3 <- read_csv("005_data/unlimited/sweeps-prob-descendants.csv") 
sweeps3 <- read_csv("005_data/unlimited/sweeps-all.csv") 

params_revised4 <- read_csv("0075_data/0075_unlimited/params.csv")
probs_revised4 <- read_csv("0075_data/0075_unlimited/sweeps-prob-descendants.csv")
sweeps4 <- read_csv("0075_data/0075_unlimited/sweeps-all.csv")

# # 2 max mutations
# params <- read_csv("simulation_data_2muts/params.csv")
# probs <- read_csv("simulation_data_2muts/sweeps-prob-descendants.csv")
# sweeps <- read_csv("simulation_data_2muts/sweeps-all.csv")
#
# params_revised3 <- read_csv("005_data/2_mut/params.csv") 
# probs_revised3 <- read_csv("005_data/2_mut/sweeps-prob-descendants.csv") 
# sweeps3 <- read_csv("005_data/2_mut/sweeps-all.csv") 
# 
# params_revised2 <- read_csv("0075_data/0075_2/params.csv")
# probs_revised2 <- read_csv("0075_data/0075_2/sweeps-prob-descendants.csv")
# sweeps2 <- read_csv("0075_data/0075_2/sweeps-all.csv")

# # 3 max mutations
# params <- read_csv("simulation_data_3muts/params.csv")
# probs <- read_csv("simulation_data_3muts/sweeps-prob-descendants.csv")
# sweeps <- read_csv("simulation_data_3muts/sweeps-all.csv")
#
# params_revised3 <- read_csv("005_data/3_mut/params.csv") 
# probs_revised3 <- read_csv("005_data/3_mut/sweeps-prob-descendants.csv") 
# sweeps3 <- read_csv("005_data/3_mut/sweeps-all.csv") 
# 
# params_revised2 <- read_csv("0075_data/0075_3/params.csv")
# probs_revised2 <- read_csv("0075_data/0075_3/sweeps-prob-descendants.csv")
# sweeps2 <- read_csv("0075_data/0075_3/sweeps-all.csv")

# join the data sets
# revised - unlimited mutations
params <- rbind(params, params_revised2)
params <- rbind(params, params_revised3)
params <- rbind(params, params_revised4)
probs <- rbind(probs, probs_revised2)
probs <- rbind(probs, probs_revised3)
probs <- rbind(probs, probs_revised4)
sweeps <- rbind(sweeps, sweeps2)
sweeps <- rbind(sweeps, sweeps3)
sweeps <- rbind(sweeps, sweeps4)

s_vector = c(0.05, 0.075, 0.1,0.15, 0.2,0.3,0.4,0.5,1,1.5,2)

# Merge the parameters with the data dataframe
df <- merge(probs, params, by.x = "id" , by.y = "index" )

# Count how many sweeps for each simulation
sweep_counts <- sweeps %>% count(id, name = "SweepCount")
sweep_counts$SweepCount <- sweep_counts$SweepCount - 1

# merge with parameter data
df2 <- merge(df, sweep_counts, by.x = "id")

###################
#### Analysis #####
###################

# find mean number of sweeps per simulation for the different s values.
means_list <-c()
for (s in s_vector)
means_list <- c(means_list, mean(filter(df2, s_driver_birth == s)$SweepCount))

# filter full sweeps list to remove things which aren't really sweeps i.e. pop_radius_at_sweep_init == 0 

sweeps_filtered <- filter(sweeps, pop_radius_at_sweep_init != 0)
sweeps_filtered <- sweeps_filtered %>% left_join(df2, by = "id")

###################
#### Plotting #####
###################

g1 <- ggplot() + 
  geom_line(aes(x = s_vector, y = means_list)) +
  geom_point(aes(x = s_vector, y = means_list))

ggplot(df2, aes(x = SweepCount)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", aes(y = ..density..)) +
  facet_wrap(~ s_driver_birth) +  # Facet by the 's_driver_birth' column
  labs(title = "Histograms of #sweeps by s_driver_birth", x = "Number of sweeps", y = "Density")

ggplot(sweeps_filtered, aes(x = pop_radius_at_sweep_init)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  xlim(0,200) +
  facet_wrap(~ s_driver_birth) +  # Facet by  the 's_driver_birth' column
  labs(title = "Histograms of pop_radius_at_sweep_init by s_driver_birth", x = "Radius", y = "Density")

print(g1)

#adjustment for missing sweeps
# Multiplication factors for each s_driver_birth
F_unlimited <- tibble(
  s_driver_birth = c(0.05, 0.075, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 1.0, 1.5, 2),
  F_factor =       c(0.51, 0.73,0.83,0.95,0.98, 1, 1, 1,  1, 1,  1)
)

s_tilde_mapping <- c("0.05" = 0.2072712, "0.075" = 0.3129246, "0.1" = 0.3942779, "0.15" = 0.5367264, "0.2" = 0.6434869, "0.3" = 0.8577694, "0.4" = 1.0565023, "0.5" = 1.2264193, "1" = 2.0550554, "1.5" = 2.8199614, "2" = 3.5271355)

#### Make a stacked area column chart ####
# Step 1: Summarize the data
df_summary <- df2 %>%
  group_by(s_driver_birth, SweepCount) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(s_driver_birth) %>%
  mutate(proportion = count / sum(count)) %>%
  left_join(F_unlimited, by = "s_driver_birth") %>%
  mutate(count_post_adjustment = ifelse(SweepCount >0, count / F_factor, sum(count) -(sum(count)- count)/F_factor)) %>%
  mutate(proportion_post_adjustment = count_post_adjustment / sum(count_post_adjustment)) %>%
  mutate(s_tilde = round(s_tilde_mapping[as.character(s_driver_birth)],2))
  

sweep_colors <- c(
  "0" = "#1f77b4",  # muted blue
  "1" = "#ff7f0e",  # orange
  "2" = "#2ca02c",  # green
  "3" = "#d62728",  # red
  "4" = "#9467bd",  # purple
  "5" = "#8c564b",  # brown
  "6" = "#e377c2",  # pink
  "7" = "#7f7f7f"   # grey
)



# Step 2: Create the stacked area plot
g2<-ggplot(df_summary, aes(x = factor(s_driver_birth), y = proportion_post_adjustment, fill = factor(SweepCount))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = sweep_colors) +
  labs(
    x = expression("Mean effect of generated mutations, " * italic(s)),
    y = "proportion",
    fill = "Number of Sweeps"
  ) +
theme_bw(base_size = 25) +
  theme(
    axis.text.x = element_text(size = 14), #angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 25)
  )
print(g2)

# Step 2: Create the stacked area plot - s tilde version
g3<-ggplot(df_summary, aes(x = factor(s_tilde), y = proportion_post_adjustment, fill = factor(SweepCount))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = sweep_colors) +
  labs(
    x = expression("mean effect of surviving mutations, " * italic(tilde(s))),
    y = "proportion",
    fill = "Number of Sweeps"
  ) +
  theme_bw(base_size = 25) +
  theme(
    axis.text.x = element_text(size = 14), #angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 25)
  )
print(g3)
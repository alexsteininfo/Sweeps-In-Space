# title: testing the gamma fits
# author: alex stein

library(tidyverse)

#############################################
### Mean and median in the unlimited case ###
#############################################

setwd("~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_5/data/")

data_unlimited <- read_csv("completion/df2_unlimitedv2.csv")

# Look at finished sweeps only
data_unlimited <- data_unlimited %>% filter(sweep_complete_time != 0.0)
# Look at radii once sweeps are completed
data_unlimited <- data_unlimited %>% mutate(radius_end = sqrt(num_cell/pi))

data_unlimited %>%
  group_by(s_driver_birth) %>%
  summarise(mean = mean(radius_end, na.rm = TRUE), median = median(radius_end, na.rm = TRUE))

##############################################
### Mean and median in the 1 mutation case ###
##############################################

data_1mut <- read_csv("completion/df2_unlimited_1mutations.csv")

# Look at finished sweeps only
data_1mut <- data_1mut %>% filter(sweep_complete_time != 0.0)
# Look at radii once sweeps are completed
data_1mut <- data_1mut %>% mutate(radius_end = sqrt(sweep_complete_size/pi))

data_1mut %>%
  group_by(s_driver_birth) %>%
  summarise(mean = mean(radius_end, na.rm = TRUE), median = median(radius_end, na.rm = TRUE))

##############################################
### Mean and median in the 2 mutation case ###
##############################################

data_2mut <- read_csv("completion/df3_unlimited_2mutations.csv")

# Look at finished sweeps only
data_2mut <- data_2mut %>% filter(sweep_complete_time != 0.0)
# Look at radii once sweeps are completed
data_2mut <- data_2mut %>% mutate(radius_end = sqrt(sweep_complete_size/pi))

data_2mut %>%
  group_by(s_driver_birth) %>%
  summarise(mean = mean(radius_end, na.rm = TRUE), median = median(radius_end, na.rm = TRUE))

##############################################
### Mean and median in the 3 mutation case ###
##############################################

data_3mut <- read_csv("completion/df2_unlimited_3mutations.csv")

# Look at finished sweeps only
data_3mut <- data_3mut %>% filter(sweep_complete_time != 0.0)
# Look at radii once sweeps are completed
data_3mut <- data_3mut %>% mutate(radius_end = sqrt(sweep_complete_size/pi))

data_3mut %>%
  group_by(s_driver_birth) %>%
  summarise(mean = mean(radius_end, na.rm = TRUE), median = median(radius_end, na.rm = TRUE))

#########################################
### Plotting over mean of generated s ###
#########################################

s_values = c(0.2,0.3,0.4,0.5,1.0,1.5,2.0)
xc_median_1mut = c(120, 102, 89.2, 78.1, 65.2, 57.3, 54.3)
xc_median_2mut = c(129, 111, 92.6, 86.7, 65.9, 58.5, 56.8)
xc_median_3mut = c(135, 115, 98.4, 87.5, 65.8, 58.8, 56.2)
xc_median_unl  = c(163, 128, 106,  91.5, 70.6, 61.0, 56.7)

df <- data.frame(s_values, xc_median_1mut, xc_median_2mut, xc_median_3mut, xc_median_unl)

ggplot(df) +
  geom_point(aes(x=s_values, y=xc_median_1mut), col='blue') +
  geom_point(aes(x=s_values, y=xc_median_2mut), col='orange') +
  geom_point(aes(x=s_values, y=xc_median_3mut), col='red') +
  geom_point(aes(x=s_values, y=xc_median_unl), col='black') +
  labs(x="selective advantage, *s*", y="completion radius, Med[*X<sub>c*]") +
  #xlim(1,20) +
  ylim(0,170) +
  #scale_shape_manual(values = shapes) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.8,0.2), legend.title = element_blank(), 
        legend.text = element_markdown())

###############################################
### Data from random mutation effect models ###
###############################################

s_values_random <- c(0.05,0.075,0.1,0.15,0.2,0.3,0.4,0.5,1.0,1.5,2.0)

# Computed from simulation results
s_values_surv <- c(0.2072712, 0.3129246, 0.3942779, 0.5367264, 0.6434869, 0.8577694, 
                   1.0565023, 1.2264193, 2.0550554, 2.8199614, 3.5271355)

# Computed in convergence analsis
xc_median_1mut <- c(288.36740, 245.38771, 205.12526, 169.28721, 146.96144, 125.05579, 109.91619, 
                    98.45656,  81.05116,  67.64014, 62.07748)
xc_median_2mut <- c(471.90788,269.40256, 219.81262, 171.44165, 145.05661, 121.51473, 103.36970, 
                    98.02082, 74.18456, 61.82755, 60.67683)
xc_median_3mut <- c(492.27537, 277.71898, 240.97641, 182.95687, 149.83512, 124.22027, 106.30898, 
                    94.27749, 69.64383, 60.23738, 57.27040)
xc_median_unl <- c(535.88135, 382.17229, 315.71654, 219.03885, 176.05320, 137.68361,
                   112.53323,  98.81933, 71.47401, 62.90015, 58.56255)

##############################################
### Data from fixed mutation effect models ###
##############################################

s_values_fixed <- c(0.1,0.2,0.3,0.4,0.5,1.0,1.5,2.0, NA,NA,NA)
xc_median_fixed_gamma <- c(144.85844, 104.72626, 82.60214, 70.36212, 66.59621, 51.76902, 46.57675, 45.25914,
                           NA,NA,NA)
xc_median_fixed_data <- c(182.20196, 109.69534, 83.29968, 69.33796, 65.70084, 52.13795, 46.35290, 44.58872,
                          NA,NA,NA)

df <- data.frame(s_values_random, s_values_surv, xc_median_1mut, xc_median_2mut, xc_median_3mut, xc_median_unl,
                 s_values_fixed, xc_median_fixed_gamma, xc_median_fixed_data)


#######################
### Analytic result ###
#######################

# Define paramters
mu <- 1e-5
c_wt <- 0.152
rho <- 0.234
theta_2d <- (3*c_wt/(pi*rho*mu))^(1/3)

s_xaxis <- c(0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8,  0.9,  1.0,
             1.1,  1.2,  1.3,  1.4,  1.5,  1.6,  1.7,  1.8,  1.9,  2.0,
             2.1,  2.2)

c_xaxis <- c(0.1399136, 0.2267043, 0.3068504, 0.3825911, 0.4592648, 0.5323676, 
             0.6044246, 0.6774701, 0.7465714, 0.8201126, 0.8936413, 0.9630367, 
             1.0319856, 1.1086058, 1.1747451, 1.2434848, 1.3095467, 1.3896184, 
             1.4566792, 1.5232894, 1.5926316, 1.6652632)

median_analytic <- function(cm) {
  beta <- cm/(cm-c_wt)
  #lambda <- theta_2d^3
  #lambda <- ( beta^2*theta_2d^3 )^(1/3)
  lambda <- ( (1+beta)^3 * beta^2*theta_2d^3 )^(1/3)
  gam <- gamma(4/3)
  lambda*gam
}

medians_ana <- median_analytic(c_xaxis)
medians_ana


########################################################
### Plotting over mean effect of surviving mutations ###
########################################################

fig <- ggplot() +
  geom_point(aes(x = s_values_surv, y = xc_median_1mut),
             colour = "red", shape = 18, size = 4) +
  geom_point(aes(x = s_values_surv, y = xc_median_2mut),
             colour = "orange", shape = 18, size = 4) +
  geom_point(aes(x = s_values_surv, y = xc_median_3mut),
             colour = "thistle", shape = 18, size = 4) +
  geom_point(aes(x = s_values_surv, y = xc_median_unl),
             colour = "darkslategray2", shape = 18, size = 4) +
  geom_point(aes(x = s_values_fixed, y = xc_median_fixed_data),
             colour = "black", shape = 17, size = 4) +
  labs(
    x = expression("mean effect of surviving mutations, " * tilde(italic(s))),
    y = "median radius at sweep"
  ) +
  xlim(0, 2.21) +
  ylim(0, 540) +
  theme_bw(base_size = 25)

fig

a <- 1.5
ggsave("~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_5/figures/sweep_completion.pdf", fig, width = 4^a, height = 3^a)


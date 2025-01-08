# title: Plotting sweep probabilities
# author: Alexander Stein

#################
### libraries ###
#################

library("tidyverse")
library("ggtext")
library("RColorBrewer")

setwd("/Users/stein02/Desktop/Plots for the Sweeps/Figure_3/")

###################
### Import data ###
###################

### Simulation data
params <- read_csv("simulation_data/params.csv")
probs <- read_csv("simulation_data/sweeps-prob.csv")

param_wt <- read_csv("simulation_data/params_wt.csv")
meas_wt <- read_csv("simulation_data/wt_speeds.csv")

param_mut <- read_csv("simulation_data/params_mut.csv")
meas_mut <- read_csv("simulation_data/mut_speeds.csv")


# Numerical solutions
df_num <- read_csv("numerical_data/sweep probability vs speed ratio c_wt 0.152 mutation rate 2.34e-06 simplifying assumption 0.csv")
df_num <- rename(df_num, c("ratio"="Speed ratio", "twodim"="2D", "threedim"="3D"))

# Some converting things
df_num$twodim <- as.complex(df_num$twodim)
df_num$twodim <- Re(df_num$twodim)

df_num$threedim <- as.complex(df_num$threedim)
df_num$threedim <- Re(df_num$threedim)

# Merge the parameters with the data dataframe
df <- merge(probs, params, by.x = "id" , by.y = "index" )
df_wt <- merge(meas_wt, param_wt, by.x = "id" , by.y = "index" )
df_mut <- merge(meas_mut, param_mut, by.x = "id" , by.y = "index" )

###########################
### Read out the speeds ###
###########################

# Filter data of interest
df_wt_f <- df_wt %>% filter(log2_deme==4, migration_rate==0.05, 
                            normal_birth_rate>=0.9, normal_birth_rate<=0.95)
df_mut_f <- df_mut %>% filter(log2_deme==4, migration_rate==0.05)

c_wt <- mean(df_wt_f$speed_late)
std_wt <- sd(df_wt_f$speed_late)

c_mut <- c(0,0,0,0,0,0,0,0)
s_vector = c(0.1,0.2,0.3,0.4,0.5,1.0,1.5,2.0)
j=1
for (s in s_vector) {
  df_mut_ff <- df_mut_f %>% filter(s_driver_birth==s)
  #print(s)
  c_mut[j] <- mean(df_mut_ff$speed_late)
  j <- j+1
}

# Prepare the speed-selection correspondence
s_xaxis = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,
            1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
c_xaxis = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,
            1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
j=1
for (s in s_xaxis) {
  df_mut_ff <- df_mut_f %>% filter(s_driver_birth==s)
  #print(s)
  c_xaxis[j] <- mean(df_mut_ff$speed_late)
  j <- j+1
}

####################
### Computations ###
####################

# Define parameters of the simulation
pi <- 3.1415
mig <- 0.05
s_wt <- 0.1
demesize <- 16
# Compute Fisher speed of the wildtype
c_Fisher <- 2*sqrt(s_wt*(mig*demesize)/2)

s_vector = c(0.1,0.2,0.3,0.4,0.5,1.0,1.5,2.0)
mu_vector = c(1e-06,1e-05,1e-04,1e-03,1e-02)

# Filter data of interest
df_f1 <- df %>% filter(mu_driver_birth == 1e-04)
df_f2 <- df %>% filter(mu_driver_birth == 1e-05)
df_f3 <- df %>% filter(mu_driver_birth == 1e-06)

# Compute analytic results
#speeds <- df_ff$pop_radius_at_first_mut_init/df_ff$first_mutant_init_time
#c_wt_data <- mean(speeds)
#c_m <- 2*sqrt(s_vector*(mig*demesize)/2)

# Compute analytic sweep probability
#c_m_art <- 100:2000/1000
#xaxis <- c_m_art/c_wt
#sweep_ana <- ((c_m_art-c_wt)/c_m_art)^2

xaxis <- s_xaxis/s_wt
sweep_ana <- ((c_xaxis-c_wt)/c_xaxis)^2


# Compute the sweep probabilities from data
x_data <- s_vector/s_wt
y_data1 <- c(0,0,0,0,0,0,0,0)
i1=1
for (s in s_vector) {
  df_ff <- df_f1 %>% filter(s_driver_birth == s)
  logical <- df_ff$x100
  prob <- sum(logical)/1000
  y_data1[i1] <- prob 
  i1 <- i1+1
}

y_data2 <- c(0,0,0,0,0,0,0,0)
i2=1
for (s in s_vector) {
  df_ff <- df_f2 %>% filter(s_driver_birth == s)
  logical <- df_ff$x100
  prob <- sum(logical)/1000
  y_data2[i2] <- prob 
  i2 <- i2+1
}

y_data3 <- c(0,0,0,0,0,0,0,0)
i3=1
for (s in s_vector) {
  df_ff <- df_f3 %>% filter(s_driver_birth == s)
  logical <- df_ff$x100
  prob <- sum(logical)/1000
  y_data3[i3] <- prob 
  i3 <- i3+1
}

fix <- (1-1/(1+s_vector))/(1-1/(1+s_vector)^16)

#############
### Plots ###
#############

cols <- brewer.pal(12,"Paired")

legend_1 <- "simulation"
legend_2 <- "prediction (exact)"
legend_3 <- "prediction (approx.)"

colours <- t(c(cols[4],cols[3]))
colnames(colours) <- c(legend_2, legend_3)

lty <- t(c("22", "solid"))
colnames(lty) <- c(legend_2, legend_3)

ggplot() +
  geom_line(aes(x=2:20, y=df_num$twodim, linetype=legend_2, color=legend_2), size=1.5) +
  geom_line(aes(x=xaxis, y=sweep_ana,linetype=legend_3, color=legend_3), linewidth=1.5) +
  geom_point(aes(x=x_data, y=y_data1), color="black", size=4.0, shape=16) +
  geom_point(aes(x=x_data, y=y_data2), color="black", size=4.0, shape=17) +
  geom_point(aes(x=x_data, y=y_data3), color="black", size=4.0, shape=18) +
  #geom_line(aes(x=x_data, y=fix),col = "black", linewidth=2.0) +
  labs(x="ratio of fitness difference, *a<sub>m* / *a<sub>wt*", y="Pr(sweep)") +
  xlim(1,20) + ylim(0,1) +
  scale_color_manual(values = colours) +
  scale_linetype_manual(values = lty) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.8,0.2), legend.title = element_blank())


### Save the figure

fig5B <- ggplot() +
  geom_line(aes(x=2:20, y=df_num$twodim, linetype=legend_2, color=legend_2), size=1.5) +
  geom_line(aes(x=xaxis, y=sweep_ana, linetype=legend_3, color=legend_3), linewidth=1.5) +
  geom_point(aes(x=x_data, y=y_data1), color="black", size=4.0, shape=16) +
  geom_point(aes(x=x_data, y=y_data2), color="black", size=4.0, shape=17) +
  geom_point(aes(x=x_data, y=y_data3), color="black", size=4.0, shape=18) +
  #geom_line(aes(x=x_data, y=fix),col = "black", linewidth=2.0) +
  labs(x="ratio of fitness difference, *a<sub>m* / *a<sub>wt*", y="Pr(sweep)") +
  xlim(1,20) + ylim(0,1) +
  scale_color_manual(values = colours) +
  scale_linetype_manual(values = lty) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.7,0.2), legend.title = element_blank())

a = 1.5
ggsave("/Users/stein02/Desktop/Plots for the Sweeps/Figure_3/figures/Sweep_sim.png", fig5B, width = 4^a, height = 3^a)



### A different legend

legend_1 <- "simulations, µ=10<sup>-4"
legend_2 <- "simulations, µ=10<sup>-5"
legend_3 <- "simulations, µ=10<sup>-6"

shapes <- t(c(16,17,18))
colnames(shapes) <- c(legend_1, legend_2, legend_3)

ggplot() +
  geom_line(aes(x=2:20, y=df_num$twodim), size=1.5, col=cols[3]) +
  geom_line(aes(x=xaxis, y=sweep_ana), linewidth=1.5, col=cols[4], linetype = "22") +
  geom_point(aes(x=x_data, y=y_data1, shape=legend_1), size=4.0) +
  geom_point(aes(x=x_data, y=y_data2, shape=legend_2), size=4.0) +
  geom_point(aes(x=x_data, y=y_data3, shape=legend_3), size=4.0) +
  labs(x="relative selective advantage, *s<sub>m* / *s<sub>wt*", y="Pr(sweep)") +
  xlim(1,20) + ylim(0,1) +
  scale_shape_manual(values = shapes) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.8,0.2), legend.title = element_blank(), 
        legend.text = element_markdown())

fig5B2 <- ggplot() +
  geom_line(aes(x=2:20, y=df_num$twodim), size=1.5, col=cols[3]) +
  geom_line(aes(x=xaxis, y=sweep_ana), linewidth=1.5, col=cols[4], linetype = "22") +
  geom_point(aes(x=x_data, y=y_data1, shape=legend_1), size=4.0) +
  geom_point(aes(x=x_data, y=y_data2, shape=legend_2), size=4.0) +
  geom_point(aes(x=x_data, y=y_data3, shape=legend_3), size=4.0) +
  labs(x="relative selective advantage, *s<sub>m* / *s<sub>wt*", y="Pr(sweep)") +
  xlim(1,20) + ylim(0,1) +
  scale_shape_manual(values = shapes) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.7,0.2), legend.title = element_blank(), 
        legend.text = element_markdown())

a = 1.5
ggsave("/Users/stein02/Desktop/Plots for the Sweeps/final figures/fig5Ball2.png", fig5B2, width = 4^a, height = 3^a)




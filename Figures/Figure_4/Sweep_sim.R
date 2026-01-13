# title: Plotting sweep probabilities
# author: Alexander Stein

#################
### libraries ###
#################

library("tidyverse")
library("ggtext")
library("RColorBrewer")

pathtosave <- "~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_4/figures/"
setwd("~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_4/")

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

# --- Labels (define ONCE) ---
#sim_1 <- "simulations, µ=10<sup>-4</sup>"
#sim_2 <- "simulations, µ=10<sup>-5</sup>"
#sim_3 <- "simulations, µ=10<sup>-6</sup>"

sim_1 <- "simulations, <i>μ̃</i>=10<sup>-4</sup>"
sim_2 <- "simulations, <i>μ̃</i>=10<sup>-5</sup>"
sim_3 <- "simulations, <i>μ̃</i>=10<sup>-6</sup>"


pred_1 <- "prediction (exact)"
pred_2 <- "prediction (approx.)"

# --- Manual scales (named vectors) ---
cols <- brewer.pal(12, "Paired")

colours   <- setNames(c(cols[4], cols[3]), c(pred_1, pred_2))
linetypes <- setNames(c("22", "solid"),     c(pred_1, pred_2))
shapes    <- setNames(c(16, 17, 18),        c(sim_1, sim_2, sim_3))

# --- Plot ---
fig <- ggplot() +
  # prediction lines
  geom_line(aes(x = 2:20,  y = df_num$twodim, color = pred_1, linetype = pred_1),
            linewidth = 1.5) +
  geom_line(aes(x = xaxis, y = sweep_ana,     color = pred_2, linetype = pred_2),
            linewidth = 1.5) +
  
  # simulation points (shape mapped -> legend)
  geom_point(aes(x = x_data, y = y_data1, shape = sim_1), size = 4, colour = "black") +
  geom_point(aes(x = x_data, y = y_data2, shape = sim_2), size = 4, colour = "black") +
  geom_point(aes(x = x_data, y = y_data3, shape = sim_3), size = 4, colour = "black") +
  
  # IMPORTANT: use VALUES only (breaks optional)
  scale_color_manual(values = colours) +
  scale_linetype_manual(values = linetypes) +
  scale_shape_manual(values = shapes) +
  
  # if you want ordering, add breaks AFTER it works:
  # scale_color_manual(values = colours, breaks = c(pred_1, pred_2)) +
  # scale_linetype_manual(values = linetypes, breaks = c(pred_1, pred_2)) +
  # scale_shape_manual(values = shapes, breaks = c(sim_1, sim_2, sim_3)) +
  
  labs(x = "ratio of fitness difference, *a<sub>m</sub>* / *a<sub>wt</sub>*",
       y = "Pr(sweep)") +
  coord_cartesian(xlim = c(1, 20), ylim = c(0, 1)) +
  
  theme_bw(base_size = 25) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.text  = element_markdown(),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.3),
    legend.box.spacing = grid::unit(0, "pt"),
    legend.spacing.y   = grid::unit(0, "pt"),
    legend.box.margin  = margin(0, 0, 0, 0),
    legend.margin      = margin(0, 0, 0, 0)
  )

fig

a=1.5
ggsave(paste(pathtosave,"sweeps_validate.pdf", sep=""), fig, device = cairo_pdf, width = 4^a, height = 3^a)


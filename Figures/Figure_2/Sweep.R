# title: Plotting sweep probabilities
# author: Alexander Stein

### libraries
library("tidyverse")
library("ggtext")
library("RColorBrewer")

# Make sure to start in the correct working directory
setwd("~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_2")
pathtosave <- "~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_2/figures/"

### Import data

df <- read_csv("numerical_data/sweep probability vs speed ratio c_wt 0.152 mutation rate 2.34e-06 simplifying assumption 0 2.csv")
df <- rename(df, c("ratio"="Speed ratio", "twodim"="2D", "threedim"="3D"))

# Some converting things
df$twodim <- as.complex(df$twodim)
df$twodim <- Re(df$twodim)

df$threedim <- as.complex(df$threedim)
df$threedim <- Re(df$threedim)

### Define paramter ranges
mu = 1e-5

# Approximate analytic results
c_wt <- 1.0
c_m <- 10:100/10
speedratio <- c_m/c_wt
sweep_1D <- ((c_m-c_wt)/c_m)
sweep_2D <- ((c_m-c_wt)/c_m)^2
sweep_3D <- ((c_m-c_wt)/c_m)^3

# Exact result in 1D
beta_prim <- (c_m-c_wt)/c_wt
sweep_exact <- beta_prim/sqrt(1+beta_prim)*atan2(1,sqrt(1+beta_prim))

# PLot data and prediction of f_Y in 2D using "pop_radius_at_first_mut_init"

cols <- brewer.pal(12,"Paired")

ggplot() + 
  geom_line(aes(x=speedratio, y=sweep_exact),col = cols[1], size=1.5) +
  geom_line(aes(x=speedratio, y=sweep_1D),col = cols[2], linewidth=1.5, linetype="22") +
  geom_line(aes(x=df$ratio, y=df$twodim),col = cols[3], size=1.5) +
  geom_line(aes(x=speedratio, y=sweep_2D),col = cols[4], linewidth=1.5, linetype="22") +
  geom_line(aes(x=df$ratio, y=df$threedim),col = cols[7], size=1.5) +
  geom_line(aes(x=speedratio, y=sweep_3D),col = cols[8], linewidth=1.5, linetype="22") +
  labs(x="relative mutant speed, *c<sub>m* / *c<sub>wt*", y="Pr(sweep)") +
  xlim(1,10) + ylim(0,1) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), legend.position = "none")

### Save the figure

fig4B <- ggplot() + 
  geom_line(aes(x=speedratio, y=sweep_exact),col = cols[1], size=1.5) +
  geom_line(aes(x=speedratio, y=sweep_1D),col = cols[2], linewidth=1.5, linetype="22") +
  geom_line(aes(x=df$ratio, y=df$twodim),col = cols[3], size=1.5) +
  geom_line(aes(x=speedratio, y=sweep_2D),col = cols[4], linewidth=1.5, linetype="22") +
  geom_line(aes(x=df$ratio, y=df$threedim),col = cols[7], size=1.5) +
  geom_line(aes(x=speedratio, y=sweep_3D),col = cols[8], linewidth=1.5, linetype="22") +
  labs(x="relative mutant speed, *c<sub>m* / *c<sub>wt*", y="Pr(sweep)") +
  xlim(1,10) + ylim(0,1) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), legend.position = "none")

a = 1.5
ggsave(paste(pathtosave,"Sweep.pdf",sep=""), fig4B, width = 4^a, height = 3^a)





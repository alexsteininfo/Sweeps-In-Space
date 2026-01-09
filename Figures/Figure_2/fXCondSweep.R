# title: Plotting sweep probabilities
# author: Alexander Stein

### libraries
library("tidyverse")
library("ggtext")
library("RColorBrewer")
library(pracma, include.only = c("logseq"))

# Make sure to start in the correct working directory
setwd("~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_2")
pathtosave <- "~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_2/figures/"

### Import and process data
df <- read_csv("numerical_data/x conditioned on sweep.csv")

df <- rename(df, c("xval_twodim"="xval 2D", "yval_twodim"="yval 2D"))
df <- rename(df, c("xval_threedim"="xval 3D", "yval_threedim"="yval 3D"))

df$yval_twodim <- as.complex(df$yval_twodim)
df$yval_twodim <- Re(df$yval_twodim)

df$yval_threedim <- as.complex(df$yval_threedim)
df$yval_threedim <- Re(df$yval_threedim)

### Approximate results for 1D, 2D and 3D

# Define paramters
rho <- 0.234
mu = rho*1e-5
c_wt <- 0.152
c_m <- 0.307

beta <- (c_m-c_wt)/c_m
theta_1d <- (c_wt/mu)^(1/2)
theta_2d <- (3*c_wt/pi/mu)^(1/3)
theta_3d <- (3*c_wt/pi/mu)^(1/4)

#x_values <- 0:500
#x_values <- ep
x_values <- logseq(1,500,400)
fX_1d <- 2*x_values*exp(-x_values^2/theta_1d^2/beta)/(theta_1d^2*beta)
fX_2d <- 3*x_values^2*exp(-x_values^3/(theta_2d^3*beta^2))/(theta_2d^3*beta^2)
fX_3d <- 4*x_values^3*exp(-x_values^4/(theta_3d^4*beta^3))/(theta_3d^4*beta^3)

### Exact result in 1D

# Pr(sweep)
beta_prim <- (c_m-c_wt)/c_wt
sweep_exact <- beta_prim/sqrt(1+beta_prim)*atan2(1,sqrt(1+beta_prim))

# fX
f_X <- 2*x_values*exp(-x_values^2/theta_1d^2)/(theta_1d^2)

# Pr(sweep|x)
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1 # define the error function
alpha_1d <- ((c_m-c_wt)/mu)^(1/2)
PrSweepX <- sqrt(pi)*alpha_1d/(2*x_values)*exp(-x_values^2/alpha_1d^2)*erf(x_values/alpha_1d)

# ultimately, we have
fX_exact <- PrSweepX*f_X/sweep_exact


### Plot data and prediction of f_Y in 2D using "pop_radius_at_first_mut_init"

cols <- brewer.pal(12,"Paired")

ggplot() + 
  geom_line(aes(x=x_values, y=fX_exact),col = cols[1], linewidth=1.5) +
  geom_line(aes(x=x_values, y=fX_1d),col = cols[2], linewidth=1.5, linetype="22") +
  geom_line(aes(x=df$xval_twodim, y=df$yval_twodim),col = cols[3], linewidth=1.5) +
  geom_line(aes(x=x_values, y=fX_2d),col = cols[4], linewidth=1.5, linetype="22") +
  geom_line(aes(x=df$xval_threedim, y=df$yval_threedim),col = cols[7], linewidth=1.5) +
  geom_line(aes(x=x_values, y=fX_3d),col = cols[8], linewidth=1.5, linetype="22") +
  labs(x="population radius, *x*", y="*f<sub>X*(*X=x*|sweep)") +
  scale_x_continuous(trans='log10', limits=c(0.5,500)) +
  scale_y_continuous(limits=c(0,0.10)) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), legend.position = "none")


### Save the figure

fig <- ggplot() + 
  geom_line(aes(x=x_values, y=fX_exact),col = cols[1], linewidth=1.5) +
  geom_line(aes(x=x_values, y=fX_1d),col = cols[2], linewidth=1.5, linetype="22") +
  geom_line(aes(x=df$xval_twodim, y=df$yval_twodim),col = cols[3], linewidth=1.5) +
  geom_line(aes(x=x_values, y=fX_2d),col = cols[4], linewidth=1.5, linetype="22") +
  geom_line(aes(x=df$xval_threedim, y=df$yval_threedim),col = cols[7], linewidth=1.5) +
  geom_line(aes(x=x_values, y=fX_3d),col = cols[8], linewidth=1.5, linetype="22") +
  labs(x="population radius, *x*", y="*f<sub>X*(*X=x*|sweep)") +
  scale_x_continuous(trans='log10', limits=c(0.5,500)) +
  scale_y_continuous(limits=c(0,0.10)) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), legend.position = "none")

a = 1.5
ggsave(paste(pathtosave,"fXCondSweep.pdf",sep=""), fig, width = 4^a, height = 3^a)














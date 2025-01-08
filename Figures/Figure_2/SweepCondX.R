# title: Plotting sweep probabilities
# author: Alexander Stein

### libraries
library("tidyverse")
library("ggtext")
library("RColorBrewer")

setwd("/Users/stein02/Desktop/Plots for the Sweeps/Figure_2/")


### Import data and conversions

df <- read_csv("numerical_data/sweep probability conditioned on x.csv")

df <- rename(df, c("xval_twodim"="x_val 2D", "yval_twodim"="cond_sweep 2D"))
df <- rename(df, c("xval_threedim"="x_val 3D", "yval_threedim"="cond_sweep 3D"))

df$yval_twodim <- as.complex(df$yval_twodim)
df$yval_twodim <- Re(df$yval_twodim)

df$yval_threedim <- as.complex(df$yval_threedim)
df$yval_threedim <- Re(df$yval_threedim)

df <- df %>% filter(xval_twodim >= 2.0)

### Define paramters
rho <- 0.234
mu = rho*1e-5
c_wt <- 0.152
c_m <- 0.307

### Approximate results for 1D, 2D and 3D
alpha_1d <- ((c_m-c_wt)/mu)^(1/2)
alpha_2d <- ( 3*(c_m-c_wt)^2/(pi*mu*(2*c_m-c_wt)) )^(1/3)
alpha_3d <- ( 3*(c_m-c_wt)^3/(pi*mu*(c_wt^2-3*c_wt*c_m+3*c_m^2)) )^(1/4)

x_values <- 0:1000
sweep_1d <- exp( -(x_values/alpha_1d)^2 )
sweep_2d <- exp( -(x_values/alpha_2d)^3 )
sweep_3d <- exp( -(x_values/alpha_3d)^4 )

### Exact result in 1D
# Pr(sweep|x)
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1 # define the error function
PrSweepX <- sqrt(pi)*alpha_1d/(2*x_values)*exp(-x_values^2/alpha_1d^2)*erf(x_values/alpha_1d)

### Plots

cols <- brewer.pal(12,"Paired")

legend_1 <- "1D exact"
legend_2 <- "1D approx."
legend_3 <- "2D exact"
legend_4 <- "2D approx."
legend_5 <- "3D exact"
legend_6 <- "3D approx."

colours <- t(c(cols[2], cols[1], cols[4], cols[3], cols[8], cols[7]))
colnames(colours) <- c(legend_1, legend_2, legend_3, legend_4, legend_5, legend_6)
lty <- t(c("22", "solid", "22", "solid", "22", "solid"))
colnames(lty) <- c(legend_1, legend_2, legend_3, legend_4, legend_5, legend_6)


ggplot() + 
  geom_line(aes(x=x_values, y=PrSweepX, color=legend_1, linetype=legend_1), linewidth=1.5) +
  geom_line(aes(x=x_values, y=sweep_1d, color=legend_2, linetype=legend_2), linewidth=1.5) +
  geom_line(aes(x=df$xval_twodim, y=df$yval_twodim, color=legend_3, linetype=legend_3), linewidth=1.5) +
  geom_line(aes(x=x_values, y=sweep_2d, color=legend_4, linetype=legend_4), linewidth=1.5) +
  geom_line(aes(x=df$xval_threedim, y=df$yval_threedim,color=legend_5, linetype=legend_5), linewidth=1.5) +
  geom_line(aes(x=x_values, y=sweep_3d, color=legend_6, linetype=legend_6), linewidth=1.5) +
  labs(x="population radius, *x*", y="Pr(sweep|*X=x*)") +
  scale_x_continuous(trans='log10', limits=c(0.5,1000)) +
  scale_color_manual(values = colours) +
  scale_linetype_manual(values = lty) +
  #scale_y_continuous(limits=c(0,0.10)) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.2,0.5), legend.title = element_blank(), legend.text = element_markdown())


### Saving the figures
fig <- ggplot() + 
  geom_line(aes(x=x_values, y=PrSweepX, color=legend_1, linetype=legend_1), linewidth=1.5) +
  geom_line(aes(x=x_values, y=sweep_1d, color=legend_2, linetype=legend_2), linewidth=1.5) +
  geom_line(aes(x=df$xval_twodim, y=df$yval_twodim, color=legend_3, linetype=legend_3), linewidth=1.5) +
  geom_line(aes(x=x_values, y=sweep_2d, color=legend_4, linetype=legend_4), linewidth=1.5) +
  geom_line(aes(x=df$xval_threedim, y=df$yval_threedim,color=legend_5, linetype=legend_5), linewidth=1.5) +
  geom_line(aes(x=x_values, y=sweep_3d, color=legend_6, linetype=legend_6), linewidth=1.5) +
  labs(x="population radius, *x*", y="Pr(sweep|*X=x*)") +
  scale_x_continuous(trans='log10', limits=c(0.5,1000)) +
  scale_color_manual(values = colours) +
  scale_linetype_manual(values = lty) +
  #scale_y_continuous(limits=c(0,0.10)) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.175,0.5), legend.title = element_blank(), legend.text = element_markdown())

a = 1.5
ggsave("/Users/stein02/Desktop/Plots for the Sweeps/Figure_2/figures/SweepCondX.png", fig, width = 4^a, height = 3^a)







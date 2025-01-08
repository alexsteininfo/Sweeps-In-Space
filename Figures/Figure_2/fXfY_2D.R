# title: Plotting sweep probabilities
# author: Alexander Stein

### libraries
library("tidyverse")
library("expint")     # import of incomplete gamma function
library("ggtext")     # for text conversion in axis labels
library("RColorBrewer")

setwd("/Users/stein02/Desktop/Plots for the Sweeps/Plots with analytical/")

# Define paramters
mu <- 1e-5
c_wt <- 0.152
c_m <- 0.307
rho <- 0.234

theta_2d <- (3*c_wt/(pi*rho*mu))^(1/3)
#theta_2d <- 1.0

xaxis <- 1:1000/10
f_X <- 3*xaxis^2*exp(-xaxis^3/(theta_2d^3))/(theta_2d^3)

f_Y <- 3*theta_2d^2*gammainc(5/3,xaxis^3/theta_2d^3)
f_Y <- f_Y - 12*theta_2d*xaxis*gammainc(4/3,xaxis^3/theta_2d^3)
f_Y <- f_Y + 9*xaxis^2*exp(-xaxis^3/theta_2d^3)
f_Y <- f_Y + 2*xaxis^2*gammainc(0,xaxis^3/theta_2d^3)
f_Y <- f_Y*3/(2*theta_2d^3)

f_Y2 <- 2*xaxis/theta_2d^2*gammainc(1/3,xaxis^3/theta_2d^3)

### Plotting

cols <- brewer.pal(12,"Paired")

ggplot() + 
  geom_line(aes(x=xaxis, y=f_X, color="f_X"), linewidth = 1.5) +
  geom_line(aes(x=xaxis, y=f_Y2, color="f_Y"), linewidth = 1.5) +
  labs(x="population radius, *x* or *y*", y="*f<sub>X*(*x*) or *f<sub>Y*(*y*)") +
  xlim(0,100) + ylim(0,0.035) +
  scale_color_manual(values = c(f_X = cols[4], f_Y = cols[3]),
                     labels = c(f_X = "*f<sub>X*", f_Y = "*f<sub>Y*"),
                     limits = c("f_X", "f_Y")) +
  #annotate("text", x = 1.0, y = 1.0, label = "A", size=8) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.9,0.8), legend.title = element_blank(), legend.text = element_markdown())



# Saving the figures

fig3B <- ggplot() + 
  geom_line(aes(x=xaxis, y=f_X, color="f_X"), linewidth = 1.5) +
  geom_line(aes(x=xaxis, y=f_Y2, color="f_Y"), linewidth = 1.5) +
  labs(x="population radius, *x* or *y*", y="*f<sub>X*(*x*) or *f<sub>Y*(*y*)") +
  xlim(0,100) + ylim(0,0.035) +
  scale_color_manual(values = c(f_X = cols[4], f_Y = cols[3]),
                     labels = c(f_X = "*f<sub>X*", f_Y = "*f<sub>Y*"),
                     limits = c("f_X", "f_Y")) +
  #annotate("text", x = 1.0, y = 1.0, label = "A", size=8) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.9,0.8), legend.title = element_blank(), legend.text = element_markdown())

a = 1.5
ggsave("/Users/stein02/Desktop/Plots for the Sweeps/Figure_2/figures/fig3B.png", fig3B, width = 4^a, height = 3^a)










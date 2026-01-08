# title: Plotting sweep probabilities
# author: Alexander Stein

### libraries
library("tidyverse")
library("expint")     # import of incomplete gamma function
library("ggtext")     # for text conversion in axis labels
library("RColorBrewer")

# Make sure to start in the correct working directory
#setwd("/Figures/Figure_2/")

# Define paramters
mu <- 1e-5
c_wt <- 0.152
c_m <- 0.307
rho <- 0.234
s <- 0.3

theta_1D <- (c_wt/(rho*mu))^(1/2)
#theta_1D <- 1.0

xaxis <- 1:100000/100
f_X <- 2*xaxis*exp(-xaxis^2/theta_1D^2)/theta_1D^2

f_Y <- 2/theta_1D*gammainc(1/2,xaxis^2/theta_1D^2)
f_Y <- f_Y - 2*xaxis/theta_1D^2*gammainc(0,xaxis^2/theta_1D^2)

f_Y2 <- gammainc(1/2,xaxis^2/theta_1D^2)/theta_1D

### Plotting

cols <- brewer.pal(12,"Paired")

ggplot() + 
  geom_line(aes(x=xaxis, y=f_X, color="f_X"), linewidth = 1.5) +
  geom_line(aes(x=xaxis, y=f_Y2, color="f_Y"), linewidth = 1.5) +
  labs(x="population radius, *x* or *y*", y="*f<sub>X*(*x*) or *f<sub>Y*(*y*)") +
  xlim(0,1000) + ylim(0,0.006) + 
  scale_color_manual(values = c(f_X = cols[2], f_Y = cols[1]),
                     labels = c(f_X = "*f<sub>X*", f_Y = "*f<sub>Y*"),
                     limits = c("f_X", "f_Y")) +
  #annotate("text", x = 1.0, y = 1.0, label = "A", size=8) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.9,0.8), legend.title = element_blank(), legend.text = element_markdown())


### If I want to save it

fig3A <- ggplot() + 
  geom_line(aes(x=xaxis, y=f_X, color="f_X"), linewidth = 1.5) +
  geom_line(aes(x=xaxis, y=f_Y2, color="f_Y"), linewidth = 1.5) +
  labs(x="population radius, *x* or *y*", y="*f<sub>X*(*x*) or *f<sub>Y*(*y*)") +
  xlim(0,1000) + ylim(0,0.006) + 
  scale_color_manual(values = c(f_X = cols[2], f_Y = cols[1]),
                     labels = c(f_X = "*f<sub>X*", f_Y = "*f<sub>Y*"),
                     limits = c("f_X", "f_Y")) +
  #annotate("text", x = 1.0, y = 1.0, label = "A", size=8) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.9,0.8), legend.title = element_blank(), legend.text = element_markdown())

a = 1.5
ggsave("/figures/fig3A.png", fig3A, width = 4^a, height = 3^a)



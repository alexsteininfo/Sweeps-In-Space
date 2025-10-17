# title: Plotting sweep probabilities
# author: Alexander Stein

### libraries
library("tidyverse")
library("expint")     # import of incomplete gamma function
library("ggtext")     # for text conversion in axis labels
library("RColorBrewer")

setwd("/Users/alexanderstein/Documents/GitHub/Sweeps-In-Space/Figures/Figure_2/figures/")

# Define paramters
mu <- 1e-5
c_wt <- 0.152
c_m <- 0.307
rho <- 0.234

theta_3d <- (3*c_wt/pi/(rho*mu))^(1/4)
#theta_3d <- 1.0

xaxis <- 1:3000/100
f_X <- 4*xaxis^3*exp(-xaxis^4/(theta_3d^4))/(theta_3d^4)

f_Y <- theta_3d*gammainc(1/4,xaxis^4/theta_3d^4)
f_Y <- f_Y - xaxis*gammainc(0,xaxis^4/theta_3d^4)
f_Y <- f_Y*12*xaxis^2/theta_3d^4

f_Y2 <- 3*xaxis^2/theta_3d^3*gammainc(1/4,xaxis^4/theta_3d^4)

### Plotting

cols <- brewer.pal(12,"Paired")

ggplot() + 
  geom_line(aes(x=xaxis, y=f_X, color="f_X"), linewidth = 1.5) +
  geom_line(aes(x=xaxis, y=f_Y2, color="f_Y"), linewidth = 1.5) +
  labs(x="population radius, *x* or *y*", y="*f<sub>X*(*x*) or *f<sub>Y*(*y*)") +
  #xlim(0,100) + ylim(0,0.05) +
  scale_color_manual(values = c(f_X = cols[8], f_Y = cols[7]),
                     labels = c(f_X = "*f<sub>X*", f_Y = "*f<sub>Y*"),
                     limits = c("f_X", "f_Y")) +
  #annotate("text", x = 1.0, y = 1.0, label = "A", size=8) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.9,0.8), legend.title = element_blank(), legend.text = element_markdown())

### If I want to save it

fig3C <- ggplot() + 
  geom_line(aes(x=xaxis, y=f_X, color="f_X"), linewidth = 1.5) +
  geom_line(aes(x=xaxis, y=f_Y2, color="f_Y"), linewidth = 1.5) +
  labs(x="population radius, *x* or *y*", y="*f<sub>X*(*x*) or *f<sub>Y*(*y*)") +
  #xlim(0,100) + ylim(0,0.05) +
  scale_color_manual(values = c(f_X = cols[8], f_Y = cols[7]),
                     labels = c(f_X = "*f<sub>X*", f_Y = "*f<sub>Y*"),
                     limits = c("f_X", "f_Y")) +
  #annotate("text", x = 1.0, y = 1.0, label = "A", size=8) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.9,0.8), legend.title = element_blank(), legend.text = element_markdown())

a = 1.5
ggsave("fig3C.png", fig3C, width = 4^a, height = 3^a)




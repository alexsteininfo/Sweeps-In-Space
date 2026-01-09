# title: sweep probabilites for alternative growth laws
# author: alexander stein

library("tidyverse")
library("ggtext")
library("RColorBrewer")

### Import data

pathtosave <- "~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_3/figures/"
setwd("~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_3/")

df <- read_csv("numerical_data/sweep probability vs speed ratio c_wt 0.152 mutation rate 2.34e-06 simplifying assumption 0 2.csv")
df <- rename(df, c("ratio"="Speed ratio", "twodim"="2D", "threedim"="3D"))

# Some converting things
df$twodim <- as.complex(df$twodim)
df$twodim <- Re(df$twodim)

df$threedim <- as.complex(df$threedim)
df$threedim <- Re(df$threedim)


### Create vectors for plotting
c_wt <- 0.15
mu <- 0.23*10^(-5)
#c_m <- c(0.15,0.23,0.31,0.38,0.46,0.53,0.60,0.68,0.75,0.82, 0.89,0.96,1.03,1.11,1.17,1.24,1.31,1.39,1.46,1.52)

c_m <- 0:100/10*c_wt

s_wt <- 0.10
s_m <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0, 1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)

r_wt_1 <- 1.0
r_m_1 <- c(1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0, 2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0)

p_approx <- (c_m-c_wt)^3/c_m^3
p_approx_2D <- (c_m-c_wt)^2/c_m^2

v <- c_m/c_wt
beta <- sqrt(pmax(v^2 - 1, 0))
p_antal <- ((9+beta^2)/beta^2)*2/(1+exp(3*pi/beta))

x0=10
p_const1 <- exp(-mu*pi*x0^4/c_m)
x0=15
p_const10 <- exp(-mu*pi*x0^4/c_m)
x0=20
p_const100 <- exp(-mu*pi*x0^4/c_m)

p_exp <- s_m
j <-1
for (s in s_m){
  r_wt = 1.0
  r_m = r_m_1[j]
  chi = 0.95
  u = 1e-5
  integrand <- function(t) {u*exp(r_wt*t -
                                    u/r_wt*
                                    exp( (r_wt*(r_m*t-log(1/chi-1)))/
                                           (r_m-r_wt) )
  )}
  sol <- integrate(integrand, lower = 0, upper = Inf)
  p_exp[j] <- sol$value
  j <- j+1
}

#xaxis <- s_m/s_wt
xaxis <- c_m

################
### Plotting ###
################

legend_1 <- "dispersal throughout"
legend_2 <- "boundary growth"
legend_3 <- "fixed population size"

cols <- brewer.pal(12,"Paired")

# named colour vector (NOT a matrix)
colours <- c(
  "dispersal throughout"   = cols[8],
  "boundary growth"        = cols[10],
  "fixed population size"  = cols[6]
)

# named linetype vector
lty <- c(
  "dispersal throughout"   = "solid",
  "boundary growth"        = "solid",
  "fixed population size"  = "solid"
)


ggplot() + 
  #geom_line(aes(x=xaxis, y=p_approx, linetype=legend_1, color=legend_1), linewidth=1.5) +
  geom_line(aes(x=df$ratio*c_wt, y=df$threedim, linetype=legend_1, color=legend_1), size=1.5) +
  geom_line(aes(x=xaxis, y=p_antal, color=legend_2), linewidth=1.5) +
  geom_line(aes(x=xaxis, y=p_const1, color=legend_3), linewidth=1.5, linetype="12") +
  geom_line(aes(x=xaxis, y=p_const10, color=legend_3), linewidth=1.5, linetype="22") +
  geom_line(aes(x=xaxis, y=p_const100, color=legend_3), linewidth=1.5, linetype="solid") +
  #geom_vline(xintercept=0.15, linewidth=2.0, linetype="12") +
  scale_color_manual(values = colours, limits = c(legend_1, legend_2, legend_3)) +
  scale_linetype_manual(values = lty, limits = c(legend_1, legend_2, legend_3)) +
  labs(x="mutant speed, *c<sub>m*", y="Pr(sweep)") +
  #xlim(1,10) + ylim(0,1) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.74,0.82), legend.title = element_blank()) + theme(legend.position = "none")

#theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), legend.position = "none", legend.title = element_blank())



fig <- ggplot() + 
  #geom_line(aes(x=xaxis, y=p_approx, linetype=legend_1, color=legend_1), linewidth=1.5) +
  geom_line(aes(x=df$ratio*c_wt, y=df$threedim, linetype=legend_1, color=legend_1), size=1.5) +
  geom_line(aes(x=xaxis, y=p_antal, color=legend_2), linewidth=1.5) +
  geom_line(aes(x=xaxis, y=p_const1, color=legend_3), linewidth=1.5, linetype="12") +
  geom_line(aes(x=xaxis, y=p_const10, color=legend_3), linewidth=1.5, linetype="22") +
  geom_line(aes(x=xaxis, y=p_const100, color=legend_3), linewidth=1.5, linetype="solid") +
  #geom_vline(xintercept=0.15, linewidth=2.0, linetype="12") +
  scale_color_manual(values = colours, limits = c(legend_1, legend_2, legend_3)) +
  scale_linetype_manual(values = lty, limits = c(legend_1, legend_2, legend_3)) +
  labs(x="mutant speed, *c<sub>m*", y="Pr(sweep)") +
  #xlim(1,10) + ylim(0,1) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.74,0.82), legend.title = element_blank()) + theme(legend.position = "none")

#theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), legend.position = "none", legend.title = element_blank())

a = 1.5
ggsave(paste(pathtosave,"SweepAlt.pdf"), fig, width = 4^a, height = 3^a)

leg <- cowplot::get_legend(
  fig + theme(
    legend.position = "right",
    legend.title = element_blank()
  )
)

ggsave(paste(pathtosave,"SweepAlt_leg.pdf"), leg, width = 4^a, height = 3^a)


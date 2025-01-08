# title: sweep probabilites for alternative growth laws
# author: alexander stein

library("tidyverse")
library("ggtext")
library("RColorBrewer")
library("pracma")

c_wt <- 0.15
mu_1 <- 1:1000/10000000
mu <- 1:100/1000000

#c_m1 <- 0.5^(1/3)*c_wt + c_wt
c_m1 <- 6.3*c_wt
beta <- 7.21675
c_m2 <- sqrt(beta^2+1)*c_wt

c_m1_vec <- rep(c_m1, length(mu))
c_m2_vec <- rep(c_m2, length(mu))
c_wt_vec <- rep(c_wt, length(mu))

x0=5
c_m3 <- mu*pi*x0^4/log(2)
x0=10
c_m4 <- mu*pi*x0^4/log(2)
x0=20
c_m5 <- mu_1*pi*x0^4/log(2)

### Plotting

legend_1 <- "dispersal throughout"
legend_2 <- "boundary growth"
legend_3 <- "fixed population size"

cols <- brewer.pal(12,"Paired")

colours <- t(c(cols[8],cols[10],cols[6]))
colnames(colours) <- c(legend_1, legend_2, legend_3)

lty <- t(c("solid", "solid", "22"))
colnames(lty) <- c(legend_1, legend_2, legend_3)

#scientific_10 <- function(x) { parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))}
mylabel <- c(expression(0.0),expression(5.0%*%10^{-5}),expression(10%*%10^{-5}))

ggplot() + 
  geom_line(aes(x=mu, y=c_m3, linetype=legend_3, color=legend_3), linewidth=1.5) +
  geom_line(aes(x=mu, y=c_m4,  linetype=legend_3, color=legend_3), linewidth=1.5) +
  geom_line(aes(x=mu_1, y=c_m5,  linetype=legend_3, color=legend_3), linewidth=1.5) +
  geom_line(aes(x=mu, y=c_wt_vec), linewidth=2.0, color="black", linetype="12") +
  #geom_hline(yintercept = c_wt, linewidth=1.5, color="black", linetype="11") +
  geom_line(aes(x=mu, y=c_m1_vec,  linetype=legend_1, color=legend_1), linewidth=1.5) +
  geom_line(aes(x=mu, y=c_m2_vec,  linetype=legend_2, color=legend_2), linewidth=1.5) +
  labs(x="mutation rate, *µ*", y="mutant speed, *c<sub>m*") +
  ylim(0,1.5) +
  scale_x_continuous(breaks = c(0.0,0.00005,0.0001), labels=mylabel) +
  scale_color_manual(values = colours, limits = c(legend_1, legend_2, legend_3)) +
  scale_linetype_manual(values = lty, limits = c(legend_1, legend_2, legend_3)) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.7,0.4), legend.title = element_blank())


fig <- ggplot() + 
  geom_line(aes(x=mu, y=c_m3, linetype=legend_3, color=legend_3), linewidth=1.5) +
  geom_line(aes(x=mu, y=c_m4,  linetype=legend_3, color=legend_3), linewidth=1.5) +
  geom_line(aes(x=mu_1, y=c_m5,  linetype=legend_3, color=legend_3), linewidth=1.5) +
  geom_line(aes(x=mu, y=c_wt_vec), linewidth=2.0, color="black", linetype="12") +
  #geom_hline(yintercept = c_wt, linewidth=1.5, color="black", linetype="11") +
  geom_line(aes(x=mu, y=c_m1_vec,  linetype=legend_1, color=legend_1), linewidth=1.5) +
  geom_line(aes(x=mu, y=c_m2_vec,  linetype=legend_2, color=legend_2), linewidth=1.5) +
  labs(x="mutation rate, *µ*", y="mutant speed, *c<sub>m*") +
  ylim(0,1.5) +
  scale_x_continuous(breaks = c(0.0,0.00005,0.0001), labels=mylabel) +
  scale_color_manual(values = colours, limits = c(legend_1, legend_2, legend_3)) +
  scale_linetype_manual(values = lty, limits = c(legend_1, legend_2, legend_3)) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.7,0.4), legend.title = element_blank())

a = 1.65
ggsave("/Users/stein02/Desktop/upload/Figures/Figure_4/fig.png", fig, width = 4^a, height = 3^a)



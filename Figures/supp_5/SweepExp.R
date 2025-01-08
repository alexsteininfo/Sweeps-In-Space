# title: sweep probabilites for alternative growth laws
# author: alexander stein

library("tidyverse")
library("ggtext")
library("RColorBrewer")

r_wt_1 <- 1.0
r_m_1 <- 1:100



p_exp1 <- r_m_1
p_exp2 <- r_m_1
p_exp3 <- r_m_1



j <-1
for (r_m in r_m_1){
  r_wt = 1.0
  #r_m = r_m_1[j]
  
  ### PLot number 1
  chi = 0.90
  u = 1e-4
  integrand <- function(t) {u*exp(r_wt*t -
                                    u/r_wt*
                                    exp( (r_wt*(r_m*t-log(1/chi-1)))/
                                           (r_m-r_wt) )
  )}
  sol <- integrate(integrand, lower = 0, upper = Inf)
  p_exp1[j] <- sol$value
  
  ### Plot number 2
  chi = 0.90
  u = 1e-5
  integrand <- function(t) {u*exp(r_wt*t -
                                    u/r_wt*
                                    exp( (r_wt*(r_m*t-log(1/chi-1)))/
                                           (r_m-r_wt) )
  )}
  sol <- integrate(integrand, lower = 0, upper = Inf)
  p_exp2[j] <- sol$value
  
  ### Plot number 3
  chi = 0.90
  u = 1e-6
  integrand <- function(t) {u*exp(r_wt*t -
                                    u/r_wt*
                                    exp( (r_wt*(r_m*t-log(1/chi-1)))/
                                           (r_m-r_wt) )
  )}
  sol <- integrate(integrand, lower = 0, upper = Inf)
  p_exp3[j] <- sol$value
  
  j <- j+1
}

#xaxis <- s_m/s_wt
xaxis <- r_m_1/r_wt

legend_1 <- "*µ=10<sup>-4*"
legend_2 <- "*µ=10<sup>-5*"
legend_3 <- "*µ=10<sup>-6*"

lty <- t(c("solid", "22", "11"))
colnames(lty) <- c(legend_1, legend_2, legend_3)

ggplot() + 
  geom_line(aes(x=xaxis, y=p_exp1, linetype=legend_1), linewidth=1.5, color = "black") +
  geom_line(aes(x=xaxis, y=p_exp2, linetype=legend_2), linewidth=1.5, color = "black") +
  geom_line(aes(x=xaxis, y=p_exp3, linetype=legend_3), linewidth=1.5, color = "black") +
  scale_linetype_manual(values = lty) +
  labs(x="mutant fitness, *r<sub>m*", y="Pr(sweep)") +
  xlim(1,50) + ylim(0,1) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.8,0.2), legend.title = element_blank(),
        legend.text = element_markdown())


fig <- ggplot() + 
  geom_line(aes(x=xaxis, y=p_exp1, linetype=legend_1), linewidth=1.5, color = "black") +
  geom_line(aes(x=xaxis, y=p_exp2, linetype=legend_2), linewidth=1.5, color = "black") +
  geom_line(aes(x=xaxis, y=p_exp3, linetype=legend_3), linewidth=1.5, color = "black") +
  scale_linetype_manual(values = lty) +
  labs(x="mutant fitness, *r<sub>m*", y="Pr(sweep)") +
  xlim(1,50) + ylim(0,1) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.8,0.2), legend.title = element_blank(),
        legend.text = element_markdown())

a = 1.5
ggsave("/Users/stein02/Desktop/Plots for the Sweeps/supp_5/SweepExp90.png", fig, width = 4^a, height = 3^a)


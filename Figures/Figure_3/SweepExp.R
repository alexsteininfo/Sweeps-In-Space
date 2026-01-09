# title: sweep probabilites for alternative growth laws
# author: alexander stein

library("tidyverse")
library("ggtext")
library("RColorBrewer")

pathtosave <- "~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_3/figures/"

r_wt <- 0.01
r_m_1 <- 1:100*r_wt

chi = 0.95

p_exp1 <- r_m_1
p_exp2 <- r_m_1
p_exp3 <- r_m_1

j <-1
for (r_m in r_m_1){
  #r_m = r_m_1[j]
  
  ### PLot number 1
  u = 1e-4
  integrand <- function(t) {u*exp(r_wt*t -
                                    u/r_wt*
                                    exp( (r_wt*(r_m*t-log(1/chi-1)))/
                                           (r_m-r_wt) )
  )}
  sol <- integrate(integrand, lower = 0, upper = Inf)
  p_exp1[j] <- sol$value
  
  ### Plot number 2
  u = 1e-5
  integrand <- function(t) {u*exp(r_wt*t -
                                    u/r_wt*
                                    exp( (r_wt*(r_m*t-log(1/chi-1)))/
                                           (r_m-r_wt) )
  )}
  sol <- integrate(integrand, lower = 0, upper = Inf)
  p_exp2[j] <- sol$value
  
  ### Plot number 3
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

### Plot Lenski formula
selCoefficient <- (r_m_1-r_wt)/r_wt
N0 = 50000
p_logistic1 <- exp(-1e-4*N0/selCoefficient*log(N0))
p_logistic2 <- exp(-1e-5*N0/selCoefficient*log(N0))
p_logistic3 <- exp(-1e-6*N0/selCoefficient*log(N0))

#xaxis <- s_m/s_wt
xaxis <- r_m_1 #/r_wt

#legend_1 <- "*µ=10<sup>-4*"
#legend_2 <- "*µ=10<sup>-5*"
#legend_3 <- "*µ=10<sup>-6*"

#lty <- t(c("solid", "22", "12"))
#colnames(lty) <- c(legend_1, legend_2, legend_3)

legend_1 <- "exponential growth"
legend_2 <- "fixed population size"

cols <- brewer.pal(12,"Paired")

lcol <- c(
  "exponential growth"     = cols[4],
  "fixed population size"  = cols[12]
)

ggplot() + 
  geom_line(aes(x=xaxis, y=p_exp1, color=legend_1), linewidth=1.5, linetype = "solid") +
  geom_line(aes(x=xaxis, y=p_exp2, color=legend_1), linewidth=1.5, linetype = "22") +
  geom_line(aes(x=xaxis, y=p_exp3, color=legend_1), linewidth=1.5, linetype = "12") +
  geom_line(aes(x=xaxis, y=p_logistic1, color=legend_2), linewidth=1.5, linetype = "solid") +
  geom_line(aes(x=xaxis, y=p_logistic2, color=legend_2), linewidth=1.5, linetype = "22") +
  geom_line(aes(x=xaxis, y=p_logistic3, color=legend_2), linewidth=1.5, linetype = "12") +
  #scale_linetype_manual(values = lty) +
  scale_color_manual(values = lcol, limits = c(legend_1, legend_2)) +
  labs(x="mutant fitness, *r<sub>m*", y="Pr(sweep)") +
  xlim(0.01,0.5) + ylim(0,1) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.74,0.48), legend.title = element_blank(),
        legend.text = element_markdown()) + theme(legend.position = "none")


fig <- ggplot() + 
  geom_line(aes(x=xaxis, y=p_exp1, color=legend_1), linewidth=1.5, linetype = "solid") +
  geom_line(aes(x=xaxis, y=p_exp2, color=legend_1), linewidth=1.5, linetype = "22") +
  geom_line(aes(x=xaxis, y=p_exp3, color=legend_1), linewidth=1.5, linetype = "12") +
  geom_line(aes(x=xaxis, y=p_logistic1, color=legend_2), linewidth=1.5, linetype = "solid") +
  geom_line(aes(x=xaxis, y=p_logistic2, color=legend_2), linewidth=1.5, linetype = "22") +
  geom_line(aes(x=xaxis, y=p_logistic3, color=legend_2), linewidth=1.5, linetype = "12") +
  #scale_linetype_manual(values = lty) +
  scale_color_manual(values = lcol, limits = c(legend_1, legend_2)) +
  labs(x="mutant fitness, *r<sub>m*", y="Pr(sweep)") +
  xlim(0.01,0.5) + ylim(0,1) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.74,0.48), legend.title = element_blank(),
        legend.text = element_markdown()) + theme(legend.position = "none")

a = 1.5
ggsave(paste(pathtosave,"SweepExp90.pdf"), fig, width = 4^a, height = 3^a)

leg <- cowplot::get_legend(
  fig + theme(
    legend.position = "right",
    legend.title = element_blank()
  )
)

ggsave(paste(pathtosave,"SweepExp90_leg.pdf"), leg, width = 4^a, height = 3^a)


# title: testing the gamma fits
# author: alex stein

library(tidyverse)
library(bbmle)

###################
### Import data ###
###################

mut_text <- "1mutations"
ran_text <- "random1"
#setwd("Documents/Sweeps-project/")
text <- paste("data/df2_unlimited_", mut_text, ".csv", sep = "")
data <- read_csv(text)

#################################
### Assess stopping criterium ###
#################################


counts <- data %>% 
  group_by(s_driver_birth) %>%
  summarise(
    count_length = 1, #sum(sim_length == 2000), #sum(sim_length == 2000) #sum(`sim_length	sim_size` == 2000),
    count_size   = 1 #sum(sim_size == 1e6)  #sum(sim_size == 1e6) #1000-count_length = sum( == 1e6)
  ) %>%
  pivot_longer(cols = starts_with("count"),
               names_to = "condition",
               values_to = "count")

# Clean condition labels
counts$condition <- recode(counts$condition,
                           "count_length" = "time = 2000",
                           "count_size"   = "size = 1e6")

fig_stop <- ggplot(counts, aes(x = factor(s_driver_birth), y = count, fill = condition)) +
  geom_col(position = "stack") +
  labs(
    x = "mutant fitness advantage, s",
    y = "Count",
    fill = "Stopping criterium"
  ) +
  theme_bw(base_size = 20) + theme(legend.position="none")

a = 1.5
text <- paste("/Users/alexanderstein/Documents/Sweeps-project/figures/", 
              ran_text, "/stoppingcriterium", ".png",sep = "")
ggsave(text, fig_stop, width = 4^a, height = 3^a)

###################################
### Fitting the finishing radii ###
###################################

# Look at finished sweeps only
data_sweepsonly <- data %>% filter(sweep_complete_time != 0.0)
# Look at radii once sweeps are completed
data_sweepsonly <- data_sweepsonly %>% mutate(radius_end = sqrt(sweep_complete_size/pi))

# Initilise a matrix/dataframe to store fitting data
s_driver_unique = unique(data_sweepsonly$s_driver_birth)
quantile_true = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)

df <- expand.grid(s_driver = s_driver_unique, quantile_true = quantile_true)
df$s_driver_birth <- df$s_driver
df$quantile_pred <- 0
df$gamma_shape <- 0
df$gamma_scale <- 0

for (i in 1:nrow(df)) {
  # Extract s_driver and quantile_true
  s_driver_i <- df$s_driver[i]
  quantile_i <- df$quantile_true[i]
  
  # Pick selection pressure of interest
  data_sweepsonly3 <- data_sweepsonly %>% filter(s_driver_birth == s_driver_i)
  # Truncate according to given quantiles
  radius_quantiles <- quantile(data_sweepsonly3$radius_end, probs=c(quantile_i))
  r_cutoff <- unname(radius_quantiles[1])
  data_sweepsonly3 <- data_sweepsonly3 %>% filter(radius_end <= r_cutoff)
  
  # Fitting procedure
  # Define negloglikelihood based on radius_end data and truncations
  negloglik_radii <- function(shape, scale) {
    -sum(log(
      dgamma(data_sweepsonly3$radius_end, shape = shape, scale = scale) / 
        pgamma(r_cutoff, shape = shape, scale = scale)
    ))
  }
  
  # Minimize the negloglikelohood
  fit_radii <- mle2(negloglik_radii, start = list(shape = 5, scale = 5))
  
  # Store predictions
  df$quantile_pred[i] <- pgamma(r_cutoff, shape = coef(fit_radii)["shape"], scale = coef(fit_radii)["scale"])
  df$gamma_shape[i] <- coef(fit_radii)["shape"]
  df$gamma_scale[i] <- coef(fit_radii)["scale"]
}

df <- df %>% mutate(mean = gamma_shape*gamma_scale)
df <- df %>% mutate(variance = gamma_shape*gamma_scale^2)

####################################
### Just plot everything for fun ###
####################################

ggplot(df, aes(x = s_driver, y = gamma_shape, color = quantile_pred)) +
  geom_point() 
ggplot(df, aes(x = s_driver, y = gamma_scale, color = quantile_pred)) +
  geom_point()
ggplot(df, aes(x = quantile_true, y = quantile_pred, color = s_driver)) +
  geom_point()

### For analysing quantiles, only look at data where we (seemingly) have all sweeps
plot1 <- ggplot(filter(df, s_driver > 0.3), aes(x = quantile_true, y = quantile_pred, color = factor(s_driver))) +
  geom_point() + xlim(0,1)  + ylim(0,1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(x = "true quantile", y = "predicted quantile", color = "s") +
  theme_bw(base_size = 25) + theme(legend.position="none")

### For predictions, we always use all data
plot2 <- ggplot(filter(df, quantile_true == 1.0), aes(x = s_driver, y = mean)) +
  geom_point() + scale_y_log10() + scale_x_log10() +
  theme_bw()
plot3 <- ggplot(filter(df, quantile_true == 1.0), aes(x = s_driver, y = variance)) +
  geom_point() + scale_y_log10() + scale_x_log10() +
  theme_bw()

#text <- paste("combined_plot_", mut_text, ".png", sep = "")
#ggsave(text, grid.arrange(plot1, plot2, plot3, ncol = 3, widths = c(2,1,1)), width = 12, height = 4)

ggsave(paste("figures/", ran_text, "/AssessGammaPred_", mut_text, ".png", sep=""), plot1, width = 5*4/3, height = 5)

##############################################
### Plotting the fitted gamma distribution ###
##############################################

# Create a tibble of fitted gamma densities
densities <- filter(df, quantile_true == 1.0) %>%
  rowwise() %>%
  mutate(density = list(
    tibble(
      x = seq(min(data_sweepsonly$radius_end),
              max(data_sweepsonly$radius_end),
              length.out = 200),
      y = dgamma(x, shape = gamma_shape, scale = gamma_scale)
    )
  )) %>%
  unnest(density)

# Plot fitted gamma distributions

labeller_vector = c("0.05" = "s = 0.05", "0.075" = "s = 0.075", "0.1" = "s = 0.1", 
                    "0.15" = "s = 0.15", "0.2" = "s = 0.2", "0.3" = "s = 0.3",
                    "0.4" = "s = 0.4", "0.5" = "s = 0.5", "1" = "s = 1.0",
                    "1.5" = "s = 1.5", "2" = "s = 2.0")

fig_fits <- ggplot(data_sweepsonly, aes(x = radius_end)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "black", color = "black") +
  geom_line(data = densities, aes(x = x, y = y, color = "steelblue"), size = 1) +
  facet_wrap(~s_driver_birth, labeller = labeller(s_driver_birth = labeller_vector ) ) +
  labs(x = "radius at sweep completion", y = "density") + 
  geom_text(data = densities, 
            aes(x = mean(x), y = max(y)/2, 
                label = paste0("F = ", round(quantile_pred, 2))),
            hjust = -0.2, vjust = -4.0,
            inherit.aes = FALSE) +
  theme_bw() + theme(legend.position="none")

a = 1.5
text <- paste("/Users/alexanderstein/Documents/Sweeps-project/figures/", 
              ran_text, "/gammafits", mut_text, ".png",sep = "")
ggsave(text, fig_fits, width = 4^a, height = 3^a)

for (s_select in s_driver_unique) {
  fig <- ggplot(filter(data_sweepsonly, s_driver_birth == s_select), aes(x = radius_end)) +
    geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "black", color = "black") +
    geom_line(data = filter(densities, s_driver_birth == s_select), aes(x = x, y = y, color = "steelblue"), size = 3) +
    #facet_wrap(~s_driver_birth) +
    labs(x = "radius at sweep completion", y = "density") +
    #ylim(0.0,0.03) +
    theme_bw(base_size = 25) + theme(legend.position="none")
  
  a = 1.5
  text <- paste("/Users/alexanderstein/Documents/Sweeps-project/figures/", ran_text, "/radiusatsweepcompletion", "s", s_select, ".png",sep = "")
  ggsave(text, fig, width = 4^a, height = 3^a)
}

##################################################
### Plotting cumulative distribution functions ###
##################################################

ggplot(data_sweepsonly, aes(x = radius_end)) +
  stat_ecdf(geom = "step", color = "black", size = 1) +
  #scale_y_log10() + scale_x_log10() +
  facet_wrap(~s_driver_birth, labeller = labeller(s_driver_birth = labeller_vector)) +
  labs(x = "radius at sweep completion", y = "cumulative probability") +
  theme_bw() +
  theme(legend.position = "none")


ggplot(data_sweepsonly, aes(x = radius_end)) +
  stat_ecdf(geom = "step", aes(y = 1 - after_stat(y)), 
            color = "black", size = 1) +
  #scale_y_log10() + scale_x_log10() +
  facet_wrap(~s_driver_birth, labeller = labeller(s_driver_birth = labeller_vector)) +
  labs(x = "radius at sweep completion", y = "1 - F(x)") +
  theme_bw() +
  theme(legend.position = "none")

ggplot(data_sweepsonly, aes(x = radius_end)) +
  geom_density(color = "black", size = 1) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~s_driver_birth, labeller = labeller(s_driver_birth = labeller_vector)) +
  labs(x = "radius at sweep completion", y = "PDF (density)") +
  theme_bw() +
  theme(legend.position = "none")




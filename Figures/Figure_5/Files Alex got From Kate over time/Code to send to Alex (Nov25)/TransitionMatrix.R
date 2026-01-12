#Sweep probability matrix calculation

library("ggplot2")
library("gridExtra")
library("grid")
library("cowplot")

# Function to create a transition matrix of size n x n
create_transition_matrix <- function(v) {
  n <- length(v)
  Tmatrix <- matrix(0, nrow = 2*n, ncol = 2*n)
  
  for (i in 1:(2*n)) {
    for (j in 1:(2*n)) {
      if(i==j & i%%2 != 0) {Tmatrix[i,j] <- 1}  # odd diagonals =1
      if(i==j & i%%2 == 0){Tmatrix[i,j] <- v[i/2]} # even diagonals = q1, q2, q3 etc..
      if(i%%2 == 0 & j%%2 != 0 & j>i){Tmatrix[i,j] <- v[(j+1)/2]} #even rows, odd columns to the right of the diagonal
      if(i!=2 & i%%2 ==0 & j == i -1){Tmatrix[i,j] <- ifelse(any(!sapply(v, is.numeric)), paste(v[1:((i/2) -1)], collapse = "+"), sum(v[1:(i/2 -1)]))} #even rows, odd columns, left of the diagonal
    } 
  }
  return(Tmatrix)
}

generate_transition_impact_plots <- function(sweep_prob, fitness_prob, bin_names, number_of_sims){
# Create P(sweep) after applying transition matrix

# what does the sweeps prob chart look like before transition
sims_in_bins_after_1mutation <- number_of_sims * fitness_prob
sweeps_after_1mutation <- sweep_prob * fitness_prob * number_of_sims
non_sweeps_after_1mutation <- sims_in_bins_after_1mutation - sweeps_after_1mutation

data1 <- data.frame(
  category = factor(bin_names, levels = bin_names),
  value = sweep_prob)

print(data1)

g1 <-ggplot(data1, aes(x = category, y = value)) +
    geom_bar(stat = "identity", fill ="#33A02C", alpha = 0.5) +
    theme_bw(base_size = 25) +
    theme(axis.text.x = element_text(size = 18)) + # Adjust the size as needed
    ylim(0,1) +
    labs(title = "1 WT mutation", x = "fitness", y = "Pr(sweep)")


# apply transition matrix to find the probability of sweeps chart after a second mutation in the wild type
n1 <- length(sweep_prob)
state_vector <- c()

for (i in 1:n1) {
  state_vector <- append(state_vector, sweeps_after_1mutation[i])
  state_vector <- append(state_vector, non_sweeps_after_1mutation[i])
  }

print("State vector at start:")
print(state_vector)

T_M1 <- create_transition_matrix(fitness_prob)

New_state_vector <- state_vector %*% T_M1
print("State vector at end:")
print(New_state_vector)

# now extract and plot the sweeps... put in some test values and see what happens...

sweeps_after_2mutations = c()
non_sweeps_after_2mutations = c()
simulations_after_2mutations = c()
Psweep_after_2mutations = c()

for (i in 1:n1) {
  sweeps_after_2mutations <- append(sweeps_after_2mutations, New_state_vector[2*i -1])
  non_sweeps_after_2mutations <- append(non_sweeps_after_2mutations, New_state_vector[2*i] )
  simulations_after_2mutations <- append(simulations_after_2mutations, sweeps_after_2mutations[i]+non_sweeps_after_2mutations[i])
  Psweep_after_2mutations <- append(Psweep_after_2mutations,sweeps_after_2mutations[i]/ simulations_after_2mutations[i])
}

data2 <- data.frame(
  category = factor(bin_names, levels = bin_names),
  value = Psweep_after_2mutations)

print(data2)

g2 <-ggplot(data2, aes(x = category, y = value)) +
  geom_bar(stat = "identity",fill ="#B2DF8A", alpha = 0.5) +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(size = 18)) + # Adjust the size as needed
  ylim(0,1) +
  labs(title = "2 WT mutation", x = "fitness", y = "Pr(sweep)")

print(plot_grid(g1, g2, ncol = 2, nrow = 1))

#return data
sweep_prob <- Psweep_after_2mutations
fitness_prob <- simulations_after_2mutations/number_of_sims

return(c(sweep_prob, fitness_prob, bin_names, number_of_sims))

}

#### Create an algebraic transition matrix ####
v <- c("q1","q2","q3", "q4", "q5")
T_M <- create_transition_matrix(v)

# Create a plot to visualise the matrix 
table_grob <- tableGrob(T_M)
grid.newpage()
grid.draw(table_grob)
      
#### Test transition matrix on input assumptions ####

## input assumptions explanation ##
#sweep_prob <- the probability of a sweep in each of the n bins
#fitness_prob <-the probability of a simulations being in each of the n bins
#bin_names <- fitness bins names/values, NB. START FROM LOWEST FITNESS TO HIGHEST IN ORDER
#number_of_sims <- total number of simulations

#Test case1
#generate_transition_impact_plots(sweep_prob = c(0.01, 0.5, 0.8), fitness_prob = c(0.01, 0.495, 0.495) , bin_names = c("Low", "Medium", "High") , number_of_sims = 1000)

# 4 state case 
#generate_transition_impact_plots(sweep_prob = c(0.01, 0.5, 0.7, 0.8), fitness_prob = c(0.01, 0.33, 0.33, 0.33) , bin_names = c("Low", "Medium", "High", "Very High") , number_of_sims = 1000)

# 5 state case 
generate_transition_impact_plots(sweep_prob = c(0.01, 0.5, 0.6, 0.7, 0.8), fitness_prob = c(0.01, 0.2475, 0.2475, 0.2475, 0.2475) , bin_names = c("Very Low", "Low","Medium", "High", "Very High") , number_of_sims = 1000)

# 20 state case 
#generate_transition_impact_plots(sweep_prob = c(0.01, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.72, 0.74, 0.76, 0.78, 0.79, 0.8), fitness_prob = c(0.01, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495) , bin_names = c("0.1", "0.2","0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0", "1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", "1.8", "1.9", "2.0") , number_of_sims = 10000)

  


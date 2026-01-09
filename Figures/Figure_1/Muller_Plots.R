# Create Muller Plots for my Master Thesis
# author: Alexander Stein

library(ggmuller)
library(tidyverse)
library(ggplot2)

#setwd("~/ETH/Master Thesis/Code/Figures")


### Make the example plot
Muller_df <- get_Muller_df(example_edges, example_pop_df)
Muller_plot(Muller_df)

### Make the plot with two types and a selective sweep

# We start creating edges and pop_df
Parent <- c(1,2)
Identity <- c(2,3)

edges <- data.frame(Parent, Identity)

Generation <- c(0,0,0,10,10,10,20,20,20,30,30,30,40,40,40,50,50,50,60,60,60,70,70,70,80,80,80,90,90,90,100,100,100)
Identity <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)
Population <- c(100,0,0,
                99,1,0,
                95,5,0,
                85,15,0,
                70,30,0,
                50,50,0,
                30,70,0,
                15,85,0,
                10,90,0,
                5,95,0,
                0,100,0)

pop_df <- data.frame(Generation, Identity, Population)

Muller_df_sweep <- get_Muller_df(edges, pop_df)
Muller_plot(Muller_df_sweep)

fig_sweep <- ggplot(Muller_df_sweep, aes_string(x="Generation", y="Frequency",group="Group_id",fill="Identity",colour="Identity")) +
  geom_area() +
  theme(legend.position = "right") + 
  guides(linetype = FALSE, color = FALSE) + 
  scale_y_continuous(labels=25*(0:4), name="frequency") +
  scale_x_continuous(labels=25*(0:4), name="time") +
  scale_fill_manual(name="Identity", values=c("orange","blue","purple")) + 
  scale_color_manual(values=c("orange","blue","purple")) + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=rel(3.5)),
        axis.title.y = element_text(size=rel(3.5)),
        panel.background = element_blank())


### Make the plot with two types and a selective sweep

# We start creating edges and pop_df
Parent <- c(1,1)
Identity <- c(2,3)

edges <- data.frame(Parent, Identity)

Generation <- c(0,0,0,10,10,10,20,20,20,30,30,30,40,40,40,50,50,50,60,60,60,70,70,70,80,80,80,90,90,90,100,100,100,110,110,110)
Identity <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)
Population <- c(100,0,0,
                99,1,0,
                95,5,0,
                85,15,0,
                70,30,0,
                50,49,1,
                30,65,7,
                15,75,15,
                10,70,20,
                5,70,25,
                0,70,30,
                0,70,30)

pop_df <- data.frame(Generation, Identity, Population)

Muller_df_sweep <- get_Muller_df(edges, pop_df)
Muller_plot(Muller_df_sweep)

fig_sweep <- ggplot(Muller_df_sweep, aes_string(x="Generation", y="Frequency",group="Group_id",fill="Identity",colour="Identity")) +
  geom_area() +
  theme(legend.position = "right") + 
  guides(linetype = FALSE, color = FALSE) + 
  scale_y_continuous(labels=25*(0:4), name="frequency") +
  scale_x_continuous(labels=25*(0:4), name="time") +
  scale_fill_manual(name="Identity", values=c("orange","blue","green")) + 
  scale_color_manual(values=c("orange","blue","green")) + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=rel(3.5)),
        axis.title.y = element_text(size=rel(3.5)),
        panel.background = element_blank())

ggsave("~/Documents/Sweeps-project/fig_sweep.svg", fig_sweep, width = 5, height = 3, dpi = 300, units = "cm", device='svg')


### Make the plot with two types and a selective sweep

# We start creating edges and pop_df
Parent <- c(1,1)
Identity <- c(2,3)

edges <- data.frame(Parent, Identity)

Generation <- c(0,0,0,
                10,10,10,
                20,20,20,
                30,30,30,
                40,40,40,
                50,50,50,
                60,60,60,
                70,70,70,
                80,80,80,
                100,100,100,
                110,110,110)

Identity <- c(1,2,3,
              1,2,3,
              1,2,3,
              1,2,3,
              1,2,3,
              1,2,3,
              1,2,3,
              1,2,3,
              1,2,3,
              1,2,3,
              1,2,3)

Population <- c(100,  0,  0,
                 99,  1,  0,
                 97,  3,  0,
                 95,  5,  0,
                 91,  8,  0,
                 85, 12,  1,
                 78, 17,  3,
                 62, 23,  5,
                 58, 30,  8,
                 45, 38, 12,
                 40, 47, 17)

pop_df <- data.frame(Generation, Identity, Population)

Muller_df_sweep <- get_Muller_df(edges, pop_df)
Muller_plot(Muller_df_sweep)

fig_interfere <- ggplot(Muller_df_sweep, aes_string(x="Generation", y="Frequency",group="Group_id",fill="Identity",colour="Identity")) +
  geom_area() +
  theme(legend.position = "right") + 
  guides(linetype = FALSE, color = FALSE) + 
  scale_y_continuous(labels=25*(0:4), name="frequency") +
  scale_x_continuous(labels=25*(0:4), name="time") +
  scale_fill_manual(name="Identity", values=c("orange","blue","green")) + 
  scale_color_manual(values=c("orange","blue","green")) + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=rel(3.5)),
        axis.title.y = element_text(size=rel(3.5)),
        panel.background = element_blank())

ggsave("~/Documents/Sweeps-project/fig_interfere.svg", fig_interfere, width = 5, height = 3, dpi = 300, units = "cm", device='svg')

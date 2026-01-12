library(ggplot2)
library(ggtext)
library(RColorBrewer)
library(scales)

# ---- Legend labels ----
pred_2d  <- "pred. (approx.)"
pred_ana <- "pred. (exact)"

sim_pts  <- "1 fixed"
m1   <- "1 random"
m2   <- "2 random"
m3   <- "3 random"
munl <- "unlimited"

# One common legend universe (controls order)
legend_levels <- c(m1, m2, m3, sim_pts, munl, pred_2d, pred_ana)

# ---- Manual scales (named vectors; SAME names across all scales) ----
cols <- brewer.pal(12, "Paired")

colours <- setNames(
  c(
    "red", "orange", "thistle", "black", "darkslategray2",  # point series
    cols[3], cols[4]                                        # line colours (your choice)
  ),
  legend_levels
)

# Lines get linetypes; points get "blank" so they don't draw lines in legend
linetypes <- setNames(
  c(
    "blank", "blank", "blank", "blank", "blank",   # points-only entries
    "solid", "22"                                  # line entries
  ),
  legend_levels
)

# Points get shapes; lines get NA so they don't draw points in legend
shapes <- setNames(
  c(
    18, 18, 18, 17, 18,   # point shapes (simulations distinct)
    NA, NA                # line entries
  ),
  legend_levels
)

# ---- Plot ----
g3 <- ggplot() +
  # prediction lines
  #geom_line(aes(x = c(2:100) * s_wt, y = df_num$twodim,color = pred_2d, linetype = pred_2d),linewidth = 1.5) +
  #geom_line(aes(x = xaxis * s_wt, y = sweep_ana,color = pred_ana, linetype = pred_ana),linewidth = 1.5) +
  
  # simulation points
  geom_point(aes(x = x_data2 * s_wt, y = y_data2,color = sim_pts, shape = sim_pts),size = 4) +
  
  # mutation-class points
  #geom_point(aes(x = mean_sr1_1mut_list_plotting * s_wt, y = y_data6,color = m1, shape = m1),size = 4) +
  #geom_point(aes(x = mean_sr1_1mut_list_plotting * s_wt, y = y_data5,color = m2, shape = m2),size = 4) +
  #geom_point(aes(x = mean_sr1_1mut_list_plotting * s_wt, y = y_data4,color = m3, shape = m3),size = 4) +
  #geom_point(aes(x = mean_sr1_1mut_list_plotting * s_wt, y = y_data3,color = munl, shape = munl), size = 4) +
  
  # IMPORTANT: all three scales share the same breaks + name => one combined legend
  scale_color_manual(values = colours, breaks = legend_levels, name = "") +
  scale_linetype_manual(values = linetypes, breaks = legend_levels, name = "") +
  scale_shape_manual(values = shapes, breaks = legend_levels, name = "") +
  
  labs(
    x = expression("mean effect of surviving mutations, " * tilde(italic(s))),
    y = "Pr(sweep)"
  ) +
  coord_cartesian(xlim = c(0, 20.56 * s_wt), ylim = c(0, 1)) +
  
  theme_bw(base_size = 25) +
  theme(
    legend.text  = element_markdown(),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.30),
    #legend.background = element_rect(fill = alpha("white", 0.8), colour = NA),
    legend.box.spacing = grid::unit(0, "pt"),
    legend.spacing.y   = grid::unit(0, "pt"),
    legend.box.margin  = margin(0, 0, 0, 0),
    legend.margin      = margin(0, 0, 0, 0)
  ) +
  guides(
    # makes legend keys look nice (bigger points + visible lines)
    color = guide_legend(override.aes = list(size = 4, linewidth = 1.5))
  )

g3

leg <- cowplot::get_legend(
  g3 + theme(
    legend.position = "right",
    legend.title = element_blank()
  )
  # + guides(
  #   color = guide_legend(
  #     ncol = 2,
  #     byrow = TRUE,   # fills row-wise (usually nicer)
  #     override.aes = list(size = 4)
  #   )
  # )
)

a=1.5
ggsave("~/Documents/GitHub/Sweeps-In-Space/Figures/Figure_5/figures/legend_1.pdf", leg, width = 4^a, height = 3^a)



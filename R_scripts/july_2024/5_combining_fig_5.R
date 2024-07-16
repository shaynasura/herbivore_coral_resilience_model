library(cowplot)
library(tidyverse)

# need to load in the respective data frames for 5.1 and 5.2 first.
Fig_5A <- ggplot(bistability_all, aes(Fishing_Pressure, Initial_Coral)) +
  geom_tile(aes(fill = Final_Coral)) +
  theme_classic(base_size=14,base_family = "Times") + 
  theme(axis.title=element_text(size=16,face="bold")) +
  # labs(fill = "Final\nCoral\nCover") +
  scale_fill_gradientn(colors = custom_colors(50), limits=c(0,0.8)) +
  scale_colour_gradientn(colors = custom_colors(50), limits=c(0,0.8)) +
  scale_x_continuous("Fishing Pressure",
                     expand = c(0, 0),
                     breaks = c(0,0.25,0.5,0.75,1),
                     labels = c(0,0.25,0.5,0.75,1)) +
  scale_y_continuous("Initial Coral Cover",
                     expand = c(0, 0),
                     breaks = c(0,0.2,0.4,0.6,0.8),
                     labels = c(0,0.2,0.4,0.6,0.8)) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               label.hjust = 0.5,
                               barheight = unit(6, "lines"),
                               barwidth = unit(1, "lines"),
                               draw.ulim = FALSE,
                               draw.llim = FALSE,
                               frame.colour = "black",
                               ticks.colour = "black",
                               label.position = "right",
                               label.theme = element_text(size = 8),
                               title = "Final\nCoral\nCover",
                               title.theme = element_text(size = 11))) +
  facet_wrap(~Id,nrow = 4) +
  theme(strip.text.x = element_text(size = 11))



Fig_5B <- ggplot(data=exp_hysteresis_all, aes(x=Fishing, Final_Coral, group=Id, color=Id)) +
  geom_line(aes(color = Id, linewidth = Id)) +
  scale_color_manual(labels=c("Forward","Reverse"),
                     values=c("red","black")) + 
  theme_classic(base_size=14, base_family = "Times") + 
  theme(axis.title=element_text(size=16, face="bold")) +
  scale_linewidth_manual(values = c(2,1)) +
  xlim(0,1) + 
  ylim(0,1) + 
  ylab("Coral Cover") +
  xlab("Fishing Pressure") +
  theme(legend.title=element_blank(),
        legend.position = c(0.75,0.965),
        legend.text=element_text(size=9),
        legend.background = element_rect(fill = "transparent")) +
  facet_wrap(~Type,nrow = 4) +
  theme(strip.text.x = element_text(size = 11))

Fig_5 <- plot_grid(Fig_5A, Fig_5B, ncol = 2, rel_widths = c(3.5, 2.5), rel_heights = c(8.5))

print(Fig_5)

pdf("output/figures/Fig_5.pdf", width=6, height=8.5)
print(Fig_5)
dev.off()

png("output/figures/Fig_5.png", width=6, height=8.5, units = "in", res = 600)
print(Fig_5)
dev.off()


getwd()

library(scales)
library(ggplot2)
library(dplyr)
library(ggforce)
library(emmeans)
##### 定义新功能
calculate_effect_sizes_new <- function(data){
  # data: each row is a species pair, with growth data (mean, se, n) 
  #       of each species in conspecific, heterospecific, and control soil
  
  data_with_SDFD <- data %>% 
    mutate( # mean stabilization, which is also -1/2 of Is 
      mean_SD = -1/2*(log(`AinA mean`) - log(`AinB mean`) - 
                        log(`BinA mean`) + log(`BinB mean`) ),
      IS = -2*mean_SD,
      # next, fitness difference (FD) effect size
      mean_FD = 1/2*(log(`AinA mean`) + log(`AinB mean`) - 
                       2*log(`AinR mean`) - log(`BinA mean`) - 
                       log(`BinB mean`) + 2*log(`BinR mean`) ),
      # absolute value of FD
      abs_FD = abs(mean_FD),
      
      # IGR & its SEM
      mean_IGR = mean_SD - abs_FD,
    )
  return(data_with_SDFD)
}

# Set plot theme
theme_plots <- function() {
  theme_minimal()+
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.tag = element_text(face = "bold")
    ) 
}


# plotting
# Make a base plot that can be modified for each sub question  
base_coex_plot <- 
  ggplot() +
  geom_ribbon(aes(x = seq(0, 8, length.out = 100),
                  ymin = rep(0, length.out = 100),
                  ymax = seq(0, 8, length.out = 100)),
              fill = alpha("#6AA94E", 0.5)) + #99D594 d9ead3ff
  geom_ribbon(aes(x = seq(0, -5, length.out = 100),
                  ymin = rep(0, length.out = 100),
                  ymax = seq(0, 5, length.out = 100)),
              fill = alpha("#0675BB", 0.5)) +
  #annotate("segment", x = 0, y = 0, xend = 5, yend = 5, linetype = 2) +
  #annotate("segment", x = 0, y = 0, xend = -5, yend = 5, linetype = 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlab(bquote(atop("",
                   -frac(1,2)~(m["1A"]-m["1B"]-m["2A"]+m["2B"]) ))) +
  ylab(bquote(atop("Fitness difference",
                   frac(1,2)~(m["1A"]+m["1B"]-m["2A"]-m["2B"])))) +
  annotate("text", x = 0.2, y = -0.12, hjust = 0, vjust = 0, label = "Stabilization", color = "black", size = 3) +
  annotate("text", x = -0.2, y = -0.12, hjust = 1, vjust = 0, label = "Destabilization",  color = "black", size = 3) +
  annotate("segment", x = 0.1, xend = 0.4, y = -.05, yend = -.05, 
           colour = "grey25", size = 0.5, arrow = arrow(length = unit(0.075, "inches"))) + 
  annotate("segment", x = -0.1, xend = -0.4, y = -.05, yend = -.05, 
           colour = "grey25", size = 0.5, arrow = arrow(length = unit(0.075, "inches"))) +
  theme_plots() + 
  theme(axis.line = element_blank(),
        axis.title.x = element_text(margin = margin(t = 0)))
base_coex_plot

library(ggthemes)
library(ggplot2)
theme <-theme(panel.background = element_blank(),
              panel.border     = element_rect(fill = NA), 
              strip.background = element_blank(),
              axis.text.x = element_text(size = 5.5, angle = 0, hjust = 0.5, colour = 'black'),
              axis.text.y  = element_text(size = 6, colour = 'black'),
              axis.title = element_text(size = 6),# size of y lab
              legend.position   = c(0.9, 0.85),
              legend.title      = element_text(size = 6),
              legend.text       = element_text(size = 6),
              legend.key.height = unit(0.7,'line'),
              legend.background = element_blank(),
              legend.key        = element_rect(colour = NA, fill = NA),
              plot.background = element_blank(), 
              plot.tag    = element_text(size = 7, face  = 'bold'),
              plot.title  = element_text(hjust = 0.5, size = 7),
              plot.margin = margin(t = 0.5, r = 0.1, b = 0, l = 0.1, unit = "cm"))

library(ggplot2)
mytheme = theme(
  legend.position = "none",
  panel.grid=element_blank(), 
  strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid"),
  strip.text.x = element_text(size = 10, color = "black"), # face = "bold.italic"
  legend.title = element_blank(),
  legend.key = element_blank(),
  legend.text = element_text(size = 10),
  legend.background = element_rect(fill = NA), #axis.ticks.length = unit(0.4,"lines"), 
  axis.ticks = element_line(color='black'),
  axis.line = element_line(colour = "black"), 
  axis.title.x = element_text(colour='black', size=13),
  axis.title.y = element_text(colour='black', size=13),
  axis.text = element_text(colour='black',size=11),
  plot.background = element_blank(), 
  plot.tag = element_text(size = 14, face = "bold")) 

mytheme2 = theme(#legend.background = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 10),
  legend.key = element_blank(),
  legend.background = element_rect(fill = NA), 
  legend.position = "none",
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.text.y = element_text(color = "black", size = 11),
  axis.title.y = element_text(color = "black", size = 13),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.x = element_blank(),
  axis.line.x = element_line(linewidth = NA))

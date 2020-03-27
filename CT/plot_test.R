###
library(tidyverse)
library(reshape2)
setwd("C:/Users/mce25/Documents/GitHub/covid19_icu/CT")

I_t = read.csv("25032020_CTEstimates_SD_R0_3.0.csv")[,-c(5:6)]

I_tmelt = melt(I_t, id="date")
I_tmelt$variable = as.factor(I_tmelt$variable)
I_tmelt$date = as.Date(I_tmelt$date, "%m/%d/%y")

title = "Minimum, mean, and maximum number of infected individuals in CT"

ggplot(data = I_tmelt, aes(x = date, y = value, color = variable)) + geom_line(size = 1.5)+
  xlab("Time (days)") + ylab("Number of infected individuals") + theme_bw() +
  theme(
    #remove grid
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    
    #resize axis labels
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size=14),
    axis.text = element_text(size=14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    
    #resize legend labels
    legend.text = element_text(size=14),
    legend.title = element_text(size=16),
    
    #plot title
    plot.title = element_text(hjust = 0.5, size=20)
    
  )+
	scale_color_manual(name=element_blank(), values=c("gray", "red", "black"), 
	labels=c("minimum", "maximum", "mean")) +
  ggtitle(title)

ggsave("plot_trajectoriesCT.pdf")
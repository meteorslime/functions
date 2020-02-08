## Torpor plot labels
torpor.plot.theme <- function(){
  theme_bw() + 
  ggtitle(title, subtitle) + 
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5) #,
#    axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
#    axis.text.y = element_text(size = 12),
#    axis.title.x = element_text(size = 14, face = "plain"),             
#    axis.title.y = element_text(size = 14, face = "plain"),             
#    legend.text = element_text(size = 12, face = "italic"),          
#    legend.title = element_blank(),                              
#    legend.position = c(0.9, 0.9)) + 
    )
}

## Torpor plot scale
torpor.plot.scale <- function(){
  scale_x_datetime(
    name = "Time of day",
    breaks = x.scale[seq(1, 25, 4)], 
    minor_breaks = x.scale[seq(1, 25, 1)],
    labels = format(x.scale[seq(1, 25, 4)], "%H:%M"),
    limits = c(min(x.scale), max(x.scale))) +
  scale_y_continuous(
    name = "Skin surface temperature (°C)",
    breaks = seq(10, 45, 5), 
    limits = c(10, 45))
}

## Torpor plot annotation labels
torpor.plot.annotation <- function(){
#  geom_text(label = "Ambient temperature (Ta)", 
#    x = text.Ta.x, y = text.Ta.y, size = 4, vjust = 3) + 

  geom_text(label = paste("Arrival:", format(srx.table$Arrival[i], "%H:%M")), 
    x = text.left.x, y = 18, hjust = "left", size = 4) + 

  geom_text(label = paste("Departure:", format(srx.table$Departure[i], "%H:%M")), 
    x = text.left.x, y = 16.5, hjust = "left", size = 4) + 

  geom_text(label = paste("Recording time:", 
      round(srx.table$Departure[i]-srx.table$Arrival[i], digits = 2), "hours"), 
    x = text.left.x, y = 15, hjust = "left", size = 4) + 
  
  geom_text(label = paste("Gaps:", srx.table$Gaps[i]), 
    x = text.left.x, y = 13.5, hjust = "left", size = 4) + 

  geom_text(label = paste("Recording days:", length(
      which(srx.table$BatID == srx.table$BatID[i]))), 
    x = text.left.x, y = 12, hjust = "left", size = 4) +

## Torpor statistics
  geom_text(label = paste("Tact (day):", srx.table$Tactday[i]), 
    x = text.center.x, y = 18, hjust = "left", size = 4) + 
      
  geom_text(label = paste("Tact (mean):", srx.table$Tact[i]), 
    x = text.center.x, y = 16.5, hjust = "left", size = 4) + 
      
  geom_text(label = paste("Torpor duration (day):", srx.table$Tdur[i]), 
    x = text.center.x, y = 15, hjust = "left", size = 4) + 

  geom_text(label = paste("Max torpor depth (day):", srx.table$Tdepth[i]), 
    x = text.center.x, y = 13.5, hjust = "left", size = 4) + 

  geom_text(label = paste("Torpor degree-minutes (day):", srx.table$Tdegmin[i]), 
    x = text.center.x, y = 12, hjust = "left", size = 4)
}
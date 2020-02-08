###############################################################################
## Plot functions
soeplot <- function (aic.table, varname, species) {

library(ggplot2)
#library(ggforce)

## Create data frame
df <- data.frame(
  x = seq(60, 360, 30), 
  y = aic.table$delta[order(rownames(aic.table))]
  )

## Plot
soeplot <- ggplot(df) +
  scale_x_continuous(
    name = "Measurement scale (m)",
    breaks = df$x, 
    minor_breaks = seq(min(df$x), max(df$x), 10), 
    limits = c(min(df$x), max(df$x))
    ) +
  scale_y_continuous(
    name = "Delta AIC(c)",
    breaks = seq(trunc(min(df$y)), ceiling(max(df$y)), 1), 
    minor_breaks = seq(trunc(min(df$y)), ceiling(max(df$y)), 0.5), 
    limits = c(floor(min(df$y)), ceiling(max(df$y)))
    ) + 
  geom_point(aes(x = df$x, y = df$y)) + 
  geom_point(
    data = df[which(df$y == min(df$y)), ], 
    aes(x = x, y = y), 
    size = 10, 
    shape = 1, 
    color = "red") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  ggtitle(
    label = paste("Scale of effect:", varname), 
    subtitle = paste("Best prediction for", species, "when measured at", df$x[which(df$y == min(df$y))], "m")) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  geom_text(label = species, aes(x = 345, y = 0.9*ceiling(max(df$y))))

#geom_text(label = "Max torpor depth (day):", x = x, y = 13.5, hjust = "left", size = 4)

## Save plot
ggsave(
  filename = paste("./Plots/SOE_", species, "_", varname, "_", as.character(format(Sys.time(), "%Y%m%d")), ".png", sep = ""), 
  plot = soeplot,
  width = 6, 
  height = 4, 
  units = c("in")
  )

return(soeplot)
}

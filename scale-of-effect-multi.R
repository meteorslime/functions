### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## Multiple scale-of-effect plots in one figure
multisoeplot <- function (aic.table, varname, species) {

library(ggplot2)
#library(ggforce)


## Create data frame
#aic.table$sample <- as.numeric(aic.table$sample)
#aic.table$scale <- substring(rownames(aic.table), 5, 6)
#aic.table$scale <- as.numeric(aic.table$scale)

aic.table <- aic.table[order(aic.table$sample, aic.table$scale), ]

## Create data frame
df <- data.frame(
  s = aic.table$sample,
  x = aic.table$scale,
  y = aic.table$delta
)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Plot
soeplot <- ggplot(df) + 
    scale_x_continuous(
        name = "Measurement scale (m)", 
        breaks = df$x, 
        minor_breaks = seq(min(df$x), max(df$x), 10), 
        limits = c(min(df$x), max(df$x))) + 
    scale_y_continuous(
        name = "Delta AIC(c)", 
        breaks = seq(trunc(min(df$y)), ceiling(max(df$y)), 1), 
        minor_breaks = seq(trunc(min(df$y)), ceiling(max(df$y)), 0.5), 
        limits = c(floor(min(df$y)), ceiling(max(df$y)))) + 
    scale_color_manual(paste("Available\n", "sample", sep = ""), 
        breaks = df$s, 
        values = cbPalette[1:3]) + 
    geom_vline(
        xintercept = df$x[which(df$y == min(df$y))], 
        group = unique(df$s), 
        linetype = 2, 
        color = cbPalette[1:3]) + 
    geom_line(aes(x = df$x, y = df$y, group = df$s, color = as.factor(df$s)), show.legend = T) + 
    geom_point(aes(x = df$x, y = df$y, group = df$s)) + 

    theme_bw() + 
    theme(panel.grid.minor = element_blank()) + 
    ggtitle(
        label = paste("Scale of effect:", varname), 
        subtitle = paste("Best performance for", species, "when measured at", df$x[which(df$y == min(df$y))], "m")) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(plot.subtitle = element_text(hjust = 0.5)) + 
    theme(legend.title.align = 0.5) + 
    geom_text(label = species, aes(x = 210, y = 0.95*ceiling(max(df$y))), size = 7)

## Save plot
ggsave(filename = paste("./Plots/Multi_SOE_", 
    species, "_", 
    varname, "_", 
    as.character(format(Sys.time(), "%Y%m%d")), ".png", sep = ""), 
  plot = soeplot,
  width = 6.5, 
  height = 4.5, 
  units = c("in")
  )

return(soeplot)
}

?scale_color_manual

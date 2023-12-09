## Load packages ----

library(data.table)
library(ggplot2) # to do: check if we need ggplot2


## Read in main results ----

dir <- "../" # run code in the script location
df <- fread(file = paste0(dir, "results/commercial_dealers_association_and_causal_results.csv"))
df <- df[Trim == "5.95" & Cat_Confounder == "state.urbanicity"] # get main models only
# to do: causal models: c("GPS Matching (CRSE)", "GPS Matching (GEE)", "GPS Weighting")


## Main Plot (5x7 inches) ----

# Create the plot
plot(1:nrow(df), df$Effect, ylim = c(0.80, 1.15), 
     xlab = "", ylab = "Odds Ratio", main = "",
     xaxt = "n", yaxt = "n", pch = 16, cex = 1.5, col = "steelblue")

# Add light grey grid
grid(lty = "dotted", col = "lightgray")

# Add error bars for the confidence intervals
arrows(1:nrow(df), df$CI_95ct_lower, 1:nrow(df), df$CI_95ct_upper, 
       length = 0.05, angle = 90, code = 3, col = "steelblue")

# Add model names as x-axis labels (horizontal and smaller)
axis(1, at = 1:nrow(df), labels = df$Model, tick = FALSE, las = 1, cex.axis = 0.7)
text(x = 1:nrow(df), y = -0.15, labels = df$Model, pos = 1, cex = 0.7, xpd = TRUE, adj = c(0.5, 0), col = "black")

# Add horizontal line at the value of a null effect
abline(h = 1, lty = 2, col = "black")

# Add Vertical line to divide the causal vs associational models
abline(v = 3.5, lty = 2, col = "red")

# Add labels to the margins
mtext("Robustness Checks", side = 1, line = 2.5, at = 4.5, cex = 0.8)
mtext("Main Models", side = 1, line = 2.5, at = 2, cex = 0.8)
mtext("(Regression Models)", side = 1, line = 3.5, at = 4.5, cex = 0.8)
mtext("(Causal Models)", side = 1, line = 3.5, at = 2, cex = 0.8)

# Make Y axis labels horizontal
axis(2, las = 1, tcl = -0.5)


## Save plot (5x7 inches) ----

# ggsave(paste0(dir, "results/main_results_plot.png")) # to do

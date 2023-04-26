# Main Plot (5x7 inches)

df <- data.frame(
  Model = c("GPS Matching (CRSE)", "GPS Matching (GEE)", "GPS Weighting", "Logistic", "Negative Binomial"),
  Estimate = c(0.9708, 0.9571, 0.9742, 0.9687, 0.9696),
  Lower_CI = c(0.9283, 0.9321, 0.9561, 0.9367, 0.9380),
  Upper_CI = c(1.0154, 0.9820, 0.9928, 1.0019, 1.0023)
)

# Create the plot
plot(1:length(df$Model), df$Estimate, ylim = c(0.80, 1.15), 
     xlab = "", ylab = "Odds Ratio", main = "",
     xaxt = "n", yaxt = "n", pch = 16, cex = 1.5, col = "steelblue")

# Add light grey grid
grid(lty = "dotted", col = "lightgray")

# Add error bars for the confidence intervals
arrows(1:length(df$Model), df$Lower_CI, 1:length(df$Model), df$Upper_CI, 
       length = 0.05, angle = 90, code = 3, col = "steelblue")

# Add model names as x-axis labels (horizontal and smaller)
axis(1, at = c(1, 2, 3, 4, 5), labels = df$Model[c(1, 2, 3, 4, 5)], tick = FALSE, las = 1, cex.axis = 0.7)
text(x = c(1, 2, 3, 4, 5), y = -0.15, labels = df$Model[c(1, 2, 3, 4, 5)], pos = 1, cex = 0.7, xpd = TRUE, adj = c(0.5, 0), col = "black")


# Add horizontal line at the mean value
abline(h = 1, lty = 2, col = "black")

# Add Vertical line at the mean value
abline(v = 3.5, lty = 2, col = "red")

# Add labels to the margins
mtext("Robustness Checks", side = 1, line = 2.5, at = 4.5, cex = 0.8)
mtext("Main Models", side = 1, line = 2.5, at = 2, cex = 0.8)
mtext("(Regression Models)", side = 1, line = 3.5, at = 4.5, cex = 0.8)
mtext("(Causal Models)", side = 1, line = 3.5, at = 2, cex = 0.8)

# Make Y axis labels horizontal
axis(2, las = 1, tcl = -0.5)
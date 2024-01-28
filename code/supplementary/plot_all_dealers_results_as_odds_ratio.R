## Load packages ----

library(data.table)


## Get main results ----

dir <- "../" # run code in the script location

# read in all dealers (commercial and residential) results
df <- fread(file = paste0(dir, "results/all_dealers_association_and_causal_results.csv"))
df <- df[Trim == "5.95" & Cat_Confounder == "state.urbanicity"] # get main models only

# get odds ratio of SGI = odds with intervention / odds without intervention
# note that a null effect would be an odds ratio of 1
df[, `:=`(Effect_pct = round((Effect), 2),
          CI_95ct_lower_pct = round((CI_95ct_lower), 2),
          CI_95ct_upper_pct = round((CI_95ct_upper), 2))]


## Main Plot ----

# Prepare to save plot
png(file = paste0(dir, "results/sensitivity_analyses/all_dealers_results_as_odds_ratio.png"),
    width = 7, height = 5, units = "in", res = 1200)

# Create the plot
# note that a null effect would be an odds ratio of 1
plot(1:nrow(df), df$Effect_pct, ylim = c(min(df$CI_95ct_lower_pct), 1), 
     xlab = "", ylab = "Odds Ratio of SGI", main = "",
     xaxt = "n", yaxt = "n", pch = 16, cex = 1.5, col = "steelblue")

# Add light grey grid
grid(lty = "dotted", col = "lightgray")

# Add error bars for the confidence intervals
arrows(1:nrow(df), df$CI_95ct_lower_pct, 1:nrow(df), df$CI_95ct_upper_pct, 
       length = 0.05, angle = 90, code = 3, col = "steelblue")

# Add model names as x-axis labels (horizontal and smaller)
axis(1, at = 1:nrow(df), labels = df$Model, tick = FALSE, las = 1, cex.axis = 0.7)

# Add horizontal line at the value of a null effect
# note that a null effect would be an odds ratio of 1
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

# Conclude
dev.off()

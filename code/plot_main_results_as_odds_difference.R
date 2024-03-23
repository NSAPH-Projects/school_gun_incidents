## Load packages ----

library(data.table)


## Get main results ----

dir <- here::here() # location of repository

# read in main results
df <- fread(file = here::here(dir, "results/commercial_dealers_association_and_causal_results.csv"))
df <- df[Trim == "5.95" & Cat_Confounder == "state.urbanicity"] # get main models only

# convert odds ratio to change (difference) in odds, as percent
# minus 1 because a difference of 0 corresponds to an odds ratio of 1
# i.e., odds ratio of SGI - 1 =  (odds with intervention / odds without intervention) - 1 = (odds with - odds without) / odds without
# note that a null effect would be an odds difference of 0
df[, `:=`(Effect_pct = round((Effect-1)*100, 2),
          CI_95ct_lower_pct = round((CI_95ct_lower-1)*100, 2),
          CI_95ct_upper_pct = round((CI_95ct_upper-1)*100, 2))]


## Main Plot ----

# Prepare to save plot
results_dir <- here::here(dir, "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = T)
png(file = here::here(results_dir, "main_results_as_odds_difference.png"),
    width = 7, height = 5, units = "in", res = 1200)

# Create the plot
# note that a null effect would be an odds difference of 0
plot(1:nrow(df), df$Effect_pct, ylim = c(min(df$CI_95ct_lower_pct), 0), 
     xlab = "", ylab = "Change in Odds of SGI (%)", main = "",
     xaxt = "n", yaxt = "n", pch = 16, cex = 1.5, col = "steelblue")

# Add light grey grid
grid(lty = "dotted", col = "lightgray")

# Add error bars for the confidence intervals
arrows(1:nrow(df), df$CI_95ct_lower_pct, 1:nrow(df), df$CI_95ct_upper_pct, 
       length = 0.05, angle = 90, code = 3, col = "steelblue")

# Add model names as x-axis labels (horizontal and smaller)
axis(1, at = 1:nrow(df), labels = df$Model, tick = FALSE, las = 1, cex.axis = 0.7)

# Add horizontal line at the value of a null effect
# note that a null effect would be an odds difference of 0
abline(h = 0, lty = 2, col = "black")

# Add Vertical line to divide the causal vs associational models
abline(v = 3.5, lty = 2, col = "red")

# Add labels to the margins
mtext("Robustness Checks", side = 1, line = 2.5, at = 4.5, cex = 0.8)
mtext("Main Models", side = 1, line = 2.5, at = 2, cex = 0.8)
mtext("(Regression Models)", side = 1, line = 3.5, at = 4.5, cex = 0.8)
mtext("(Causal Models)", side = 1, line = 3.5, at = 2, cex = 0.8)

# Make Y axis labels horizontal
axis(2, las = 1, tcl = -0.5) # to do: consider adding percent

# Conclude
dev.off()

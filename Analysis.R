# Load data
data <- read.csv("school_scores.csv")

# =====
# Plot.
# =====
plot(
  data$Total.Test.takers, 
  data$Total.Math,
  main = "Relationship Between Total Test-takers and Total Math Score",
  xlab = "Total Test-takers (number of students)",
  ylab = "Total Math Score (points)",
  pch = 19,
  col = "blue"
)

# Add regression line
abline(lm(Total.Math ~ Total.Test.takers, data = data),
       col = "red",
       lwd = 2)

# Save the plot as PNG
png("figures/my_plot.png", width = 1200, height = 900)

# Re-run your plot code here
plot(data$Total.Test.takers, data$Total.Math,
     main = "Relationship Between Total Test-takers and Total Math Score",
     xlab = "Total Test-takers (number of students)",
     ylab = "Total Math Score (points)",
     pch = 19)

abline(lm(Total.Math ~ Total.Test.takers, data = data), lwd = 2)

dev.off()


# =========================
# Histogram of Math Scores.
# =========================

min_score <- min(data$Total.Math)
max_score <- max(data$Total.Math)

cat("\nMath Score Range:", round(min_score, 2), "to", round(max_score, 2), "\n")

# X-axis range 350 - 650.
xlim_lower <- 350
xlim_upper <- 650

h <- hist(data$Total.Math, plot = FALSE)
max_freq <- max(h$counts)

# 15% padding for Y axis to have some space over the histogram max.
ylim_upper <- ceiling(max_freq * 1.15)

cat("X-axis limits:", xlim_lower, "to", xlim_upper, "(steps of 50)\n")
cat("Y-axis limits: 0 to", ylim_upper, "\n\n")

# Plotting Histogram.
hist(
  data$Total.Math,
  main = "Distribution of Total Math Scores",
  xlab = "Total Math Score (points)",
  ylab = "Frequency",
  col = "lightblue",
  border = "black",
  xlim = c(350, 650),
  ylim = c(0, ylim_upper),
  xaxp = c(350, 650, 6)
)

# Saving histogram.
png("figures/histogram_math.png", width = 1200, height = 900)
hist(
  data$Total.Math,
  main = "Distribution of Total Math Scores",
  xlab = "Total Math Score (points)",
  ylab = "Frequency",
  col = "lightblue",
  border = "black",
  xlim = c(350, 650),
  ylim = c(0, ylim_upper),
  xaxp = c(350, 650, 6)
)

#Closing Opened File.
dev.off()

cat("Histogram saved to figures/histogram_math.png\n")

# ==================
# Summary Statistics
# ==================

cat("\n--- Summary Statistics ---\n")
cat("Mean Math Score:", round(mean(data$Total.Math), 2), "\n")
cat("Regression:", round(summary(model)$r.squared, 4), "\n")

# Create folder "figures" if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}


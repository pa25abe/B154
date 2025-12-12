# 
data <- read.csv("school_scores.csv")

# =====
# Plot.
# =====
plot(data$Total.Test.takers, data$Total.Math,
     main = "Relationship Between Total Test-takers and Total Math Score",
     xlab = "Total Test-takers (number of students)",
     ylab = "Total Math Score (points)",
     pch = 19, col = "blue",
     ylim = c(350, 650),
     yaxt = "n")

axis(side = 2, at = seq(350, 650, by = 50))

abline(lm(Total.Math ~ Total.Test.takers, data = data), col = "red", lwd = 2)

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
cat("\n---------------------------------------------\n")
cat("Plot saved to figures/my_plot.png\n")
cat("---------------------------------------------\n")

# =========================
# Histogram of Math Scores.
# =========================

min_score <- min(data$Total.Math)
max_score <- max(data$Total.Math)

# X-axis range 350 - 650.
xlim_lower <- 350
xlim_upper <- 650

h <- hist(data$Total.Math, plot = FALSE)
max_freq <- max(h$counts)

# 15% padding for Y axis to have some space over the histogram max.
ylim_upper <- ceiling(max_freq * 1.15)

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

#Closing Opened Image Files.
dev.off()

cat("Histogram saved to figures/histogram_math.png\n")
cat("---------------------------------------------\n")

# ==================
# Summary Statistics
# ==================

# Creating the regression model
model <- lm(Total.Math ~ Total.Test.takers, data = data)

cat("--- Summary Statistics ---\n")
cat("Mean Math Score:", round(mean(data$Total.Math), 2), "\n")
cat("Regression:", round(summary(model)$r.squared, 4), "\n")

# Create folder "figures" if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}


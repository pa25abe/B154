# Load Data
data <- read.csv("school_scores.csv")

# =====
# Plot.
# =====
plot(data$Total.Test.takers, data$Total.Math,
     main = "Relationship Between Total Test-takers and Total Math Score",
     xlab = "Total Test-takers (number of students)",
     ylab = "Total Math Score (points)",
     pch = 19, col = "blue",
     ylim = c(400, 650),
     xlim = c(20000, 250000),
     yaxt = "n",
     xaxt = "n"
)

axis(side = 2, at = seq(400, 650, by = 50))

axis(side = 1,
     at = c(20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000),
     labels = c("20k", "40k", "60k", "80k", "100k", "120k", "140k", "160k", "180k", "200k", "220k", "240k")
)

# Regression line
abline(lm(Total.Math ~ Total.Test.takers, data = data), col = "red", lwd = 2)

# =========================
# Histogram of Math Scores.
# =========================

min_score <- min(data$Total.Math)
max_score <- max(data$Total.Math)

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

# ==================
# Summary Statistics
# ==================

# Creating the regression model
model <- lm(Total.Math ~ Total.Test.takers, data = data)

cat("--- Summary Statistics ---\n")
cat("Mean Math Score:", round(mean(data$Total.Math), 2), "\n")
cat("Regression:", round(summary(model)$r.squared, 4), "\n")

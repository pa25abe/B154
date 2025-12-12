# Load Data
data <- read.csv("school_scores.csv")

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

# ==================
# Summary Statistics
# ==================

# Creating the regression model
model <- lm(Total.Math ~ Total.Test.takers, data = data)

cat("--- Summary Statistics ---\n")
cat("Mean Math Score:", round(mean(data$Total.Math), 2), "\n")
cat("Regression:", round(summary(model)$r.squared, 4), "\n")

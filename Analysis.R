# Load data
data <- read.csv("/Users/hadimac/Documents/Group Project/B154/school_scores.csv")

# ----------------------------
# Scatter Plot: Test Takers vs Math Score
# ----------------------------

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


# ----------------------------
# Histogram of Math Scores
# ----------------------------

hist(
  data$Total.Math,
  main = "Distribution of Total Math Scores",
  xlab = "Total Math Score (points)",
  col = "lightblue",
  border = "black"
)


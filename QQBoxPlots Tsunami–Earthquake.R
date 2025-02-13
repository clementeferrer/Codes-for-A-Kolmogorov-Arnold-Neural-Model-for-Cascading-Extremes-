################################################################################

library(qqboxplot)
library(dplyr)
library(ggplot2)

################################################################################

DS_residuals_binary <- function(y, alpha_hat) {
  if (y == 1) {
    W_i <- runif(1, 1 - alpha_hat, 1)
    return(qnorm(W_i))
  } else {
    V_i <- runif(1, 0, 1 - alpha_hat)
    return(qnorm(V_i))
  }
}

################################################################################

file_path_1 <- "/Users/clementeferrer/Documents/Extreme Values PhD Project/Paper Neural Modeling of Conditional Extremes/Python Codes/Percentile 15.csv"
file_path_2 <- "/Users/clementeferrer/Documents/Extreme Values PhD Project/Paper Neural Modeling of Conditional Extremes/Python Codes/Percentile 40.csv"
file_path_3 <- "/Users/clementeferrer/Documents/Extreme Values PhD Project/Paper Neural Modeling of Conditional Extremes/Python Codes/Percentile 60.csv"
file_path_4 <- "/Users/clementeferrer/Documents/Extreme Values PhD Project/Paper Neural Modeling of Conditional Extremes/Python Codes/Percentile 85.csv"

df_1 <- read.csv(file_path_1)
df_2 <- read.csv(file_path_2)
df_3 <- read.csv(file_path_3)
df_4 <- read.csv(file_path_4)

residuals_1 <- mapply(DS_residuals_binary, df_1$y_train, df_1$probabilities)
residuals_2 <- mapply(DS_residuals_binary, df_2$y_train, df_2$probabilities)
residuals_3 <- mapply(DS_residuals_binary, df_3$y_train, df_3$probabilities)
residuals_4 <- mapply(DS_residuals_binary, df_4$y_train, df_4$probabilities)

data <- data.frame(
  value = c(residuals_1, residuals_2, residuals_3, residuals_4),
  group = c(
    rep("Percentile 15", length(residuals_1)),
    rep("Percentile 40", length(residuals_2)),
    rep("Percentile 60", length(residuals_3)),
    rep("Percentile 85", length(residuals_4))
  )
)

data %>%
  ggplot(aes(factor(group, levels = c("Percentile 15", "Percentile 40",
                                      "Percentile 60", "Percentile 85")), y = value)) +
  geom_qqboxplot(notch = TRUE, varwidth = TRUE, reference_dist = "norm") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-3.5, 3.5) +
  guides(color = "none") +
  theme(
    axis.text.x = element_text(angle = 0, size = 25),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_text(size = 20),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "grey70")
  )

### 10 trajectories ###

residuals_list <- list()

for (i in 1:10) {
  residuals_1 <- dunn_smyth_residuals_binomial(df_1$y_train, df_1$probabilities)
  residuals_2 <- dunn_smyth_residuals_binomial(df_2$y_train, df_2$probabilities)
  residuals_3 <- dunn_smyth_residuals_binomial(df_3$y_train, df_3$probabilities)
  residuals_4 <- dunn_smyth_residuals_binomial(df_4$y_train, df_4$probabilities)
  
  residuals_list[[i]] <- data.frame(
    value = c(residuals_1, residuals_2, residuals_3, residuals_4),
    group = c(
      rep("Percentile 15", length(residuals_1)),
      rep("Percentile 40", length(residuals_2)),
      rep("Percentile 60", length(residuals_3)),
      rep("Percentile 85", length(residuals_4))
    ),
    iteration = rep(i, length(residuals_1) + length(residuals_2) + length(residuals_3) + length(residuals_4))
  )
}

all_residuals <- do.call(rbind, residuals_list)

all_residuals %>%
  ggplot(aes(factor(group, levels = c("Percentile 15", "Percentile 40", "Percentile 60", "Percentile 85")), y = value, group = interaction(group, iteration))) +
  geom_qqboxplot(notch = TRUE, varwidth = TRUE, reference_dist = "norm", alpha = 0.1) +  
  xlab(NULL) +
  ylab(NULL) +
  ylim(-3.5, 3.5) +
  guides(color = "none") +
  theme(
    axis.text.x = element_text(angle = 0, size = 25),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_text(size = 20),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "grey70")
  )

################################################################################

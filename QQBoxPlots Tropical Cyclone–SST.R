################################################################################

library(qqboxplot)
library(dplyr)
library(ggplot2)

################################################################################

DS_residuals_multinoulli <- function(df) {
  residuals <- numeric(nrow(df))
  
  for (i in 1:nrow(df)) {
    y_true <- df$y_true[i]
    probs <- df[i, 2:ncol(df)]
    
    a_i <- sum(probs[1:y_true])       
    b_i <- sum(probs[1:(y_true + 1)])
    
    if (y_true == 0) {
      a_i <- 0
    }
    
    u_i <- runif(1, a_i, b_i)
    residuals[i] <- qnorm(u_i)
  }
  
  return(residuals)
}

################################################################################

file_path <- "/Users/clementeferrer/Documents/Extreme Values PhD Project/Paper Neural Modeling of Conditional Extremes/Python Codes/residuals_app2.csv"
df <- read.csv(file_path)
residuals <- DS_residuals_multinoulli(df)

data <- data.frame(
  value = c(residuals),
  group = c(
    rep("Ap2", length(residuals))
  )
)

data %>%
  ggplot(aes(factor(group, levels = c("Ap2")), y = value)) +
  geom_qqboxplot(notch = TRUE, varwidth = TRUE, reference_dist = "norm", width = 0.23) +
  geom_vline(xintercept = c(0.71, 1.29), color = "grey70", size = 0.5) +
  geom_vline(xintercept = c(0.42, 0.71, 1.29, 1.58), color = "grey70", size = 0.5) +
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
  residuals <- DS_residuals_multinoulli(df)
  
  residuals_list[[i]] <- data.frame(
    value = residuals,
    group = rep("Multinomial", length(residuals)),
    iteration = rep(i, length(residuals))
  )
}


all_residuals <- do.call(rbind, residuals_list)
all_residuals %>%
  ggplot(aes(
    factor(group, levels = c("Multinomial")), 
    y = value, 
    group = interaction(group, iteration)
  )) +
  geom_qqboxplot(notch = TRUE, varwidth = TRUE, reference_dist = "norm", alpha = 0.1, width = 0.23) +  # Ajustar alpha para la transparencia
  geom_vline(xintercept = c(0.42, 0.71, 1.29, 1.58), color = "grey70", size = 0.5) +
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
source("code/functions.R")
set.seed(14)

result <- simulation(B = 500)
data <- result$bootstrap

write.csv(data, "data/simulation_data.csv")

g1 <- ggplot(data) +
  geom_density(aes(x = lm), fill = "orange", alpha = 0.3) +
  geom_vline(xintercept = 0.8, linetype = "dashed") +
  geom_vline(xintercept = mean(data$lm), col = "red") +
  xlim(0, 1.5) +
  labs(x = "", y = "", subtitle = "lm", title = TeX("Density of Coefficient $\\Delta Y_{it-1}"))
g2 <- ggplot(data) +
  geom_density(aes(x = ivregLag2), fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = 0.8, linetype = "dashed") +
  geom_vline(xintercept = mean(data$ivregLag2), col = "red") +
  xlim(0, 1.5) +
  labs(x = "", y = "", subtitle = TeX("IV $\\Delta Y_{it-2}$"))
g3 <- ggplot(data) +
  geom_density(aes(x = ivregLag2and3), fill = "green", alpha = 0.3) +
  geom_vline(xintercept = 0.8, linetype = "dashed") +
  geom_vline(xintercept = mean(data$ivregLag2and3), col = "red") +
  xlim(0, 1.5) +
  labs(x = "", y = "", subtitle = TeX("IV $\\Delta Y_{it-2},\\Delta Y_{it-3}$"))
  
g <- g1 / g2 / g3

g4 <- ggplot(data) +
  geom_histogram(aes(x = covYU)) +
  geom_vline(xintercept = -1, linetype = "dashed") +
  labs(x = "", y = "", title = TeX("Distribution of $Cov(\\Delta Y_{it-1}, u_{it})$"))

table <- modelsummary(
  list("lm" = result$lm,
       "IV $\\Delta Y_{it-2}$" = result$ivregLag2,
       "IV $\\Delta Y_{it-2},\\Delta Y_{it-3}$" = result$ivregLag2and3),
  escape = FALSE,
  stars = TRUE,
  coef_map = c(
    "laggedDeltaY" = "$\\Delta Y_{it-1}$",
    "DeltaX" = "$\\Delta X_{it}$",
    "(Intercept)"
  ),
  gof_omit = "AIC|BIC|RMSE|Log.Lik.",
  metrics = "all",
  output = "output/result.tex",
  options = options("modelsummary_format_numeric_latex" = "plain")
)

ggsave("figure/density_of_coefficiets.png", g, height = 14, width = 8)
ggsave("figure/distribution_of_covariance.png", g4, height = 5, width = 8)

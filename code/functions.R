library(tidyverse)
library(dplyr)
library(AER)
library(coefplot)
library(tcltk)
library(modelsummary)
library(latex2exp)

gen_data <- function(maxT, N){
  y <- c()
  lagY <- c()
  x <- c()
  # y_{it} = \alpha_i + \rho * y_{it-1} + \beta * x_{it} + u_{it}
  alpha <- rnorm(N, 10)
  rho <- 0.8
  beta <- 0.8
  
  ui <- rnorm(N)
  xi <- rnorm(N, 10)
  yi <- alpha + beta * xi + ui
  
  y <- yi
  laggedY <- rep(NaN, N)
  x <- xi
  u <- ui
  i <- 1:N
  time <- rep(1, N)
  
  for(t in 2:maxT){
    laggedY <- c(laggedY, yi)
    ui <- rnorm(N)
    xi <- rnorm(N)
    yi <- alpha + rho * yi + beta * xi + ui
    y <- c(y, yi)
    x <- c(x, xi)
    i <- c(i, 1:N)
    u <- c(u, ui)
    time <- c(time, rep(t, N))
  }
  data <- tibble(
    i = i,
    t = time,
    y = y,
    laggedY = laggedY,
    x = x,
    u = u
  )
  return(data)
}
simulation <- function(B, N = 1000){
  pb <- txtProgressBar(min = 1, max = B, style = 3)
  rho1 <- c()
  rho2 <- c()
  rho3 <- c()
  covYU <- c()
  for(b in 1:B){
    setTxtProgressBar(pb, b)
    raw_data <- gen_data(5, N)
    data <- raw_data %>% 
      group_by(i) %>% 
      mutate(DeltaY = y - laggedY,
             laggedDeltaY = lag(DeltaY),
             laggedDeltaY2 = lag(DeltaY, 2),
             laggedDeltaY3 = lag(DeltaY, 3),
             DeltaX = x - lag(x),
             DeltaU = u - lag(u))
    result1 <- lm(DeltaY ~ laggedDeltaY + DeltaX, data = data)
    result2 <- ivreg(DeltaY ~ laggedDeltaY + DeltaX | laggedDeltaY2 + DeltaX, data = data)
    result3 <- ivreg(DeltaY ~ laggedDeltaY + DeltaX | laggedDeltaY2 + laggedDeltaY3 + DeltaX, data = data)
    rho1 <- c(rho1, result1$coefficients[2] [[1]])
    rho2 <- c(rho2, result2$coefficients[2] [[1]])
    rho3 <- c(rho3, result3$coefficients[2] [[1]])
    covYU <- c(covYU, cov(filter(data, t == 5)$laggedDeltaY, filter(data, t == 5)$DeltaU))
  }
  result_data <- tibble(
    lm = rho1,
    ivregLag2 = rho2,
    ivregLag2and3 = rho3,
    covYU = covYU
  )
  result <- c()
  result$lm = result1
  result$ivregLag2 = result2
  result$ivregLag2and3 = result3
  result$bootstrap = result_data
  return(result)
}

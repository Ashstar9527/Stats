# ---- Setup ----
ans <- read.csv("ps1/ans.csv")

datasets <- unique(ans$dataset)

# ---- 1.1: Summary statistics per dataset ----
results_1_1 <- data.frame(
  dataset = character(),
  mean_x = numeric(),
  mean_y = numeric(),
  var_x = numeric(),
  var_y = numeric(),
  corr_xy = numeric(),
  r_squared = numeric()
)

for (d in datasets) {
  sub <- ans[ans$dataset == d, ]

  mean_x <- mean(sub$x)
  mean_y <- mean(sub$y)
  var_x <- var(sub$x) # sample variance, n-1 denominator
  var_y <- var(sub$y)
  corr_xy <- cor(sub$x, sub$y)

  # 1.1.6: R^2 from a simple linear regression of y on x equals corr(x,y)^2
  r_squared <- corr_xy^2

  results_1_1 <- rbind(results_1_1, data.frame(
    dataset = d,
    mean_x = mean_x,
    mean_y = mean_y,
    var_x = var_x,
    var_y = var_y,
    corr_xy = corr_xy,
    r_squared = r_squared
  ))
}

rownames(results_1_1) <- NULL
print(results_1_1)

# ---- 1.2: OLS estimates per dataset ----
results_1_2 <- data.frame(
  dataset = character(),
  intercept = numeric(),
  slope = numeric()
)

for (d in datasets) {
  sub <- ans[ans$dataset == d, ]

  fit <- lm(y ~ x, data = sub)

  results_1_2 <- rbind(results_1_2, data.frame(
    dataset = d,
    intercept = unname(coef(fit)[1]),
    slope = unname(coef(fit)[2])
  ))
}

rownames(results_1_2) <- NULL
print(results_1_2)

write.csv(results_1_1, "ps1/results_1_1.csv", row.names = FALSE)
write.csv(results_1_2, "ps1/results_1_2.csv", row.names = FALSE)



# ---- 1.4: Scatterplot ----
png("/Users/evie/Desktop/Yale/Stats/ps1/scatterplots.png",
  width = 10, height = 8, units = "in", res = 300
)

par(mfrow = c(2, 2))

for (d in datasets) {
  sub <- ans[ans$dataset == d, ]
  fit <- lm(y ~ x, data = sub)

  plot(sub$x, sub$y,
    main = paste("Dataset", d),
    xlab = "x", ylab = "y",
    pch = 19, col = "steelblue"
  )

  abline(fit, col = "firebrick", lwd = 2)
}

par(mfrow = c(1, 1))
dev.off()

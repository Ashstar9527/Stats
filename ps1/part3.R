# Part 3: Predicting portfolio risk

df <- read.csv("jkp_factors.csv")

# Keep only eom, market, value, momentum, quality
df <- df[, c("eom", "market", "value", "momentum", "quality")]

# ---- 3.1: Mean of market, value, momentum, quality ----
means <- sapply(df[, c("market", "value", "momentum", "quality")], mean)
print(means)

# ---- 3.2: Sample standard deviation of each of the 4 columns ----
sds <- sapply(df[, c("market", "value", "momentum", "quality")], sd)
print(sds)

# ---- 3.3: Correlation matrix of the 4 return columns ----
corr_mat <- cor(df[, c("market", "value", "momentum", "quality")])
print(corr_mat)

# ---- 3.4: Standard deviation of The Portfolio ----
# Weights: 50% value, 25% momentum, 25% quality, 0% market
w <- c(market = 0, value = 0.5, momentum = 0.25, quality = 0.25)

# Reorder sds and corr_mat to match weight vector order
assets <- names(w)
sd_vec <- sds[assets]
corr_sub <- corr_mat[assets, assets]

# Var(P) = sum_i sum_j w_i * w_j * sd_i * sd_j * corr(i,j)
var_p <- 0
for (i in assets) {
  for (j in assets) {
    var_p <- var_p + w[i] * w[j] * sd_vec[i] * sd_vec[j] * corr_sub[i, j]
  }
}

sd_p <- unname(sqrt(var_p))
print(sd_p)

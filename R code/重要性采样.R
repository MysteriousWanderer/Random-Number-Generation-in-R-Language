# 目标分布：正态分布
target_pdf <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}
# 提议分布：另一个正态分布
proposal_pdf <- function(x) {
  dnorm(x, mean = 0, sd = 2)  # 使用均值为0，标准差为2的正态分布作为提议分布
}
# 生成样本数量
N <- 1000
# 从提议分布中抽样
samples <- rnorm(N, mean = 0, sd = 2)
# 计算权重
weights <- target_pdf(samples) / proposal_pdf(samples)
# 归一化权重
normalized_weights <- weights / sum(weights)
# 生成随机数
random_indices <- sample.int(N, size = N, replace = TRUE, prob = normalized_weights)
random_samples <- samples[random_indices]
# 绘制生成的随机数的直方图与正态分布的密度曲线进行比较
hist(random_samples, freq = FALSE, main = "Generated Random Numbers vs. Normal Distribution",
     xlab = "Value", ylab = "Density")
curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "blue", lwd = 2, lty = 2)  # 目标分布的密度曲线
curve(dnorm(x, mean = 0, sd = 2), add = TRUE, col = "red", lwd = 2, lty = 2)   # 提议分布的密度曲线
legend("topright", legend = c("Target Distribution", "Proposal Distribution"),
       col = c("blue", "red"), lty = 2, lwd = 2, cex = 0.8)

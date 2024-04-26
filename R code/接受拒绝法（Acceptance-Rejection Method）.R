# 定义目标概率密度函数
f <- function(x) 2 * x^3 * exp(-x^2)

# 定义辅助概率密度函数g，这里选择一个简单的指数分布作为辅助函数
g <- function(x) dexp(x, rate = 1)

# 定义常数k，使得f(x) <= k * g(x) 对所有x都成立
k <- 2

# 生成随机数的数量
n <- 1000

# 初始化接受的样本
accepted_samples <- numeric(0)

# 使用接受-拒绝法生成样本
while(length(accepted_samples) < n) {
  # 从辅助分布中生成一个随机数
  x <- rexp(1, rate = 1)
  # 从均匀分布中生成一个随机数
  u <- runif(1)
  # 接受条件
  if(u <= f(x) / (k * g(x))) {
    accepted_samples <- c(accepted_samples, x)
  }
}

# 绘制生成的样本与目标分布进行比较
hist(accepted_samples, breaks = 30, freq = FALSE, col = "lightblue", main = "Acceptance-Rejection Method Example")

# 绘制目标概率密度函数
curve(f, from = 0, to = 5, add = TRUE, col = "red", lwd = 2)

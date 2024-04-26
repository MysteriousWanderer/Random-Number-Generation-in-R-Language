# 设置参数
N <- 1000
mean <- 0
sd <- 1

# 生成服从正态分布的随机数
random_numbers <- rnorm(N, mean, sd)

# 绘制直方图
hist(random_numbers, breaks = 30, freq = FALSE, col = "lightblue", main = "Normal Distribution")

# 绘制密度函数
curve(dnorm(x, mean, sd), add = TRUE, col = "red", lwd = 2)

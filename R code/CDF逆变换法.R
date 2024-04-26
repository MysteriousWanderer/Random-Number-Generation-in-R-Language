# 定义伽玛分布的概率密度函数和累积分布函数
shape <- 2  
rate <- 1  
f <- function(x) ifelse(x > 0, (rate^shape / gamma(shape)) * x^(shape - 1) * exp(-rate * x), 0)  # 伽玛分布的概率密度函数
F <- function(x) ifelse(x <= 0, 0, pgamma(x, shape, rate))  # 伽玛分布的累积分布函数

# 定义累积分布函数的反函数
inv_F <- function(u) qgamma(u, shape, rate)

# 生成均匀分布的随机数
u <- runif(1000)

# 映射到伽玛分布
x <- inv_F(u)

# 绘制直方图和真实的概率密度函数进行比较
hist(x, breaks = 30, freq = FALSE, xlim = c(0, 5), main = "Gamma Distribution", xlab = "x", ylab = "Density")
curve(f, from = 0, to = 5, col = "red", add = TRUE, lwd = 2)

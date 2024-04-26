# 定义生成三角形分布随机数的函数
triangle_random <- function(a, b, c, size) {
  U <- runif(size)
  X <- ifelse(U < (c - a) / (b - a), a + sqrt(U * (b - a) * (c - a)), b - sqrt((1 - U) * (b - a) * (b - c)))
  return(X)
}
# 设置参数
a <- 1
b <- 9
c <- 5
size <- 10000
# 生成三角形分布的随机数
X <- triangle_random(a, b, c, size)
# 绘制直方图
hist(X, breaks = 50, prob = TRUE, col = "lightblue", border = "white")

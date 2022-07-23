# a 100-sample with X1 ~ U(0.5, 1.5)
#                   X2 ~ U(1.5, 4.5)
#                   X3 ~ U(4.5, 13.5)

library(boot)
n <- 100
X <- data.frame(X1 = runif(n, 0.5, 1.5),
                X2 = runif(n, 1.5, 4.5),
                X3 = runif(n, 4.5, 13.5))

# linear model : Y = X1 + X2 + X3
X

y <- with(X, X1 + X2 + X3)
y

# sensitivity analysis
library(sensitivity)
x <- src(X, y, nboot = 100)
print(x)
plot(x)

library(ggplot2)
ggplot(x)

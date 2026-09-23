
set.seed(1345)

x <- rgamma(1000, shape = 2, rate = 2)

par(mar = c(4, 0.1, 0.1, 0.1))

hist(x, 
     freq = F, 
     col = "lightblue", 
     yaxt = "n", 
     ylab = "", 
     xaxt = "n",
     border = "white",
     breaks = "Scott",
     xlab = "", 
     main = "",
     xlim = c(0, 0.8 * max(x)))
curve(dgamma(x, shape = 2, rate = 2), from = 0, to = max(x), n = 1000, 
      col = "red", add = TRUE, lwd = 10)
abline(h = 0, lwd = 2)
mtext(expression(theta[0]), side = 1, at = 1, cex = 4, line = 3)
abline(v = 1, lwd = 6, lty = 2)
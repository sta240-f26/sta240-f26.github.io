# ==============================================================================
# helper functions
# ==============================================================================

pmf_custom <- function(x) {6 / (pi^2 * x^2)}

r_custom <- function(n) {
  replicate(n, {
    u <- runif(1)
    
    x <- 1
    p <- pmf_custom(x)
    cdf <- p
    
    while (cdf < u) {
      x <- x + 1
      p <- pmf_custom(x)
      cdf <- cdf + p
    }
    
    x
  })
}

# ==============================================================================
# plot the pmf
# ==============================================================================

range_X <- 1:20
PMF <- 6 / (pi^2 * range_X^2)

plot(range_X, PMF, type = "h", lwd = 3)

# ==============================================================================
# simulate and compare to truth
# ==============================================================================

n <- 1000

x <- r_custom(n)

obs_X <- sort(unique(x))
props <- table(x) / n

plot(range_X, PMF, type = "h", lwd = 3)
points(obs_X + 0.1, props, type = "h", lwd = 3, col = "red")
legend("topright", legend = c("Theoretical probs", "sample proportions"), lwd = 2, col = c("black", "red"), bty = "n")
x
mean(x)

# ==============================================================================
# track the running average
# ==============================================================================

set.seed(23456)

n <- 10000

x <- r_custom(n)

plot(1:n, cumsum(x) / 1:n, type = "l")

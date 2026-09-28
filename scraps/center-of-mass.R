# ==============================================================================
# specify the distribution of the random variable
# ==============================================================================

range_X <- -6:2

mass <- c(3, 7, 6, 4, 4, 3, 3, 2, 1)

PMF <- mass / sum(mass)

# ==============================================================================
# plot the PMF
# ==============================================================================

plot(range_X, PMF, type = "h", xlim = c(-6, 6), lwd = 3, 
     xlab = "k",
     ylab = "P(X = k)",
     xaxt = "n",
     bty = "n")
axis(1, at = -6:6, labels = -6:6)

# ==============================================================================
# compute the theoretical expected value (center of mass)
# ==============================================================================

EX <- sum(range_X * PMF)
medX <- range_X[which(cumsum(PMF) >= 0.5)[1]]


pmf <- plot(range_X, PMF, type = "h", xlim = c(-6, 6), lwd = 3, 
     xlab = "k",
     ylab = "P(X = k)",
     xaxt = "n",
     bty = "n")
axis(1, at = -6:6, labels = -6:6)
abline(v = EX, col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Theoretical E(X)", lty = 2, lwd = 2, col = "red", bty = "n")

# ==============================================================================
# compute the true center of mass accounting for mass of the rig
# ==============================================================================

rig_mass <- 11.6
rig_com <- 0
bead_mass <- 2 * sum(mass)

com <- (rig_mass * rig_com + bead_mass * EX) / (rig_mass + bead_mass)


plot(range_X, PMF, type = "h", xlim = c(-6, 6), lwd = 3, 
     xlab = "k",
     ylab = "P(X = k)",
     xaxt = "n",
     bty = "n")
axis(1, at = -6:6, labels = -6:6)
abline(v = EX, col = "red", lwd = 2, lty = 2)
abline(v = com, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Theoretical E(X)", "Physical center-of-mass"), lty = 2, lwd = 2, col = c("red", "blue"), bty = "n")



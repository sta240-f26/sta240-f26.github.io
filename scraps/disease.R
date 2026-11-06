disease_viz <- function(p, TNR, TPR, my_main){
  FNR <- 1 - TPR
  FPR <- 1 - TNR
  
  par(mar = c(4, 4, 4, 4))
  
  plot(0, col = "white", xlim = c(0, 1), ylim = c(0, 1),
       xaxs = "i", yaxs = "i",
       xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "", main = my_main)
  
  grid(col = "black", nx = 10)
  
  polygon(c(0, p, p, 0), c(FNR, FNR, 1.0, 1.0), col = rgb(1, 0, 0, alpha = 0.25), lwd = 4)
  polygon(c(0, p, p, 0), c(0, 0, FNR, FNR), col = rgb(0, 1, 0, alpha = 0.25), lwd = 4)
  polygon(c(p, 1, 1, p), c(0, 0, TNR, TNR), col = rgb(0, 0, 1, alpha = 0.25), lwd = 4)
  polygon(c(p, 1, 1, p), c(TNR, TNR, 1, 1), col = rgb(1, 0.647, 0, alpha = 0.25), lwd = 4)
  
  mtext("TNR", side = 4, at = TNR / 2, las = 1, line = 1)
  mtext("FPR", side = 4, at = (1 + TNR) / 2, las = 1, line = 1)
  
  mtext("TPR", side = 2, at = (1 + FNR) / 2, las = 1, line = 1.5)
  mtext("FNR", side = 2, at = FNR / 2, las = 1, line = 1.5)
  
  mtext("p(D = +)", side = 1, at = p / 2, las = 1, line = 1.75)
  mtext("p(D = -)", side = 1, at = (1 + p) / 2, las = 1, line = 1.75)
  
  mtext(c("0", "1"), side = 1, at = c(0, 1), las = 1, line = 0.5)
  mtext(c("0", "1"), side = 2, at = c(0, 1), las = 1, line = 0.5)
}



par(mfrow = c(2, 2))

disease_viz(0.5, 0.90, 0.90, "Test A")

disease_viz(0.7, 0.50, 0.50, "Test B")

disease_viz(0.1, 0.70, 0.5, "Test C")

disease_viz(0.4, 0.70, 0.30, "Test D")




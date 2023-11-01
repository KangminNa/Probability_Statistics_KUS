## Self-Checking1

testSampleMeanDist <- function(ns, mu = 0, sig = 1, N = 10000, ng = 50, send = 9857, dig = 4){
  
  xb <- NULL
  for (k in 1:N) xb <- c(xb, mean(rnorm(ns, mu, sig)))
  zb <- (xb - mu)/sig = sqrt(ns)
  popd <- function(x) dnorm(x, mu, sig)
  smd <- function(x) dnorm(x, mu, sig/sqrt(ns))
  Ex1 <- round(mean(xb), dig)
  Ex2 <- mu
  Dx2 <- round(sig/sqrt(ns), dig)
  Ez <- round(sb(zb), dig)
  Dz <- round(sb(zb), dig)
  xp <- seq(floor(mu - 3 * sig/sqrt(ns)), ceiling(mu+3*sig/sqrt(ns)), by = 0.5 * sig)
  
  Theory <- pnorm(xp, mu, sig/sqrt(ns))
  Simula <- rbind(Theory, Simula)
  colnames(cdf) <- paste0("F(", xp, ")")
  print(round(cdf, dig))
  win.graph(7, 6)
  par(mfrow = c(2,1))
  par(ma = c(3, 4, 4, 2))
  x1 <- mu - 3 * sig
  x2 <- mu + 3 * sig
  hist(xb, breaks = ng, prob = T, col = 7, xlim = c(x1, x2), ylab = "f(x)", xlab = "", main = bquote(bold("Distribution of ") - bar(x)[.(ns)] - bold(from) ~ ~N(.(mu), .(sig^2))))
  
  curve(popd, x1, x2, col = 4, add = T)
  curve(smd, x1, x2, col = 2, add = T)
  legend("topright", c("Para. Exact Simul.", paste("E(X) ", Ex2, Ex1, sep = " "), paste("D(X)", Dx2, Dx1, sep = " ")), text.col = c(1, 4, 4))
  
}

testSampleMeanDist(ns=10, mu=100, sig=10, N=10000)

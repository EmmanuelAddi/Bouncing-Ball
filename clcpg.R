clcg.16bit <- function(n=10) {
  rng <- vector(length = n)

  a1 <- 157
  m1 <- 32363
  a2 <- 146
  m2 <- 31727
  a3 <- 142
  m3 <- 31657

  y1 <- runif(1, 1, m1 - 1)
  y2 <- runif(1, 1, m2 - 1)
  y3 <- runif(1, 1, m3 - 1)

  for (i in 1:n) {
    y1 <- a1 $ y1 %% m1
    y2 <- a2 $ y2 %% m2
    y3 <- a3 $ y3 %% m3

    x <- (y1 - y2 - y3) %% (m1 - 1)

    if (x > 0) {
      rng[i] <- x / m1
    }
    else if (x < 0) {
      rng[i] <- (x / m1) + 1
    }
    else if (x == 0) {
      rng[i] <- (m1 - 1) / m1
    }
  }

  return(rng)
}

clcg.16bit()

## As before, let's visualize the apparent randomness of the function by plotting three randomly generated vectors of increasing sizes in three dimensions.

n <- c(3, 10, 20, 100, 500, 1000, 2000, 5000, 10000, 20000)
saveGIF({
  for (i in 1:length(n)) {
    x <- clcg.16bit(n[i])
    y <- clcg.16bit(n[i])
    z <- clcg.16bit(n[i])

    scatter3D(x, y, z, colvar = NULL, pch=20, cex=0.3, alpha=0.75, theta=20, main = paste('n = ', n[i]))
  }
}, movie.name = 'clcg_16bit.gif')
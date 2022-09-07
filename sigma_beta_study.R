###############################################################################
# SIGMA^BETA BEHAVIOR
###############################################################################


# general case
ff <- function(x, y) y^x
ll <- function(x, y) y
x <- seq(0, 1, by = .01) # beta
y <- seq(0, .01, by = .01) # sigma
l <- y # ell
z <- outer(x, y, ff)
z2 <- outer(x, l, ll)
filled.contour(
  x, y, z, 
  nlevels = 30,
  xlab = expression(beta), 
  ylab = expression(sigma),
  key.title = {
    par(cex.main = 1)
    title(main = expression(sigma^beta))
  },
  plot.axes = {
    axis(1)
    axis(2)
    contour(x, y, z, nlevels = 500, add = T, lwd = 1)
    contour(x, l, z2, nlevels = 6, add = T, lwd = 2, lty = 2, col = 'green')
    abline(v = c(.5, 1, 1.5), lwd = 2, lty = 2)
    legend(
      'bottomright', 
      c('ell', expression(beta), expression(sigma^beta)), 
      col = c('green', 'black', 'black'), 
      lty = c(2, 2, 1), 
      cex = 1
    )
  }
)



# evaluating (sigmax/abs(mt))^beta
gg <- function(x, y, m) (y/abs(m))^x
ll <- function(x, y) y
x <- seq(0, 2, by = .01) # beta
y <- seq(.01, 1, by = .01) # sigma
m <- c(.5, 1, 5, 10)
l <- seq(.01, 1, by = .01) # ell
z <- list()
for (i in 1:length(m)) {
  z[[i]] <- outer(x, y, gg, m = m[i])
}
dim(z)
z2 <- outer(x, l, ll)
for (i in 1:length(m)) {
  pdf(
    file = paste0('simulacao_', m[[i]], '.pdf'),
    width = 10, height = 6, bg = 'white'
  )
  filled.contour(
    x, y, z[[i]], 
    nlevels = 30,
    xlab = expression(beta), 
    ylab = expression(sigma/abs(m)),
    main = paste0('|m| = ', m[[i]]),
    key.title = {
      par(cex.main = 1)
      title(main = expression((sigma/abs(m))^beta))
    },
    plot.axes = {
      axis(1)
      axis(2)
      contour(x, y, z[[i]], nlevels = 30, add = T, lwd = 1)
      contour(x, l, z2, nlevels = 6, add = T, lwd = 3, lty = 2, col = 'green')
      abline(v = c(.5, 1), lwd = 2, lty = 2)
      legend(
        'bottomright', 
        c('ell', expression(beta), expression((sigma/abs(m))^beta)), 
        col = c('green', 'black', 'black'), 
        lty = c(2, 2, 1), 
        cex = 1
      )
    }
  )
  dev.off()
}

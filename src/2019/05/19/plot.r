x <- read.table('src/2019/05/19/abs.txt')

svg('2019/05/19/abs.svg', 8, 4)
par(mar = c(5, 5, 2, 2) + .1)
plot(
	x, type = 'l',
	xlab = bquote(lambda*', nm'), ylab = bquote('Absorbance, '*cm^{-1})
)
dev.off()

window <- 7
crit <- 3
spdiff <- function(x) {
	ret <- data.frame(
			x = x[-nrow(x),1],
			y = diff(x[,2]) / diff(x[,1])
	)
	attr(ret, 'y0') <- x[1,2]
	attr(ret, 'xn') <- x[nrow(x),1]
	ret
}

spundiff <- function(x)
	data.frame(
		x = c(x$x, attr(x, 'xn')),
		y = cumsum(c(attr(x, 'y0'), x$y * diff(c(x$x, attr(x, 'xn')))))
	)

roll <- function(x, fun, window) {
	stopifnot(window %% 2 == 1)
	ret <- x
	for (i in seq_along(x))
		ret[i] <- fun(
			x[max(1, i - window %/% 2):min(length(x), i + window %/% 2)]
		)
	ret
}

x <- spdiff(x)

svg('2019/05/19/absd.svg', 8, 4)
par(mar = c(5, 6, 2, 2) + .1)
plot(x, col = 'red', xlab = bquote(lambda*', nm'), ylab = bquote(frac(Delta * A, Delta * lambda)))

meds <- roll(x[,2], median, window)
mads <- roll(x[,2], mad, window)
x[,2] <- ifelse(abs(x[,2] - meds) > mads * crit, meds, x[,2])
points(x, col = 'green')
dev.off()

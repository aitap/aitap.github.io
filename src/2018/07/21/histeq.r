require(pixmap)

sigma <- function(x,c,s) 1/(1+exp(-s*(x-c)))

mow <- read.pnm('src/2018/07/21/moscow.ppm')

png('2018/07/21/moscow.png', width=643, height=1502, res=96)
layout(matrix(nrow=6, ncol=2, data=c(1,1,1,1,2,3,4,5,6,7,8,9), byrow=T))

pixmap::plot(mow, axes=T)

for (p in list(
	c(c=.25, s=10),
	c(c=.75, s=10),
	c(c=.50, s=10),
	c(c=.50, s=20)
)) {
	plot(function(x) sigma(x,p['c'], p['s']), from=0, to=1, xlab='', ylab='')
	mow.c <- mow
	mow.c@red <- sigma(mow@red, p['c'], p['s'])
	mow.c@green <- sigma(mow@green, p['c'], p['s'])
	mow.c@blue <- sigma(mow@blue, p['c'], p['s'])
	pixmap::plot(mow.c, axes=T)
}

dev.off()

brks <- 42
hists <- list(red=hist(mow@red, breaks=brks, plot=F), green=hist(mow@green, breaks=brks, plot=F), blue=hist(mow@blue, breaks=brks, plot=F))

svg('2018/07/21/mow_hist.svg', width=6, height=6)

plot(c(0,1), c(0, max(unlist(lapply(hists, function(h) max(h$density))))), type='n', xlab='Value', ylab='Density')

plot.histp <- function(h, new=F, ...) {
	if (new) plot(c(0,1), c(0, max(h$density)), type='n', xlab='Value', ylab='Density', ...)
	with(h, polygon(c(0,mids,1),c(0,density,0), ...))
}

for (ch in names(hists))
	plot.histp(hists[[ch]], col=rgb(t(col2rgb(ch)), alpha=255/3, maxColorValue=255))

dev.off()

pp <- read.pnm('src/2018/07/21/paper.pgm')

png('2018/07/21/paper.png', width=1000, height=400, res=96)
par(mfrow=c(1,3))

pixmap::plot(pp, axes=T)
pp.h <- hist(pp@grey, breaks=32, plot=F)

pp.h$density <- with(pp.h, scale(density, center=min(density), scale=diff(range(density))))
plot.histp(pp.h, T, col=rgb(.5,.5,.5,.5))
plot(function(x) sigma(x,.32,20), from=0, to=1, add=T)

pp.t <- pp
pp.t@grey <- sigma(pp.t@grey, .25, 16)
pixmap::plot(pp.t, axes=T, main=bquote(sigma(x)))

dev.off()

my.ecdf <- function(x) {
	ret <- ecdf(x)(x)
	dim(ret) <- dim(x)
	ret
}

png('2018/07/21/mow_flat.png', width=635, height=485)
mow.fh <- mow
mow.fh@red <- my.ecdf(mow@red)
mow.fh@green <- my.ecdf(mow@green)
mow.fh@blue <- my.ecdf(mow@blue)
pixmap::plot(mow.fh, axes=T, main=bquote(eCDF[x](x)))
dev.off()

svg('2018/07/21/inthist.svg', width=8, height=4)
par(mfrow=c(1,3))
plot.histp(pp.h, T, main='Histogram', col='gray')
pp.csh <- list(density=cumsum(c(pp.h$density,0)), mids=c(pp.h$mids, 1))
plot.histp(pp.csh, T, main='Cumulative sum of histogram', col='gray')
plot(ecdf(pp@grey), xlim=c(0,1), main='eCDF')
dev.off()

plot.pal <- function(p, ...) image(1:length(p), 1, matrix(1:length(p), ncol=1), col=p, xlab="", ylab="", yaxt="n", ...)
require(rje)
svg('2018/07/21/palettes.svg', width=6, height=12)
par(mfrow=c(8,1))
plot.pal(palette(), main='palette("default")')
for (f in c("heat.colors", "terrain.colors", "topo.colors", "rainbow", "grey.colors", "cm.colors", "cubeHelix"))
	plot.pal(get(f)(32), main=paste(f,"(32)"))
dev.off()

svg('2018/07/21/color_ramp_palette.svg', width=6, height=2)
plot.pal(colorRampPalette(c("blue","white","red"))(32),main='colorRampPalette(c("blue","white","red"))(32)')
dev.off()

myds <- (1:8)^3
myds.e <- ecdf(myds)

svg('2018/07/21/ecdf_uniform.svg', width=8, height=4)
par(mfrow=c(1,2))
plot(myds.e, main="eCDF")
lingrid <- (0:10)/10*max(myds)
plot(lingrid, myds.e(lingrid), main="eCDF evaluated on uniform grid", xlab="", ylab="")
abline(v=lingrid)
abline(h=myds.e(lingrid))
dev.off()

svg('2018/07/21/ecdf_uniform2.svg', width=4, height=4)
coords <- myds.e(lingrid)
zeroes <- which(diff(coords) == 0)
zeroes[zeroes==1] <- 2 # don't replace at start
coords[zeroes] <- approx(lingrid[-zeroes], coords[-zeroes], lingrid[zeroes])$y
matplot(lingrid, cbind(myds.e(lingrid), coords), main="eCDF interpolated at uniform grid", xlab="", ylab="", pch=1:2, col=1:2)
legend('bottomright', legend=c("Original","Interpolated"), pch=1:2, col=1:2)
dev.off()

histeq.palette <- function(x, pal, max.col=2048, ...) {
	function(n) {
		grid <- seq(from=min(x), to=max(x), length.out=n)
		coords <- ecdf(x)(grid)
		zeroes <- which(diff(coords) == 0)
		zeroes[zeroes==1] <- 2
		coords[zeroes] <- approx(grid[-zeroes], coords[-zeroes], grid[zeroes])$y
		n.pal <- as.integer(min(max.col, 1/min(diff(coords))))
		pal(n.pal, ...)[round(1+coords*(n.pal-1))]
	}
}
require(raster)
fe <- as.matrix(raster('src/2018/07/21/Fe.tif'))
require(fields)
png('2018/07/21/wavy_spectrum.png', width=1000, height=1200)
par(mfrow=c(2,1))
image.plot(t(fe), axes=F, col=topo.colors(512), main="Linear palette")
image.plot(t(fe), axes=F, col=histeq.palette(fe, topo.colors)(512), main="Histogram-flattening palette")
dev.off()

## -----------------------------------------------------------------------------
# let's cache operations taking traffic or spending CPU time
cache <- function(file, expr) {
	file <- file.path('src', '2020', '07', '10', file)
	tryCatch(
		suppressWarnings(readRDS(file)),
		error = function(e) {
			ret <- force(expr)
			saveRDS(ret, file, version = 2)
			ret
		}
	)
}
pkgs <- cache('available_packages.rds',
	available.packages(filters = c("duplicates"))
)


## -----------------------------------------------------------------------------
base <- c(
	"base", "tools", "utils", "grDevices", "graphics", "stats",
	"datasets", "methods", "grid", "splines", "stats4", "tcltk",
	"compiler", "parallel"
)

recommended <- c(
	"MASS", "lattice", "Matrix", "nlme", "survival", "boot", "cluster",
	"codetools", "foreign", "KernSmooth", "rpart", "class", "nnet",
	"spatial", "mgcv"
)

installed <- installed.packages(fields = c('Package'))[c(base, recommended),]
installed[,'Version'] <- '4.0.2' # stable wersion as of time of writing


## -----------------------------------------------------------------------------

# This function is lifted straight out of the `utils` package. It represents
# the dependency resolution algorithm used by `install.packages`, with minor
# edits to always assume a fresh system with only core and recommended packages
# installed. It references other internal functions from `utils`, which is
# bad design, but for this one-off code we can let it slip. By the way, the
# referenced function hasn't changed since 2012, and most of the edits date as
# far back as 2008-2009; while `getDependencies` was last edited in 2014, the
# meat of the dependency resolution logic dates back to 2008, too.
#
# In theory, we _could_ have used `tools::package_dependencies`, but we also
# want the dependency tree depths, which it's not designed to return.

getDependencies <- function(pkgs, available, dependencies = NA, binary = FALSE) {
	if (is.null(dependencies)) return(unique(pkgs))
	dep2 <- NULL
	if(is.logical(dependencies) && is.na(dependencies))
		dependencies <- c("Depends", "Imports", "LinkingTo")
	depends <-
		is.character(dependencies) || (is.logical(dependencies) && dependencies)
	if(depends && is.logical(dependencies)) {
		if(binary) {
			dependencies <-  c("Depends", "Imports", "Suggests")
			dep2 <- c("Depends", "Imports")
		} else {
			dependencies <-  c("Depends", "Imports", "LinkingTo", "Suggests")
			dep2 <- c("Depends", "Imports", "LinkingTo")
		}
	}
	p00 <- p0 <- unique(pkgs)
	miss <-  !p0 %in% row.names(available)
	if(sum(miss)) {
		msg <- paste0(if(binary) "as a binary package ", "for ",
			sub(" *\\(.*","", R.version.string))
		warning(sprintf(ngettext(sum(miss),
				"package %s is not available (%s)",
				"packages %s are not available (%s)"),
			paste(sQuote(p0[miss]), collapse = ", "), msg),
			domain = NA, call. = FALSE)
		base <- vapply(p0[miss], isBasePkg, FALSE)
		if (sum(base))
			warning(sprintf(ngettext(sum(base),
					"package %s is a base package, and should not be updated",
					"packages %s are base packages, and should not be updated"),
				paste(sQuote(p0[miss][base]), collapse = ", ")),
				domain = NA, call. = FALSE)
			if (sum(miss) == 1L &&
				!is.na(w <- match(tolower(p0[miss]),
					tolower(row.names(available))))) {
				warning(sprintf("Perhaps you meant %s ?",
						sQuote(row.names(available)[w])),
					call. = FALSE, domain = NA)
			}
		flush.console()
	}
	p0 <- p0[!miss]

	if(depends) { # check for dependencies, recursively
		depth <- 0
		p1 <- p0
		repeat {
			deps <- apply(available[p1, dependencies, drop = FALSE],
					1L, function(x) paste(x[!is.na(x)], collapse=", "))
			res <- utils:::.clean_up_dependencies2(deps, installed, available)
			deps <- unique(res[[1L]])
			## R should not get to here, but be safe
			deps <- deps[!deps %in% c("R", pkgs)]
			if(!length(deps)) break
			depth <- depth + 1
			pkgs <- c(deps, pkgs)
			p1 <- deps
			if(!is.null(dep2)) { dependencies <- dep2; dep2 <- NULL }
		}

		pkgs <- unique(pkgs)
		pkgs <- pkgs[pkgs %in% row.names(available)]
		p0 <- pkgs
	}
	list(deps = setdiff(p0, p00), depth = depth)
}


## -----------------------------------------------------------------------------
summarize <- function(db, dependencies) {
	ret <- parallel::mclapply(
		db[,'Package'], getDependencies,
		available = db, dependencies = dependencies,
		mc.cores = max(1, parallel::detectCores()-1)
	)
	# this relation inversion with nested pure-R loops is slow,
	# but the getDependencies loop is much slower anyway
	for (pkg in names(ret))
		for (dep in ret[[pkg]]$deps)
			ret[[dep]]$revdeps <- union(ret[[dep]]$revdeps, pkg)
	# turn list(package = list(deps, depth, revdeps), ...) into
	# list(deps=list(pkg=..., ...), depth=list(pkg=..., ...), revdeps=list(pkg=..., ...))
	lapply(
		setNames(nm = c('deps', 'revdeps', 'depth')),
		function(n) lapply(ret, `[[`, n)
	)
}

required <- cache('required1.rds', summarize(pkgs, NA))
optional <- cache('optional1.rds', summarize(pkgs, TRUE))

saveplot <- function(name, expr, width = 7, height = 3.5, ...) {
	svg(
		file.path('2020', '07', '10', name),
		width = width, height = height, ...
	)
	par(cex = .8)
	on.exit(dev.off())
	force(expr)
}

## -----------------------------------------------------------------------------
depdepths <- list(Required = unlist(required$depth), Optional = unlist(optional$depth))
depdepths.sum <- do.call(rbind, lapply(depdepths, summary))
#knitr::kable(depdepths.sum)


## ---- fig.asp = 1-------------------------------------------------------------
saveplot('depdepths.svg', {
	par(mfrow = c(1,2))
	hist(
		unlist(required$depth), xlab = 'Dependency tree depth',
		main = 'Required dependencies only', breaks = -1:max(depdepths.sum) + .5
	)
	hist(
		unlist(optional$depth), xlab = 'Dependency tree depth',
		main = 'Including optional dependencies', breaks = -1:max(depdepths.sum) + .5
	)
})


## ---- fig.asp = .5------------------------------------------------------------
required.nrevdeps <- sapply(required$revdeps, length)
optional.nrevdeps <- sapply(optional$revdeps, length)
saveplot('revdepdepths.svg', {
	par(mfrow = c(1,2))
	local({
		x <- required.nrevdeps
		y <- jitter(unlist(required$depth))
		plot(
			x, y,
			xlab = 'Number of reverse dependencies', ylab = 'Forward dependency tree depth',
			main = 'Required dependencies only'
		)
		pkg <- c('ggplot2', 'testthat', 'tibble', 'dplyr', 'tidyr', 'viridis', 'isoband')
		text(x[pkg], y[pkg], pkg, pos = 4, off = .25, cex = .75, col = 'blue')
	})
	plot(
		optional.nrevdeps, jitter(unlist(optional$depth)),
		xlab = 'Number of reverse dependencies', ylab = 'Forward dependency tree depth',
		main = 'Including optional dependencies'
	)
})


## ---- fig.asp = .5------------------------------------------------------------
direct.deps <- function(pkg, dependencies)
	setdiff(
		tools::package_dependencies(pkg, pkgs, dependencies)[[1]],
		c(base, recommended)
	)

direct.required <- cache('direct_required.rds', lapply(
	rownames(pkgs), direct.deps, dependencies = c("Depends", "Imports", "LinkingTo")
))
direct.optional <- cache('direct_optional.rds', lapply(
	rownames(pkgs), direct.deps, dependencies = c("Depends", "Imports", "LinkingTo", "Suggests")
))
direct.required.n <- sapply(direct.required, length)
direct.optional.n <- sapply(direct.optional, length)

saveplot('direct.svg', {
	par(mfrow = c(1,2))
	hist(
		direct.required.n, xlab = 'Number of direct dependencies',
		main = 'Required dependencies only'
	)
	hist(
		direct.optional.n, xlab = 'Number of direct dependencies',
		main = 'Including optional dependencies'
	)
})
#knitr::kable(rbind(
#	Required = summary(direct.required.n),
#	Optional = summary(direct.optional.n)
#))


## ---- fig.asp = .5------------------------------------------------------------
required.ndeps <- sapply(required$deps, length)
optional.ndeps <- sapply(optional$deps, length)
saveplot('totaldeps.svg', {
	par(mfrow = c(1,2))
	hist(
		required.ndeps, xlab = 'Total number of dependencies',
		main = 'Required dependencies only'
	)
	hist(
		optional.ndeps, xlab = 'Total number of dependencies',
		main = 'Including optional dependencies'
	)
})
#knitr::kable(rbind(
#	Required = summary(required.ndeps),
#	Optional = summary(optional.ndeps)
#))


## ---- fig.asp = .5------------------------------------------------------------
saveplot('totalrev.svg', {
	par(mfrow = c(1, 2))
	plot(
		required.nrevdeps, required.ndeps,
		xlab = 'Number of reverse dependencies', ylab = 'Total forward dependencies',
		main = 'Required dependencies only'
	)
	plot(
		optional.nrevdeps, optional.ndeps,
		xlab = 'Number of reverse dependencies', ylab = 'Total forward dependencies',
		main = 'Including optional dependencies'
	)
})


## ---- fig.asp = .5------------------------------------------------------------
saveplot('depthndeps.svg', {
	par(mfrow = c(1, 2))
	plot(
		jitter(unlist(required$depth)), required.ndeps,
		xlab = 'Dependency tree depth', ylab = 'Total forward dependencies',
		main = 'Required dependencies only'
	)
	plot(
		jitter(unlist(optional$depth)), optional.ndeps,
		xlab = 'Dependency tree depth', ylab = 'Total forward dependencies',
		main = 'Including optional dependencies'
	)
})

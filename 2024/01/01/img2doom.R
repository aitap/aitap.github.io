# SPDX-License-Identifier: GPL-3.0-or-later

# Split a heightmap on a rectangular grid into triangular sectors
# suitable for setting UDMF vertex heights.
#
# x: coordinates corresponding to rows of z
# y: coordinates corresponding to columns of z
# z: vertex heights
# levels: the thresholds that decide the sector number in
#         [0, length(levels)].
triangulate <- function(x, y, z, levels) {
	stopifnot(
		!is.unsorted(x, strictly = TRUE),
		!is.unsorted(y, strictly = TRUE),
		length(x) == nrow(z), length(y) == ncol(z),
		length(levels) >= 1,
		!is.unsorted(levels, strictly = TRUE),
		min(x, y, z) >= -32768,
		max(x, y, z) <= +32767
	)
	vertices <- data.frame(
		x = x[row(z)],
		y = y[col(z)],
		z = as.vector(z)
	)
	# map from [i,j] indices to vertex numbers
	vidx <- z
	vidx[] <- seq_len(length(z))

	# make note of the floor heights
	floors <- array(
		c(
			z[-1,-1] + z[-nrow(z),-ncol(z)] + z[-1,-ncol(z)],
			z[-1,-1] + z[-nrow(z),-ncol(z)] + z[-nrow(z),-1]
		) / 3,
		c(dim(z)-1, 2)
	)
	# NOTE: our coordinate system is counter-intuitive. The *rows* of
	# the matrix (that we call `x` here) will be used as the horizontal
	# coordinate.
	# On a grid of N*M points, there are (N-1)*(M-1) rectangles. Every
	# rectangle is divided into two triangles using N*(M-1) horizontal
	# lines, (N-1)*M vertical lines and (N-1)*(M-1) diagonal lines,
	# which are shared between triangles.
	#
	# (x,y+1) --------------> (x+1,y+1)
	#     ^                  ____/^
	#     |  T(x,y,2)  _____/     |
	#     |      _____>           |
	#     | ____/       T(x,y,1)  |
	#     |/                      |
	# (x, y ) --------------> (x+1,y)
	sectors <- array(findInterval(floors, levels), dim(floors))
	# Make individual triangular sectors distinct.
	# We tried to compress the map into length(levels)+1 unique sectors,
	# but those were't triangular, which may have been breaking the
	# vertex heights.
	sidx <- sectors
	sidx[] <- seq_len(length(sidx))

	# Construct the linedefs from horizontals, verticals, and diagonals.
	linedefs <- rbind(
		# Horizontals: (x,y) to (x+1,y).
		# Front at T(x,y-1,2), back at T(x,y,1).
		data.frame(
			start = as.vector(vidx[-nrow(z),]),
			end   = as.vector(vidx[-1,]),
			# Lowest horizontals don't have a front.
			front = as.vector(cbind(NA, sidx[,,2])),
			# Topmost horizontals don't have a back
			back  = as.vector(cbind(sidx[,,1], NA))
		),
		# Verticals: (x,y) to (x,y+1).
		# Front at T(x,y,2), back at T(x-1,y,1).
		data.frame(
			start = as.vector(vidx[,-ncol(z)]),
			end   = as.vector(vidx[,-1]),
			# Rightmost verticals don't have a front
			front = as.vector(rbind(sidx[,,2], NA)),
			# Leftmost verticals don't have a back
			back  = as.vector(rbind(NA, sidx[,,1]))
		),
		# Diagonals (x,y) to (x+1,y+1).
		# Front at T(x,y,1), back at T(x,y,2).
		data.frame(
			start = as.vector(vidx[-nrow(z),-ncol(z)]),
			end   = as.vector(vidx[-1,-1]),
			front = as.vector(sidx[,,1]),
			back  = as.vector(sidx[,,2])
		)
	)
	# Something's wrong with linedefs without a front sidedef? The
	# editor definitely doesn't like them.
	linedefs <- within(linedefs, {
		flip <- is.na(front) & !is.na(back)

		tmp <- start[flip]
		start[flip] <- end[flip]
		end[flip] <- tmp

		tmp <- front[flip]
		front[flip] <- back[flip]
		back[flip] <- tmp

		rm(tmp, flip)
	})

	list(
		vertices = vertices,
		linedefs = linedefs,
		sectors = sectors,
		floors = floors
	)
}

asTEXTMAP <- function(vertices, linedefs, sectors, floors, textures) c(
	'namespace = "zdoom";',
	paste0(
		'vertex\n',
		'{\n',
		'x = ',round(vertices$x),';\n',
		'y = ',round(vertices$y),';\n',
		'zceiling = ',round(max(vertices$z)+128),';\n',
		'zfloor = ',round(vertices$z),';\n',
		'}\n'
	),
	paste0(
		'linedef\n',
		'{\n',
		'v1 = ',linedefs$start-1,';\n',
		'v2 = ',linedefs$end-1,';\n',
		ifelse(
			is.na(linedefs$front), '',
			paste0('sidefront = ',linedefs$front-1,';\n')
		),
		ifelse(
			is.na(linedefs$back), '',
			paste0('sideback = ',linedefs$back-1,';\n')
		),
		ifelse(
			is.na(linedefs$front) | is.na(linedefs$back),
			'blocking = true;\nspecial = 9;\n', 'twosided = true;\n'
		),
		'}\n'
	),
	paste0(
		'sidedef\n',
		'{\n',
		'sector = ',seq_len(length(sectors))-1,';\n',
		'}\n'
	),
	paste0(
		'sector\n',
		'{\n',
		'texturefloor = "',as.vector(textures[sectors+1]),'";\n',
		'textureceiling = "F_SKY1";\n',
		'heightceiling = ',round(max(vertices$z)+128),';\n',
		'heightfloor = ',round(as.vector(floors)),';\n',
		'}\n'
	)
)

img2doom <- function(x, y, z, levels, textures, file) {
	stopifnot(length(levels)+1 == length(textures))
	tri <- triangulate(x, y, z, levels)
	writeLines(asTEXTMAP(
		tri$vertices, tri$linedefs, tri$sectors, tri$floors, textures
	), file)
}

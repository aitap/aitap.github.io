// [[Rcpp::plugins(openmp)]]
#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

struct WeightFunc {
	double dif_pow, dst_pow, dst_mul;

	enum type : int {
		poly,
		poly_exp
	} type;

	WeightFunc(int type_, double dif_pow_, double dst_pow_, double dst_mul_)
		: type((enum type)type_), dif_pow(dif_pow_), dst_pow(dst_pow_), dst_mul(dst_mul_) {
		if (type < poly || type > poly_exp) throw std::runtime_error("invalid weight type");
	}
	double operator()(double dx, double dy) const {
		double dist = pow(pow(abs(dx), dif_pow) + pow(abs(dy), dif_pow), dst_pow);
		return type == poly ? dist : /* type == poly_exp ? */ exp(dst_mul * dist);
	}
};

/*** R

idw_interp <- function(
	z, x=as.numeric(rownames(z)), y=as.numeric(colnames(z)),
	type=0, dif_pow=2, dst_pow=-2, dst_mul=-1
) {
	idw_interp_real(x, y, z, list(type=type, dif_pow=dif_pow, dst_pow=dst_pow, dst_mul=dst_mul))
}

*/

// [[Rcpp::export]]
NumericMatrix idw_interp_real(const NumericVector x, const NumericVector y, const NumericMatrix z, List args) {
	if (z.nrow() != x.size())
		throw std::runtime_error("x should correspond to rows of z");
	if (z.ncol() != y.size())
		throw std::runtime_error("y should correspond to columns of z");

	NumericMatrix ret(clone(z));

	const WeightFunc weight(args["type"], args["dif_pow"], args["dst_pow"], args["dst_mul"]);

#pragma omp parallel for shared(ret) default(none) collapse(2) schedule(dynamic)
	for (int i = 0; i < z.nrow(); i++)
		for (int j = 0; j < z.ncol(); j++) {
			if (std::isnan(z(i,j))) {
				ret(i,j) = 0;
				double total_w = 0;

				for (int ii = 0; ii < z.nrow(); ii++)
					for (int jj = 0; jj < z.ncol(); jj++) {
						if (std::isnan(z(ii,jj))) continue;
						double w = weight(x[i] - x[ii], y[j] - y[jj]);
						total_w += w;
						ret(i,j) += w * z(ii,jj);
					}

				ret(i,j) /= total_w;
			} else {
				ret(i,j) = z(i,j);
			}
		}

	return ret;
}

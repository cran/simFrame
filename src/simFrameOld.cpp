///*
// * Author: Andreas Alfons
// *         Vienna University of Technology
// *
// *         based on R code by Yves Tille and Alina Matei
// */
//
//#include <R.h>
//#include "simFrame.h"
//
//
//// compute first order inclusion probabilities
//
//SEXP inclusionProb(SEXP prob, SEXP size) {
////	prob ... vector of initial probability weights
////	size ... sample size
//    using namespace Rcpp;
//
//    // initializations
//	NumericVector xprob(prob);	// probability weights
//	IntegerVector xsize(size);	// sample size
//
//    // check initial result
//    int i;						// index for loops
//    int nneg = 0;				// number of negative values
//    int N = xprob.size();		// number of observations
//    double sum = 0.0;			// sum of probability weights
//    NumericVector result(N);	// inclusion probabilities
//    for(i = 0; i < N; i++) {
//        if(xprob[i] < 0.0) {
//            result[i] = 0.0;	// set negative values to 0
//            nneg++;  			// count negative values
//        } else {
//        	result[i] = xprob[i];
//        	if(xprob[i] > 0.0) {
//        		sum += xprob[i];  // add current element to sum
//        	}
//        }
//    }
//    if(nneg > 0) warning("negative probability weights are set to 0");
//
//    // compute inclusion probabilities
//    if(sum > 0.0) {
//        int ngeq1 = 0;	// number of values >= 1
//        for(i = 0; i < N; i++) {
//            if(result[i] > 0.0) {
//                result[i] = result[i] * xsize[0] / sum;	// initial adjustment
//                if(result[i] >= 1.0) ngeq1++;  // count values >= 1
//            }
//        }
//        // values >= 1 are set to 1 and the others are readjusted
//        if(ngeq1 > 0) {
//            int nset = 0;	// ???
//        	// I think this results in an infinite loop
//            while(ngeq1 != nset) {
//                // compute sum of values less than 1
//                sum = 0.0;	// reset sum
//                for(i = 0; i < N; i++) {
//                    if(result[i] > 0.0 && result[i] < 1.0) {
//                        sum += result[i];  // add current element to sum
//                    }
//                }
//                // readjust values
//                if(sum > 0.0) {
//                    for(i = 0; i < N; i++) {
//                        if(result[i] > 0.0 && result[i] < 1.0) {
//                            result[i] = result[i] * (xsize[0]-ngeq1) / sum;
//                        }
//                    }
//                }
//                nset = ngeq1;
//                // recount values >= 1 and set them to 1
//                ngeq1 = 0;
//                for(i = 0; i < N; i++) {
//                    if(result[i] >= 1.0) {
//                        result[i] = 1.0;
//                        ngeq1++;  // count values >= 1
//                    }
//                }
//            }
//        }
//    }
//
//    return result;
//}
//
//
//// compute sample size from inclusion probabilities
//
//int sampleSize(Rcpp::NumericVector prob) {
////	prob ... vector of inclusion probabilities
//	int N = prob.size();	// number of elements
//    double n = 0.0;			// sample size to be computed
//    // compute and return sample size
//    for(int i = 0; i < N; i++) {
//    	n += prob[i];		// sum up inclusion probabilities
//    }
//	return int(n + 0.5);	// round to return integer
//}
//
//
//// draw a random number from uniform distribution
//// and find first element with larger value
//
//int findFirst(Rcpp::NumericVector p) {
////	p ... cumulative probabilities
//	using namespace Rcpp;
//	using namespace stats;
//	NumericVector u = runif(1);		// random number from uniform distribution
//	int i;							// index to be computed
//	int N = p.size();				// number of observations
//	// find index of first element with larger cumulative probability
//	// this is ugly, but it works
//	for(i = 0; i < N; i++) {
//		if(u[0] < p[i]) {
//			break;
//		}
//	}
//	return i;
//}
//
//
//// Tille sampling
//
//SEXP tille(SEXP prob) {
////	prob ... vector of inclusion probabilities
//	using namespace Rcpp;
//	using namespace stats;
//	RNGScope scope;		// initializes the state of the R RNG correctly
//
//	// initializations
//	NumericVector xprob(prob);	// inclusion probabilities
//	int N = xprob.size();		// number of observations in population
//	int n = sampleSize(xprob);	// number of observations to be sampled
//
//	// initialize result
//	int i;						// index for loops
//	IntegerVector result(N);	// vector indicating sampled observations
//	NumericVector b(N);			// temporary variable
//	for(i = 0; i < N; i++) {
//		result[i] = 1;
//		b[i] = 1.0;
//	}
//
//	// computations
//	int numIterations = N - n;	// number of iterations of outer loop
//	int j;						// index for inner loops
//	IntegerVector ni(1);		// reverse sample size in each iteration
////	NumericVector a(N);			// temporary variable
////	NumericVector v(N);			// temporary variable
//	double v;					// temporary variable
//	NumericVector p(N);			// cumulative probabilities
//	for(i = 0; i < numIterations; i++) {
//		ni[0] = N - i - 1;	// reverse sample size for inclusion probabilities
//		// compute inclusion probabilities for current iteration
//		NumericVector a(inclusionProb(prob, ni));
//		// compute probabilities according to R implementation
////		if(i == 1) break;
//		for(j = 0; j < N; j++) {
//			v = 1.0 - a[j]/b[j];
//			b[j] = a[j];
//			p[j] = v * result[j];
//		}
//		// compute cumulative sum of probabilities
//		for(j = 1; j < N; j++) {
//			p[j] += p[j-1];
//		}
//		// draw a random number from uniform distribution and find first
//		// element with larger cumulative probability
//		result[findFirst(p)] = 0;
//	}
//
//	return result;
//}
//
//
//// Brewer sampling
//
//SEXP brewer(SEXP prob) {
////	prob ... vector of inclusion probabilities
//	using namespace Rcpp;
//	using namespace stats;
//	RNGScope scope;		// initializes the state of the R RNG correctly
//
//	// initializations
//	NumericVector xprob(prob);	// inclusion probabilities
//	int N = xprob.size();		// number of observations in population
//	int n = sampleSize(xprob);	// number of observations to be sampled
//
//	// initialize result
//	int i;						// index for loops
//	IntegerVector result(N);	// vector indicating sampled observations
//	for(i = 0; i < N; i++) {
//		result[i] = 0;
//	}
//
//	// computations
//	int j;					// index for inner loops
//	NumericVector p(N);		// cumulative probabilities
//	for(i = 0; i < n; i++) {
//		// initial computations according to R implementation
//		double a = 0.0;		// temporary variable
//		for(j = 0; j < N; j++) {
//			a += xprob[j] * result[j];
//		}
//		for(j = 0; j < N; j++) {
//			p[j] = (1.0 - result[j]) * xprob[j] * ((n-a) - xprob[j]) /
//					((n-a) - xprob[j] * (n-i));
//		}
//		// compute cumulative sum
//		for(j = 1; j < N; j++) {
//			p[j] += p[j-1];
//		}
//		// compute cumulative probabilities
//		for(j = 0; j < N; j++) {
//			p[j] /= p[N-1];
//		}
//		// draw a random number from uniform distribution and find first
//		// element with larger cumulative probability
//		result[findFirst(p)] = 1;
//    }
//
//	return result;
//}

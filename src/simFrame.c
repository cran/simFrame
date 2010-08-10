/*
 * Authors: Andreas Alfons
 *          Vienna University of Technology
 *
 *          Pablo Burgard
 *          University of Trier
 *
 *          based on R code by Yves Tille and Alina Matei
 */

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Random.h>


// compute first order inclusion probabilities

SEXP inclusionProb(SEXP prob, SEXP size) {
    SEXP result;
    double *xresult, sum;
    int xsize, i, n, ngeq1, nneg, nset;

    // initializations
    PROTECT(result = coerceVector(duplicate(prob), REALSXP));
    PROTECT(size = coerceVector(size, INTSXP));
    xresult = REAL(result);
    xsize = INTEGER(size)[0];

    // check initial result
    n = length(prob);
    nneg = 0; sum = 0;
    for(i = 0; i < n; i++) {
        if(xresult[i] < 0) {
            xresult[i] = 0;  // set negative values to 0
            nneg++;  // count negative values
        } else if(xresult[i] > 0) {
            sum += xresult[i];  // add current element to sum
        }
    }
    if(nneg > 0) warning("negative probability weights are set to 0");

    // compute inclusion probabilities
    if(sum > 0) {
        ngeq1 = 0;
        for(i = 0; i < n; i++) {
            if(xresult[i] > 0) {
                xresult[i] = xresult[i] * xsize / sum;
                if(xresult[i] >= 1) ngeq1++;  // count values >= 1
            }
        }
        // values >= 1 are set to one and the others are adjusted
        if(ngeq1 > 0) {
            nset = 0;
            while(ngeq1 != nset) {
                // get sum of values less than 1
                sum = 0;
                for(i = 0; i < n; i++) {
                    if(xresult[i] > 0 && xresult[i] < 1) {
                        sum += xresult[i];  // add current element to sum
                    }
                }
                // adjust values
                if(sum > 0) {
                    for(i = 0; i < n; i++) {
                        if(xresult[i] > 0) {
                            xresult[i] = xresult[i] * (xsize-ngeq1) / sum;
                        }
                    }
                }
                nset = ngeq1;
                ngeq1 = 0;
                for(i = 0; i < n; i++) {
                    if(xresult[i] >= 1) {
                        xresult[i] = 1;
                        ngeq1++;  // count values >= 1
                    }
                }
            }
        }
    }
    // unprotect R objects
    UNPROTECT(2);

    return result;
}


// Tille sampling

/*
SEXP tille (SEXP prob, SEXP size) {
	SEXP result, ni, a;
	double *xresult, *xa;                              // pointers
	int N = LENGTH(prob), xsize, i, j, numIterations;  // integers
	double u;                                          // real numbers
	double b[N], v[N], p[N];                           // real number arrays

	// initializations
	PROTECT(result = allocVector(REALSXP, N));
	PROTECT(size = coerceVector(size, INTSXP));
	xresult = REAL(result);
	xsize = INTEGER(size)[0];
	for(i = 0; i < N; i++) {
		xresult[i] = 1;
		b[i] = 1;
	}

	GetRNGstate();  // get state of the random number generator

	// computations
	numIterations = N - xsize;
	for(i = 0; i < numIterations; i++) {
		// R objects need to be PROTECTed every time they are changed
		PROTECT(ni = allocVector(INTSXP, 1));
		// set sample size for inclusion probabilities in current iteration
		INTEGER(ni)[0] = N - i - 1;  // this is done with a pointer
		// compute inclusion probabilities for current iteration
		PROTECT(a = coerceVector(inclusionProb(prob, ni), REALSXP));
		xa = REAL(a);
		// other computations according to R implementation
		for(j = 0; j < N; j++) {
			v[j] = 1 - xa[j]/b[j];
			b[j] = xa[j];
		}
		// R objects defined in this iteration no longer need to be PROTECTed
		UNPROTECT(2);
		// compute cumulative sum of probabilities
		p[0] = v[0] * xresult[0];
		for(j = 1; j < N; j++) {
			p[j] = p[j-1] + v[j] * xresult[j];
		}
		// draw a random number from uniform distribution and find first
		// element with larger cumulative probability
		u = runif(0, 1);
		for(j = 0; j < N; j++) {
			if(u < p[j]) {
				xresult[j] = 0;
				break;  // this is ugly, but it works
			}
		}
	}

	PutRNGstate();  // set state of the random number generator
	UNPROTECT(2);   // unprotect remaining R objects

	return result;
}
*/

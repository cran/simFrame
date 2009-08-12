/*
 * Author: Andreas Alfons
 *         Vienna University of Technology
 */

#include <R.h>
#include <Rinternals.h>

SEXP getProb(SEXP prob, SEXP size) {
    SEXP result;
    double *xresult, sum;
    int xsize, i, n, ngeq1, nneg, nset;

    /* initializations */
    PROTECT(result = coerceVector(duplicate(prob), REALSXP));
    PROTECT(size = coerceVector(size, INTSXP));
    xresult = REAL(result);
    xsize = INTEGER(size)[0];

    /* check initial result */
    n = length(prob);
    nneg = 0; sum = 0;
    for(i = 0; i < n; i++) {
        if(xresult[i] < 0) {
            xresult[i] = 0;  /* set negative values to 0 */
            nneg++;  /* count negative values */
        } else if(xresult[i] > 0) {
            sum += xresult[i];  /* add current element to sum */
        }
    }
    if(nneg > 0) warning("negative probability weights are set to 0");

    /* compute inclusion probabilities */
    if(sum > 0) {
        ngeq1 = 0;
        for(i = 0; i < n; i++) {
            if(xresult[i] > 0) {
                xresult[i] = xresult[i] * xsize / sum;
                if(xresult[i] >= 1) ngeq1++;  /* count values >= 1 */
            }
        }
        /* values >= 1 are set to one and the others are adjusted */
        if(ngeq1 > 0) {
            nset = 0;
            while(ngeq1 != nset) {
                /* get sum of values less than 1 */
                sum = 0;
                for(i = 0; i < n; i++) {
                    if(xresult[i] > 0 && xresult[i] < 1) {
                        sum += xresult[i];  /* add current element to sum */
                    }
                }
                /* adjust values */
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
                        ngeq1++;  /* count values >= 1 */
                    }
                }
            }
        }
    }
    /* unprotect R objects */
    UNPROTECT(2);

    return result;
}

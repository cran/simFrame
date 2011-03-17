/*
 * Author: Andreas Alfons
 *         Vienna University of Technology
 */

#ifndef _simFrame_H
#define _simFrame_H

#include <Rcpp.h>

RcppExport SEXP inclusionProb(SEXP prob, SEXP size);
RcppExport SEXP tille(SEXP prob);
RcppExport SEXP brewer(SEXP prob);

#endif

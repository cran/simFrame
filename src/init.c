/*
 * Author: Andreas Alfons
 *         Vienna University of Technology
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "simFrame.h"

static const R_CallMethodDef callMethods[] = {
        {"inclusionProb", &inclusionProb, 2},
        {NULL, NULL, 0}
};

void R_init_simFrame(DllInfo *info) {
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}

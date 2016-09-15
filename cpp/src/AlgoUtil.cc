/**
 * AlgoUtil.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "AlgoUtil.h"
#include "Options.h"

int AlgoUtil::computeSMaxTree(int k, int p) {
    if (k >= 2*p + 1) return 4*k + 2*p*p - 6*p - 4;
    else              return (p + 2)*k - (3*p + 2);    
}



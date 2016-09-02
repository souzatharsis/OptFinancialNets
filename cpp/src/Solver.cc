/**
 * Solver.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "Solver.h"
#include "Util.h"

/**
 * INITIAL METHODS
 *
 */

Solver::Solver() {
    status = 0;
}

Solver::~Solver() {
}

int Solver::getColIndex(string name) {
    if (colIndices.find(name) == colIndices.end()) {
        return -1;
    } else {
        return colIndices[name];
    }
}

void Solver::addKey(string name, int index) {
    colIndices[name] = index;
}

double Solver::getColValue(string name) {
    if ((int)colSolution.size() == 0) {
        getColSolution();
 
    }

    int ind = getColIndex(name);
   
    if (ind == -1) Util::throwInvalidArgument("Error: Could not find variable %s in solver.", name.c_str());
    return colSolution[ind];
}


void Solver::solve() {
    colSolution.clear();
    doSolve();
}

void Solver::printSolverName() {
    printf("Generic solver assigned\n");
}


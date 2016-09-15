/**
 * Solution.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "Solution.h"
#include "Options.h"

Solution::Solution() {
    
    resetSolution();
    debug = Options::getInstance()->getIntOption("debug");
    
}

Solution::~Solution() {
}

void Solution::resetSolution() {
    for (int i = 0; i < (int)edges.size(); i++) edges[i].clear();
    edges.clear();

    solutionExists = false;
    isOptimal      = false;
    isInfeasible   = false;
    isUnbounded    = false;

    value          = 0;
    bestBound      = 0;

}

void Solution::setSolutionStatus(bool exists, bool optimal, bool infeasible, bool unbounded) {
    solutionExists = exists;
    isOptimal      = optimal;
    isInfeasible   = infeasible;
    isUnbounded    = unbounded;
}


void Solution::setNumAssets(int N) {
    edges.clear();
    edges.resize(N);
}

void Solution::addEdge(int i, int j) {
    if (i < 0 || i >= (int)edges.size()) Util::throwInvalidArgument("Error: Out of range parameter i in addEdges");
    if (j < 0 || j >= (int)edges.size()) Util::throwInvalidArgument("Error: Out of range parameter j in addEdges");
    edges[i].push_back(j);
}

bool Solution::checkFeasibility(const Data &data) {

    if (edges.size() == 0) return false;

    // TODO
    // Ver se tem aresta repetida
    // Ver se tem ciclo
    // Ver se eh MST

    return true;
}
        

void Solution::print(int overrideDebug) {
        
    int ddebug = overrideDebug == -1 ? debug : overrideDebug;
    if (!debug) return;

    printf("\n------\nSolution:\n\n");
    if (!solutionExists) {
         if (isInfeasible)     printf("Model is infeasible\n");
         else if (isUnbounded) printf("Model is unbounded\n");
         else                  printf("Solution does not exist\n");
    } else {
        
        printf("Solution %s (gap: %.2f%%)\n", isOptimal ? "is optimal" : "is not optimal", getGap()*100);
        printf("Obj   = %.5f\n", value);
        if (ddebug > 1) printf("Bound = %.5f\n", bestBound);
    
        //printf("\nPicked by:");
        //for (int o = 0; o < pickedBy.size(); o++) printf(" %2d:%2d", o, pickedBy[o]);
        if (ddebug > 0) {
            printf("\n");
            for (int i = 0; i < (int)edges.size(); i++) {
                printf("%3d:", i);
                for (int j = 0; j < (int)edges[i].size(); j++) {
                    printf(" %3d", edges[i][j]);
                }
            }
        }

       
    }
    printf("-----\n");
 
}





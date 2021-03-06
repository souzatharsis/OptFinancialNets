/**
 * AssortMST.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "AssortMST.h"
#include "Data.h"
#include "ModelAssortMST.h"
#include "Options.h"
#include "AlgoUtil.h"



AssortMST::AssortMST() {
    totalTime = 0;
}

AssortMST::~AssortMST() {
}

void AssortMST::execute() {
    float startTime = Util::getTime();

    Data data;
    data.readData();
    data.print();
   
    // Aqui seria executado o for pra resolver o numero
    // quadratico de problemas

    ModelAssortMST model;

    int K = Options::getInstance()->getIntOption("min_tree_size");

    for (int k = K; k < data.getNumAssets();k++) {
        for (int p = 2; p < k; p++) {
            int smax = AlgoUtil::computeSMaxTree(k, p);
            if (smax > 1000) printf("k = %2d, p = %2d, smax = %2d\n", k, p, smax);
        }
    }
    
    model.execute(data);
    //model.printSolution();
 
    totalTime = Util::getTime() - startTime;
    
    /*
    if (Options::getInstance()->getIntOption("debug")) {
        printf("\n");
        printf("Solution value:           %8.2f\n",   model.getSolution().getValue());
        printf("Final lower bound:        %8.2f\n",   model.getSolution().getBestBound());
        printf("Gap:                      %8.2f%%\n", model.getSolution().getGap()*100);
        printf("Bound at first node:      %8.2f\n\n", model.getFirstNodeBound());
        printf("Number of nodes solved:   %8d\n",     model.getTotalNodes());
        if (model.getCallbackCalls() > 0) {
            printf("Callback time              %7.3fs (%d calls)\n", model.getCallbackTime(), model.getCallbackCalls());
            printf("   Adding cuts             %7.3fs (%d cuts added)\n", model.getCallbackCutsTime(), model.getCutsAdded());
            printf("   Data processing         %7.3fs\n", model.getCallbackDataTime());
            printf("   BFS                     %7.3fs\n", model.getBfsTime());
        }
        printf("Solver time:               %7.3fs\n",   model.getSolvingTime());
        printf("   First node solved in    %7.3fs\n",   model.getFirstNodeTime());
        printf("   Best solution time      %7.3fs\n",   model.getBestSolutionTime());
        printf("BC time:                   %7.3fs\n",   model.getTotalTime());
        printf("\nEverything finalised in:   %7.3fs\n", totalTime);
    }
    */
    
}

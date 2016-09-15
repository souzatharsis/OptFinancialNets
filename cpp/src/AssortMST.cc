/**
 * AssortMST.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "AssortMST.h"
#include "Options.h"


AssortMST::AssortMST() {
    totalTime = 0;
    
    //model = new ModelAssortMST();
    model = new Model();

}

AssortMST::~AssortMST() {
    delete(model);
}

void AssortMST::execute() {
    float startTime = Util::getTime();

    data.readData();
    data.print();
   
    // Aqui seria executado o for pra resolver o numero
    // quadratico de problemas
    
    int K = Options::getInstance()->getIntOption("min_tree_size");

    for (int k = K; k < data.getNumAssets();k++) {
        for (int p = 2; p < k; p++) {

        }
    }
    







    //model->execute(data);
    //model->printSolution();
 
    totalTime = Util::getTime() - startTime;
    
    if (Options::getInstance()->getIntOption("debug")) {
        printf("\n");
        /*
        printf("Solution value:           %8.2f\n",   model->getSolution().getValue());
        printf("Final lower bound:        %8.2f\n",   model->getSolution().getBestBound());
        printf("Gap:                      %8.2f%%\n", model->getSolution().getGap()*100);
        printf("Bound at first node:      %8.2f\n\n", model->getFirstNodeBound());
        printf("Number of nodes solved:   %8d\n",     model->getTotalNodes());
        printf("Number of trolleys:       %8d\n",     model->getNumTrolleys());
        printf("Actual trolleys used:     %8d\n\n",   model->getSolution().getNumTrolleysUsed());
        if (model->getCallbackCalls() > 0) {
            printf("Callback time              %7.3fs (%d calls)\n", model->getCallbackTime(), model->getCallbackCalls());
            printf("   Adding cuts             %7.3fs (%d cuts added)\n", model->getCallbackCutsTime(), model->getCutsAdded());
            printf("   Data processing         %7.3fs\n", model->getCallbackDataTime());
            printf("   Max flow                %7.3fs (%d calls, %d solved)\n", model->getMaxFlowTime(), model->getMaxFlowCalls(), model->getMaxFlowsSolved());
            printf("   BFS                     %7.3fs\n", model->getBfsTime());
        }
        printf("Solver time:               %7.3fs\n",   model->getSolvingTime());
        printf("   First node solved in    %7.3fs\n",   model->getFirstNodeTime());
        printf("   Best solution time      %7.3fs\n",   model->getBestSolutionTime());
        printf("BC time:                   %7.3fs\n",   model->getTotalTime());
        */
        printf("\nEverything finalised in:   %7.3fs\n", totalTime);
    }
    
}

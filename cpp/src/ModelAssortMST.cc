/**
 * ModelAssortMST.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "ModelAssortMST.h"
#include "Options.h"

ModelAssortMST::ModelAssortMST() : Model(){
    x = "x";
}

ModelAssortMST::~ModelAssortMST() {
}


void ModelAssortMST::execute(const Data &data) {

    float startTime = Util::getTime();
    prepareExecution(data);
    //solver->addLazyCallback(this);
    //if (!Options::getInstance()->getBoolOption("integral_callbacks")) solver->addUserCutCallback(this);
    //solve();
    totalTime = Util::getTime() - startTime;
    //printSolutionVariables();
}  



void ModelAssortMST::prepareExecution(const Data &data) {


    if (debug > 1) solver->printSolverName();
    
    /*
    createModel(data);
    reserveSolutionSpace();
    assignWarmStart();
    setSolverParameters();    
 
    solver->addIncumbentCallback(this);
    solver->addNodeCallback(this);
    */
}


void ModelAssortMST::solve() {

    solverStartTime = Util::getTime();
    solver->solve();
    solvingTime = Util::getTime() - solverStartTime;

    if (debug > 1) printf("\n---------\n");
    if (debug > 1) printf("Model solved in %.2fs, status = %d\n", solvingTime, solver->getStatus());

    readSolution();

}  




void ModelAssortMST::reserveSolutionSpace() {

    sol_x.resize(N);
    for (int i = 0; i < N-1; i++) {
        sol_x[i].resize(N - i - 1);
        std::fill(sol_x[i].begin(), sol_x[i].end(), 0);
    }

}


void ModelAssortMST::printSolutionVariables(int digits, int decimals) {
    printXSolutionVariables(digits, decimals);
}

void ModelAssortMST::printXSolutionVariables(int digits, int decimals) {
    
    printf("\n");
    Util::printDiagonalDoubleMatrix(sol_x, 4, 2);
}



void ModelAssortMST::readSolution() {
    totalNodes = solver->getNodeCount();
    
    solution.resetSolution();
    solution.setSolutionStatus(solver->solutionExists(), solver->isOptimal(),  solver->isInfeasible(), solver->isUnbounded());
    if (!solver->solutionExists()) {
        if (debug) printf("Solution could not be read as it does not exist\n");     
    } else {
        solution.setValue    (solver->getObjValue() );
        solution.setBestBound(solver->getBestBound());

        for (int i = 0; i < N-1; i++) {
            for (int j = i+1; j < N; j++) {
                sol_x[i][j - i - 1] = solver->getColValue(x + lex(i) + "_" + lex(j));
                if (sol_x[i][j -i - 1] < 0) sol_x[i][j - i - 1] = 0;
            }
        }
    }
}


void ModelAssortMST::assignWarmStart() {
    
    // TODO
    //solution.resetSolution();
}

void ModelAssortMST::createModel(const Data& data) {

    N = data.getNumAssets();
    
    // Add binary x variables
    for (int i = 0; i < N-1; i++)
        for (int j = i+1; j < N; j++) 
            solver->addBinaryVariable(0, x + lex(i) + "_" + lex(j));

    vector<string> colNames;
    vector<double> elements;
    
    /*
    // (2) Capacity constraints Sum b z <= B
    colNames.resize(O);
    elements.resize(O);
    if (includeAlphaVars) {
        colNames.resize(O+1);
        elements.resize(O+1);
    }

    for (int t = 0; t < T; t++) {
        string number = lex(t);
        for (int o = 0; o < O; o++) {
            colNames[o] = z + number + "_" + lex(o);
            elements[o] = graph.getBasketsPerOrder(o);
        }
        if (includeAlphaVars) {
            colNames[O] = a + number;
            elements[O] = -B;
        }
        solver->addRow(colNames, elements, includeAlphaVars ? 0 : B, 'L', "capacity" + number);
    }
    */

}

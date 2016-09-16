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
    y = "y";
    z = "z";
    v = "v";

    N = 0;
    D = 0;
    K = 0;

}

ModelAssortMST::~ModelAssortMST() {
}


void ModelAssortMST::execute(const Data &data) {

    float startTime = Util::getTime();
    prepareExecution(data);

    //solver->addLazyCallback(this);
    //if (!Options::getInstance()->getBoolOption("integral_callbacks")) solver->addUserCutCallback(this);
    solve();
    totalTime = Util::getTime() - startTime;
    printSolutionVariables(4, 1);
}  



void ModelAssortMST::prepareExecution(const Data &data) {


    if (debug > 1) solver->printSolverName();
    
    createModel(data);
    reserveSolutionSpace();
    assignWarmStart();
    setSolverParameters();    
 
    /*
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

    sol_y.resize(N);
    sol_x.resize(N-1);
    for (int i = 0; i < N-1; i++) {
        sol_x[i].resize(N - i - 1);
        std::fill(sol_x[i].begin(), sol_x[i].end(), 0);
    }

}


void ModelAssortMST::printSolutionVariables(int digits, int decimals) {
    printf("\n");
    printf("Solution value: %.2f\n", solution.getValue());
    printf("Solution bound: %.2f\n", solution.getBestBound());
    printYSolutionVariables(digits, decimals);
    printXSolutionVariables(digits, decimals);
}

void ModelAssortMST::printYSolutionVariables(int digits, int decimals) {

    printf("Y: ");
    for (int i = 0; i < N; i++) printf(" %*d", digits, i);
    printf("\n   ");
    for (int i = 0; i < N; i++) printf(" %*.*f", digits, decimals, sol_y[i]);
    printf("\n");
        
}


void ModelAssortMST::printXSolutionVariables(int digits, int decimals) {
    
    printf("X:\n");
    Util::printDiagonalDoubleMatrix(sol_x, digits, decimals);
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
        for (int i = 0; i < N; i++) {
            sol_y[i] = solver->getColValue(y + lex(i));
            if (sol_y[i] < 0) sol_y[i] = 0;
        }
    }
}


void ModelAssortMST::assignWarmStart() {
    
    // TODO
    //solution.resetSolution();
}

void ModelAssortMST::createModel(const Data& data) {

    N = data.getNumAssets();
    D = (int)(N+1)/2;
    K = Options::getInstance()->getIntOption("min_tree_size");
    
    int E = (N*N - N)/2;

    int numVariables = E + N + N*D + D*D*E;

    if (debug) printf("Number of variables: %d\n", numVariables);
    solver->changeObjectiveSense(true);

    // Add binary x variables
    for (int i = 0; i < N-1; i++)
        for (int j = i+1; j < N; j++) 
            solver->addBinaryVariable(0, x + lex(i) + "_" + lex(j));

    // Add y variables
    for (int i = 0; i < N; i++)
        solver->addVariable(0, 1, 0, y + lex(i));

    // Add z variables
    for (int i = 0; i < N; i++)
        for (int d = 1; d <= D; d++)
            solver->addBinaryVariable(0, z + lex(i) + "_" + lex(d));

    // Add v variables
    for (int i = 0; i < N-1; i++)
        for (int j = i+1; j < N; j++) 
            for (int d = 1; d <= D; d++) 
                for (int e = 1; e <= D; e++) 
                    solver->addVariable(0, 1, (double)d*e, v + lex(i) + "_" + lex(d) + "_" + lex(j) + "_" + lex(e));


    vector<string> colNames;
    vector<double> elements;
    

    // (16) Sum y_i >= K
    colNames.resize(N);
    elements.resize(N);
    for (int i = 0; i < N; i++) {
        colNames[i] = y + lex(i);
        elements[i] = 1;
    }
    solver->addRow(colNames, elements, K, 'G', "minTreeSize");


    // (17) Sum x_ij = Sum y_i - 1;
    colNames.resize(E + N);
    elements.resize(E + N);
    int count = 0;
    for (int i = 0; i < N-1; i++) {
        for (int j = i+1; j < N; j++) {
            colNames[count  ] = x + lex(i) + "_" + lex(j);
            elements[count++] = 1;
        }
    }
    for (int i = 0; i < N; i++) {
        colNames[count  ] = y + lex(i);
        elements[count++] = -1;
    }
    solver->addRow(colNames, elements, -1, 'E', "numEdges");


    // (19) only those of size |W| = 2
    colNames.resize(2);
    elements.resize(2);
    for (int i = 0; i < N-1; i++) {
        for (int j = i+1; j < N; j++) {
            elements[0] = 1;
            colNames[0] = x + lex(i) + "_" + lex(j);
            elements[1] = -1;
            colNames[1] = y + lex(i);
            solver->addRow(colNames, elements, 0, 'L', "GSEC2" + lex(i) + "_" + lex(j) + "_" + lex(i));
            colNames[1] = y + lex(j);
            solver->addRow(colNames, elements, 0, 'L', "GSEC2" + lex(i) + "_" + lex(j) + "_" + lex(j));
        }
    }

    // (27) x = dz
    colNames.resize(D + N - 1);
    elements.resize(D + N - 1);
    for (int i = 0; i < N; i++) {
        int count = 0;
        for (int j = 0; j < N; j++) {
            if (i == j) continue;
            int f1 = i < j ? i : j;
            int f2 = i < j ? j : i;
            colNames[count  ] = x + lex(f1) + "_" + lex(f2);
            elements[count++] = 1;
        }
        for (int d = 1; d <= D; d++) {
            colNames[count  ] = z + lex(i) + "_" + lex(d);
            elements[count++] = -d;
        }
        solver->addRow(colNames, elements, 0, 'E', "Degree" + lex(i));
    }

    // (30, 31, 32) 
    colNames.resize(2);
    elements.resize(2);
    for (int i = 0; i < N-1; i++) {
        for (int j = i+1; j < N; j++) {
            for (int d = 1; d <= D; d++) {
                for (int e = 1; e <= D; e++) {
                    elements[0] = 1;
                    colNames[0] = v + lex(i) + "_" + lex(d) + "_" + lex(j) + "_" + lex(e);
                    elements[1] = -1;
                    colNames[1] = z + lex(i) + "_" + lex(d);
                    solver->addRow(colNames, elements, 0, 'L', "Va" + lex(i) + "_" + lex(j) + "_" + lex(d) + "_" + lex(e));
                    colNames[1] = z + lex(j) + "_" + lex(e);
                    solver->addRow(colNames, elements, 0, 'L', "Vb" + lex(i) + "_" + lex(j) + "_" + lex(d) + "_" + lex(e));
                    colNames[1] = x + lex(i) + "_" + lex(j);
                    solver->addRow(colNames, elements, 0, 'L', "Vc" + lex(i) + "_" + lex(j) + "_" + lex(d) + "_" + lex(e));
                }
            }
        }
    }
 
}

/**
 * ModelAssortMST.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "ModelAssortMST.h"
#include "Options.h"
#include "AlgoUtil.h"

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

    solver->addLazyCallback(this);
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


    // ATTEMPT TO IMPROVE y <= Sum x
    colNames.resize(N);
    elements.resize(N);
    for (int i = 0; i < N; i++) {
        colNames[0] = y + lex(i);
        elements[0] = 1;
        int count = 1;
        for (int j = 0; j < N; j++) {
            if (i == j) continue;
            int f1 = i < j ? i : j;
            int f2 = i < j ? j : i;
            colNames[count  ] = x + lex(f1) + "_" + lex(f2);
            elements[count++] = -1;
        }
        solver->addRow(colNames, elements, 0, 'L', "BoundOnY" + lex(i));
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


//////////////////////////////
//////////////////////////////
//////////////////////////////
//////////////////////////////
// Cutting plane
vector<SolverCut> ModelAssortMST::separationAlgorithm(vector<double> sol) {

    
    float startTime = Util::getTime();

    callbackCalls++;
    int callbackControl = std::numeric_limits<int>::max();

    vector<SolverCut> cuts;

    //if (callbackCalls != 3) return cuts;
    
    int integer = 1;
    for (unsigned i = 0; i < sol.size(); i++) {
        if (fabs(sol[i] - round(sol[i])) > TOLERANCE) {
            integer = 0;
            break;
        }
    }

    if (callbackCalls > callbackControl) printf("%04d Callback (%s)\n", callbackCalls, integer ? "integer   " : "fractional");

    // First thing we do: read the x values from the solution and create a directed graph where for every edge ij there is
    // an edge ji with weights x_{ij} equal to the solution we just read.
    //
    // This graph will be reduced as it will contain only the necessary number of elements 
    // (those for which at least one x_ij is different from zero), then we need to map the indices in the 
    // reduced graph to the indices in the old graph
    vector<double>         y_sol;
    vector<vector<int>>    graph;
    vector<vector<double>> x_sol;
    vector<int>            newIndicesToOld;
    vector<int>            oldIndicesToNew(N);

    newIndicesToOld.reserve(N);
    std::fill(oldIndicesToNew.begin(), oldIndicesToNew.end(), -1);
    int currentIndex = 0;
    
    //////////////////
    // Reading y and x
    for (int i = 0; i < N; i++) {
        double y_temp = sol[solver->getColIndex(y + lex(i))];
        if (y_temp > TOLERANCE) {
            newIndicesToOld.push_back(i);
            y_sol.push_back(y_temp);
            graph.push_back(vector<int>());
            x_sol.push_back(vector<double>());
            oldIndicesToNew[i] = currentIndex++;
        }
    }
    for (int i = 0; i < (int)newIndicesToOld.size()-1; i++) {
        int ii = newIndicesToOld[i];
        for (int j = i+1; j < (int)newIndicesToOld.size(); j++) {
            int jj = newIndicesToOld[j];

            double x_temp = sol[solver->getColIndex(x + lex(ii) + "_" + lex(jj))];
            if (x_temp > TOLERANCE) {
                graph[i].push_back(j);
                graph[j].push_back(i);
                x_sol[i].push_back(x_temp);
                x_sol[j].push_back(x_temp);
            }
        }
    }
    //////////////////
    
    //////////////////
    // Checking for disconnected components
    float tempTime = Util::getTime();
    vector<vector<int>> verticesInCut;
    int disconnectedComponents = AlgoUtil::disconnectedComponents(graph, x_sol, verticesInCut);
    bfsTime += (Util::getTime() - tempTime);
    //////////////////
    
    /*
    printf("In solution:\n");
    for (int i = 0; i < (int)graph.size(); i++) {
        printf("Y %2d -> %2d, value = %.2f\n", i, newIndicesToOld[i], y_sol[i]);
    }
    for (int i = 0; i < (int)graph.size(); i++) {
        printf("X %2d:", i);
        for (int j = 0; j < (int)graph[i].size(); j++) {
            //printf(" %d", graph[i][j]);
            printf(" %d:%.1f", graph[i][j], x_sol[i][j]);
        }
        printf("\n");
    }
    */


    //////////////////
    // Adding cuts
    if (disconnectedComponents) {
        tempTime = Util::getTime();
        for (int v = 0; v < (int)verticesInCut.size(); v++) {
            
            vector<int> W;
            int maxYIndex =  0;
            double maxY   = -1;
            for (unsigned i = 0; i < verticesInCut[v].size(); i++) {
                int ii = verticesInCut[v][i];
                W.push_back(newIndicesToOld[ii]);
                if (y_sol[ii] > maxY) {
                    maxY = y_sol[ii];
                    maxYIndex = newIndicesToOld[ii];
                }
            }

            //printf("Add cut: [y=%d]", maxYIndex);
            //for (int i = 0; i < (int)W.size(); i++) printf(" %d", W[i]);
            //printf("\n");



            SolverCut cut;
            cut.setSense('L');
            cut.setRHS(0);
            for (unsigned i = 0; i < W.size()-1; i++) {
                for (unsigned j = i+1; j < W.size(); j++) {
                    int f1 = W[i] < W[j] ? W[i] : W[j];
                    int f2 = W[i] < W[j] ? W[j] : W[i];
                    cut.addCoef(solver->getColIndex(x + lex(f1) + "_" + lex(f2)), 1);
                }
            }
            for (unsigned i = 0; i < W.size(); i++) {
                if (W[i] != maxYIndex) cut.addCoef(solver->getColIndex(y + lex(W[i])), -1);
            }
            //printf("Cut evaluation: %.2f\n", cut.evaluate(sol));
            if (cut.evaluate(sol) > TOLERANCE) cuts.push_back(cut);
        }                 
        callbackCutsTime += Util::getTime() - tempTime;
    }
    //////////////////
    


    callbackTime += Util::getTime() - startTime;
    
    //printf("\n");
    return cuts;
}

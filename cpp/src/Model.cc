/**
 * Model.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "Model.h"
#include "CPLEX.h"
#include "Options.h"

/**
 * INITIAL METHODS
 *
 */

Model::Model() {
    string solverUsed = Options::getInstance()->getStringOption("solver");
    
    if (solverUsed.compare("cplex") == 0) {
        solver = new CPLEX();
    } else {
        solver = new Solver();
    }


    totalTime         = 0;
    solvingTime       = 0;
    callbackTime      = 0;

    callbackDataTime  = 0;
    callbackCutsTime  = 0;


    maxFlowTime       = 0;
    maxFlowsSolved    = 0;
    bfsTime           = 0;
    
    callbackCalls     = 0;
    maxFlowCalls      = 0;
    cutsAdded         = 0;

    bestSolutionTime  = 0;
    bestSolutionNodes = 0;
    
    firstNodeTime     = 0;
    firstNodeBound    = 0;
    firstNodeSolved   = false;

    totalNodes        = 0;
    
    counter = 0;
    debug = Options::getInstance()->getIntOption("debug");

}


Model::~Model() {
    delete(solver);
}

void Model::setSolverParameters() {

    solver->debugInformation(debug);
    if (debug) solver->debugLevel(Options::getInstance()->getIntOption("solver_debug_level"));
    
    solver->enablePresolve(Options::getInstance()->getBoolOption("presolve"));
    solver->setMIPEmphasis(Options::getInstance()->getIntOption("mip_emphasis"));
    solver->setProbingLevel(Options::getInstance()->getIntOption("probing_level"));
    solver->setTimeLimit((double)Options::getInstance()->getIntOption("time_limit"));
    solver->setSolverCuts();
    solver->setLPMethod();
    if (Options::getInstance()->getBoolOption("first_node_only")) solver->setNodeLimit(1);

    if ( Options::getInstance()->getBoolOption("export_model")) solver->exportModel("bc_model.lp");

}


void Model::incumbentCallbackFunction() {
    bestSolutionTime = Util::getTime() - solverStartTime;
}



void Model::nodeCallbackFunction(double bound) {
    if (firstNodeSolved) return;
    firstNodeTime = Util::getTime() - solverStartTime;
    firstNodeBound = bound;
    firstNodeSolved = true;
}

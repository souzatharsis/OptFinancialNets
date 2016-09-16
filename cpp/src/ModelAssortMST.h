/**
 * ModelAssortMST.h
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#ifndef MODELASSORTMST_H
#define MODELASSORTMST_H

#include "Model.h"
#include "Solution.h"
#include "Data.h"

class ModelAssortMST : public Model {

    protected:

        Solution solution;

        string x; // x variables
        string y; // x variables
        string z; // x variables
        string v; // x variables

        int N;
        int D;
        int K;

        void assignWarmStart();

        // Model creation
        virtual void createModel(const Data& data);

        // Execution
        void prepareExecution(const Data& data);
        void solve();

        // Solution values
        vector<vector<double > > sol_x;
        vector<double> sol_y;
 
        // Solution functions
        virtual void reserveSolutionSpace();
        virtual void readSolution();
        
        // Print functions
        void printSolutionVariables(int digits = 5, int decimals = 2);
        void printXSolutionVariables(int digits = 5, int decimals = 2);
        void printYSolutionVariables(int digits = 5, int decimals = 2);


    public:
        
        ModelAssortMST();

        virtual ~ModelAssortMST();

        void execute(const Data &data);
        
        Solution getSolution()  { return solution;  }
        void printSolution()    { solution.print(); }

        void setDebug(int d) { debug = d; }


};    

#endif 



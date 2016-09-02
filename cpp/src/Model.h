/**
 * Model.h
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#ifndef MODEL_H
#define MODEL_H

#include "Solver.h"

/**
 * Model, superclass of ssd, etc.
 */
class Model {

    protected:
        
       Solver* solver;
      
       vector<string> warmStartNames;
       vector<double> warmStartValues;

       // General times
       double totalTime;
       double solvingTime;
       double callbackTime;
       double callbackDataTime;
       double callbackCutsTime;

       // Algorithms
       double maxFlowTime;
       double bfsTime;
        
       // Counters
       int maxFlowCalls;
       int maxFlowsSolved;
       int callbackCalls;
       int cutsAdded;
 
       // Times
       double bestSolutionTime;
       double firstNodeTime;
       double firstNodeBound;
       bool firstNodeSolved;
        
       // Nodes
       int totalNodes;
       // TODO do bestSolutionNodes
       // Look for CPX_CALLBACK_INFO_NODE_COUNT in 
       int bestSolutionNodes;
       
       // Just so I can measure the bestSolutionTime by hand
       double solverStartTime;

       int debug;
       int counter;

       void setSolverParameters();
       
       virtual void readSolution() { }
       virtual void assignWarmStart() { }

    public:
        
        // Create and destroy
        Model();
        virtual ~Model();

        virtual vector<SolverCut> separationAlgorithm(vector<double> sol) {
            vector<SolverCut> sc;
            return sc;
        }
        virtual void incumbentCallbackFunction();
        virtual void nodeCallbackFunction(double bound);

        Solver* getSolver()           {return solver;            }
        double getTotalTime()         {return totalTime;         }
        double getSolvingTime()       {return solvingTime;       }

        double getCallbackTime()      {return callbackTime;      }
        double getMaxFlowTime()       {return maxFlowTime;       }
        double getBfsTime()           {return bfsTime;           }
        
        double getCallbackDataTime()  {return callbackDataTime;  }
        double getCallbackCutsTime()  {return callbackCutsTime;  }
        
        int getMaxFlowCalls()         {return maxFlowCalls;      }
        int getMaxFlowsSolved()       {return maxFlowsSolved;    }
        int getCallbackCalls()        {return callbackCalls;     }
        int getCutsAdded()            {return cutsAdded;         }
 
        double getBestSolutionTime()  {return bestSolutionTime;  }
        double getFirstNodeTime()     {return firstNodeTime;     }
        double getFirstNodeBound()    {return firstNodeBound;    }

        int getTotalNodes()           {return totalNodes;        }
        int getBestSolutionNodes()    {return bestSolutionNodes; }
       
        int getCounter() {return counter++;}
        
};    

#endif 



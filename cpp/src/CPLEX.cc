/**
 * CPLEX.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "CPLEX.h"
#include "Options.h"
#include "Model.h"


inline void Check(int result, CPXENVptr env = NULL) {
    if (result != 0) {
        printf("Result = %d\n", result);
        if ( env != NULL ) {
            CPXcloseCPLEX (&env);
        }

        throw SolverError(result);
    }
}


/**
 * INITIAL METHODS
 *
 */


CPLEX::CPLEX() : Solver() {
    int status = 0;
    env = CPXopenCPLEX(&status);
    Check(status, env);
    
    status = 0;
    problem = CPXcreateprob(env, &status, "");
    Check(status, env);
}

CPLEX::~CPLEX() {
    if (env != NULL) CPXcloseCPLEX(&env);
}

void CPLEX::deleteAndRecreateProblem() {

    Check(CPXfreeprob(env, &problem));
    status = 0;
    problem = CPXcreateprob(env, &status, "");
    Check(status, env);
}


void CPLEX::printSolverName() {
    printf("Solver used is CPLEX\n");
}

int CPLEX::getNumRows() { 
    return CPXgetnumrows(env, problem); 
}

int CPLEX::getNumCols() { 
    return CPXgetnumcols(env, problem); 
}

void CPLEX::changeObjectiveSense(bool isMax) {
    CPXchgobjsen(env, problem, isMax ? -1 : 1); 
}

int CPLEX::getNodeCount() { 
    return CPXgetnodecnt(env, problem); 
}


/**
 * lower      -> lower bound of variable
 * upper      -> upper bound of variable
 * obj        -> objective function coefficient
 * name       -> if not empty set the variable name
 */
void CPLEX::addVariable(const double lower, const double upper, const double obj, string name) {
    double up[1];
    double lo[1];
    up[0] = upper;
    lo[0] = lower;

    Check(CPXaddcols(env, problem, 1, 0, &obj, 0, 0, 0, lo, up, 0), env);
    //Check(CPXaddcols(env, problem, 1, 0, &obj, 0, 0, 0, &lower, &upper, 0));

    if (!name.empty()) {
       int col = getNumCols() - 1;
       Check(CPXchgname(env, problem, 'c', col, name.c_str()), env);
       
       //char* n;
       //n = const_cast<char*>(name.c_str());
       //Check(CPXchgcolname(env, problem, 1, &col, &n));
       
       addKey(name, col);
    }
}

void CPLEX::setVariableWarmStart(string colName, double value) {
    int index = getColIndex(colName);
    
    int beg[1]; beg[0] = 0;
    int eff[1]; eff[0] = 0;

    Check(CPXaddmipstarts(env, problem, 1, 1, beg, &index, &value, eff, NULL), env);
}

void CPLEX::setVariablesWarmStart(vector<string> colNames, vector<double> values) {
    int num = colNames.size();

    vector<int> colIndices(num);
    for (int i = 0; i < num; i++) colIndices[i] = getColIndex(colNames[i]);
    int beg[1]; beg[0] = 0;
    int eff[1]; eff[0] = 0;

    Check(CPXaddmipstarts(env, problem, 1, num, beg, &colIndices[0], &values[0], eff, NULL), env);
}

void CPLEX::refineMIPStart() {

    int a1;
    int a2;
    Check(CPXrefinemipstartconflict(env, problem, 0, &a1, &a2));
    if (a1 > 0 || a2 > 0) {
        Check(CPXclpwrite(env, problem, "cplexConflict.clp"));
    }
}

/**
 * numCols    -> number of variables to be added
 * lower      -> array of size (numCols), lower bound of variables
 * upper      -> array of size (numCols), upper bound of variables
 * obj        -> Array of size (numCols), objective function coefficients
 * name       -> if not empty set the variable name (includes index in the end starting from zero)
 */

void CPLEX::addVariables(int numCols, const double lower, const double upper, const double* obj, string& name) {
    double lo[numCols];
    double up[numCols];
    for (int i = 0; i < numCols; i++) {
        lo[i] = lower;
        up[i] = upper;
    }

    Check(CPXaddcols(env, problem, numCols, 0, obj, 0, 0, 0, lo, up, 0), env);
    if (!name.empty()) {
        int col = getNumCols() - numCols;
        for (int i = 0; i < numCols; i++) {
            string n = name + lex(i);
            Check(CPXchgname(env, problem, 'c', col, n.c_str()), env);
            addKey(n, col);
            col++;
        }
    }

}

/**
 * numCols    -> number of variables to be added
 * ub         -> upper bound of all variables
 * lb         -> lower bound of all variables
 * obj        -> Array of size (numCols), objective function coefficients
 * name       -> if not empty set the variable name (includes index in the end starting from zero)
 */
void CPLEX::addIntegerVariables(int numCols, double lb, double ub, const double* obj, string& name) {
    
    double upper[numCols];
    double lower[numCols];
    for (int i = 0; i < numCols; i++) {
        upper[i] = ub;
        lower[i] = lb;
    }
    Check(CPXaddcols(env, problem, numCols, 0, obj, 0, 0, 0, lower, upper, 0), env);

    int indices[numCols];
    char type[numCols];
    int currentCol = getNumCols() - numCols;

    for (int i = 0; i < numCols; i++) {
        indices[i] = currentCol;
        type[i] = CPX_INTEGER;
        string n = name + lex(i);
        Check(CPXchgname(env, problem, 'c', currentCol, n.c_str()), env);
        addKey(n, currentCol);
        currentCol++;
    }
    Check(CPXchgctype(env, problem, numCols, indices, type), env);
    
}

/**
 * obj        -> objective function coefficient
 * name       -> if not empty set the variable name
 */
void CPLEX::addBinaryVariable(const double obj, string name) {

    double upper = 1.0;
    Check(CPXaddcols(env, problem, 1, 0, &obj, 0, 0, 0, 0, &upper, 0), env);
    //Check(CPXaddcols(env, problem, 1, 0, &obj, 0, 0, 0, &lower, &upper, 0));

   
    int col = getNumCols() - 1;
    char type = CPX_BINARY;
    Check(CPXchgname(env, problem, 'c', col, name.c_str()), env);
    addKey(name, col);
    Check(CPXchgctype(env, problem, 1, &col, &type), env);

 
}



/**
 * numCols    -> number of variables to be added
 * obj        -> Array of size (numCols), objective function coefficients
 * name       -> if not empty set the variable name (includes index in the end starting from zero)
 */
void CPLEX::addBinaryVariables(int numCols, const double* obj, string& name) {
    
    double upper[numCols];
    for (int i = 0; i < numCols; i++) {
        upper[i] = 1.0;
    }
    Check(CPXaddcols(env, problem, numCols, 0, obj, 0, 0, 0, 0, upper, 0), env);

    int indices[numCols];
    char type[numCols];
    int currentCol = getNumCols() - numCols;

    for (int i = 0; i < numCols; i++) {
        indices[i] = currentCol;
        type[i] = CPX_BINARY;
        string n = name + lex(i);
        Check(CPXchgname(env, problem, 'c', currentCol, n.c_str()), env);
        addKey(n, currentCol);
        currentCol++;
    }
    Check(CPXchgctype(env, problem, numCols, indices, type), env);
    
}


/**
 * colNames - Variable names of the corresponding constraint
 * elements - non zero coefficients
 * rhs      - right hand side
 * sense    - 'L': <=
 *            'E': ==
 *            'G': >=
 * name     - constraint name
 */
void CPLEX::addRow(vector<string> colNames, vector<double> elements, double rhs, char sense, string name) {
    
    int matbeg = 0;
    int numNonZero = (int)colNames.size();

    vector<int> colIndices(numNonZero);
    for (int i = 0; i < numNonZero; i++) {
        colIndices[i] = getColIndex(colNames[i]);
        if (colIndices[i] == -1) printf("In addRow, colIndice was not found for variable %s. Is the name wrong?\n", colNames[i].c_str());
    }

    Check(CPXaddrows(env, problem, 0, 1, numNonZero, &rhs, &sense, &matbeg, &colIndices[0], &elements[0], 0, 0), env);
    
    int row = getNumRows() - 1;

    if (!name.empty()) {
        Check(CPXchgname(env, problem, 'r', row, name.c_str()), env);
    }
}


void CPLEX::setPriorityInBranching(vector<string> colNames, int priority) {

    vector<int> priorities(colNames.size());
    std::fill(priorities.begin(), priorities.end(), priority);
    
    setPriorityInBranching(colNames, priorities);

}

void CPLEX::setPriorityInBranching(vector<string> colNames, vector<int> priorities) {
    int cnt = (int) colNames.size();

    vector<int> indices(cnt);
    if (priorities.size() != colNames.size()) printf("In setPriorityInBranching, sizes are different\n");

    for (int i = 0; i < cnt; i++) {
        indices[i] = getColIndex(colNames[i]);
        if (indices[i] == -1) printf("In setPriorityInBranching, colIndice was not found for variable %s. Is the name wrong?\n", colNames[i].c_str());
    }
    
    Check(CPXcopyorder(env, problem, cnt, &indices[0], &priorities[0], NULL), env);

}




void CPLEX::relax() { 
    Check(CPXchgprobtype(env, problem, CPXPROB_LP), env); 
}

void CPLEX::doSolve() {
    int type = CPXgetprobtype(env, problem);
    Check(type == CPXPROB_MILP ? CPXmipopt(env, problem) : CPXoptimize(env, problem), env);
    status = CPXgetstat(env, problem);
}

int CPLEX::getStatus() { 
    return status; 
}

double CPLEX::getObjValue() {
    double objValue = 0;
    Check(CPXgetobjval(env, problem, &objValue), env);
    return objValue;
}

double CPLEX::getBestBound() {
    double bestBound = 0;
    int result = CPXgetbestobjval(env, problem, &bestBound);
    if (result != 0) {
        bestBound = getObjValue();
    }
    return bestBound;
}


void CPLEX::getColSolution() {
    int numCols = getNumCols();
    colSolution.resize(numCols);
    Check(CPXgetx(env, problem, &colSolution[0], 0, numCols-1), env);
}

// PARAMS

void CPLEX::setLPMethod() {
    //  0 = Automatic
    //  1 = Primal
    //  2 = Dual
    //  3 = Network simplex
    //  4 = Barrier 
    //  5 = Sifting
    //  6 = Concurrent
    
    int lp = Options::getInstance()->getIntOption("lp_method");
    Check(CPXsetintparam(env, CPX_PARAM_LPMETHOD, lp), env);
}

void CPLEX::setTimeLimit(double time) {
    if (time > 0) {
        Check(CPXsetdblparam(env, CPX_PARAM_TILIM, time), env);
        Check(CPXsetintparam(env, CPX_PARAM_CLOCKTYPE, 1), env);
    }
}

void CPLEX::setNodeLimit(int lim) {
    if (lim > 0) {
        Check(CPXsetintparam(env, CPX_PARAM_NODELIM, lim), env);
    }
}


void CPLEX::setLPTolerance(double tolerance) {
    Check(CPXsetdblparam(env, CPX_PARAM_EPRHS, tolerance));
}


void CPLEX::enablePresolve(bool enable) {
    Check(CPXsetintparam(env, CPX_PARAM_PREIND, enable ? CPX_ON : CPX_OFF), env);
}

void CPLEX::setSolverCuts() {
    // -1 = disable
    //  0 = Automatic
    //  1 = Moderately
    //  2 = Aggressively
    //  3 = Very aggressively

    // For individual cuts: 
    //   -2:            use value from solver_cuts option
    //   anything else: overrides solver_cuts                  

    int cuts = Options::getInstance()->getIntOption("solver_cuts");

    // -1 to 3
    int temp = Options::getInstance()->getIntOption("clique_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_CLIQUES,      temp != -2 ? temp : cuts), env);

    temp = Options::getInstance()->getIntOption("cover_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_COVERS,       temp != -2 ? temp : cuts), env);

    temp = Options::getInstance()->getIntOption("disj_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_DISJCUTS,     temp != -2 ? temp : cuts), env);

    temp = Options::getInstance()->getIntOption("landp_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_LANDPCUTS,    temp != -2 ? temp : cuts), env);

    // -1 to 2
    if (cuts == 3) cuts--;
    
    temp = Options::getInstance()->getIntOption("flowcover_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_FLOWCOVERS,   temp != -2 ? temp : cuts), env);
    
    temp = Options::getInstance()->getIntOption("flowpath_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_FLOWPATHS,    temp != -2 ? temp : cuts), env);
    
    temp = Options::getInstance()->getIntOption("gomory_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_FRACCUTS,     temp != -2 ? temp : cuts), env);
    
    temp = Options::getInstance()->getIntOption("gub_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_GUBCOVERS,    temp != -2 ? temp : cuts), env);
    
    temp = Options::getInstance()->getIntOption("implbd_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_IMPLBD,       temp != -2 ? temp : cuts), env);
    
    temp = Options::getInstance()->getIntOption("mir_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_MIRCUTS,      temp != -2 ? temp : cuts), env);
    
    temp = Options::getInstance()->getIntOption("mcf_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_MCFCUTS,      temp != -2 ? temp : cuts), env);
    
    temp = Options::getInstance()->getIntOption("zerohalf_cuts");
    Check(CPXsetintparam(env, CPX_PARAM_ZEROHALFCUTS, temp != -2 ? temp : cuts), env);
    
    
}
 
void CPLEX::setMIPEmphasis(int t) {
    if      (t == 0) Check(CPXsetintparam(env, CPX_PARAM_MIPEMPHASIS, CPX_MIPEMPHASIS_BALANCED), env);
    else if (t == 1) Check(CPXsetintparam(env, CPX_PARAM_MIPEMPHASIS, CPX_MIPEMPHASIS_FEASIBILITY), env);
    else if (t == 2) Check(CPXsetintparam(env, CPX_PARAM_MIPEMPHASIS, CPX_MIPEMPHASIS_OPTIMALITY), env);
    else if (t == 3) Check(CPXsetintparam(env, CPX_PARAM_MIPEMPHASIS, CPX_MIPEMPHASIS_BESTBOUND), env);
    else if (t == 4) Check(CPXsetintparam(env, CPX_PARAM_MIPEMPHASIS, CPX_MIPEMPHASIS_HIDDENFEAS), env);
}

void CPLEX::setProbingLevel(int t) {
    Check(CPXsetintparam(env, CPX_PARAM_PROBE, t), env);
}

// DEBUG

void CPLEX::debugInformation(int debug) {
    Check(CPXsetintparam(env, CPX_PARAM_SCRIND, debug > 0 ? CPX_ON : CPX_OFF), env);
}

void CPLEX::debugLevel(int debugLevel) {
    Check(CPXsetintparam(env, CPX_PARAM_MIPDISPLAY, debugLevel), env);
}

void CPLEX::exportModel(const char* filename) {
    Check(CPXwriteprob(env, problem, filename, 0), env);
}

///////////////////////////////
//                           //
//                           //
// STATUS FUNCTIONS          //
///////////////////////////////

bool CPLEX::solutionExists() {
    return isOptimal() || isSolutionLimit() || isMIPTimeLimitFeasible() || isBestNumerical() ||
           status == CPXMIP_NODE_LIM_FEAS || status == CPXMIP_FAIL_FEAS || status == CPXMIP_MEM_LIM_FEAS || 
           status == CPXMIP_ABORT_FEAS || status == CPXMIP_FAIL_FEAS_NO_TREE;
}

bool CPLEX::isOptimal() { 
    return isIntegerOptimal() || status == CPX_STAT_OPTIMAL; 
}

bool CPLEX::isIntegerOptimal() {
    return status == CPXMIP_OPTIMAL || status == CPXMIP_OPTIMAL_TOL;
}

bool CPLEX::isInfeasible() {
    return status == CPX_STAT_INFEASIBLE || status == CPXMIP_INFEASIBLE;
}

bool CPLEX::isInfeasibleDueToScaling() {
    return status == CPX_STAT_OPTIMAL_INFEAS;
}

bool CPLEX::isUnbounded() {
    return status == CPX_STAT_UNBOUNDED || status == CPXMIP_UNBOUNDED;
}

bool CPLEX::isInfeasibleOrUnbounded() {
    return status == CPX_STAT_INForUNBD || status == CPXMIP_INForUNBD;
}

bool CPLEX::isSolutionLimit() { 
    return status == CPXMIP_SOL_LIM; 
}

bool CPLEX::isTimeLimit() {
    return status == CPX_STAT_ABORT_TIME_LIM; 
}

bool CPLEX::isMIPTimeLimitFeasible() {
    return status == CPXMIP_TIME_LIM_FEAS;
}

bool CPLEX::isMIPTimeLimitInfeasible() {
    return status == CPXMIP_TIME_LIM_INFEAS;
}

bool CPLEX::isObjectiveLimitReached() {
    return status == CPX_STAT_ABORT_OBJ_LIM;
}

bool CPLEX::isIterationLimitReached() {
    return status == CPX_STAT_ABORT_IT_LIM;
}

bool CPLEX::isBestNumerical() {
    return status == CPX_STAT_NUM_BEST;
}

bool CPLEX::isNumericalDifficulties() { 
    return status == CPX_STAT_NUM_BEST; 
}



///////////////////////////////
//                           //
//                           //
// STATUS FUNCTIONS          //
///////////////////////////////



void CPLEX::addLazyCallback(void* userData) {
    // Ask for variables in terms of original problem instead of presolved.
    Check(CPXsetintparam(env, CPX_PARAM_MIPCBREDLP, CPX_OFF), env);
    Check(CPXsetintparam(env, CPX_PARAM_PRELINEAR, CPX_OFF), env);
    Check(CPXsetlazyconstraintcallbackfunc(env, functionCallback, userData), env);
}

void CPLEX::addUserCutCallback(void* userData) {
    // Ask for variables in terms of original problem instead of presolved.
    Check(CPXsetintparam(env, CPX_PARAM_MIPCBREDLP, CPX_OFF), env);
    Check(CPXsetintparam(env, CPX_PARAM_PRELINEAR, CPX_OFF), env);
    Check(CPXsetusercutcallbackfunc(env, functionCallback, userData), env);
}

void CPLEX::addIncumbentCallback(void* userData) {
    Check(CPXsetincumbentcallbackfunc(env, incumbentCallback, userData), env);
}

void CPLEX::addNodeCallback(void* userData) {
    Check(CPXsetnodecallbackfunc(env, nodeCallback, userData), env);
}


int CPXPUBLIC CPLEX::functionCallback(CPXCENVptr env, void* cbdata, int wherefrom, void* cbhandle, int* useraction_p) {

    Model* model = static_cast<Model*>(cbhandle);
    
    int numCols = model->getSolver()->getNumCols();
    vector<double> x(numCols);
    int status = CPXgetcallbacknodex(env, cbdata, wherefrom, &x[0], 0, numCols-1);
    if (status != 0) {
        printf("Error in functionCallback, status = %d\n", status);
        return 0;
    }
    
    vector<SolverCut> cuts = model->separationAlgorithm(x);
    for (int i  = 0; i < (int)cuts.size(); i++) {
        vector<int> indices  = cuts[i].getIndices();
        vector<double> coefs = cuts[i].getCoefs();
        CPXcutcallbackadd(env, cbdata, wherefrom, (int)cuts[i].getNumCoefs(), cuts[i].getRHS(), 
                          cuts[i].getSense(), &indices[0], &coefs[0], CPX_USECUT_FORCE);
    }

    
    /* Temporary, export model to check if constraints are correct
    if (cuts.size() > 0) {
        CPXLPptr lpProblem;
        string namelp = "LPRelax" + lex(model->getCounter()) + ".lp";
        CPXgetcallbacknodelp(env, cbdata, wherefrom, &lpProblem);
        CPXwriteprob(env, lpProblem, namelp.c_str(), 0);
    }
    */
    return 0;
}


int CPXPUBLIC CPLEX::incumbentCallback(CPXCENVptr env, void* cbdata, int wherefrom, void* cbhandle, double objval, 
                                       double *x, int *isfeas_p, int* useraction_p) {
    Model* model = static_cast<Model*>(cbhandle);
    model->incumbentCallbackFunction();
    return 0;
}

int CPXPUBLIC CPLEX::nodeCallback(CPXCENVptr env, void* cbdata, int wherefrom, void* cbhandle, int* nodeindex_p, int* useraction_p) {

    Model* model = static_cast<Model*>(cbhandle);
    
    double value;
    CPXgetcallbacknodeinfo(env, cbdata, wherefrom, 0, CPX_CALLBACK_INFO_NODE_OBJVAL, &value);
    model->nodeCallbackFunction(value);
    return 0;
}



/**
 * Options.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "Options.h"
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/join.hpp>

//#ifndef _WIN32
//#include <boost/regex.hpp>
//#endif


using std::string;
using std::vector;

/**
 * INITIAL METHODS
 *
 */

Options* Options::instance = new Options();

Options* Options::getInstance () {
	return instance;
}

void Options::factory() {
	delete instance;
	instance = new Options();
}

Options::Options() {
    assignDefaultValues();
}

Options::~Options() {
    for (int i = 0; i < (int)options.size(); i++) {
        delete(options[i]);
    }
}

void Options::finalise() {
    delete(instance);
}


/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
// PRIVATE METHODS //////
/////////////////////////

void Options::assignDefaultValues() {

    inputFile = "";

    vector<string> modelValues;
    modelValues.push_back("mtc_assort");

    vector<string> solverValues;
    solverValues.push_back("cplex");
    
    vector<string> empty;
   
    double dmax = std::numeric_limits<double>::max();
    double imax = std::numeric_limits<int>::max();

    // Debug options
    options.push_back(new IntOption ("debug",        "Level of debug information [0-4, 0 means no debug]", 0, 0, 4, 0));
    options.push_back(new BoolOption("export_model", "If (1) exports model to lp file", 0, 0));
    
    // General options
    options.push_back(new StringOption("model",     "Choose which model to solve (default: mtc_assort)", 1, "mtc_assort", modelValues));
    options.push_back(new StringOption("output",    "Output file where solution will be written", 0, "", empty));
    
    options.push_back(new DoubleOption("cuts_tolerance",      "Tolerance level when adding violated cuts [default: 1e-7]", 1, 1e-7, dmax, 0));

    // Solver options 
    options.push_back(new StringOption("solver",             "Choose which solver to use [Default: cplex)", 1, "cplex", solverValues));
    options.push_back(new IntOption   ("solver_debug_level", "Choose the solver debug level [Default: 2]", 1, 2, 5, 0));
    options.push_back(new IntOption   ("time_limit",         "Time limit for the solver (in seconds, if zero time limit is not set)", 1, 21600, imax, 0));
    options.push_back(new BoolOption  ("presolve",           "Presolve is (0) disabled or (1) enabled [Default: 1]",            1, 1));
    options.push_back(new IntOption   ("mip_emphasis",       "MIP emphasis (0 to 4) [Default: 0]",                       1, 0, 4,  0));
    options.push_back(new IntOption   ("lp_method",          "Set LP method [Default: 0]",                               1, 0, 6,  0));
    options.push_back(new IntOption   ("probing_level",      "MIP probing lebel (-1 to 3) [Default: 1]",                 1, 0, 3, -1));
   
    // Solver cuts
    options.push_back(new IntOption  ("solver_cuts",    "All cuts are (-1) disabled, (0) automatic or (1-3) enabled            [Default: -1]", 1, -1, 3, -1));
    options.push_back(new IntOption  ("cover_cuts",     "(-1 to 3) Overrides or (-2) not the option for cover cuts             [Default: -2]", 1, -2, 3, -2));
    options.push_back(new IntOption  ("landp_cuts",     "(-1 to 3) Overrides or (-2) not the option for lift-and-project cuts  [Default: -2]", 1, -2, 3, -2));
    options.push_back(new IntOption  ("gomory_cuts",    "(-1 to 2) Overrides or (-2) not the option for gomory fractional cuts [Default: -2]", 1, -2, 2, -2));
    options.push_back(new IntOption  ("mir_cuts",       "(-1 to 2) Overrides or (-2) not the option for MIR cuts               [Default: -2]", 1, -2, 2, -2));
    options.push_back(new IntOption  ("zerohalf_cuts",  "(-1 to 2) Overrides or (-2) not the option for zero-half cuts         [Default: -2]", 1, -2, 2, -2));
    options.push_back(new IntOption  ("disj_cuts",      "(-1 to 3) Overrides or (-2) not the option for disjunctive cuts       [Default: -1]", 1, -1, 3, -2));
    options.push_back(new IntOption  ("flowcover_cuts", "(-1 to 2) Overrides or (-2) not the option for flow cover cuts        [Default: -1]", 1, -1, 2, -2));
    options.push_back(new IntOption  ("mcf_cuts",       "(-1 to 2) Overrides or (-2) not the option for MCF cuts               [Default: -1]", 1, -1, 2, -2));
    options.push_back(new IntOption  ("clique_cuts",    "(-1 to 3) Overrides or (-2) not the option for cliques cuts           [Default: -1]", 1, -1, 3, -2));
    options.push_back(new IntOption  ("flowpath_cuts",  "(-1 to 2) Overrides or (-2) not the option for flow path cuts         [Default: -1]", 1, -1, 2, -2));
    options.push_back(new IntOption  ("gub_cuts",       "(-1 to 2) Overrides or (-2) not the option for GUB cuts               [Default: -1]", 1, -1, 2, -2));
    options.push_back(new IntOption  ("implbd_cuts",    "(-1 to 2) Overrides or (-2) not the option for implied bound cuts     [Default: -1]", 1, -1, 2, -2));



    for (int i = 0; i < (int)options.size(); i++) {
        optionsMap[options[i]->getName()] = i;
    }

}

/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
// GETS /////////////////
/////////////////////////

bool Options::getBoolOption(string name) {
    if (optionsMap.find(name) != optionsMap.end()) {
        BoolOption* b = dynamic_cast<BoolOption*>(options[optionsMap[name]]);
        if (b != 0) {
            return b->getValue();
        } else {
            Util::throwInvalidArgument("Error: Attempting to get boolean value from option %s which is not boolean.", name.c_str());
        }
    } else {
        Util::throwInvalidArgument("Error: Attempting to get value from non-existing option %s.", name.c_str());
    }
    return false;
}

int Options::getIntOption(string name) {
    if (optionsMap.find(name) != optionsMap.end()) {
        IntOption* b = dynamic_cast<IntOption*>(options[optionsMap[name]]);
        if (b != 0) {
            return b->getValue();
        } else {
            Util::throwInvalidArgument("Error: Attempting to get integer value from option %s which is not integer.", name.c_str());
        }
    } else {
        Util::throwInvalidArgument("Error: Attempting to get value from non-existing option %s.", name.c_str());
    }
    return 0;
}

double Options::getDoubleOption(string name) {
    if (optionsMap.find(name) != optionsMap.end()) {
        DoubleOption* b = dynamic_cast<DoubleOption*>(options[optionsMap[name]]);
        if (b != 0) {
            return b->getValue();
        } else {
            Util::throwInvalidArgument("Error: Attempting to get double value from option %s which is not double.", name.c_str());
        }
    } else {
        Util::throwInvalidArgument("Error: Attempting to get value from non-existing option %s.", name.c_str());
    }
    return 0;
}

string Options::getStringOption(string name) {
    if (optionsMap.find(name) != optionsMap.end()) {
        StringOption* b = dynamic_cast<StringOption*>(options[optionsMap[name]]);
        if (b != 0) {
            return b->getValue();
        } else {
            Util::throwInvalidArgument("Error: Attempting to get string value from option %s which is not string.", name.c_str());
        }
    } else {
        Util::throwInvalidArgument("Error: Attempting to get value from non-existing option %s.", name.c_str());
    }
    return 0;
}

vector<int> Options::getArrayOption(string name) {
    if (optionsMap.find(name) != optionsMap.end()) {
        ArrayOption* b = dynamic_cast<ArrayOption*>(options[optionsMap[name]]);
        if (b != 0) {
            return b->getValue();
        } else {
            Util::throwInvalidArgument("Error: Attempting to get array from option %s which is not an array option.", name.c_str());
        }
    } else {
        Util::throwInvalidArgument("Error: Attempting to get value from non-existing option %s.", name.c_str());
    }
    return vector<int>();
}

vector<vector<int>> Options::getMatrixOption(string name) {
    if (optionsMap.find(name) != optionsMap.end()) {
        MatrixOption* b = dynamic_cast<MatrixOption*>(options[optionsMap[name]]);
        if (b != 0) {
            return b->getValue();
        } else {
            Util::throwInvalidArgument("Error: Attempting to get matrix from option %s which is not a matrix option.", name.c_str());
        }
    } else {
        Util::throwInvalidArgument("Error: Attempting to get value from non-existing option %s.", name.c_str());
    }
    return vector<vector<int>>();
}






/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
// PARSER ///////////////
/////////////////////////

void Options::parseOptions(int numOptions, char* pairs[]) {
    
    bool fileFound = false;

    // Empty argv
    if (numOptions == 1) {
        printHelp();
        Util::throwInvalidArgument("");
    }


    for (int i = 1; i < numOptions; i++) {
        string op = pairs[i];

//#ifdef _WIN32        
        int fail = 0;
        if (op.size() < 2) fail = 1;
        if (op[0] != '-' || op[1] != '-') fail = 1;
        if (fail == 0) op.erase(0, 2);
        vector<string> temp;
        boost::split(temp, op, boost::is_any_of("="));
        
        if (!fail && temp.size() == 2) {
//#else
//        if (boost::regex_search(op.c_str(), boost::regex("^--[^-=]*=[^=]*$"))) {
//            op.erase(0, 2);
//            vector<string> temp;
//            boost::split(temp, op, boost::is_any_of("="));
//#endif

            
            if (optionsMap.find(temp[0]) != optionsMap.end()) {
                int index = optionsMap[temp[0]];
                options[index]->checkOption(temp[1], temp[0]);
            } else {
                Util::throwInvalidArgument("Error: Option %s was not recognised.", temp[0].c_str());
            }


        } else {
            if (!fileFound) {
                fileFound = true;
                inputFile = op.c_str();

            } else {
                Util::throwInvalidArgument("Error: Invalid option %s", op.c_str());
            }
        }
    }

    //if (!fileFound) Util::throwInvalidArgument("Error: input file was not provided");

    print(); 

}

////////////////////////////
////////////////////////////
////////////////////////////
// PRINT INFORMATION ///////
////////////////////////////

void Options::print() {
    
    int debug = getIntOption("debug");    
    if (debug > 0) {
        printf("---------\n");
        printf("Input file: %s\n\n", inputFile.c_str());
        
        bool modified = false;
        for (int i = 0; i < (int)options.size(); i++) {
            if (options[i]->wasChanged()) {
                modified = true;
                break;
            }
        }
        
        printf("%s", debug > 2 ? "All options:\n" : modified ? "Modified options:\n" : "");

        for (int i = 0; i < (int)options.size(); i++) {
            if (debug > 2 || options[i]->wasChanged()) {
                printf("%s=%s\n", options[i]->getName().c_str(), options[i]->getValueAsString().c_str());
            }
        }
        
        if (debug <= 2 || modified)
            printf("\n");

    }

}

void Options::printHelp() {
    printf("Usage:\n");
    printf("ssd <data-file> [options]\n");
    printf("\n");
    printf("Options:\n");
    for (int i = 0; i < (int)options.size(); i++) {
        if (options[i]->getShowOutput())
            printf(" --%-20s %s\n", options[i]->getName().c_str(), options[i]->getDescription().c_str());
    }
}



string Options::getAllOptionsAsText() {
    string ret = "";
    for (int i = 0; i < (int)options.size(); i++) {
        char buffer[300];
        sprintf(buffer, "%s: %s\n", options[i]->getName().c_str(), options[i]->getValueAsString().c_str());
        ret = ret + buffer;
    }
    return ret;
}

string Options::getOutputOptionsAsText() {
    string ret = "";
    for (int i = 0; i < (int)options.size(); i++) {
        if (options[i]->getShowOutput()) {
            char buffer[8000];
            sprintf(buffer, "%s: %s\n", options[i]->getName().c_str(), options[i]->getValueAsString().c_str());
            ret = ret + buffer;
        }
    }
    return ret;
}


string Options::getModifiedOptionsAsText() {
    string ret = "";
    for (int i = 0; i < (int)options.size(); i++) {
        if (options[i]->wasChanged()) {
            char buffer[300];
            sprintf(buffer, "%s: %s\n", options[i]->getName().c_str(), options[i]->getValueAsString().c_str());
            ret = ret + buffer;
        }
    }
    return ret;
}



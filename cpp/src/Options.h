/**
 * Options.h
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

/*********************************************
 * List of options
 *
 * Format: --optionName=value
 * 
 * Example: ./ssd --debug=1
 *
 * The exception is the input file. Its format is just its name.
 *
 * Example: ./ssd inputFile --debug=1
 *
 */


#ifndef OPTIONS_H
#define OPTIONS_H

#include "Util.h"
#include "Option.h"

using std::string;
using std::vector;
using std::map;



/**
 * Parser of options
 */
class Options {

    private:

        /**
         * Instancia singleton desta classe.
         */
         static Options* instance;

        // Input file, must be provided (no name for this option)
        string inputFile;
        
        vector<Option*> options;
        map<string, int> optionsMap;

        // Private methods
        void assignDefaultValues();

    public:
        
        // Singleton instance
        static Options* getInstance();
        
        // Create and destroy
        Options();
        ~Options();
        void factory();
        static void finalise();



        // parse options
        void parseOptions(int numOptions, char* pairs[]);
        
        // print debug information
        void print();
        void printHelp();
        string getAllOptionsAsText();
        string getOutputOptionsAsText();
        string getModifiedOptionsAsText();

        // input file
        string getInputFile()     {return inputFile;}

        // get option values
        bool                getBoolOption  (string name);
        int                 getIntOption   (string name);
        double              getDoubleOption(string name);
        string              getStringOption(string name);
        vector<int>         getArrayOption (string name);
        vector<vector<int>> getMatrixOption(string name);

};    

#endif 



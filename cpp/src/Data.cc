/**
 * Data.cc
 *
 * Copyright(c) 2015
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "Data.h"
#include "Options.h"


Data::Data() {
    numAssets = 0;

}

Data::~Data() {
}

void Data::readData() {
    string inputFile = Options::getInstance()->getInputFile();

    printf("File: %s\n", inputFile.c_str());

    // Correlation is a diagonal matrix (N assets from 0 to N-1)
    // Asset 0  : from (0 to N-2) represents assets (1 to N-1)
    // Asset 1  : from (0 to N-3) represents assets (2 to N-1)
    // Asset 2  : from (0 to N-4) represents assets (3 to N-1)
    // ...
    // Asset N-2: from (0 to 1) represents (N-2 to N-1) assets
    // Asset N-1: from (0 to 0) represents (N-1 to N-1) assets

    FILE* file;
 
    if (!Util::openFile(&file, inputFile.c_str(), "r")) 
        Util::throwInvalidArgument("Error: Input file '%s' was not found or could not be opened.", inputFile.c_str());

    try {
        if (fscanf(file, "%d", &numAssets) != 1) throw std::invalid_argument("");
        correlation.resize(numAssets-1); 
        for (int i = 0; i < numAssets-1; i++) {
            for (int j = i+1; j < numAssets; j++) {
                float corr;
                if (fscanf(file, "%f", &corr) != 1) throw std::invalid_argument("");
                correlation[i].push_back(corr);
            }
        }
    
    } catch ( const std::invalid_argument& e) {
        if (!Util::closeFile(&file)) Util::throwInvalidArgument("Error: Products file %s could not be closed.", inputFile.c_str());
        Util::throwInvalidArgument("Error: Products file '%s' is invalid.", inputFile.c_str());
    }
    if (!Util::closeFile(&file)) Util::throwInvalidArgument("Error: Products file %s could not be closed.", inputFile.c_str());

    Util::printDiagonalDoubleMatrix(correlation);
}

double Data::getCorrelation(int i, int j) const {
    if (i >= (int)correlation.size())    Util::throwInvalidArgument("Error: Out of range parameter i in getCorrelation");
    if (j >= (int)correlation[i].size()) Util::throwInvalidArgument("Error: Out of range parameter j in getCorrelation");
    return correlation[i][j];
}

double Data::getTransformedCorrelation(int i, int j) const {
    if (i >= (int)correlation.size())    Util::throwInvalidArgument("Error: Out of range parameter i in getCorrelation");
    if (j >= (int)correlation[i].size()) Util::throwInvalidArgument("Error: Out of range parameter j in getCorrelation");
    // TODO apply the formula
    return correlation[i][j];
}


void Data::print() {
    int debug =  Options::getInstance()->getIntOption("debug");
    if (debug > 0) {
        printf("Test instance:\n\n");
        printf("Num Assets:    %d\n", numAssets);
    }

}



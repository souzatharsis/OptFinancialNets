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
    /*
    FILE* file;
    string inputFile = Options::getInstance()->getStringOption("products_file");
 
    if (!Util::openFile(&file, inputFile.c_str(), "r")) 
        Util::throwInvalidArgument("Error: Products file '%s' was not found or could not be opened.", inputFile.c_str());

    try {
        if (fscanf(file, "%d", &numProducts) != 1) throw std::invalid_argument("");
    
        char buffer[100];
        if (fscanf(file, "%s", buffer) != 1) throw std::invalid_argument("");
        if (fscanf(file, "%s", buffer) != 1) throw std::invalid_argument("");
    
        for (int i = 0; i < numProducts; i++) {
            int prod;
            int loc;
            if (fscanf(file, "%d", &prod) != 1) throw std::invalid_argument("");
            if (fscanf(file, "%d", &loc)  != 1) throw std::invalid_argument("");

            locations[loc]         = prod;
            vertices[locationsToVertices[loc]].push_back(prod);
            productsVertex[prod]   = locationsToVertices[loc];
            productsLocation[prod] = loc;
        }
    
    } catch ( const std::invalid_argument& e) {
        if (!Util::closeFile(&file)) Util::throwInvalidArgument("Error: Products file %s could not be closed.", inputFile.c_str());
        Util::throwInvalidArgument("Error: Products file '%s' is invalid.", inputFile.c_str());
    }
    if (!Util::closeFile(&file)) Util::throwInvalidArgument("Error: Products file %s could not be closed.", inputFile.c_str());
    */
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



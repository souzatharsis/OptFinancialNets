/**
 * Data.h
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#ifndef DATA_H
#define DATA_H

#include "Util.h"

/**
 * Data data
 */
class Data {

    private:

        int numAssets;
        vector<vector<double>> correlation;

    public:

        Data();
        ~Data();

        // Gets
        int getNumAssets() const {return numAssets;};
        
        double getCorrelation(int i, int j) const;

        void readData();
        void print();

};

#endif

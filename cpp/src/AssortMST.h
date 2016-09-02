/**
 * AssortMST.h
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#ifndef MSTASSORT_H
#define MSTASSORT_H

#include "Data.h"
#include "Model.h"

class AssortMST {

    private:

        Data data;
        Model* model;

        double totalTime;

    public:
   
        AssortMST();
        ~AssortMST();

        void execute();
};    

#endif 

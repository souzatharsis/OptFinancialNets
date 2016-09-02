/**
 * main.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle / Tharsis T. P. Souza
 * All rights reserved.
 */

#include "Options.h"
#include "AssortMST.h"

void finalise() {
    Options::finalise();
}

int main(int argc, char *argv[]) {
  
    try {
        // Read and parse options
        Options::getInstance()->factory();
        Options::getInstance()->parseOptions(argc, argv);

        if (Options::getInstance()->getStringOption("model").compare("assort_mst") == 0) {
            AssortMST assortMST;
            assortMST.execute();
        }


    } catch (std::invalid_argument& e) {
        printf("%s\n", e.what());
    }
    
    finalise();

    return 0;
}

/**
 * main.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle / Tharsis T. P. Souza
 * All rights reserved.
 */

#include "Options.h"

void finalise() {
    Options::finalise();
}

int main(int argc, char *argv[]) {
  
    try {
        // Read and parse options
        Options::getInstance()->factory();
        Options::getInstance()->parseOptions(argc, argv);
        Options::getInstance()->print();
    } catch (std::invalid_argument& e) {
        printf("%s\n", e.what());
    }
    
    finalise();

    return 0;
}

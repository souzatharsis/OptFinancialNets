/**
 * AlgoUtil.h
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#ifndef ALGOUTIL_H
#define ALGOUTIL_H

#include "Util.h"

////////////////////////////////////////

class AlgoUtil {

    private:


    public:

        static int computeSMaxTree(int k, int p);


        static int isConnected(const vector<vector<int> >    &graph, 
                               const vector<vector<double> > &distance,
                               vector<int>                   &notConnected); 


        // This only works properly for graphs where every edge is bidirectional
        static int disconnectedComponents(const vector<vector<int> >    &graph, 
                                          const vector<vector<double> > &distance,
                                          vector<vector<int>>           &components);


};    

#endif 

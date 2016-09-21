/**
 * AlgoUtil.cc
 *
 * Copyright(c) 2016
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "AlgoUtil.h"
#include "Options.h"

int AlgoUtil::computeSMaxTree(int k, int p) {
    if (k >= 2*p + 1) return 4*k + 2*p*p - 6*p - 4;
    else              return (p + 2)*k - (3*p + 2);    
}

int AlgoUtil::isConnected(const vector<vector<int> >    &graph, 
                          const vector<vector<double> > &distance,
                          vector<int>                   &notConnected) { 
    
    notConnected.resize(graph.size());
    std::fill(notConnected.begin(), notConnected.end(), 1);
    
    vector<int> visitQueue(1);
    visitQueue[0] = 0;
    notConnected[0] = 0;

    while (visitQueue.size() > 0) {
        int i = visitQueue[0];
        for (int j = 0; j < (int)graph[i].size(); j++) {
            int jj = graph[i][j];
            if (distance[i][j] > TOLERANCE) {
                 if (notConnected[jj] == 1) {
                    visitQueue.push_back(jj);
                    notConnected[jj] = 0;
                 }
            }
        }
        visitQueue.erase(visitQueue.begin());
    }

    int numDisconnected = 0;
    
    for (unsigned i = 0; i < notConnected.size(); i++) {
        if (notConnected[i] == 1) {
            notConnected[numDisconnected] = i;
            numDisconnected++;
        }
    }
    notConnected.resize(numDisconnected);
    
    return numDisconnected == 0;
}


// This only works properly for graphs where every edge is bidirectional
int AlgoUtil::disconnectedComponents(const vector<vector<int> >    &graph, 
                                     const vector<vector<double> > &distance,
                                     vector<vector<int>>           &components) { 
   
    if (graph.size() == 0) return 0;        

    components.resize(1);
    vector<int> visited(graph.size());
    std::fill(visited.begin()+1, visited.end(), 0);
    
    int verticesLeft = (int) graph.size();

    int minIndexLeft = 0;
    int currentComponent = 0;
    while (minIndexLeft != -1) {
        
        components[currentComponent].reserve(verticesLeft);
        
        vector<int> visitQueue(1);
        
        visitQueue[0] = minIndexLeft;
        components[currentComponent].push_back(minIndexLeft);
        visited[minIndexLeft] = 1;
        verticesLeft--;
 
        while (visitQueue.size() > 0) {
            int i = visitQueue[0];
            for (int j = 0; j < (int)graph[i].size(); j++) {
                int jj = graph[i][j];
                if (visited[jj] == 0) {
                    visitQueue.push_back(jj);
                    components[currentComponent].push_back(jj);
                    visited[jj] = 1;
                    verticesLeft--;
                }
            }
            visitQueue.erase(visitQueue.begin());
        }
       
        int found = 0;
        for (unsigned i = minIndexLeft + 1; i < graph.size(); i++) {
            if (visited[i] == 0) {
                minIndexLeft = i;
                found = 1;
                break;
            }
        }
        if (!found) {
            minIndexLeft = -1;
        } else {
            currentComponent++;
            components.resize(components.size()+1);
        }

        
    }
   
    //for (unsigned i = 0; i < components.size(); i++) {
    //    printf("Cmae %d:", i);
    //    for (unsigned j = 0; j < components[i].size(); j++) printf(" %d", components[i][j]);
    //    printf("\n");
    //}
    //printf("\n");

    //components.erase(components.begin());
    
    return components.size() - 1;
}



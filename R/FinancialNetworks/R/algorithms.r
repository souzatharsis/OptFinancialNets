computeMST <- function(graph, debug = 0) {

    if (!is.matrix(graph)) stop0("Graph is not a matrix"); 
    if (nrow(graph) == 0)  stop0("Matrix has zero rows");
    if (ncol(graph) == 0)  stop0("Matrix is not square");

    N = ncol(graph);

    mst = initialiseGraph(N);
  
    component = rep(0, N); 
    
    edges1 = numeric();
    edges2 = numeric();
    values = numeric();
    for (i in 1 : (N-1)) {    
        for (j in (i+1) : N) {    
            edges1 = c(edges1, i);
            edges2 = c(edges2, j);
            values = c(values, graph[i,j]);
        }
    }

    ordering = order(values);
    values = values[ordering];
    edges1 = edges1[ordering];
    edges2 = edges2[ordering];
    
    numAdded = 0;
    maxAdded = N - 1;
    maxComponent = 0;
    for (i in 1: length(values)) {
        v1 = edges1[i];
        v2 = edges2[i];
        if (debug) printf("Trying to add %d-%d [comps = %d and %d, value = %5.2f]", v1, v2, component[v1], component[v2], values[i]);
        
        if (!(component[v1] == component[v2] && component[v1] != 0)) {
            if (component[v1] == 0 && component[v2] == 0) {
                maxComponent = maxComponent + 1;
                component[v1] = maxComponent;
                component[v2] = maxComponent;
            } else if (component[v1] == 0 && component[v2] != 0) {
                component[v1] = component[v2];
            } else if (component[v1] != 0 && component[v2] == 0) {
                component[v2] = component[v1];
            } else if (component[v1] != component[v2]) {
                smaller = min(component[v1], component[v2]);
                larger  = max(component[v1], component[v2]);
                # This for can disappear if we use a list for each component, 
                # but not necessary right now
                for (j in  1 : N) {
                    if (component[j] == larger) component[j] = smaller;
                }
            }
            numAdded = numAdded + 1;
            mst$addEdge(v1, v2, values[i]);
            if (debug) printf(" - added\n");
            if (numAdded >= maxAdded) break;
        } else {
            if (debug) printf("\n");
        }
    }

    return(mst);
}

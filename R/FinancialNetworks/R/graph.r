
initialiseGraph <- function(numVertices) {
    graph <- graphObject$new(
        numVertices = numVertices
    );

    graph$prepareAndCheckData();

    return(graph);
}

graphObject <- setRefClass("graphObject", 
    
    fields = list(
        numVertices = "numeric",
        edges       = "list",
        values      = "list",
        degree      = "numeric"
    ),

    # Unnecessary to keep degree but just for convenience

    methods = list(

################
# Methods (non idented with the definition of the class)

prepareAndCheckData = function() {
    .errorCheckPositiveInteger(numVertices);
    edges  <<- list();
    values <<- list();
    for (i in 1 : numVertices) {
        edges [[i]] <<- numeric(0);
        values[[i]] <<- numeric(0);
    }
    degree <<- rep(0, numVertices);
},


addEdge = function(i, j, value) {
    
    .errorCheckPositiveInteger(i);
    .errorCheckPositiveInteger(j);
    .errorCheckNumeric(value);
    
    if (i > numVertices) stop0("Parameter i bigger than numVertices.");
    if (j > numVertices) stop0("Parameter j bigger than numVertices.");
    
    if (length(edges[[i]]) > 0) {
        for (k in 1 : length(edges[[i]])) {
            if (edges[[i]][k] == j) stop0(sprintf("Error in addEge: edge %d,%d already exists.", i, j));
        }
    }
    
    # Edges are ordered by vertex index
    
    if (length(edges[[i]]) > 0) {
        for (k in 1 : length(edges[[i]])) {
            if (edges[[i]][k] > j) {
                if (k > 1) {
                    edges [[i]] <<- c(edges [[i]][1:(k-1)], j,     edges [[i]][k:length(edges [[i]])]);
                    values[[i]] <<- c(values[[i]][1:(k-1)], value, values[[i]][k:length(values[[i]])]);
                    break;
                } else {
                    edges [[i]] <<- c(j,     edges [[i]]);
                    values[[i]] <<- c(value, values[[i]]);
                    break;
                }
            } else if (k == length(edges[[i]])) {
                edges [[i]] <<- c(edges [[i]], j    );
                values[[i]] <<- c(values[[i]], value);
            }
        }
    } else {
        edges [[i]] <<- c(j    );
        values[[i]] <<- c(value);
    }


    if (length(edges[[j]]) > 0) {
        for (k in 1 : length(edges[[j]])) {
            if (edges[[j]][k] > i) {
                if (k > 1) {
                    edges [[j]] <<- c(edges [[j]][1:(k-1)], i,     edges [[j]][k:length(edges [[j]])]);
                    values[[j]] <<- c(values[[j]][1:(k-1)], value, values[[j]][k:length(values[[j]])]);
                    break;
                } else {
                    edges [[j]] <<- c(i,     edges [[j]]);
                    values[[j]] <<- c(value, values[[j]]);
                    break;
                }
            } else if (k == length(edges[[j]])) {
                edges [[j]] <<- c(edges [[j]], i    );
                values[[j]] <<- c(values[[j]], value);
            }
        }
    } else {
        edges [[j]] <<- c(i    );
        values[[j]] <<- c(value);
    }

    degree[i] <<- degree[i] + 1;
    degree[j] <<- degree[j] + 1;
},

assortativityIndex = function() {
    v1 = numeric(0);
    v2 = numeric(0);
    assorta = 0;

    for (i in 1 : numVertices) {
        for (j in 1 : length(edges[[i]])) {
            v1 = c(v1, degree[i]            );
            v2 = c(v2, degree[edges[[i]][j]]);
        }
    }

    o = order(v1);
    v1 = v1[o];
    v2 = v2[o];

    return(cor(v1, v2));
},


absoluteDiff = function() {
    diff = 0;
    for (i in 1 : numVertices) {
        for (j in 1 : length(edges[[i]])) {
            jj = edges[[i]][j];
            if (jj > i) {
                diff = diff + abs(degree[i] - degree[jj]);
            }
        }
    }
    return(diff);
},


squaredDiff = function() {
    diff = 0;
    for (i in 1 : numVertices) {
        for (j in 1 : length(edges[[i]])) {
            jj = edges[[i]][j];
            if (jj > i) {
                diff = diff + (degree[i] - degree[jj])^2;
            }
        }
    }
    return(diff);
},


randicIndex = function(alpha = 1) {
    randic = 0;
    for (i in 1 : numVertices) {
        for (j in 1 : length(edges[[i]])) {
            jj = edges[[i]][j];
            if (jj > i) {
                randic = randic + (degree[i] * degree[jj])^alpha;
            }
        }
    }
    return(randic);
},

printGraph = function(printDistances = FALSE) {

    printf("GRAPH, num vertices: %4d\n", numVertices);
    for (i in 1 : numVertices) {
        printf("%4d:", i);
        if (length(edges[[i]]) > 0) {
            for (j in 1 : length(edges[[i]])) {
                printf(" %4d", edges[[i]][j]);
            }
        } else {
            printf(" no edges");
        }
        printf("\n");
    }
    if (printDistances) {
        printf("\n");
        for (i in 1 : numVertices) {
            printf("%4d:", i);
            if (length(values[[i]]) > 0) {
                for (j in 1 : length(values[[i]])) {
                    printf(" %5.2f", values[[i]][j]);
                }
            } else {
                printf(" no edges");
            }
            printf("\n");
        }
    }

}

################

    )
)







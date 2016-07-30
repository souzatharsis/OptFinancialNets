testConstraint <- function(settingsTrading  = NULL,
                           settingsStrategy = NULL) {

    TIMEALL = proc.time();

    if (is.null(settingsStrategy)) settingsStrategy = initialiseSettingsStrategy();
    if (is.null(settingsTrading))  settingsTrading  = initialiseSettingsTrading ();

    # These settings cannot be null
    .errorCheckSettingsObjNotNull(settingsTrading);
    .errorCheckSettingsObjNotNull(settingsStrategy);
    
    set.seed(2016);

    db = connectDatabase();

    ###################################################
    # Load all data, prepare everything
    data = .loadData(db=db, 
                     settingsTrading  = settingsTrading, 
                     settingsStrategy = settingsStrategy);
    ###################################################

    printf("\n");

    # Iterating through each rebalance
    currentRebalance = 1;

    end = data$rebalances[currentRebalance];
    begin = end - data$settingsStrategy$inSample + 1;
    if (begin < 2) begin = 2;
    scen = .prepareDataForScenarios(returns = data$returns[begin:end,], wasInIndex=data$wasInIndex[currentRebalance,]);
    
    # Scenarios for assets (date x number assets) and index (dates x 1),
    scenarios          = scen$scenarios;
    referenceScenarios = data$indexReturns[begin:end];
    numCompanies = ncol(scenarios);
 
    # Correlation matrix
    correlationMatrix = cor(scenarios);
    
    # Mantegna transformation
    correlationMatrix = sqrt(2 * (1 - correlationMatrix)); 
    
    #print(round(correlationMatrix, 2));
    #printf("\n");
    
    numVertices = 50;
    vertices = sort(sample(numCompanies, numVertices, replace = FALSE));
    corr = correlationMatrix[vertices, vertices];
    mst = computeMST(corr);

    mst$printGraph();
   
    xij = matrix(0, ncol = numVertices, nrow = numVertices);
    
    for (i in 1 : length(vertices)) {
        ii = vertices[i];
        for (j in 1 : length(mst$edges[[i]])) {
            jj = vertices[mst$edges[[i]][j]];
            printf("%2d %2d = %2d %2d\n", i, mst$edges[[i]][j], ii, jj);
            xij[ii,jj] = 1;
        }
    }

    for (i in 1 : 1) {
        

    }

    ### Closing database
    disconnectDatabase(db);

    printf("\nAll finished in %.2fs\n\n", (proc.time() - TIMEALL)[3]);

}




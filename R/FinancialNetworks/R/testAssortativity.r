testAssortativity <- function(settingsTrading  = NULL,
                              settingsStrategy = NULL,
                              settingsNews     = NULL,
                              settingsTest     = NULL) {

    TIMEALL = proc.time();

    if (is.null(settingsStrategy)) settingsStrategy = initialiseSettingsStrategy();
    if (is.null(settingsTrading))  settingsTrading  = initialiseSettingsTrading ();
    if (is.null(settingsTest))     settingsTest     = initialiseSettingsTest    ();

    # These settings cannot be null
    .errorCheckSettingsObjNotNull(settingsTrading);
    .errorCheckSettingsObjNotNull(settingsStrategy);
    .errorCheckSettingsObjNotNull(settingsTest);
    .errorCheckSettingsObjCanBeNull(settingsNews);
    
    set.seed(settingsTest$seed);

    db = connectDatabase();

    ###################################################
    # Load all data, prepare everything
    data = .loadData(db=db, 
                     settingsTrading  = settingsTrading, 
                     settingsStrategy = settingsStrategy,
                     settingsNews     = settingsNews);
    ###################################################

    printf("\n");

    # Iterating through each rebalance
    for (currentRebalance in 1 : data$numRebalances) {
        
        end = data$rebalances[currentRebalance];
        begin = end - data$settingsStrategy$inSample + 1;
        if (begin < 2) begin = 2;
        scen = .prepareDataForScenarios(returns = data$returns[begin:end,], wasInIndex=data$wasInIndex[currentRebalance,]);
        companiesThatCanBeChosen = scen$columns;
        
        # Scenarios for assets (date x number assets) and index (dates x 1),
        scenarios          = scen$scenarios;
        referenceScenarios = data$indexReturns[begin:end];
        numCompanies = ncol(scenarios);
 
        printf("First matrix of returns has %d companies, goes from %d to %d (%d days)\n\n", numCompanies, data$dates[begin], data$dates[end], nrow(scenarios));            
        # Correlation matrix
        correlationMatrix = cor(scenarios);
        
        # Mantegna transformation
        correlationMatrix = sqrt(2 * (1 - correlationMatrix)); 
        
        #print(round(correlationMatrix, 2));
        #printf("\n");
        
        
        assortativity = rep(0, settingsTest$numRandomTests);
        randic        = rep(0, settingsTest$numRandomTests);
        squaredDiff   = rep(0, settingsTest$numRandomTests);
        absoluteDiff  = rep(0, settingsTest$numRandomTests);

        printf("Computing MSTs...");
        for (j in 1 : settingsTest$numRandomTests) {
            if (j %% 5 == 0) printf(" %d", j);
            numVertices = settingsTest$verticesInTree;
            if (settingsTest$verticesInTree == 0) 
                numVertices = sample(settingsTest$minVerticesInTree:ncol(scenarios), 1);

            vertices = sort(sample(numCompanies, numVertices, replace = FALSE));
            corr = correlationMatrix[vertices, vertices];
            mst = computeMST(corr);

            #mst$printGraph();

            assortativity[j] = mst$assortativityIndex();
            randic       [j] = mst$randicIndex       ();
            squaredDiff  [j] = mst$squaredDiff       ();
            absoluteDiff [j] = mst$absoluteDiff      ();
        }
        printf("\n");
       
        ordAssortativity = order(assortativity, decreasing = TRUE);
        ordRandic        = order(randic,        decreasing = TRUE);
        ordSquaredDiff   = order(squaredDiff );
        ordAbsoluteDiff  = order(absoluteDiff);

        printf("   #     A    R   SD   AD   Assort.  Abs.Diff\n");
        for (i in 1 : settingsTest$numRandomTests) {
            printf("%4d: %4d %4d %4d %4d %9.5f %9.0f\n", i, ordAssortativity[i], ordRandic[i], ordSquaredDiff[i], ordAbsoluteDiff[i], 
                                                         assortativity[ordAssortativity[i]], absoluteDiff[ordAbsoluteDiff[i]]);
            #if (i %% 20 == 0) printf("   #     A    R   SD   AD\n");
        }
        printf("\n");
        printf("Correlation assortativity / randic:       %7.4f\n", cor(ordAssortativity, ordRandic      ));
        printf("Correlation assortativity / squaredDiff:  %7.4f\n", cor(ordAssortativity, ordSquaredDiff ));
        printf("Correlation assortativity / absoluteDiff: %7.4f\n", cor(ordAssortativity, ordAbsoluteDiff));
        
        diff1 = differenceBetweenSameNumbersInTwoVectors(ordAssortativity, ordRandic);
        diff2 = differenceBetweenSameNumbersInTwoVectors(ordAssortativity, ordSquaredDiff)
        diff3 = differenceBetweenSameNumbersInTwoVectors(ordAssortativity, ordAbsoluteDiff);


        
        printf("\nSum of difference in positions:\n");
        printf("Assortativity / randic:       %5d\n", diff1);
        printf("Assortativity / squaredDiff:  %5d\n", diff2);
        printf("Assortativity / absoluteDiff: %5d\n", diff3);
        
        printf("Assortativity / rev absoDiff: %5d\n", differenceBetweenSameNumbersInTwoVectors(ordAssortativity, rev(ordAbsoluteDiff)));
        break;
    }


    # TODO normalizar medidas por tamanho da árvore? Isso depende do número de vértices

    ### Closing database
    disconnectDatabase(db);

    printf("\nAll finished in %.2fs\n\n", (proc.time() - TIMEALL)[3]);

}




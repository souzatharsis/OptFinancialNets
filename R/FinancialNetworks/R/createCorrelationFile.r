createCorrelationFile <- function(settingsTrading  = NULL,
                                  settingsStrategy = NULL) {

    TIMEALL = proc.time();

    if (is.null(settingsStrategy)) settingsStrategy = initialiseSettingsStrategy();
    if (is.null(settingsTrading))  settingsTrading  = initialiseSettingsTrading ();

    .errorCheckSettingsObjNotNull(settingsTrading);
    .errorCheckSettingsObjNotNull(settingsStrategy);
    
    db = connectDatabase();

    ###################################################
    # Load all data, prepare everything
    data = .loadData(db=db, 
                     settingsTrading  = settingsTrading, 
                     settingsStrategy = settingsStrategy);
    ###################################################
    
    disconnectDatabase(db);

    printf("\n");

    currentRebalance = 1;
    end = data$rebalances[currentRebalance];
    begin = end - data$settingsStrategy$inSample + 1;
    if (begin < 2) begin = 2;
    scen = .prepareDataForScenarios(returns = data$returns[begin:end,], wasInIndex=data$wasInIndex[currentRebalance,]);
    companiesThatCanBeChosen = scen$columns;
        
    # Scenarios for assets (date x number assets) and index (dates x 1),
    scenarios          = scen$scenarios;
    referenceScenarios = data$indexReturns[begin:end];
    numCompanies = ncol(scenarios);
 
    # Correlation matrix
    correlationMatrix = cor(scenarios);
        
    # Mantegna transformation
    correlationMatrix = sqrt(2 * (1 - correlationMatrix)); 

    print(correlationMatrix);

    #print(scenarios);

    
    dataFile = "correlation.txt";
    fileConn <- file(dataFile, "w");
    
    # Number of companies
    writeLines(paste0(nrow(correlationMatrix)), con=fileConn);
    writeLines("", con=fileConn);
   
    # First the index scenarios
    for (i in 1 : (nrow(correlationMatrix)-1)) {
        str = "";
        for (j in (i+1) : ncol(correlationMatrix)) {
            if (str != "") str = paste0(str, " ");
            str = paste0(str, sprintf("%.8f", correlationMatrix[i,j]));
        }
        writeLines(str, con=fileConn);
    }
    close(fileConn);  


    printf("\nAll finished in %.2fs\n\n", (proc.time() - TIMEALL)[3]);

}




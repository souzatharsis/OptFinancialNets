# loadNews               -> If 1, news will be loaded from the database and impact will be computed. If 0, there will be 
#                           vectors of impacts, but they will be zero
# newsSource              -> "ravenpack", "thomsonReuters"
# newsRelevance           -> 0 to 100
# positiveImpactThreshold -> 50 to 100
# negativeImpactThreshold -> 0 to 50
# minutesDecay            -> minutes for half decat
# includeMarketCap        -> market capitalisation
# includeCategories       -> categories in impact calculation
#
initialiseSettingsNews <- function(loadNews                  = 0,             # Use news analytics
                                  newsSource                 = "ravenpack",   # News source
                                  newsRelevance              = 100,
                                  positiveImpactThreshold    = 51,
                                  negativeImpactThreshold    = 49,
                                  minutesDecay               = 90,            # This Impact decay rate
                                  limitCompaniesByNewsFlow   = 0,
                                  includeMarketCap           = 0,
                                  includeCategories          = 0) {

    settingsNews <- settingsNewsObject$new(
        loadNews                 = loadNews,
        newsSource               = newsSource,
        newsRelevance            = newsRelevance,
        positiveImpactThreshold  = positiveImpactThreshold,
        negativeImpactThreshold  = negativeImpactThreshold,
        limitCompaniesByNewsFlow = limitCompaniesByNewsFlow,
        minutesDecay             = minutesDecay,
        includeMarketCap         = includeMarketCap,
        includeCategories        = includeCategories);


    settingsNews$prepareAndCheckData();

    return(settingsNews);
}

settingsNewsObject <- setRefClass("settingsNewsObject", 
    
    fields = list(
        loadNews                 = "numeric",
        newsSource               = "character",
        newsRelevance            = "numeric",
        positiveImpactThreshold  = "numeric",
        negativeImpactThreshold  = "numeric",
        limitCompaniesByNewsFlow = "numeric",
        minutesDecay             = "numeric",
        includeMarketCap         = "numeric",
        includeCategories        = "numeric"
    ),

    methods = list(

################
# Methods (non idented with the definition of the class)

prepareAndCheckData = function() {

    .errorCheckBinaryParameter(loadNews);
    .errorCheckBinaryParameter(includeMarketCap);
    .errorCheckBinaryParameter(includeCategories);
    .errorCheckBinaryParameter(limitCompaniesByNewsFlow);
    .errorCheckIntegerBetweenZeroAnd100(newsRelevance);
    .errorCheckIntegerBetweenZeroAnd100(positiveImpactThreshold);
    .errorCheckIntegerBetweenZeroAnd100(negativeImpactThreshold);
    .errorCheckPositiveInteger(minutesDecay);

    if (newsSource != "ravenpack" && newsSource != "thomsonReuters") stop0("News source must be ravenpack or thomsonReuters");

    if (positiveImpactThreshold < negativeImpactThreshold) stop0("Error: Negative impact threshold is bigger than the positive one.");

    if (loadNews == 0) limitCompaniesByNewsFlow <<- 0;

},

getAsString = function() {
    s = "";
    s = paste0(s, "settingsNews:loadNews ",                 loadNews,                 "\n");
    s = paste0(s, "settingsNews:newsSource ",               newsSource,               "\n");
    s = paste0(s, "settingsNews:newsRelevance ",            newsRelevance,            "\n");
    s = paste0(s, "settingsNews:positiveImpactThreshold ",  positiveImpactThreshold,  "\n");
    s = paste0(s, "settingsNews:negativeImpactThreshold ",  negativeImpactThreshold,  "\n");
    s = paste0(s, "settingsNews:limitCompaniesByNewsFlow ", limitCompaniesByNewsFlow, "\n");
    s = paste0(s, "settingsNews:minutesDecay ",             minutesDecay,             "\n");
    s = paste0(s, "settingsNews:includeMarketCap ",         includeMarketCap,         "\n");
    s = paste0(s, "settingsNews:includeCategories ",        includeCategories,        "\n");
    return(s);
}

################

    )
)








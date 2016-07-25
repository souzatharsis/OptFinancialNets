initialiseSettingsReturn <- function(perfectDirection = 0,
                                     amountPerfect    = 0,
                                     logitTolerance   = 0,
                                     rsiRatio         = 0.33,
                                     rsiDays          = 15) {


    settingsReturn <- settingsReturnObject$new(
        perfectDirection           = perfectDirection,
        amountPerfect              = amountPerfect,
        logitTolerance             = logitTolerance,
        rsiRatio                   = rsiRatio,
        rsiDays                    = rsiDays);

    settingsReturn$prepareAndCheckData();

    return(settingsReturn);
}

settingsReturnObject <- setRefClass("settingsReturnObject", 
    
    fields = list(
        perfectDirection           = "numeric",
        amountPerfect              = "numeric",
        logitTolerance             = "numeric",
        rsiRatio                   = "numeric",
        rsiDays                    = "numeric"
    ),

    methods = list(

################
# Methods (non idented with the definition of the class)

prepareAndCheckData = function() {

    .errorCheckBinaryParameter(perfectDirection);
    .errorCheckBetweenZeroAndOne(amountPerfect);
    .errorCheckBetweenZeroAndOne(logitTolerance);
    .errorCheckBetweenZeroAndOne(rsiRatio);
    .errorCheckPositiveInteger(rsiDays);
    
    if (perfectDirection == 0) amountPerfect <<- 0;
},

getAsString = function() {
    s = "";
    s = paste0(s, "settingsReturn:perfectDirection ", perfectDirection, "\n");
    s = paste0(s, "settingsReturn:amountPerfect ",    amountPerfect,    "\n");
    s = paste0(s, "settingsReturn:logitTolerance ",   logitTolerance,   "\n");
    s = paste0(s, "settingsReturn:rsiRatio ",         rsiRatio,         "\n");
    s = paste0(s, "settingsReturn:rsiDays ",          rsiDays,          "\n");
   return(s);
}


################

    )
)









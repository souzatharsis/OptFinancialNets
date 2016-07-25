initialiseSettingsTest <- function(numRandomTests    = 1,
                                   verticesInTree    = 0,
                                   minVerticesInTree = 5,
                                   seed              = 2016) {

    settingsTest <- settingsTestObject$new(
        numRandomTests    = numRandomTests,
        verticesInTree    = verticesInTree,
        minVerticesInTree = minVerticesInTree,
        seed              = seed);

    settingsTest$prepareAndCheckData();

    return(settingsTest);
}

settingsTestObject <- setRefClass("settingsTestObject", 
    
    fields = list(
        # Basic parameters
        numRandomTests    = "numeric",
        verticesInTree    = "numeric",
        minVerticesInTree = "numeric",
        seed              = "numeric"
    ),

    methods = list(

################
# Methods (non idented with the definition of the class)

prepareAndCheckData = function() {

    .errorCheckPositiveInteger(numRandomTests);
    .errorCheckNonNegativeInteger(verticesInTree);
    .errorCheckNonNegativeInteger(minVerticesInTree);
    .errorCheckNonNegativeInteger(seed);

    if (verticesInTree != 0) minVerticesInTree <<- 0;

}

################

    )
)









.findPropertiesFile <- function (){
    folder = findBasePath();
    fileName = "config/financialNetworks.properties";
    
    

    for (i in 1 : 4) {
        if (file.exists(paste0(folder, fileName))) return (paste0(folder, fileName));
        folder = paste0(folder, "../");
    }

    return("");
}

.findPropertyInPropertiesFile <- function(propertyName) {
    propertiesFile = .findPropertiesFile();
    if (propertiesFile == "") return("");
    
    propertyValue = "";
    conn = file(propertiesFile, "r");
    while (TRUE) {
        line = readLines(conn, n = 1)
        if ( length(line) == 0 ) break;
        temp = noquote(strsplit(line, "\\s", perl=TRUE)[[1]]);
        if (length(temp) == 2 && temp[1] == propertyName) {
            propertyValue = temp[2];
        }
    }
    close(conn);
    propertyValue = as.character(propertyValue);
    return(propertyValue);
}


connectDatabase <- function(path = "") {

    if (path == "") {
        name = .findPropertyInPropertiesFile("databaseName");
        fold = .findPropertyInPropertiesFile("databaseFolder");
        if (name != "" && fold != "") {
            # Adds a / to the end if needed
            if(substr(fold, nchar(fold), nchar(fold)) != "/") fold <- paste0(fold, "/");

            temp = paste0(fold, name);
            if (file.exists(temp)) path = temp;
        }
    }
   
    if (!file.exists(path)) stop0("Database file not found");

    suppressMessages(library("RSQLite"));
    sqlite    <- dbDriver("SQLite")
    db <- dbConnect(sqlite, path)

    return(db);
}
    
    
disconnectDatabase <- function(dbHandle) {
    dbDisconnect(dbHandle);
}

.query <- function(dbHandle, statement) {
    #printf("%s", statement);
    res <- dbSendQuery(conn=dbHandle, statement=statement);
    results <- fetch(res, n=-1);
    dbClearResult(res);
    return(results);
}


.queryAlteration <- function(dbHandle, statement) {
    #printf("%s", statement);
    res <- dbSendQuery(conn=dbHandle, statement=statement);
    dbClearResult(res);
}

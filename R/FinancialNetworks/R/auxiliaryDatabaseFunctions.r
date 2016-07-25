

###################################################################
###################################################################
###################################################################
###################################################################
##### AUXILIARY FUNCTIONS, NOT SUPPOSED TO BE CALLED DIRECTLY #####
###################################################################
###################################################################
###################################################################
###################################################################

.selectAndCheckDates <- function(db, initialDate, finalDate, includePreviousDates = 1) {
    date1 = initialDate;
    date2 = finalDate;
    if (initialDate == 0) {
        #res = .query(dbHandle=db, statement="Select MIN(date) as d1 from dates");
        #date1 = res$d1[1]; 
        date1 = 20000101;
    } else {
        if (!checkDateFormat(initialDate)) stop0("Invalid format for initial date, should be YYYYMMDD."); 
    }

    if (finalDate == 0) {
        #res = .query(dbHandle=db, statement="Select MAX(date) as d2 from dates");
        #date2= res$d2[1]; 
        date2 = 20401231;
    } else {
        if (!checkDateFormat(finalDate)) stop0("Invalid format for final date, should be YYYYMMDD."); 
    }
    if (date2 < date1) stop0("Final date is less than initial date.");
    return(list("date1"=date1, "date2"=date2));
}

.checkIndex <- function(db, index="") {
    if (index != "") {
        stat=paste0("Select count(*) as r from marketIndex where id=\"", index, "\"")
        res = .query(dbHandle=db, statement=stat);
        if (res$r == 0) stop0("Index does not exist");
    }
}


.createQuerySelectCompanies <- function(index, initialDate, finalDate, limitToTopCompanies, limitByPercentageMV, valueTopCompanies) {
    stat = "";
    if (!limitToTopCompanies) {
        stat = paste0("select distinct c.ravenpackId as id, c.name as name ",
                     "from company as c, companyIndex as i ",
                     "where i.indexId=\"", index, "\" ", 
                     "and i.companyId = c.ravenpackId and ",
                     "i.startDate < ", as.character(finalDate), 
                     " and i.endDate > ", as.character(initialDate), 
                     " order by c.name");
    } else if (!limitByPercentageMV) {
        stat = paste0("select c.ravenpackId as id, c.name as name, ",
                     "m.startDate as date1 ",
                     "from company as c, companyIndex as i , marketWeight as m ", 
                     "where i.indexId=\"", index, "\" ",
                     "and i.companyId = c.ravenpackId ",
                     "and i.companyId = m.companyId ",
                     "and i.indexId = m.indexId ",
                     "and i.startDate < ", as.character(finalDate), 
                     " and i.endDate > ", as.character(initialDate), 
                     " and m.startDate < ", as.character(finalDate), 
                     " and m.endDate > ", as.character(initialDate), 
                     " and m.position <= ", valueTopCompanies, 
                     " order by m.startDate, c.name");
    } else {
        valueTopCompanies = valueTopCompanies / 100;
        
        stat = paste0("select c.ravenpackId as id, c.name as name, ",
                     "m.startDate as date1 ",
                     "from company as c, companyIndex as i, marketWeight as m ",
                     "where i.indexId=\"", index, "\" ",
                     "and i.companyId = c.ravenpackId ",
                     "and i.companyId = m.companyId ", 
                     "and i.indexId = m.indexId ", 
                     "and i.startDate < ", as.character(finalDate), 
                     " and i.endDate > ", as.character(initialDate), 
                     " and m.startDate < ", as.character(finalDate), 
                     " and m.endDate > ", as.character(initialDate), 
                     " and m.cumulative <= ", valueTopCompanies,
                     " order by m.startDate, c.name");
    }
    return (stat);
}



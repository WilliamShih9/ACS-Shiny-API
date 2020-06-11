library(httr)
library(tidyverse)
library(jsonlite)
library(shiny)

name = "https://api.census.gov/data/"

year = "2018"

acs_version = "/acs/acs5"

allvarnames = paste0(name, year, acs_version, "/groups/")
r0 = GET(allvarnames)
json0 = content(r0, as = "text", encoding = "UTF-8")
data0 = fromJSON(json0)
var_names = data0[[1]][,1]
var_descriptions = str_to_title(data0[[1]][,2])


var_name = var_names[1]
geo_name = "us:*"

apinames = paste0(name, year, acs_version, "?get=group(", var_name, ")&for=", geo_name)


r1 = GET(apinames)
json1 <- content(r1, as = "text", encoding = "UTF-8")
data1 = t(fromJSON(json1))


varcaption = paste0(name, year, acs_version, "/groups/", var_name)
varname = GET(varcaption)
json2 = content(varname, as = "text", encoding = "UTF-8")
data2 = fromJSON(json2)

result = map_chr(data1[,1][-length(data1[,1])], function(x){
    if (x == "GEO_ID"){
        return("Geography")
    }
    else if (x == "NAME"){
        return("Geographic Area Name")
    }
    else{
        return(data2$variables[[x]]$label)
    }
}
)
combineddata = cbind(result, data1[,2][-length(data1[,2])])
noannotate = combineddata[str_which(combineddata[,1], "Annotation", negate = TRUE),]
margin = noannotate[str_which(noannotate[,1], "Margin"),]
actualdata = noannotate[str_which(noannotate[,1], "Margin", negate = TRUE),]

actualdata = actualdata[str_which(actualdata[,1], "Geograph", negate = TRUE),]

if (length(margin) > 2){
    sp = data.table::transpose(str_split(margin[,1], "!!"))[-1]
    level = data.frame(sapply(sp,c))
    margin[,2] = map_chr(margin[,2], function(x){if (x == "-555555555"){return("0")} else{return(x)}})
    result = cbind(level, actualdata[,2], margin[,2])
    len = length(colnames(result))
    colnames(result)[1] = "Primary"
    if (len >= 4){
        colnames(result)[2] = "Secondary"
    } 
    if (len >= 5){
        colnames(result)[3] = "Tertiary"
    }
    if (len >= 6){
        colnames(result)[4] = "Quaternary"
    }
    colnames(result)[len] = "Margin of Error (90%)"
    colnames(result)[len - 1] = "Estimate"
} else if (length(margin) == 2){
    sp = data.table::transpose(str_split(margin[1], "!!"))[-1]
    level = data.frame(sapply(sp,c))
    margin[2] = map_chr(margin[2], function(x){if (x == "-555555555"){return("0")} else{return(x)}})
    result = cbind(level, actualdata[2], margin[2])
    colnames(result)[1] = "Primary"
    colnames(result)[length(colnames(result))] = "Margin of Error"
    colnames(result)[length(colnames(result)) - 1] = "Estimate"
} else {
    sp = data.table::transpose(str_split(actualdata[,1], "!!"))[-1]
    level = data.frame(sapply(sp,c))
    result = cbind(level, actualdata[,2])
    colnames(result)[1] = "Primary"
    len = length(colnames(result))
    if (len >= 3){
        colnames(result)[2] = "Secondary"
    }
    if (len >= 4){
        colnames(result)[3] = "Tertiary"
    }
    if (len >= 5){
        colnames(result)[4] = "Quaternary"
    }
    colnames(result)[len] = "Estimate"
}
result
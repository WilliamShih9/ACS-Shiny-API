library(httr)
library(tidyverse)
library(jsonlite)
library(shiny)
library(DT)
library(readxl)
library(googlesheets4)

MSA <- "www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls"

gs4_deauth()

#Getting MSA and CSA codes
GET(MSA, write_disk(tf <- tempfile(fileext = ".xls")))
df <- read_xls(tf, skip = 2)

df2 = df %>% filter(`Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area")
df3 = distinct(df2, `CBSA Code`, `CBSA Title`)
df3 = filter(df3, !str_detect(`CBSA Title`, "PR"))

MSA_code = as.list(df3[[1]])
names(MSA_code) = paste0(df3[[2]], " MSA")

df4 = distinct(df2, `CSA Code`, `CSA Title`)
df4 = filter(df4, !is.na(`CSA Code`))
df4 = filter(df4, !is.na(`CSA Code`) & !str_detect(`CSA Title`, "PR"))
CSA_code = as.list(df4[[1]])
names(CSA_code) = paste0(df4[[2]], " CSA")

# Getting State Codes
states = "www2.census.gov/programs-surveys/popest/geographies/2019/state-geocodes-v2019.xlsx" 
GET(states, write_disk(tf <- tempfile(fileext = ".xlsx")))
df5 <- read_xlsx(tf, skip = 4)
df5 = filter(df5, `State (FIPS)` != "00")
df5 = arrange(df5, Name)
State_code = as.list(df5[[3]])
names(State_code) = df5[[4]]

USA_code = list("United States")
names(USA_code) = "United States"
all_codes = c(USA_code, MSA_code, CSA_code, State_code)

name = "https://api.census.gov/data/"
year = "2018"

between = "/acs/acs"

acs_version = "5"
api_key = "&key=897f6218f014c945b472ba926f87840f4e196efc"

allvarnames = paste0(name, year, between, acs_version, "/groups/")
acs5 = paste0(name, paste0(2009:2018), between, acs_version, "/groups/")
acs1 = paste0(name, paste0(2005:2018), between, "1", "/groups/")

get_names <- function(link, year, group){
    r0 = GET(link)
    json0 = content(r0, as = "text", encoding = "UTF-8")
    data0 = fromJSON(json0)[[1]]
    data00 = data0[order(data0[,2]),]
    var_names = data00[,1]
    var_descriptions = str_to_title(data00[,2])
    nopuerto = str_which(var_descriptions, "Puerto Rico")
    var_names = var_names[-nopuerto]
    var_descriptions = var_descriptions[-nopuerto]
    return(tibble(var_names = var_names, var_descriptions = var_descriptions,
                  year = year, group = group))
}

get_groups <- function(data){
    var_names = data[[1]]
    var_descriptions = data[[2]]
    
    
    indices = str_which(var_names, "C$")
    this = var_descriptions[indices]
    comparison = var_descriptions[indices - 1]
    
    this_reduced = str_replace(this, " \\([A-HJ-Z a-z]+\\)", "")
    
    compare = (this_reduced == comparison)
    
    groups = vector("list", length = length(indices))
    
    
    for(i in seq_along(this)){
        if (compare[i]){
            groups[[i]] = (indices[i]-1):(indices[i]+8)
        } else{
            groups[[i]] = (indices[i]):(indices[i]+8)
        }
    }
    
    names(groups) = map_chr(groups, function(x) str_replace(var_descriptions[x[1]], 
                                                            " \\([A-HJ-Z a-z]+\\)", ""))
    which_income = str_which(names(groups), "Inflation")
    income = groups[which_income]
    race = groups[-which_income]
    all_indices = as.numeric(unlist(groups))
    any_income = str_which(var_descriptions, "Inflation")
    other_income = any_income[!any_income %in% all_indices]
    indices = 1:length(var_descriptions)
    
    
    other_indices = indices[!indices %in% c(other_income, all_indices)]
    
    names(other_indices) = var_descriptions[other_indices]
    names(other_income) = var_descriptions[other_income]
    return(tibble::lst(var_names, income, race, other_income, other_indices))
}


get_data <- function(x, data2){
    r1 = GET(x)
    json1 <- content(r1, as = "text", encoding = "UTF-8")
    data1 = t(fromJSON(json1))
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
    ret = cbind(actualdata[,2], margin[,2])
    return(ret)
}
get_data2 <- function(x, data2){
    r1 = GET(x)
    json1 <- content(r1, as = "text", encoding = "UTF-8")
    data1 = t(fromJSON(json1))
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
    return(cbind(actualdata[2], margin[2]))
}
get_data3 <- function(x, data2){
    r1 = GET(x)
    json1 <- content(r1, as = "text", encoding = "UTF-8")
    data1 = t(fromJSON(json1))
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
    return(actualdata[,2])
}

acsfile = "STA141B-Shiny-Spring2020-ACS"

years1 = paste0(2005:2018)
years5 = paste0(2009:2018)


acsdata = range_read(
    "https://docs.google.com/spreadsheets/d/1paIAyl-Mo4obUpAn-pxOWp5pTXTmjzt4kB2QnzC-sqc/edit#gid=778098391")
#From lines 136-152, uses googlesheets4 package
#Google credentials a problem for shinyapps.io so just use direct link

"bool = gs4_find(acsfile)

if (nrow(bool) == 0){
    result = get_names(acs1[1], years1[1], 1)
    for (i in seq_along(acs1)[-1]){
        result = bind_rows(result, get_names(acs1[i], years1[i], 1))
    }
    for (i in seq_along(acs5)){
        result = bind_rows(result, get_names(acs5[i], years5[i], 5))
    }
    result = unite(result, delineate, year, group)
    z = pivot_wider(result, names_from = delineate, values_from = var_descriptions)
    gs4_create(name = acsfile, sheets = z)
} else{
    #acsdata = range_read(bool)
}"

other = acsdata %>% 
    pivot_longer(-var_names, names_to = "delineate", values_to = "var_descriptions")

acsdata = other %>% 
    separate(delineate, sep = "_", into = c("year", "type"))

acsdata_NA = filter(acsdata, !is.na(var_descriptions))
further = group_split(acsdata_NA, type, year, keep = FALSE)

group = lapply(further, get_groups)
group_current = group[[15]]

#ACS 1-Year is from 2005-2018
#ACS 5-Year is from 2009-2018

#1-14 is ACS 1-Year 2005-2018
#15-24 is ACS 5-Year 2009-2018

#For ACS 1-Year it is (year-2004)
#For ACS 5-Year it is (year-1994)

categ =  c("Non-Income Variables By Race",
           "Income Variables By Race",
           "Income Variables Not By Race",
           "All Other Variables")

races = c("American Indian And Alaska Native Alone",
          "Asian Alone",
          "Black Or African American Alone",
          "Hispanic Or Latino",
          "Native Hawaiian And Other Pacific Islander Alone",
          "Some Other Race Alone",
          "Two Or More Races",
          "White Alone",
          "White Alone, Not Hispanic Or Latino")


ui <- fluidPage(
    titlePanel("American Community Survey"),
    fluidRow(
        column(2,
            selectizeInput("version", "Version", c("ACS 1-Year", "ACS 5-Year"),
                        selected = "ACS 5-Year")),
        column(1,
            selectizeInput("yr", "Year", paste0(2009:2018))),
        column(3,
            selectizeInput("category", "Category", c("-"))),
        column(6,
            selectizeInput("var", "Variable", c("-"), width = "100%"))
    ),
    fluidRow(
        column(4,
            selectizeInput("race", "Race", choices = NULL,
                           multiple = TRUE)),        
        column(5,
            selectizeInput("geo", "Geography", names(all_codes),
                           multiple = TRUE, options = list(maxItems = 3))),
        column(3,
            selectizeInput("otheryrs", "Years Included", choices = NULL,
                           multiple = TRUE)),
    ),
    fluidRow(       
        column(3,
               actionButton("update", "Update")),
    ),
        h4(textOutput("title")),
        dataTableOutput("resulttable", width = "90%")
    )




server <- function(input, output, session){
    values = reactiveValues(group = group_current, codes = all_codes, 
                            year = "2018", version = "5", category = "1",
                            table = NULL)
    observeEvent(input$version, {
        output$title = NULL
        output$resulttable = NULL
        if(input$version == "ACS 1-Year"){
            updateSelectizeInput(session, "yr", "Year", c(paste0(2005:2018)), server = TRUE) 
            values$version = "1"
        } else if (input$version == "ACS 5-Year"){
            updateSelectizeInput(session, "yr", "Year", c(paste0(2009:2018)), server = TRUE) 
            values$version = "5"
        }
    })
    observeEvent(input$yr, {
        output$title = NULL
        output$resulttable = NULL
        if (input$yr %in% c(2009:2018)){
            vect = categ
            updateSelectizeInput(session, "category", "Category", vect)
            values$year = input$yr
            if (values$version == "5"){
                values$group = group[[as.numeric(input$yr) - 1994]]
            } else{
                values$group = group[[as.numeric(input$yr) - 2004]]
            }
        }
    })
    observeEvent(c(input$version, input$yr, input$category), {
        if (values$version == "5"){
            values$group = group[[as.numeric(input$yr) - 1994]]
        } else{
            values$group = group[[as.numeric(input$yr) - 2004]]
        }
            if (input$category == categ[1]){
                vect = names(values$group$race)
                updateSelectizeInput(session, "var", "Variable", vect)
                values$category = "1"
            } else if (input$category == categ[2]){
                vect = names(values$group$income)
                updateSelectizeInput(session, "var", "Variable", vect)
                values$category = "2"
            } else if (input$category == categ[3]){
                vect = names(values$group$other_income)
                updateSelectizeInput(session, "var", "Variable", vect)
                values$category = "3"
            } else if (input$category == categ[4]){
                vect = names(values$group$other_indices)
                updateSelectizeInput(session, "var", "Variable", vect)
                values$category = "4"
            }
    })
    observeEvent(c(input$version, input$yr, input$var), {
        if (values$version == "5"){
            values$group = group[[as.numeric(input$yr) - 1994]]
        } else{
            values$group = group[[as.numeric(input$yr) - 2004]]
        }
            if (values$category == "1"){
                index = values$group$race[[input$var]]
                if (length(index) == "9"){
                    result = races
                }
                else{
                    result = c("Total", races)
                }
                updateSelectizeInput(session, "race", "Race", result)
            } else if (values$category == "2"){
                index = values$group$income[[input$var]]
                if (length(index) == "9"){
                    result = races
                }
                else{
                    result = c("Total", races)
                }
                updateSelectizeInput(session, "race", "Race", result)
            } else if (values$category == "3"){
                updateSelectizeInput(session, "race", "Race", "Total")
            } else if (values$category == "4"){
                updateSelectizeInput(session, "race", "Race", "Total")
            }
    })
    observeEvent(input$var,{
        filt = filter(acsdata_NA, 
               var_descriptions == input$var, 
               type == as.numeric(values$version))
        if (nrow(filt) > 14){
            filt = filter(filt, str_detect(var_names, "^B"))
        }
        years_included = unique(select(filt, year)[[1]])
        updateSelectizeInput(session, "otheryrs", "Years Included",
                             choices = years_included, selected = input$yr)
    })
    observeEvent(input$update, {
        if (!is.null(input$var) && input$var != "-" && !is.null(input$geo) &&
            !is.null(input$version) && !is.null(input$yr) &&
            !is.null(input$category) && input$category != "-"){   
            if (values$category == "1" | values$category == "2"){
                if(values$category == "1"){
                    left = values$group$race[[input$var]]
                } else{
                    left = values$group$income[[input$var]]
                } 
                add = (length(left) >= 10)
                index = sapply(input$race, function(x){
                        if (x == "Total"){
                            return(left[1])
                        } else{
                            return(left[match(input$race, races) + add])
                        }
                    })
            } else if (values$category == "3"){
                index = values$group$other_income[input$var]
            } else if (values$category == "4"){
                index = values$group$other_indices[input$var]
            }
            var_name = values$group$var_names[index]
            geo_name = sapply(input$geo, function(x){
                if (x == "United States"){
                    return("us:*")
                }  else if (str_detect(x, "MSA")){
                    return(paste0("metropolitan%20statistical%20area/micropolitan%20statistical%20area:",
                                      all_codes[x]))
                } else if (str_detect(x, "CSA")){
                    return(paste0("combined%20statistical%20area:",
                                      all_codes[x]))
                } else{
                    return(paste0("state:", all_codes[x]))
                }
            })
            var_name = values$group$var_names[index]
            apinames = levels(interaction(name, input$otheryrs, between, values$version, "?get=group(", var_name, ")&for=", geo_name, api_key,sep=''))
            apinames = sort(apinames)[[1]]
            
            # Sorted by Year, then Race, then Area
            if (http_error(apinames)){
            } else{
            r1 = GET(apinames)
            json1 <- content(r1, as = "text", encoding = "UTF-8")
            data1 = t(fromJSON(json1))
            varcaption = paste0(name, values$year, between, values$version, "/groups/", var_name)
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
                result = cbind(level, actualdata[,2], margin[,2])

                len = length(colnames(result))
                colnames(result)[1] = "Primary"
                if (len >= 4){
                    colnames(result)[2] = "2nd"
                } 
                if (len >= 5){
                    colnames(result)[3] = "3rd"
                }
                if (len >= 6){
                    colnames(result)[4] = "4th"
                }
                if (len >= 7){
                    colnames(result)[5] = "5th"
                }
                if (len >= 8){
                    colnames(result)[6] = "6th"
                }                
                if (length(apinames) > 1){
                       #result = cbind(result, sapply(apinames[-1], function(x) get_data(x, data2)))         
                }
                colnames(result)[len] = "Margin of Error (90%)"
                colnames(result)[len - 1] = "Estimate"
                result[,len] = prettyNum(result[,len], preserve.width = "common",big.mark = ",")
                result[,len - 1] = prettyNum(result[,len - 1], preserve.width = "common",big.mark = ",")
            } else if (length(margin) == 2){
                sp = data.table::transpose(str_split(margin[1], "!!"))[-1]
                level = data.frame(sapply(sp,c))
                margin[2] = map_chr(margin[2], function(x){if (x == "-555555555"){return("0")} else{return(x)}})
                result = cbind(level, actualdata[2], margin[2])
                if (length(apinames) > 1){
                    #result = cbind(result, sapply(apinames[-1], get_data2, data2))
                }
                colnames(result)[1] = "Primary"
                len = length(colnames(result))
                colnames(result)[len] = "Margin of Error"
                colnames(result)[len - 1] = "Estimate"
                result[,len] = prettyNum(result[,len], preserve.width = "common", big.mark = ",")
                result[,len - 1] = prettyNum(result[,len - 1], big.mark = ",")
            } else {
                sp = data.table::transpose(str_split(actualdata[,1], "!!"))[-1]
                level = data.frame(sapply(sp,c))
                result = cbind(level, actualdata[,2])

                colnames(result)[1] = "Primary"
                len = length(colnames(result))
                if (len >= 3){
                    colnames(result)[2] = "2nd"
                } 
                if (len >= 4){
                    colnames(result)[3] = "3rd"
                }
                if (len >= 5){
                    colnames(result)[4] = "4th"
                }
                if (len >= 6){
                    colnames(result)[5] = "5th"
                }
                if (len >= 7){
                    colnames(result)[6] = "6th"
                }                
                if (length(apinames) > 1){
                    #result = cbind(result, sapply(apinames[-1], get_data3, data2))
                }
                
                colnames(result)[len] = "Estimate"
                result[,len] = prettyNum(result[,len], big.mark = ",")
            }
            text = paste0("Table ", var_name, ": ", input$var, " (", input$race, ")")
            output$title = renderText({text})
            output$resulttable = renderDataTable({
                datatable(result, options = list(
                    scrollX = TRUE
                ))})
            }
        }
    })
}

shinyApp(ui, server)

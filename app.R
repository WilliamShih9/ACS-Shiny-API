library(httr)
library(Cairo)
library(tidyverse)
library(jsonlite)
library(viridis)
library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)
library(readxl)
library(Rcpp)
library(shinyWidgets)

# Read CSV directly to save time instead of downloading it from online
##################################
'
temp = tempfile()


file = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1317&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPALTT01USA661S&scale=left&cosd=1960-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-03-04&revision_date=2024-03-04&nd=1960-01-01"
download.file(file, temp)


CPI = read_csv(temp)
CPI[1] = format(CPI[[1]], "%Y")
CPI = filter(CPI, DATE >= 2005)
CPI[[2]] = CPI[[2]]/100
'
################################
CPI = read_csv("CPI.csv")
CPI = setNames(as.list(CPI[[2]]/100), year(CPI[[1]]))



# List of Races
races = c("White Alone",
          "Black Or African American Alone",
          "American Indian And Alaska Native Alone",
          "Asian Alone", 
          "Native Hawaiian And Other Pacific Islander Alone", 
          "Some Other Race Alone",  
          "Two Or More Races",
          "White Alone, Not Hispanic Or Latino",
          "Hispanic Or Latino")

race_order = c("Total", races)


##### Getting MSA and CSA codes
MSA <- "www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls"

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

###### Getting State Codes
states = "www2.census.gov/programs-surveys/popest/geographies/2022/state-geocodes-v2022.xlsx" 
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
year = "2022"

between = "/acs/acs"

acs_version = "5"
api_key = "&key=897f6218f014c945b472ba926f87840f4e196efc"


acs5year_range = c(2009:2019,2021:2022)
acs1year_range = c(2005:2019,2021:2022)

allvarnames = paste0(name, year, between, acs_version, "/groups/")
acs5 = paste0(name, paste0(acs5year_range), between, acs_version, "/groups/")
acs1 = paste0(name, paste0(acs1year_range), between, "1", "/groups/")

# Get all variable names and variable descriptions that exist of each year of the ACS
get_names <- function(link, year, group){
    r0 = GET(link)
    json0 = content(r0, as = "text", encoding = "UTF-8")
    data0 = jsonlite::fromJSON(json0)[[1]]
    data00 = data0[order(data0[,2]),]
    var_names = data00[,1]
    var_descriptions = str_to_title(data00[,2])
    nopuerto = str_which(var_descriptions, "Puerto Rico")
    var_names = var_names[-nopuerto]
    var_descriptions = var_descriptions[-nopuerto]
    return(tibble(var_names = var_names, 
                  year = year, 
                type = group, var_descriptions = var_descriptions))
}

# Organizes the variable names (C++ version)
Rcpp::cppFunction('
    DataFrame get_groups_Cpp(DataFrame acsdata_NA){
        std::vector<std::string> races(9);
        races[0] = "American Indian And Alaska Native Alone";
        races[1] = "Asian Alone";
        races[2] = "Black Or African American Alone";
        races[3] = "Hispanic Or Latino";
        races[4] = "Native Hawaiian And Other Pacific Islander Alone";
        races[5] = "Some Other Race Alone";
        races[6] = "Two Or More Races";
        races[7] = "White Alone";
        races[8] = "White Alone, Not Hispanic Or Latino";
        std::vector<std::string> var_names = acsdata_NA[0];
        std::vector<std::string> var_descriptions = acsdata_NA[3];
        std::vector<int> var_type = acsdata_NA[2];
        int rows = var_descriptions.size();
        std::vector<std::string> all_races(rows, "Total");
        std::vector<std::string> all_groups(rows, "Other");
        std::vector<std::string> all_detailed(rows, "Yes");
        std::vector<std::string> all_race_group(rows, "No");
        std::vector<std::pair<std::string, int>> race_vars;
        for (int i = 0; i < rows; i++){
            std::string comparison(1, var_names[i].back());
            if (comparison == "A"){
                std::string save = var_names[i].substr(0, var_names[i].size() - 1);
                std::pair<std::string, int> p1(save, var_type[i]);
                race_vars.push_back(p1);
            }
            std::string comparison2(1, var_names[i][0]);
            if (comparison2 == "C"){
                all_detailed[i] = "No";
            }
            for (int j = 0; j < races.size(); j++){
                if (var_descriptions[i].find(races[j]) != std::string::npos){
                    all_races[i] = races[j];
                    break;
                }
            }
        } 
        std::set<std::pair<std::string, int>> race_vars2(race_vars.begin(), race_vars.end());
        for (int i = 0; i < rows; i++){
            char end_char = var_names[i].back();
            std::string comp = var_names[i];
            if (isalpha(end_char)){
                comp.pop_back();
            }
            std::pair<std::string, int> p1(comp, var_type[i]);
            if (race_vars2.find(p1) != race_vars2.end()){
                all_race_group[i] = "Yes";
                all_groups[i] = "Non-Income By Race";
                if (var_descriptions[i].find("Dollars") != std::string::npos){
                    all_groups[i] = "Income By Race";
                }
            }
            else if (var_descriptions[i].find("Inflation") != std::string::npos){
                all_groups[i] = "Income Not by Race";
            }
        }
        acsdata_NA.push_back(all_races, "Race");
        acsdata_NA.push_back(all_groups, "Group");
        acsdata_NA.push_back(all_detailed, "Detailed");
        acsdata_NA.push_back(all_race_group, "RaceGroup");
        return(acsdata_NA);
    }
')


get_groups2 <- function(acsdata_NA){
    var_names = acsdata_NA[[1]]
    var_descriptions = acsdata_NA[[4]]
    
    
    race_vars = var_names[str_which(var_names, "A$")]
    race_vars = unique(substr(race_vars, 1, nchar(race_vars)-1))
    all_races = replicate(length(var_descriptions), "Total")
    all_groups = replicate(length(var_descriptions), "Other")
    all_detailed = replicate(length(var_descriptions), "Yes")
    not_detailed = str_which(var_names, "^C")
    all_detailed[not_detailed] = "No"
    two_digit = substr(var_names, 2, 3)
    for (i in seq_along(var_descriptions)){
        for (j in seq_along(races)){
            if (grepl(races[j], var_descriptions[i], fixed = TRUE)){
                all_races[i] = races[j]
                break
            }
        }
        for (j in seq_along(race_vars)){
            if (grepl(race_vars[j], var_names[i], fixed = TRUE)){
                all_groups[i] = "Non-Income By Race"
                if (grepl("Inflation", var_descriptions[i], fixed = TRUE)){
                    all_groups[i] = "Income By Race"
                }
                break
            }
            else if (grepl("Inflation", var_descriptions[i], fixed = TRUE)){
                all_groups[i] = "Income Not by Race"
                break
            }
        }
    }
    acsdata_NA["Race"] = all_races
    acsdata_NA["Group"] = all_groups
    acsdata_NA["Detailed"] = all_detailed
    return(acsdata_NA)
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

# Organizes the JSON data from multiple links into a single table without description (no rownames)
get_mass_data <- function(list1, inflation = FALSE){
    return(lapply(list1,
           function(x){
               data = jsonlite::fromJSON(x)
               combine = cbind(data[2, str_which(data[1,], "[0-9]E$")], 
                               data[2, str_which(data[1,], "[0-9]M$")])
               #combine[,2] = map_chr(combine[,2], function(x){if (x == "-555555555"){return("0")} else{return(x)}})
               if (inflation){
                 adjust = CPI[['2022']]/CPI[[substr(x, 29, 32)]]
                 combine[,2] = as.character(round(as.numeric(combine[,2])*adjust))
                 combine[,1] = as.character(round(as.numeric(combine[,1])*adjust))
                
                }
               if (is.null(dim(combine))){
                   combine = t(as.matrix(combine))
               }
               colnames(combine) = c("Estimate","Margin of Error (90%)")
               return(combine)
            }))
}

# Organizes the JSON data from the link into table with descriptions (with rownames)
get_data <- function(link1, link2, inflation = FALSE){
    data1 = t(jsonlite::fromJSON(link1))
    if (data1[1,1] == "GEO_ID"){
        data1 = data1[-1,]
    }
    data2 = jsonlite::fromJSON(link2)
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
    margin_index = intersect(str_which(combineddata[,1], "Margin"), 
                                      str_which(combineddata[,1], "Annotation", negate = TRUE))
    margin = combineddata[margin_index,]
    data_index = intersect(str_which(combineddata[,1], "Margin", negate = TRUE), 
                           str_which(combineddata[,1], "Annotation", negate = TRUE))
    data_index = data_index[1:length(margin_index)]
    actualdata = combineddata[data_index,]   
    if (inflation){
        adjust = CPI[['2022']]/CPI[[substr(link1, 29, 32)]]
        if (is.null(dim(actualdata))){
            actualdata = t(as.matrix(actualdata))
            margin = t(as.matrix(margin))
        }
        actualdata[,2] = as.character(round(as.numeric(actualdata[,2])*adjust))
        margin[,2] = as.character(round(as.numeric(margin[,2])*adjust))
    }
    return(list(margin, actualdata))
}

# Gets the list of groups (for purposes of graphing) from the results table
getVarList <- function(names_total){
    if (ncol(names_total) == 1){
        return(names_total[[1]])
    }
    names_remove = apply(names_total[-ncol(names_total)], 1, paste, collapse = "/")
    names_unique = unique(names_remove)
    names_unique = str_remove_all(names_unique, "/NA")
    names_uniqueremove = names_unique
    if (ncol(names_total) > 2){
        # Is there a subset for all of names_unique. If no sub-variables, remove
        temp = sapply(names_remove, str_count, coll(names_unique))
        temp = as.matrix(temp)
        count = apply(temp, 1, sum)
        names_uniqueremove = names_unique[count > 2]
    }
    if (length(names_uniqueremove) == 0){
        print(names_unique)
        return(names_unique[[1]])
    }
    return(names_uniqueremove)
}
years1 = paste0(acs1year_range)
years5 = paste0(acs5year_range)

# Save time by reading directly from CSV instead of calling API many times to get variable names, which should
# be updated once per year anyways
################
"
result = get_names(acs1[1], years1[1], 1)
for (i in seq_along(acs1)[-1]){
    result = bind_rows(result, get_names(acs1[i], years1[i], 1))
}
for (i in seq_along(acs5)){
    result = bind_rows(result, get_names(acs5[i], years5[i], 5))
}

acsdata = get_groups_Cpp(result)
write.csv(acsdata, file = 'acsdata.csv')
"
################
# First column is useless
acsdata = read_csv("acsdata.csv")[-1]

# Filter out year counts because the same description needs to be used for all of the years
acsdata$var_descriptions = 
    str_remove(acsdata$var_descriptions, "In 2[0-9][0-9][0-9] ") 

# Filter those that are all races
acsdata_All = filter(acsdata, (Race == "Total" | (Group == "Other" | Group == "Income Not by Race")))
group = sort(unique(acsdata_All$Group))

acsdata_All1 = filter(acsdata, type == 1 & (Race == "Total" | (Group == "Other" | Group == "Income Not by Race")))
acsdata_All5 = filter(acsdata, type == 5 & (Race == "Total" | (Group == "Other" | Group == "Income Not by Race")))

var_group1 = sapply(group_split(acsdata_All1,Group), function(x) unique(x$var_descriptions))
var_group5 = sapply(group_split(acsdata_All5,Group), function(x) unique(x$var_descriptions))

#ACS 1-Year is from 2005-2022
#ACS 5-Year is from 2009-2022

wrapper <- function(x, ...)({
    paste(strwrap(x, ...), collapse = "\n")
})


ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
        tags$style(
            "#div_id .selectize-control.single .selectize-input:after{
            content: none;
            }"
        )
    ),
    titlePanel("American Community Survey (2005-2022)"),
    sidebarLayout(
        sidebarPanel(id = "Sidebar",
            selectizeInput("version", "Version", c("ACS 1-Year", "ACS 5-Year"),
                        selected = "ACS 5-Year"),
            selectizeInput("category", "Category", group, 
                           selected = "Non-Income By Race"),        
            selectizeInput("var", "Variable", var_group5[[3]], 
            selected = "Sex By Age", width = "100%"),
        tags$div(id = "div_id"), 
            pickerInput("yr", "Year", paste0(acs5year_range),
                           selected = "2022", multiple = TRUE, width = "100%",
                        options = list(`actions-box` = TRUE)),
            pickerInput("race", "Race", choices = NULL,
                           multiple = TRUE, width = "100%"),        
            pickerInput("geo", "Geography", names(all_codes),
                           select = "United States",
                           multiple = TRUE, options = pickerOptions(
                               maxOptions = 10,
                               liveSearch = TRUE
                           )),
            selectizeInput("detailed", "More Detailed", choices = NULL),
               prettyCheckbox("error", "Include Margin of Error (90%)",
               value = TRUE,
               icon = icon("check")),
               prettyCheckbox("inflation", "Inflation-Adjusted 2022 dollars (CPI) (SEE Information tab)",
               value = FALSE,
               icon = icon("check")),
               prettyCheckbox("simplify", "Minimize Column Descriptions",
                              value = FALSE,
                              icon = icon("check")),
              actionButton("update", "Update"),
        ),
        mainPanel(id = "Main",
            bsButton("toggleSidebar", "Toggle sidebar", type = "toggle", value = TRUE),
            tabsetPanel(type = "tabs",
                tabPanel("Table",
                    downloadBttn("downloaddata", "Download Column Names"),
                    downloadBttn("downloaddatafull", "Download Data With Column Names"),
                    h4(textOutput("title")),
                    dataTableOutput("resulttable", width = "100%")),
                tabPanel("Graph Options", 
                         textOutput("optionstext"),
                         pickerInput("vargroup", label = "Select Group of Variables", choices = character(0)),
                         prettyCheckboxGroup("varchart", label = "List of Variables", choices = character(0)),
                         radioGroupButtons("barorline", label = "Line Graph (All Years) or Bar Graph (Latest Year)", choices = c("Line Graph", "Bar Graph")),
                         actionButton("graphupdate", "Make Table and Chart Graph")),
                tabPanel("Graph Table",
                         h4(textOutput("graphtitle")),
                        dataTableOutput("graphtable", width = "100%")),
                tabPanel("Graph", 
                         downloadBttn("downloadgraph", "Download Graph"),
                         plotOutput("graphing")),
                tabPanel("Information",
                         verbatimTextOutput("infotext"))
            )
        )
    )
)

server <- function(input, output, session){
    output$infotext = renderText({"
        Each query takes about 1 second, so adding a large number of years, races, and 
        geographies will take an extremely long time for the data to load. Turns out 
        the API system is bad for loading multiple years simulatenously, as after 
        all, in most case, loading all the years takes too long. It was unanticipated by me
        that it would be this inefficient to download the data from all the years from 
        the API for any variable. Also, I thought the data tables would be 
        consistent from year to year but they are not, which makes the program 
        less likely to produce a table successfully more complicated the data request (especially 
        adding multiple years and geographies) is. I only added MSAs, CSAs, and states
        as I believed the data would be consistent for regions
        with large population. This assumption too is wrong. Some geographies may work
        and others won't work even if the population size is large for these geographies.
        If a table is not produced properly, the title of the table is replaced by an error message.
    
        Version: There are two versions of the American Community Survey 
        available, the ACS 1-Year and the ACS 5-Year. The ACS 3-Year
        was discontinued years ago and is no longer available. The ACS 
        1-Year covers the year indicated, but the ACS 5-Year covers 
        the year indicated and the previous years. That means that 
        the ACS 1-Year for 2018 covers just 2018. But the ACS 5-Year 
        for 2018 covers 2014-2018, kind of representing 2016 on average.
        That means the ACS 5-Year mostly represents 2 years prior
        to the year indicated. This is important for interpreting the data.
        
        Category: There are four categories that were divided. If the 
        data was separable by race, I put it into Income By Race
        and Non-Income by Race. If the data did not have multiple tables
        for each race, I put it into Income Not By Race and Other.
        The Income categories were meant to be categories where 
        the 'Adjust for Inflation' option would be applicable. But there is a 
        problem. Many of the tables that mention income have their data 
        as counting (counting households by income), not represented as income.
        So the 'Adjust for Inflation' option will wrongly change adjust
        for inflation when the underlying data is for counting. So
        do not adjust for inflation if the underlying data is the count.
        
        Detailed: There is another bug for the more detailed and less detailed
        option. For some years, more detailed is available and for some
        years detailed is not available. In this case, if the option 
        that doesn't exist is selected, the program will crash.
        
        There is a significant problem where data tables between
        tables are inconsistent and will vary in their format from year
        to year.For example, despite that the table code and name are the same,
        the formatting of the table changes. In these cases, the program will crash.
        After all, the ACS was never meant to compare multiple years, so
        it is not convenient to do so.
        
        Minimize Column Descriptions reduces the clutter of the table.
        It was onl added because this is similar to how the data.census.gov
        website displays the data, but it is essentially pointless.
        
        There are download buttons integrated into the data.table.
        The data.table can be copy and pasted, downloaded as Excel,
        downloaded as CSV, or downloaded as PDF. However,
        the data.table download does not contain the full column names,
        since the full column names consist of multiple rows and 
        you can only have one column name per column. Thus, there 
        are two extra buttons: One just to download the column names
        (called Download Column Names)
        and one to download the column names all concanated into
        one name per column and the data below that (called Download Data with Column Names).
        the dataas well.
        
        You can toggle the sidebar to make the table fullsize. This feature
        is needed because the tables are often very wide and the 
        information in the sidebar is useless and unnecessary while reading 
        the table. Thus, I can view more infrmation in the table and graphs when the sidebar
        is toggled. The size of the table should automatically adjust when 
        toggling the sidebar. Switch between tabs to adjust the graph. 
        The size of the graph table should automatically
        adjust by pressing one the column names.
        
        The graph options tab allow conversion from the wide table format 
        to the long table format and also to 
        graph the data, but only for one group of variables.
        One complicated thing about these tables from the ACS is that 
        there are many sublayers. So only one sublayer of any 
        variable is allowed to be chosen to make the graph table and graph.
        
        The graph options tab allows making for a line graph (all years possible)
        and a bar graph (latest year in the query). One problem with
        these graphs is that the names get extremely long.
        Sometimes the geograph name is very long or the race label is very long.
        So the graph might turn out to be small relative to the graph legend
        which is bad. If there are an extreme number of data points,
        then the legend will overlap with the title and go offscreen.
        This is another reason why only sublayer is allowed to chosen,
        as these graphs get unreadable as more variables are added.
        A line graph for just 1 year is possible but it is hard to interpret
        the data, so a line graph should not be used for 1 year.
        Sometimes, the graph options are glitched due to some edge case,
        and some options that should be available are not available.
        Sometimes, graph options can lead to crashes because of a bug in
        the program. This is likely due to inconsistencies between different 
        years (same problem as above) that cause problems when I try to 
        combine the years together. Also, all geographies and races from the 
        'Table' tab are automatically chosen for the graphs and graph table.
        
        The graph table (underlying data for the graph) can be downloaded
        as a CSV, Excel, or PDF as well. The automated graph is probably
        not optimal and a better version can be made for that specific variable.
        
        The graph can be downloaded as a .png file. Another thing to
        note about these graphs is that all the graphs for the same
        table and sublayer have the same title. So it can be possible
        to make many different graphs with the same title. There 
        is no good solution to this as making the graph title more
        informative would make it way too long.
        "})
    output$optionstext = renderText({"Choose the group of variables that you want to select.
                                    Only one group of variables can be selected. A group of variables
                                    is all the variables with the same prefix and one level down. This
                                also allows you to view and download the data in narrow format rather than 
                                wide format as in the first tab. Also,
        the table allows you to use the data to customize your own graph. Only bar graphs (one year)
        and line graphs (all years) are supported."})
    # values$data_complete[[1]] is the printed data (incomplete column names)
    # values$data_complete[[2]] is the name of the table
    # values$data_complete[[3]] is the column names separated by rows
    # values$data_complete[[4]] is the column names concatenated that replaces column names of the pritned data
    # values$data_complete[[5]] is the columns left of the data
    # values$data_complete[[6]] is the graph table
    # values$data_complete[[7]] is the graph itself
    # buffer is the filtered acsdata so that filtering again takes less time
    values = reactiveValues(data_complete = list(1,2,3,4,5,6), buffer = acsdata, var_description = "Sex By Age", 
                            year = "2022", version = "5", group = "Non-Income By Race")
    observeEvent(input$version, {    
        if(input$version == "ACS 1-Year"){
            values$version = 1
        }
        else if (input$version == "ACS 5-Year"){
            values$version = 5
        }
        yr_range = unique(filter(acsdata_All, type == values$version & var_descriptions == 
                       values$var_description)$year)
        if (values$year %in% yr_range){
            this = values$year
            values$buffer = filter(acsdata_All, type == values$version & var_descriptions == 
                                       values$var_description)
        }
        else{
             Maximum = filter(acsdata_All, type == values$version & Group == values$group)
             values$buffer = Maximum
             values$var_descriptions = Maximum$var_descriptions[1]
             yr_range = unique(filter(Maximum, 
                                    var_descriptions == values$var_description)$year)
            if (values$year %in% yr_range){
                this = values$year
            }
             else{
                 this = max(yr_range)
             }
            values$year = this
        }
        updatePickerInput(session, "yr", "Year", yr_range, select = this)
    })
    observeEvent(input$category, {
        values$group = input$category
        if (input$version == 1){
            var_group = var_group1
        }
        else {
            var_group = var_group5
        }
        if (input$category == group[1]){
            var_range =  var_group[[1]]
        }
        else if(input$category == group[2]){
            var_range = var_group[[2]]
        }
        else if(input$category == group[3]){
            var_range = var_group[[3]]
        }
        else if(input$category == group[4]){
            var_range = var_group[[4]]
        }
        var_range = var_range[var_range %in% unique(filter(acsdata_All, type == values$version))$var_descriptions]
        this = var_range[1]
        Maximum = filter(acsdata_All, type == values$version &
                         Group == values$group & var_descriptions == this)
        values$buffer = Maximum
        if (Maximum$RaceGroup[1] == "Yes"){
            updatePickerInput(session, "race", "Race", race_order,
                                 select = "Total")
        }
        else{
            updatePickerInput(session, "race", "Race", "Total",
                                 select = "Total")
        }
        yr_range = unique(Maximum$year)
        values$year = max(yr_range) 
        values$var_description = this
        updateSelectizeInput(session, "var", "Variable", 
                                 var_range, select = this, server = TRUE)
        updatePickerInput(session, "yr", "Year", yr_range, select = values$year)
    })
    observeEvent(c(input$version, input$var), {
        values$var_description = input$var
        Maximum = filter(acsdata_All, type == values$version &
                             Group == values$group & var_descriptions == input$var)
        yr_range = unique(Maximum$year)
        this = max(yr_range)
        values$year = this  
        constant_name = filter(Maximum, year == values$year)$var_names[1]
        Maximum = filter(Maximum, var_names == constant_name)
        yr_range = unique(Maximum$year)
        ###############
        ## This part is for determining the "More Detailed" 
        Remaining = filter(acsdata_All, type == values$version &
                               Group == values$group & var_descriptions == input$var)
        values$buffer = Remaining
        Remaining = substring(Remaining$var_names, 1, 1)
        Details = c()
        if ("C" %in% Remaining){
            Details = c(Details, "No")
        }
        if ("B" %in% Remaining){
            Details = c(Details, "Yes")
        }
        ##########
        updateSelectizeInput(session, "detailed", "More Detailed", Details, 
                             server = TRUE)
        updatePickerInput(session, "yr", "Year", yr_range, select = this) 
    })
    observeEvent(input$yr, {
        values$year = input$yr
    })
    observe({
        rows = nrow(values$data_complete[[1]])
        if (!is.null(rows)){
            shinyjs::enable("downloaddata")
            shinyjs::enable("downloaddatafull")
        }
        else{
            shinyjs::disable("downloaddata")
            shinyjs::disable("downloaddatafull")
        }
    })
    observeEvent(input$toggleSidebar, {
        if(input$toggleSidebar == TRUE) {
            removeCssClass("Main", "col-sm-12")
            addCssClass("Main", "col-sm-8")
            shinyjs::show(id = "Sidebar")
            shinyjs::enable(id = "Sidebar")
        }
        else {
            removeCssClass("Main", "col-sm-8")
            addCssClass("Main", "col-sm-12")
            shinyjs::hide(id = "Sidebar")
        }
        output$resulttable = renderDataTable(
            values$datatable)
    })
    observeEvent(input$update, {
        if (!is.null(input$yr) & !is.null(input$race)){
            shinyjs::enable("downloaddata")
            left = filter(values$buffer, year == max(values$year) &
                           type == values$version & grepl(values$var_description, var_descriptions, fixed = TRUE)
                       & Detailed == input$detailed)
            var_name = left$var_names[1]
            race_list = c()
            race_factors = c()
            for (i in seq_along(input$race)){
                if (input$race[i] == "Total"){
                    race_list = c(race_list, "")
                    race_factors = c(race_factors, "Total")
                }
                else if (input$race[i] == "White Alone, Not Hispanic Or Latino"){
                    race_list = c(race_list, "H")
                    race_factors = c(race_factors, "White Alone, Not Hispanic Or Latino")
                }
                else if (input$race[i] == "White Alone"){
                    race_list = c(race_list, "A")
                    race_factors = c(race_factors, "White Alone")
                }
                else if (input$race[i] == "Two Or More Races"){
                    race_list = c(race_list, "G")
                    race_factors = c(race_factors, "Two Or More Races")
                }
                else if (input$race[i] == "Some Other Race Alone"){
                    race_list = c(race_list, "F")
                    race_factors = c(race_factors, "Some Other Race Alone")
                }
                else if (input$race[i] == "Native Hawaiian And Other Pacific Islander Alone"){
                    race_list = c(race_list, "E")
                    race_factors = c(race_factors, "Native Hawaiian And Other Pacific Islander Alone")
                }
                else if (input$race[i] == "Some Other Race Alone"){
                    race_list = c(race_list,  "F")
                    race_factors = c(race_factors, "Some Other Race Alone")
                }
                else if (input$race[i] == "Hispanic Or Latino"){
                    race_list = c(race_list, "I")
                    race_factors = c(race_factors, "Hispanic Or Latino")
                }
                else if (input$race[i] == "Black Or African American Alone"){
                    race_list = c(race_list, "B")
                    race_factors = c(race_factors, "Black Or African American Alone")
                }
                else if (input$race[i] == "Asian Alone"){
                    race_list = c(race_list, "D")
                    race_factors = c(race_factors,  "Asian Alone")
                }
                else if (input$race[i] == "American Indian And Alaska Native Alone"){
                    race_list = c(race_list, "C")
                    race_factors = c(race_factors,  "American Indian And Alaska Native Alone")
                }
            }
            var_name = paste0(var_name, race_list)
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
                year_factors = values$year
                race_factors = race_factors
                geo_factors = names(sort(geo_name))
                col_labels = expand.grid(year_factors,race_factors,geo_factors)
                if (input$inflation & (input$category == "Income By Race" | input$category == "Income Not by Race")){
                    adjust_inflation = TRUE
                }
                else{
                    adjust_inflation = FALSE
                }
                apinames = levels(interaction(name, values$year, between, values$version, "?get=group(", var_name, ")&for=", geo_name, api_key,sep=''))
                varcaption = levels(interaction(name, values$year, between, values$version, "/groups/", var_name, sep = ''))
                combined = get_data(apinames[1], varcaption[1], adjust_inflation)
                margin = combined[[1]]
                margin = as.matrix(margin)
                margin[,1] = str_remove_all(margin[,1], ":")
                margin[,1] = str_remove_all(margin[,1], "2[0-9][0-9][0-9] ") 
                actualdata = combined[[2]]
                otherdata = get_mass_data(apinames[-1], adjust_inflation)
                test = TRUE
                if (length(margin) > 2){
                    sp = data.table::transpose(str_split(margin[,1], "!!"))[-1]
                    level = data.frame(sapply(sp,c))
                    result = cbind(level, actualdata[,2], margin[,2])
                    oldlen = length(colnames(result))
                    colnames(result)[1] = "Primary"
                    if (oldlen >= 4){
                        colnames(result)[2] = "2nd"
                    } 
                    if (oldlen >= 5){
                        colnames(result)[3] = "3rd"
                    }
                    if (oldlen >= 6){
                        colnames(result)[4] = "4th"
                    }
                    if (oldlen >= 7){
                        colnames(result)[5] = "5th"
                    }
                    if (oldlen >= 8){
                        colnames(result)[6] = "6th"
                    }                    
                    colnames(result)[oldlen] = "Margin of Error (90%)"
                    colnames(result)[oldlen - 1] = "Estimate"  
                    row_keep = nrow(result)
                    if (length(apinames) > 1){
                        consistency = sapply(otherdata, nrow)
                        compare = nrow(result)
                        test = all(consistency %% compare == 0)
                        if (test){
                            result = cbind(result, otherdata)
                            result = result[1:row_keep,]
                            all_data = result
                            newlen = ncol(result)
                            for (i in (oldlen-1):newlen){
                                result[,i] = replace_na(result[,i], "-1")
                                result[,i] = prettyNum(result[,i],preserve.width = "common",big.mark = ",")
                            }
                        }
                    }
                    else{
                        newlen = ncol(result)
                        for (i in (oldlen-1):newlen){
                            result[,i] = replace_na(result[,i], "-1")
                            result[,i] = prettyNum(result[,i],preserve.width = "common",big.mark = ",")
                        }
                    }
                } else if (length(margin) == 2){
                    sp = data.table::transpose(str_split(margin[1], "!!"))[-1]
                    level = data.frame(sapply(sp,c))
                    margin[2] = map_chr(margin[2], function(x){if (x == "-555555555"){return("0")} else{return(x)}})
                    result = cbind(level, actualdata[2], margin[2])
                    colnames(result)[1] = "Primary"
                    oldlen = length(colnames(result))
                    colnames(result)[oldlen] = "Margin of Error (90%)"
                    colnames(result)[oldlen - 1] = "Estimate"  
                    row_keep = nrow(result)
                    if (length(apinames) > 1){
                        result = cbind(result, otherdata)
                    }
                    result = result[1:row_keep,]
                    all_data = result
                    newlen = ncol(result)
                    for (i in (oldlen-1):newlen){
                        result[,i] = prettyNum(result[,i],preserve.width = "common",big.mark = ",")
                    }
                } else {
                    sp = data.table::transpose(str_split(actualdata[,1], "!!"))[-1]
                    level = data.frame(sapply(sp,c))
                    result = cbind(level, actualdata[,2])
    
                    colnames(result)[1] = "Primary"
                    oldlen = length(colnames(result))
                    if (oldlen >= 3){
                        colnames(result)[2] = "2nd"
                    } 
                    if (oldlen >= 4){
                        colnames(result)[3] = "3rd"
                    }
                    if (oldlen >= 5){
                        colnames(result)[4] = "4th"
                    }
                    if (oldlen >= 6){
                        colnames(result)[5] = "5th"
                    }
                    if (oldlen >= 7){
                        colnames(result)[6] = "6th"
                    }                
                    colnames(result)[oldlen] = "Estimate"
                    newlen = oldlen
                    result[,oldlen] = prettyNum(result[,len], big.mark = ",")
                } 
                if (!test){
                    text = paste0("Number of rows for the tables are ", compare, ",", paste(consistency, collapse = ','),
                                  " Tables cannot be merged. The table format
                                  is not the same for all the geographies, races, and/or years
                                  , so the table is invalid. The order of the number of rows
                                  may give a clue to which geographies, races, and years are 
                                  incompatible with each other.")
                    output$title = renderText({text})
                }
                else{
                    text = paste0("Table ", var_name[1], ": ", input$var, ' ', input$version)
                   
                    output$title = renderText({text})
                    values$complete_data[[4]] = result[1:(oldlen-2)]
                    if (input$error == TRUE){
                        sketch = htmltools::withTags(table(
                            class = 'display',
                            thead(
                                tr(
                                    lapply(colnames(result)[1:(oldlen-2)], th, rowspan = 4,
                                           style = "border-right: solid 2px;"),
                                    lapply(geo_factors, th, colspan = 2*length(race_factors)*length(year_factors),
                                           style = "border-right: solid 2px;"),
                                ),
                                tr(
                                    lapply(rep(race_factors, length(geo_factors)), th, colspan = 2*length(year_factors),
                                           style = "border-right: solid 2px;")
                                ),
                                tr(
                                    lapply(rep(year_factors, length(race_factors)*length(geo_factors)), th, colspan = 2,
                                           style = "border-right: solid 2px;")
                                ),
                                tr(
                                    lapply(colnames(result)[(oldlen-1):newlen], th)
                                )
                            )
                        ))
                    }
                    else{
                        columns = (oldlen-1):newlen
                        keep_columns = seq(1, length(columns), 2)
                        result = result[c(1:(oldlen-2), columns[keep_columns])]
                        sketch = htmltools::withTags(table(
                            class = 'display',
                            thead(
                                tr(
                                    lapply(colnames(result)[1:(oldlen-2)], th, rowspan = 3,
                                           style = "border-right: solid 2px;"),
                                    lapply(geo_factors, th, colspan = length(race_factors)*length(year_factors),
                                           style = "border-right: solid 2px;"),
                                ),
                                tr(
                                    lapply(rep(race_factors, length(geo_factors)), th, colspan = length(year_factors),
                                           style = "border-right: solid 2px;")
                                ),
                                tr(
                                    lapply(rep(year_factors, length(race_factors)*length(geo_factors)), th, 
                                           style = "border-right: solid 2px;")
                                )
                            )
                        ))
                    }
                    values$data_complete[[1]] = result
                    values$data_complete[[2]] = paste0("Table ", var_name[1], " ", input$var, ' ', input$version)
                    num_estimates = nrow(col_labels)
                    col_labels = col_labels[c(3,2,1)]                
                    values$data_complete[[3]] = t(col_labels)
                    
                    if (input$error == TRUE){
                        col_labels = cbind(V1 = 1:num_estimates, col_labels)
                        col_labels = purrr::map_dfr(seq_len(2), ~col_labels)
                        col_labels = arrange(col_labels, V1)
                        col_labels = col_labels[-1]
                        error_cols = rep.int(c("Estimate","Margin of Error (90%)"), num_estimates)
                        values$data_complete[[3]] = t(cbind(col_labels, error_cols))
                    }        
                    temp = result
                    colnames(temp) = c(colnames(temp)[1:(oldlen-2)], 
                                         apply(values$data_complete[[3]], 2, paste, collapse = "|"))
                    values$data_complete[[4]] = colnames(temp)
                    values$data_complete[[5]] = temp[1:(oldlen-2)]
                    varlist = getVarList(values$data_complete[[5]])
                    if (input$simplify){
                        change = result[1:(oldlen-2)]
                        left = 1:nrow(change)
                        for (i in seq_along(change)){
                            same = change[1,i]
                            for (j in 1:(length(left)-1)){
                                if (change[j+1,i] %in% same){
                                    change[j+1,i] = ""
                                }
                                else{
                                    same = change[j+1,i]
                                }
                            }
                        }
                        result[1:(oldlen-2)] = change
                    }
                    values$datatable = datatable(result, 
                                                 rownames = FALSE,
                                                 container = sketch,
                                                 extensions = c('Buttons','FixedColumns'),
                                                 options = list(
                                                     pageLength = 999,
                                                     dom = 'Bfrtip',
                        buttons = list(
                            list(extend = 'copy', title = text),
                            list(extend = 'csv', title = text),
                            list(extend = 'excel', title = text), 
                            list(extend = 'pdf', title = text),
                            list(extend = 'print', title = text)),
                                                     scrollX = TRUE,
                                                     autoWidth = FALSE,
                                                     fixedColumns = list(leftColumns = oldlen - 2)))
                    output$resulttable = renderDataTable(
                        values$datatable)
                    updatePickerInput(session, "vargroup", choices = varlist, selected = varlist[1])
                }
        }
        observeEvent(input$vargroup, {
            if (!is.null(input$vargroup) && str_length(input$vargroup) > 0){
                filt = apply(values$data_complete[[5]], 1, paste, collapse = "/")
                filt = unique(filt)
                filt = str_remove_all(filt, "/NA")
                    slashes_count = str_count(input$vargroup, "/")
                    vars = filt[str_count(filt, coll("/")) == (slashes_count + 1)]
                    vars_left = str_detect(vars, pattern = coll(input$vargroup))
                    menu = c()
                    if (input$vargroup %in% filt){
                        menu = c(menu, input$vargroup)
                    }
                    menu = c(menu, vars[vars_left])
                    updatePrettyCheckboxGroup(session, "varchart", label = "List of Variables", 
                                              choices = menu, selected = menu)
            }
        })
        observeEvent(input$graphupdate, {
            legit = nrow(values$data_complete[[5]])
            if (length(input$varchart) > 0 && length(input$vargroup) > 0 && 
                !is.null(legit) && !is.null(input$vargroup)){
                filt = apply(values$data_complete[[5]], 1, paste, collapse = "/")
                filt = str_remove_all(filt, "/NA")
                    count = which(filt %in% input$varchart)
                    if (length(count) > 0){
                        data_to_graph = values$data_complete[[1]][count,]
                        count_layers = str_count(input$vargroup, "/") + 1
                        other_counts = c()
                        if (ncol(values$data_complete[[5]]) >= count_layers + 2){
                            other_counts = c((count_layers+2):ncol(values$data_complete[[5]]))
                        }
                        data_to_graph = data_to_graph[-c(1:count_layers, other_counts)]
                        transpose_data = t(data_to_graph)
                        transpose_data = as_tibble(transpose_data)
                        if(ncol(transpose_data) == 1 && ncol(values$data_complete[[5]]) == 1){
                            
                        }
                        else{
                            colnames(transpose_data) = transpose_data[1,]
                            transpose_data = transpose_data[-1,]
                        }
                        columns = t(values$data_complete[[3]])
                        has_error = (ncol(columns) == 4)
                        if (!has_error){
                            colnames(columns) = c("Geography", "Race", "Year")
                        }
                        else{
                            colnames(columns) = c("Geography", "Race", "Year", "Error")
                        }
                        data = cbind(columns, transpose_data)
                        colnames(data) = replace_na(colnames(data), "Total")
                        data = pivot_longer(data, cols = (ncol(columns)+1):ncol(data), names_to = "Variables")
                        if (has_error){
                            data = pivot_wider(data, names_from = Error)
                            data = data %>%
                                mutate(Estimate = as.numeric(gsub(",", "", Estimate)),
                                       Year = as.numeric(Year),
                                       `Margin of Error (90%)` = 
                                           as.numeric(gsub(",", "", `Margin of Error (90%)`)))
                        }
                        else{
                            data = data %>%
                                mutate(Year = as.numeric(Year),
                                    Estimate = as.numeric(gsub(",", "",value)))
                        }
                        text = values$data_complete[[2]]
                        print(text)
                        if(nchar(text) > 60){
                          positions = unlist(gregexpr(' ', text))
                          position_space = positions[positions < 60][length(positions[positions < 60])]
                          text = paste0(substring(text, 1, position_space), "\n", substring(text, position_space+1))
                        }
                        print(text)
                        text = paste0(text, "-", input$vargroup)
                        output$graphtitle = renderText({text})
                        output$graphtable = renderDataTable({datatable(data,
                                                                       rownames = FALSE,
                                                                       extensions = c('Buttons','FixedColumns'),
                                                                       options = list(
                                                                           pageLength = 999,
                                                                           dom = 'Bfrtip',
                                                                           buttons = list(
                                                                               list(extend = 'copy', title = text),
                                                                               list(extend = 'csv', title = text),
                                                                               list(extend = 'excel', title = text), 
                                                                               list(extend = 'pdf', title = text),
                                                                               list(extend = 'print', title = text)),
                                                                           scrollX = TRUE,
                                                                           autoWidth = FALSE))})
                        data = data %>%
                            mutate(Variables = ifelse(Variables == "1", "Total", Variables))
                        values$data_complete[[6]] = data
                        if (input$barorline == "Line Graph"){
                            graph = ggplot(data, aes(x = Year, y = Estimate, shape = Race,
                                                     group = interaction(Geography, Race, Variables), color = interaction(Geography, Variables))) +
                                geom_point(size = 3) +
                                geom_line() + 
                                theme_bw() + 
                                scale_x_continuous(breaks = scales::pretty_breaks())+
                                scale_y_continuous(labels = scales::comma) +
                                ggtitle(wrapper(text, width = 90 ))
                            if (has_error){
                                graph = graph + geom_errorbar(aes(ymin = Estimate - `Margin of Error (90%)`, 
                                                                  ymax = Estimate + `Margin of Error (90%)`),
                                                              width = 0.5)
                            }
                        }
                        else{
                            data = dplyr::filter(data, Year == max(data$Year))
                            graph = ggplot(data, aes(x = Variables, y = Estimate, 
                                                     group = interaction(Geography, Race),
                                                     fill = interaction(Geography, Race))) +
                                geom_bar(stat = "identity", position = "dodge") +
                                scale_y_continuous(labels = scales::comma) +
                                ggtitle(wrapper(text, width = 90)) + 
                                theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1)) +
                                theme_bw()
                            if (has_error){
                                graph = graph + geom_errorbar(aes(ymin = Estimate - `Margin of Error (90%)`, 
                                                                  ymax = Estimate + `Margin of Error (90%)`), 
                                                              width = 0.5, position = position_dodge(0.9))
                            }
                            # Bar Graph
                        }
                        values$data_complete[[7]] = graph
                        output$graphing = renderPlot({graph})
                    }
            }
        })
        output$downloaddata = downloadHandler(
            filename = function(){
                paste(values$data_complete[[2]],'-ColumnNames.csv',sep = '')
            },
            content = function(file){
                write.csv(values$data_complete[[3]], row.names = FALSE, col.names = FALSE, file)
            }
        )
        output$downloaddatafull = downloadHandler(
            filename = function(){
                paste(values$data_complete[[2]],'-Full.csv', sep = '')
            },
            content = function(file){
                temp = values$data_complete[[1]]
                colnames(temp) = values$data_complete[[4]]
                write.csv(temp, row.names = FALSE, file)
            }
        )
        output$downloadgraph = downloadHandler(
            filename = function(){
                paste(values$data_complete[[2]],'-Graph.png', sep = '')
            },
            content = function(file){
                ggsave(file, type = 'cairo', width = 8, height = 6, plot = values$data_complete[[7]])
            }
        )
    })
}

shinyApp(ui, server)

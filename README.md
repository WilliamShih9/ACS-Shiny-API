# STA141B-API
API Project for STA141B

For ACS 1-Year Detailed Tables (2005-2022) and ACS 5-Year Detailed Tables (2009-2022).

All the graphs in the repository were generated automatically using the shiny application.

The updated link to the application is https://williamshih.shinyapps.io/version2/.

This version can have multiple years, multiple geographies, and multiple races selected. The updated 
application can also download data, ,convert from wide format to narrow format, graph data, download the graph, and download the raw data used to make the graph.
Also, the updated version uses a sidebar, which allows for viewing the table in full screen after toggling the sidebar.
The updated version also adds 2019, 2021, and 2022.

The work in progress link is https://williamshih.shinyapps.io/test/.

The work in progress can only query an individual year and an individual race and is much slower to load initially.

The following paragraph of information below this line is also in the "Information" tab of the application.

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
        do not adjust for inflation if the underlying is the count.
        
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


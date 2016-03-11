# US Baby Names

## Synopsis
The purpose of this code is to demonstrate how R can be used with large database files to create beautiful visualizations without loading all observations into R.  A large data file can be queried from R using SQLlite, a dataframe returned, and visualizations of that data created.  This particular database file contains data of all babies born in the United States of America since 1879. 



## Code Example 
The following command, "dbGetQuery", can be used to send SQL code to an established SQL connection that will then return back
the data requested as a data frame.  The data frame can then be used for further statistical analysis, maniplation, visualization and more once in R.  

  Total_Spent <- dbGetQuery(con, "SELECT Customer.LastName...

## Motivation

The motivation for this project is to show how one might take large database queries, return the results as data objects in R, and render those as visualizations.  

## Files Contained Here

Baby_Names.R is the file containing the R code that can query the data from the file and render the charts. 
BabyNames.RMD is a markdown file that creates html using an R package called Rmarkdown and Knitr.  The styling for the webpage was created with knitr-bootstrap.   

The database file is too large to host here, but it can be downloaded here: (https://catalog.data.gov/dataset/baby-names-from-social-security-card-applications-national-level-data) and converted to SQLlite. 

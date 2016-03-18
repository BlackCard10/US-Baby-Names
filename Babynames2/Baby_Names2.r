
# Load the packages 
library(DBI)
library(RSQLite)
library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
library(dplyr)



# Connect to the SQL Database file
con = dbConnect (RSQLite::SQLite(), dbname = "~/Desktop/R Project/Baby_Names/US-Baby-Names/database.sqlite")


# Get a list of all tables and view the tables
alltables = dbListTables(con)


# Turn all tables into dataframes where the first five observations are present
A <- list()
for (i in 1:length(alltables)) {
	A[[i]] <- dbGetQuery(con, paste0("SELECT * FROM ", alltables[i], " LIMIT 5"))
}


# Add the names for easy search
names(A) <- alltables

# What are the names that show significant positive change in the last 50 years? 
Names_Change <- dbGetQuery(con, "SELECT a.Name, a.gender, (b.Count - a.Count) AS Diff
                		FROM NationalNames a
                        JOIN NationalNames b ON (a.Name = b.Name AND a.Gender = b.Gender)
                        WHERE a.Year = 1964
                        AND b.Year = 2014
                        GROUP BY a.name, a.gender, a.Year
                        ORDER BY Diff DESC")

Names_ChangeTop <- Names_Change[1:10, 1:3]
Names_ChangeBottom <- Names_Change[6382:6391, 1:3]

# Query the database for the top names' yearly values since 1964
TopNameValues <- list()
for (i in 1:length(Names_ChangeTop$Name))
TopNameValues[[i]] <- dbGetQuery(con, paste0('SELECT Name, Year, Count 
								FROM NationalNames 
								WHERE Name = ', " \'",Names_ChangeTop$Name[i],"\'", 
                                ' AND Gender = ', " \'",Names_ChangeTop$Gender[i],"\'",
                                ' AND Year BETWEEN 1964 AND 2014'))

# Query the database for the bottom names' yearly values since 1964
BottomNameValues <- list()
for (i in 1:length(Names_ChangeBottom$Name))
  BottomNameValues[[i]] <- dbGetQuery(con, paste0('SELECT Name, Year, Count 
  								FROM NationalNames 
  								WHERE Name = ', " \'",Names_ChangeBottom$Name[i],"\'", 
                                ' AND Gender = ', " \'",Names_ChangeBottom$Gender[i],"\'",
                                ' AND Year BETWEEN 1964 AND 2014'))


# Name the list that contains the data for the bottom names
BottomNameValues$Name <- as.factor(BottomNameValues$Name)
BottomNameValues$Year <- as.factor(BottomNameValues$Year)
df2 <- ldply(BottomNameValues, data.frame)
P2 <- ggplot(df2, aes(x= Year, y= Count, colour= Name , group= Name)) + geom_line() + 
            ggtitle("Names With the Largest Declines")
P2

# Name the list that contains the data for the top names 
Names_ChangeTop$Gender <- as.factor(Names_ChangeTop$Gender)
Names_ChangeTop$Name <- as.factor(Names_ChangeTop$Name)
df <- ldply(TopNameValues, data.frame)
P1 <- ggplot(df, aes(x= Year, y= Count, colour= Name , group= Name)) + geom_line() +
          ggtitle("Names with the Largest Gains")
P1

# Write a function to remove the last character from any given string
LastChar <- function (x) substring(as.character(x), nchar(as.character(x)), 
			nchar(as.character(x)))


# Query the Database for counts of all male names since 1964
MalesSince_1964 <- dbGetQuery(con, "SELECT Year, Count, Name
                                  FROM NationalNames
                                  WHERE Gender = 'M'
                                  AND Year BETWEEN 1964 AND 2014" ) 


# Remove that last character from the name 
MalesSince_1964$Ending <- sapply(MalesSince_1964$Name, LastChar)


# Sum the number of names which end in each letter, grouped by year
Endings1 <- unique(MalesSince_1964$Ending)
ME_1 <- list() 
for (i in 1:length(Endings1)) {
  ME_1[[i]] <- MalesSince_1964 %>% 
    select(Year, Count, Ending) %>% 
    group_by(Year) %>% 
    filter(Ending == Endings1[i]) %>% 
    summarise(Endings1[i], sum(Count))
}
names(ME_1) <- Endings1 

# Add names in dataframes
for (i in 1:length(ME_1)) {
  names(ME_1[[i]]) <- c('Year', "Ending", 'Total')
}

# Clone ME into ME2, combined it into a single vector and plot it 
ME_2 <- ldply(ME_1, data.frame)
MEP <- ggplot(ME_2, aes(x = Year, y = Total, colour = Ending, group = Ending)) + 
			geom_line() +
            ggtitle("Change in Ending Letter")
MEP

# Query the Database for all Female names since 1964
FemalesSince_1964 <- dbGetQuery(con, "SELECT Year, Count, Name 
                                    FROM NationalNames
                                    WHERE Gender = 'F'
                                    AND Year BETWEEN 1964 AND 2014" )


# Remove the last characters from the female names 
FemalesSince_1964$Ending <- sapply(FemalesSince_1964$Name, LastChar)


# Sum the number of names which end in each letter, grouped by year
Endings2 <- unique(FemalesSince_1964$Ending)
FE_1 <- list() 
for (i in 1:length(Endings2)) {
  FE_1[[i]] <- FemalesSince_1964 %>% 
    select(Year, Count, Ending) %>% 
    group_by(Year) %>% 
    filter(Ending == Endings2[i]) %>% 
    summarise(Endings2[i], sum(Count))
}
names(FE_1) <- Endings2

# Add names in dataframes
for (i in 1:length(FE_1)) {
  names(FE_1[[i]]) <- c('Year', "Ending", 'Total')
}

# Clone FE_1 into FE_2, combined it into a single vector and plot it 
FE_2 <- ldply(FE_1, data.frame)
FEP <- ggplot(FE_2, aes(x = Year, y = Total, colour = Ending, group = Ending)) + 
		geom_line() + 
      	ggtitle("Change in Ending Letter: Female Names")
FEP

# Net Change Per Character Males
ChangeAlphaM <- list()
for (i in 1:length(ME_1)) {
  ChangeAlphaM[[i]] <- ME_1[[i]][51,3] - ME_1[[i]][1,3]
}


# Find the characters with the greatest absolute value change over the period in Males
names(ChangeAlphaM) <- names(ME_1)
AlphaM <- ldply(ChangeAlphaM, data.frame)
TopChangeM <- AlphaM[order(-abs(AlphaM$Total)),][1:10, 1]


# Plot the ending values with the greatest absolute value change for Males 
MEP2Sub <- ggplot(ME_2[ME_2$Ending %in% TopChangeM,], aes(x = Year, y = Total, 
		   colour = Ending, group = Ending)) + 
		   geom_line() +
           ggtitle("Change in Ending Letter: Top Male Names")
MEP2Sub

# Net Change Per Character Females
ChangeAlphaF <- list()
for (i in 1:length(FE_1)) {
  ChangeAlphaF[[i]] <- FE_1[[i]][51,3] - FE_1[[i]][1,3]
}

# Find the characters with the greatest absolute value change over the period in Females
names(ChangeAlphaF) <- names(FE_1)
AlphaF <- ldply(ChangeAlphaF, data.frame)
TopChangeF <- AlphaF[order(-abs(AlphaF$Total)),][1:10, 1]


# Plot the ending values with the greatest absolute value change for Females
FEP2Sub <- ggplot(FE_2[FE_2$Ending %in% TopChangeF,], aes(x = Year, y = Total, 
		   colour = Ending, group = Ending)) + 
		   geom_line() +
           ggtitle("Change in Ending Letter: Top Female Names")
FEP2Sub
































  
                           











				
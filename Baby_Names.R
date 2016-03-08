
# Load the packages 
library(DBI)
library(RSQLite)
library(leaflet)
library(ggplot2)
library(grid)
library(gridExtra)
library(maps)
library(mapproj)
library(RColorBrewer)


# Read in a list of states and their abbreviations 
StateAbrev <- read.csv("~/Desktop/states.csv")

# Connect to the SQL Database file
con = dbConnect (RSQLite::SQLite(), dbname = "~/Desktop/R Project/Baby_Names/database.sqlite")


# Get a list of all tables and view the tables
alltables = dbListTables(con)


# Turn all tables into dataframes where the first five observations are present
A <- list()
for (i in 1:length(alltables)) {
	A[[i]] <- dbGetQuery(con, paste0("SELECT * FROM ", alltables[i], " LIMIT 5"))
}


# Add the names for easy search
names(A) <- alltables

# Return the occurance of the name James over time
James_Time <- dbGetQuery(con, "SELECT Year, Sum(Count) AS Count 
                         From NationalNames 
                         WHERE Name = 'James' AND Gender = 'M'
                         GROUP BY Year")
                              

# Plot the occurance of the name James over time
LostG <- ggplot(subset(James_Time, Year > 1883 & Year < 1900 ), aes(x = Year, y = Count)) + 
  ggtitle("Lost Generation")
GreatestG <- ggplot(subset(James_Time, Year > 1901 & Year < 1924 ), aes(x = Year, y = Count)) + 
  ggtitle("Greatest Generation")
SilentG <- ggplot(subset(James_Time, Year > 1925 & Year < 1946 ), aes(x = Year, y = Count)) + 
  ggtitle("Silent Generation")
BabyBoom <- ggplot(subset(James_Time, Year > 1947 & Year < 1964 ), aes(x = Year, y = Count)) + 
  ggtitle("Baby Boomers")
XG <- ggplot(subset(James_Time, Year > 1965 & Year < 1980 ), aes(x = Year, y = Count)) + 
  ggtitle("Generation X")
MillenG <- ggplot(subset(James_Time, Year > 1981), aes(x = Year, y = Count)) + 
  ggtitle("Millennial Generation")


LostG <- LostG + geom_smooth(col = "firebrick") + theme_bw() + 
			theme(axis.title.y = element_text(size = rel(.75))) + 
      theme(axis.title.x = element_text(size = rel(.75))) +
      scale_y_continuous(limits = c(0, 100000))

			
GreatestG <- GreatestG + geom_smooth(col = "firebrick") + theme_bw() + 
      theme(axis.title.x = element_text(size = rel(.75))) + 
			theme(axis.title.y = element_text(size = rel(.75))) +
      scale_y_continuous(limits = c(0, 100000))

SilentG <- SilentG + geom_smooth(col = "firebrick") + theme_bw() + 
      theme(axis.title.x = element_text(size = rel(.75))) + 
			theme(axis.title.y = element_text(size = rel(.75))) + 
      scale_y_continuous(limits = c(0, 100000))
      
			
BabyBoom <- BabyBoom + geom_smooth(col = "firebrick") + 
      theme_bw() + theme(axis.title.x = element_text(size = rel(.75))) + 
			theme(axis.title.y = element_text(size = rel(.75))) +
      scale_y_continuous(limits = c(0, 100000))
			
XG <- XG + geom_smooth(col = "firebrick") + 
      theme_bw() + theme(axis.title.x = element_text(size = rel(.75))) + 
			theme(axis.title.y = element_text(size = rel(.75))) +
      scale_y_continuous(limits = c(0, 100000))
      
			
MillenG <- MillenG + geom_smooth(col = "firebrick") + 
      theme_bw() + theme(axis.title.x = element_text(size = rel(.75))) + 
			theme(axis.title.y = element_text(size = rel(.75))) +
      scale_y_continuous(limits = c(0, 100000))
			
grid.arrange(LostG, GreatestG, SilentG, BabyBoom, XG, MillenG, ncol = 3, 
             top = textGrob("The Name James Across the Generations", gp =gpar(fontsize = 15)))

# Query the DB for the incidence of James by state
James_By_State <- dbGetQuery(con, "SELECT State, sum(Count) AS TotalJ
					          FROM StateNames
					          WHERE Name = 'James'
					          AND Year = 1988
					          GROUP BY State 
                    ORDER BY State")

# Import the shape files for US states
stateShapes <- map("state", plot = FALSE, fill = TRUE)
stateShapes <- fortify(stateShapes)  
uniqueStates <- sort(unique(stateShapes$region))


James_By_State <- James_By_State[-c(1,12),] # Remove Alaska and Hawaii. Only lower 48 + DC are shown
names(James_By_State) <- c("Abbreviation", "TotalJ")
James_By_State <- merge(James_By_State, StateAbrev)
James_By_State <- James_By_State[,-c(1)] # Remove the Abbreviations for States 
names(James_By_State) <- c("TotalJ", "region")  
James_By_State$region <- tolower(James_By_State$region)

# Create data frame for map plot
MapData <- merge(x = stateShapes, y = James_By_State, by = "region", all.x = TRUE)


# Create a color scheme to fit the model
myPal <- colorRampPalette(brewer.pal(9,'Reds'))(20)


# Map the fill color to the number of babies born.  
mapPlot <- ggplot(MapData,
                  aes(x = long, y = lat, group = group,
                      fill = TotalJ))
mapPlot <- mapPlot + geom_polygon(colour = "DarkGrey")
mapPlot <- mapPlot + coord_map(project="conic", lat0 = 30)
mapPlot <- mapPlot + scale_fill_gradientn(colours = myPal)
mapPlot <- mapPlot + ggtitle("Babies Named James Born in 1988")
print(mapPlot)


# Five most popular baby names by Generation
Lost_Gen_Top <- dbGetQuery(con, "SELECT Name, Year, Sum(Count) AS Total
                              FROM NationalNames
                              WHERE Year BETWEEN 1883 AND 1900
                              GROUP BY Name
                              ORDER BY Total DESC
                              Limit 5 ")

Greatest_Gen_Top <- dbGetQuery(con, "SELECT Name, Year, Sum(Count) AS Total
                              FROM NationalNames
                         	    WHERE Year BETWEEN 1901 AND 1924
                         	    GROUP BY Name
                         	    ORDER BY Total DESC
                              Limit 5 ")

Silent_Gen_Top <- dbGetQuery(con, "SELECT Name, Year, Sum(Count) AS Total
                              FROM NationalNames
                         	    WHERE Year BETWEEN 1925 AND 1946
                              GROUP BY Name
                              ORDER BY Total DESC
                              Limit 5 ")

BabyBoom_Gen_Top <- dbGetQuery(con, "SELECT Name, Year, Sum(Count) AS Total
                              FROM NationalNames
                              WHERE Year BETWEEN 1947 AND 1964
                              GROUP BY Name
                              ORDER BY Total DESC
                              Limit 5 ")

X_Gen_Top <- dbGetQuery(con, "SELECT Name, Year, Sum(Count) AS Total
                              FROM NationalNames
                              WHERE Year BETWEEN 1965 AND 1980
                              GROUP BY Name
                              ORDER BY Total DESC
                              Limit 5 ")

Millennial_Gen_Top <- dbGetQuery(con, "SELECT Name, Year, Sum(Count) AS Total
                              FROM NationalNames
                              WHERE Year >= 1980
                              GROUP BY Name
                              ORDER BY Total DESC
                              Limit 5 ")

Lost_Gen_Plot <- ggplot(Lost_Gen_Top, aes(x = Name, y = Total)) + 
                  geom_bar(stat = "identity", fill = "DarkBlue") +
                  theme_bw() + theme(axis.title.x = element_text(size = rel(.30))) + 
                  theme(axis.title.y = element_text(size = rel(.30))) +
                  theme(axis.title.x = element_blank()) +
                  ggtitle("Lost Generation") +
                  scale_y_continuous(limits = c(0, 2500000))

Greatest_Gen_Plot <- ggplot(Greatest_Gen_Top, aes(x = Name, y = Total)) + 
                  geom_bar(stat = "identity", fill = "DarkBlue") +
                  theme_bw() + theme(axis.title.x = element_text(size = rel(.30))) + 
                  theme(axis.title.x = element_blank()) +
                  theme(axis.title.y = element_text(size = rel(.30))) +
                  ggtitle("Greatest Generation") +
                  scale_y_continuous(limits = c(0, 2500000))

Silent_Gen_Plot <- ggplot(Silent_Gen_Top, aes(x = Name, y = Total)) + 
                  geom_bar(stat = "identity", fill = "DarkBlue") +
                  theme_bw() + theme(axis.title.x = element_text(size = rel(.30))) + 
                  theme(axis.title.y = element_text(size = rel(.30))) +
                  theme(axis.title.x = element_blank()) +
                  ggtitle("Silent Generation") +
                  scale_y_continuous(limits = c(0, 2500000))

BabyBoom_Gen_Plot <- ggplot(BabyBoom_Gen_Top, aes(x = Name, y = Total)) + 
                 geom_bar(stat = "identity", fill = "DarkBlue") +
                 theme_bw() + theme(axis.title.x = element_text(size = rel(.30))) + 
                 theme(axis.title.y = element_text(size = rel(.30))) +
                 theme(axis.title.x = element_blank()) +
                 ggtitle("Baby Boomers") + 
                 scale_y_continuous(limits = c(0, 2500000))

X_Gen_Plot <- ggplot(X_Gen_Top, aes(x = Name, y = Total)) + 
                geom_bar(stat = "identity", fill = "DarkBlue") +
                theme_bw() + theme(axis.title.x = element_text(size = rel(.30))) + 
                theme(axis.title.x = element_blank()) +
                theme(axis.title.y = element_text(size = rel(.30))) +
                ggtitle("Generation X ") +
                scale_y_continuous(limits = c(0, 2500000))


Millenial_Gen_Plot <- ggplot(Millennial_Gen_Top, aes(x = Name, y = Total)) + 
                geom_bar(stat = "identity", fill = "DarkBlue") +
                theme_bw() + theme(axis.title.x = element_text(size = rel(.30))) + 
                theme(axis.title.y = element_text(size = rel(.30))) +
                theme(axis.title.x = element_blank()) +
                ggtitle("Millennial") + 
                scale_y_continuous(limits = c(0, 2500000))

grid.arrange(Lost_Gen_Plot, Greatest_Gen_Plot, Silent_Gen_Plot, BabyBoom_Gen_Plot, X_Gen_Plot, Millenial_Gen_Plot, ncol = 3, 
             top = textGrob("The Top Names Across the Generations", gp =gpar(fontsize = 15)))


# First names of the last 10 presidents
celebrity <- c("Jaden" , "Ryder", "Apple", "Blue", "Willow")

celebrityBirthY <- c(1998, 2004, 2004, 2012, 2000)


# Store the results of a query to the database of the year and incidence of a presidential name 
Celeb_Count <- list()
for (i in 1:length(celebrity)) {
	Celeb_Count[[i]] <- dbGetQuery(con, paste0('SELECT Sum(Count) AS Count, Year 
	                                          FROM NationalNames 
	                                          WHERE Name = ' ," \'", celebrity[i], "\'", 
	                                          'GROUP BY Year'))
}

# Name the Pres Count List
names(Celeb_Count) <- celebrity

# Create a table of rectangles that will be used to outline celebrity charts
rect_list <- list()
for (i in 1:length(celebrityBirthY)) {
  rect_list[[i]] <- data.frame(xmin=celebrityBirthY[i], xmax= celebrityBirthY[i] + 10, ymin=-Inf, ymax=Inf)
}


celeb_plot_Jaden <- ggplot(Celeb_Count$Jaden, aes(x = Year, y = Count)) + geom_line(col = "Blue") + 
                    ggtitle("Babies Named Jaden") +
                    geom_rect(data=rect_list[[1]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                    color="Red",
                    alpha=0.5,
                    inherit.aes = FALSE)

celeb_plot_Ryder <- ggplot(Celeb_Count$Ryder, aes(x = Year, y = Count)) + geom_line(col = "Blue") + 
                    ggtitle("Babies Named Ryder") +
                    geom_rect(data=rect_list[[2]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                    color="Red",
                    alpha=0.5,
                    inherit.aes = FALSE)

celeb_plot_Apple <- ggplot(Celeb_Count$Apple, aes(x = Year, y = Count)) + geom_line(col = "Blue") + 
                    ggtitle("Babies Named Apple") +
                    geom_rect(data=rect_list[[3]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                    color="Red",
                    alpha=0.5,
                    inherit.aes = FALSE)
  

celeb_plot_Blue <- ggplot(Celeb_Count$Blue, aes(x = Year, y = Count)) + geom_line(col = "Blue") + 
                    ggtitle("Babies Named Blue") +
                    geom_rect(data=rect_list[[4]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                    color="Red",
                    alpha=0.5,
                    inherit.aes = FALSE)


celeb_plot_Willow <- ggplot(Celeb_Count$Willow, aes(x = Year, y = Count)) + geom_line(col = "Blue") + 
                    ggtitle("Babies Named Willow") + 
                    geom_rect(data=rect_list[[5]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                    color="Red",
                    alpha=0.5,
                    inherit.aes = FALSE)


grid.arrange(celeb_plot_Jaden,celeb_plot_Ryder, celeb_plot_Apple,celeb_plot_Blue, celeb_plot_Willow, ncol = 1, 
             top = textGrob("Celebrity Baby Names", gp =gpar(fontsize = 15)))


# First names of Characters in Friends and Cosby + dates of air 
Friends <- c("Rachel" , "Monica", "Phoebe", "Joey", "Chandler", "Ross")
Friends_Time <- c(2004,2014)
Cosby <- c("Cliff", "Clair", "Denise", "Theo", "Vanessa", "Rudy", "Sandra")
Cosby_Time <- c(1984, 1992)


# Create rect objects from the beginning of the each show to the end 
rect_Friends <- data.frame(xmin=Friends_Time[1], xmax= Friends_Time[2], ymin=-Inf, ymax=Inf)
rect_Cosby <- data.frame(xmin=Cosby_Time[1], xmax= Cosby_Time[2], ymin=-Inf, ymax=Inf)


# Store the results of a query to the database of the Friends Names
Friends_Count <- list()
for (i in 1:length(Friends)) {
  Friends_Count[[i]] <- dbGetQuery(con, paste0('SELECT Sum(Count) AS Count, Year 
                                             FROM NationalNames 
                                             WHERE Name = ' ," \'", Friends[i], "\'", 
                                             'GROUP BY Year'))
}
names(Friends_Count) <- Friends


# Store the results of a query to the database of the Cosby names
Cosby_Count <- list()
for (i in 1:length(Cosby)) {
  Cosby_Count[[i]] <- dbGetQuery(con, paste0('SELECT Sum(Count) AS Count, Year 
                                               FROM NationalNames 
                                               WHERE Name = ' ," \'", Cosby[i], "\'", 
                                               'GROUP BY Year'))
}
names(Cosby_Count) <- Cosby


# Friends Character baby name plots 
TV_plot_Rachel <- ggplot(Friends_Count$Rachel, aes(x = Year, y = Count)) + geom_line(col = "Red") + 
                  ggtitle("Babies Named Rachel") + 
                  geom_rect(data=rect_Friends, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                  color="Red",
                  alpha=0.5,
                  inherit.aes = FALSE)

TV_plot_Monica <- ggplot(Friends_Count$Monica, aes(x = Year, y = Count)) + geom_line(col = "Red") + 
                  ggtitle("Babies Named Monica") +
                  geom_rect(data=rect_Friends, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                  color="Red",
                  alpha=0.5,
                  inherit.aes = FALSE)

TV_plot_Phoebe <- ggplot(Friends_Count$Phoebe, aes(x = Year, y = Count)) + geom_line(col = "Red") + 
                  ggtitle("Babies Named Phoebe") +
                  geom_rect(data=rect_Friends, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                  color="Red",
                  alpha=0.5,
                  inherit.aes = FALSE)

TV_plot_Joey <- ggplot(Friends_Count$Joey, aes(x = Year, y = Count)) + geom_line(col = "Red") + 
                  ggtitle("Babies Named Joey") + 
                  geom_rect(data=rect_Friends, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                  color="Red",
                  alpha=0.5,
                  inherit.aes = FALSE)

TV_plot_Chandler <- ggplot(Friends_Count$Chandler, aes(x = Year, y = Count)) + geom_line(col = "Red") + 
                  ggtitle("Babies Named Chandler") +
                  geom_rect(data=rect_Friends, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                  color="Red",
                  alpha=0.5,
                  inherit.aes = FALSE)

TV_plot_Ross <- ggplot(Friends_Count$Ross, aes(x = Year, y = Count)) + geom_line(col = "Red") + 
                  ggtitle("Babies Named Ross") + 
                  geom_rect(data=rect_Friends, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                  color="Red",
                  alpha=0.5,
                  inherit.aes = FALSE)

Friends_Grid <- grid.arrange(TV_plot_Rachel,TV_plot_Monica,TV_plot_Phoebe,TV_plot_Joey,TV_plot_Chandler,TV_plot_Ross, ncol = 2, 
             top = textGrob("Friends Characters Baby Names", gp =gpar(fontsize = 15)))

Friends_Grid



# Friends Character baby name plots 
TV_plot_Cliff <- ggplot(Cosby_Count$Cliff, aes(x = Year, y = Count)) + geom_line(col = "DarkGreen") + 
                ggtitle("Babies Named Cliff") +
                geom_rect(data=rect_Cosby, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                color="Blue",
                alpha=0.5,
                inherit.aes = FALSE)

TV_plot_Clair <- ggplot(Cosby_Count$Clair, aes(x = Year, y = Count)) + geom_line(col = "DarkGreen") + 
                ggtitle("Babies Named Clair") +
                geom_rect(data=rect_Cosby, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                color="Blue",
                alpha=0.5,
                inherit.aes = FALSE)
  
TV_plot_Denise <- ggplot(Cosby_Count$Denise, aes(x = Year, y = Count)) + geom_line(col = "DarkGreen") + 
                ggtitle("Babies Named Denise") +
                geom_rect(data=rect_Cosby, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                color="Blue",
                alpha=0.5,
                inherit.aes = FALSE)
  
TV_plot_Theo <- ggplot(Cosby_Count$Theo, aes(x = Year, y = Count)) + geom_line(col = "DarkGreen") + 
                ggtitle("Babies Named Theo") +
                geom_rect(data=rect_Cosby, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                color="Blue",
                alpha=0.5,
                inherit.aes = FALSE)
  
TV_plot_Vanessa <- ggplot(Cosby_Count$Vanessa, aes(x = Year, y = Count)) + geom_line(col = "DarkGreen") + 
                ggtitle("Babies Named Vanessa") +
                geom_rect(data=rect_Cosby, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                color="Blue",
                alpha=0.5,
                inherit.aes = FALSE)
  
TV_plot_Rudy <- ggplot(Cosby_Count$Rudy, aes(x = Year, y = Count)) + geom_line(col = "DarkGreen") + 
                ggtitle("Babies Named Rudy") +
                geom_rect(data=rect_Cosby, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                color="Blue",
                alpha=0.5,
                inherit.aes = FALSE)
  
TV_plot_Sandra <- ggplot(Cosby_Count$Sandra, aes(x = Year, y = Count)) + geom_line(col = "DarkGreen") + 
                ggtitle("Babies Named Sandra") +
                geom_rect(data=rect_Cosby, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                color="Blue",
                alpha=0.5,
                inherit.aes = FALSE)
              

Cosby_Grid <- grid.arrange(TV_plot_Cliff,TV_plot_Clair,TV_plot_Denise,TV_plot_Theo,TV_plot_Vanessa,TV_plot_Rudy,TV_plot_Sandra, ncol = 2, 
             top = textGrob("Friends Characters Baby Names", gp =gpar(fontsize = 15)))

Cosby_Grid






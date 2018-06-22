library(RSQLite)
library(DBI)

# Project Directory is in Documents
# getwd()

# https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html

# connect to SQLite database
my_db = dbConnect(SQLite(), dbname = "C:/Users/ayoder/Documents/LevelSender/db/levelsender.sqlite")

# list tables
dbListTables(my_db)
#dbListFields(my_db)
#dbListResults(my_db)
#bListFields(my_db)

# read tables
dbReadTable(my_db, "ConfigEmailSetup")

#myQuery <- dbSendQuery(my_db, "SELECT Time FROM Logger 2 Samples")

dbSendQuery(my_db, "SHOW DATABASES")

?dbSendQuery

dbListFields(my_db)



# Walking through manual: https://cran.r-project.org/web/packages/RSQLite/RSQLite.pdf

# connect to a built in database
db <- datasetsDb()

# list some tables
dbListTables(db)

# read a table
dbReadTable(db, "CO2")
dbGetQuery(db, "SELECT * FROM CO2 WHERE conc < 100")

# disconnect from database
dbDisconnect(db)

# Contains read
library("tidyverse")

# Read/write excel
library("readxl")
library("writexl")

# Connect to databases
library("odbc")
library("RSQLite")


# Read from SQLite DB
conn <- RSQLite::dbConnect(drv = SQLite(), dbname = "data/chinook/Chinook_Sqlite.sqlite")

# List all the tables
RSQLite::dbListTables(conn)

# Read a table. This function does not bring the table into memory. It's a cursor.
tbl(conn, "Album")

tbl(conn, "Album") %>%
  
  # Materializes the cursor view i.e reads the dataset into memory
  collect() %>%
  
  # Gets the 10th row
  slice(10) %>% 
  
  glimpse()

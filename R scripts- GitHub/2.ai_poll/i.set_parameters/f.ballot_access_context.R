# clean workspace
rm(list = ls())
# clean garbadge
gc()
# clear graphics device
# dev.off()
# set decimals to digits instead of scientific
options(scipen = 999)
# set timeout limit (laxed)
options(timeout = 10000)
# set work directory
setwd(dir = "~/Desktop/Mechanical Pollster/")

# load utils for scraping
library(rvest)

# set url of ballot access data
url <- "https://en.wikipedia.org/wiki/Ballot_access_in_the_2024_United_States_presidential_election"

# Read the webpage content
webpage <- read_html(url)

# Extract the table
table <- 
  html_nodes(
    webpage,
    xpath = '//*[@id="General_election"]/following::table[1]'
    )

# Extract the header (I care about the first 10 columns, other headers are for 
# rows)
header <- 
  html_text(
    html_nodes(
      table,
      "th"
      ),
    trim = TRUE
    )[1:10]
header[2] <- 'Electoral votes'

# Remove the unwanted CSS part and clean party names
header <- gsub("@supports\\(writing-mode:vertical-rl\\)\\{.*?\\}\\}\\}", "", header)
# Ensure the names are correctly spaced and add a hyphen between party names and candidates
header <- gsub("([a-z])([A-Z])", "\\1 \\2", header)
header <- gsub("(Constitution|Independent|Libertarian|Democratic|Green|PSL|Republican)(?=[A-Z])", "\\1-", header, perl=TRUE)
header <- gsub(" \\(presumptive\\)", "", header)

# Extract the rows
rows <- html_nodes(table,"tr")

# keep rows if they mention a state
rows <- rows[grepl(paste0('<th>',usmap::fips_info()$abbr,collapse = '|'),rows,ignore.case = FALSE,)]

# extract state vector 
rows_html <- sapply(rows, as.character)

# Function to parse HTML and extract image descriptions
parse_html_table <- function(html) {
  # Remove newline characters
  html <- gsub("\n", "", html)
  
  # Find all table cell matches
  matches <- gregexpr("(<th[^>]*>|<td[^>]*>)(.*?)(</th>|</td>)", html, perl=TRUE)
  cells <- regmatches(html, matches)[[1]]
  
  # Initialize an empty list to store cell contents
  cell_list <- list()
  
  for (cell in cells) {
    # Extract content inside the cell
    content <- sub("(<th[^>]*>|<td[^>]*>)(.*?)(</th>|</td>)", "\\2", cell)
    
    # Extract image description if present
    description <- sub(".*<img [^>]*alt=\"([^\"]*)\".*", "\\1", content)
    
    # If no image description found, use the raw content
    if (description == content) {
      description <- gsub("<[^>]*>", "", content) # Remove any remaining tags
      description <- trimws(description) # Trim leading and trailing whitespace
    }
    
    # Determine colspan attribute if present
    colspan <- as.numeric(sub(".*colspan=\"([0-9]+)\".*", "\\1", cell))
    if (is.na(colspan)) {
      colspan <- 1
    }
    
    # Repeat the description based on colspan and add to list
    for (i in 1:colspan) {
      cell_list <- append(cell_list, description)
    }
  }
  
  return(cell_list)
}

ballot_access <- data.table()
for(s in 1:length(rows)){
  # Parse the HTML string
  parsed_cells <- as.data.table(t(as.character(parse_html_table(rows_html[s]))))
  ballot_access <- rbindlist(list(ballot_access,parsed_cells))
}
names(ballot_access) <- header

# clean ballot access table
ballot_access <-
ballot_access[,
              lapply(
                .SD,
                function(x){
                  ifelse(
                    grepl('as|yes|write-in|registration not required|petitioning finished|write-in',x,ignore.case = TRUE),
                    'Yes','No'
                    )
                  }
                ),
              by = c('State / electors','Electoral votes'),
              .SDcols = c(names(ballot_access)[!names(ballot_access) %in% c('State / electors','Electoral votes')])
              ]

# get rid of minor parties 
ballot_access <- ballot_access[,grepl('State / electors|Electoral votes|Democratic|Independent|Republican|Green|Libertarian',names(ballot_access)),with=F]
names(ballot_access)[grepl('Democratic',names(ballot_access))] <- 'Kamala Harris, the Democratic Party candidate'
names(ballot_access)[grepl('Republican',names(ballot_access))] <- 'Donald Trump, the Republican Party candidate'
names(ballot_access)[grepl('Green',names(ballot_access))] <- 'Jill Stein, the Green Party candidate'
names(ballot_access)[grepl('Kennedy',names(ballot_access))] <- 'Robert F. Kennedy Jr., who is not affiliated with any political party'
names(ballot_access)[grepl('West',names(ballot_access))] <- 'Dr. Cornel West, who is not affiliated with any political party'
names(ballot_access)[grepl('Libertarian',names(ballot_access))] <- 'Chase Oliver, the Libertarian Party candidate'

# save ballot access context 
save(file = 'auxiliary_data/Wikipedia/Ballot access context/ballot_access.RData',ballot_access)

# check random state
ballot_access[sample(1:dim(ballot_access)[1],size = 1)]


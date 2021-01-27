appendixDF <- utils::read.csv("data-raw/codes.csv", stringsAsFactors = FALSE, 
                              colClasses = "character")
usethis::use_data(appendixDF, internal = TRUE, overwrite = TRUE)
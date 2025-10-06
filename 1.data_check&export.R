# read --------------------------------------------------------------------
library(tidyverse)
contains_cyrillic <- function(text) {
    text <- paste0(text, collapse = "")
    str_detect(text, "[\\u0400-\\u04FF]")
}

path <- dir() %>% 
    str_subset("^data.*\\.xlsx$") %>% 
    str_subset("~", negate = TRUE) %>% 
    sort(decreasing = TRUE) %>% 
    `[`(1)

lit <- readxl::read_excel(path, sheet = "lib-data") %>% 
    select(RECORD:taxonRemarks) %>% 
    select(-RECORD, -OCCURRENCE, -EVENT, -LOCATION, -TAXON) %>% 
    mutate_at(c("decimalLongitude", "decimalLatitude"), as.numeric)

izrk <- readxl::read_excel(path, sheet = "raw-data") %>% 
    select(-RECORD, -OCCURRENCE, -EVENT, -LOCATION, -TAXON) %>% 
    mutate(
        scientificName = str_replace_all(scientificName, "L. ", "L."),
        scientificName = str_replace_all(scientificName, "C. ", "C."),
        scientificName = str_replace_all(scientificName, "O. ", "O."),
        scientificName = str_replace_all(scientificName, "\\. ", "\\.")) %>% 
    mutate_at(c("decimalLongitude", "decimalLatitude"), as.numeric)

# check part 1 ------------------------------------------------------------
lit %>% 
    mutate_all(contains_cyrillic) %>% 
    sapply(unique) %>% 
    sort 

values <- lit %>% 
    flatten_chr() %>% 
    unique()
symbols <- lit %>% 
    flatten_chr() %>% 
    strsplit("") %>% 
    flatten_chr() %>% 
    table  %>% 
    tibble(l = names(.), n  = as.numeric(.)) %>% 
    select(l, n)

# export ------------------------------------------------------------------
lit %>% 
    mutate(
        decimalLatitude = case_when(
            coordinateUncertaintyInMeters > 10000 ~ round(decimalLatitude, 2), 
            coordinateUncertaintyInMeters > 5000 ~ round(decimalLatitude, 3), 
            coordinateUncertaintyInMeters > 1000 ~ round(decimalLatitude, 4), 
            TRUE ~ round(decimalLatitude, 5)),
        
        decimalLongitude = case_when(
            coordinateUncertaintyInMeters > 10000 ~ round(decimalLongitude, 2), 
            coordinateUncertaintyInMeters > 5000 ~ round(decimalLongitude, 3), 
            coordinateUncertaintyInMeters > 1000 ~ round(decimalLongitude, 4), 
            TRUE ~ round(decimalLongitude, 5)),
    ) %>% 
    readr::write_delim("data/part1/occurrences_lit.txt", delim = "\t", na = "")

izrk %>% 
    mutate(
        decimalLatitude = case_when(
            coordinateUncertaintyInMeters > 10000 ~ round(decimalLatitude, 2), 
            coordinateUncertaintyInMeters > 5000 ~ round(decimalLatitude, 3), 
            coordinateUncertaintyInMeters > 1000 ~ round(decimalLatitude, 4), 
            TRUE ~ round(decimalLatitude, 5)),
        
        decimalLongitude = case_when(
            coordinateUncertaintyInMeters > 10000 ~ round(decimalLongitude, 2), 
            coordinateUncertaintyInMeters > 5000 ~ round(decimalLongitude, 3), 
            coordinateUncertaintyInMeters > 1000 ~ round(decimalLongitude, 4), 
            TRUE ~ round(decimalLongitude, 5)),
    ) %>% 
    readr::write_delim("data/part2/occurrences_coll.txt", delim = "\t", na = "")



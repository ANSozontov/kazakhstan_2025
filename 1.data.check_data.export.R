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
# rm(path)
# check parts 1&2 ---------------------------------------------------------
lit %>% 
    mutate_all(contains_cyrillic) %>% 
    sapply(unique) %>% 
    sort 
izrk %>% 
    mutate_all(contains_cyrillic) %>% 
    sapply(unique) %>% 
    sort 

values <- lst(lit, izrk) %>% 
    map(~.x %>% 
            flatten_chr %>% 
            unique
    )

symbols <- values %>% 
    map(~.x %>% 
        # flatten_chr() %>% 
        strsplit("") %>% 
        flatten_chr() %>% 
        table  %>% 
        tibble(l = names(.), n  = as.numeric(.)) %>% 
        select(l, n)
    )
symbols <- left_join(symbols$lit, symbols$izrk, by = "l") %>% 
    rename(lit = 2, izrk = 3)

# export ------------------------------------------------------------------
# rm(symbols, values, contains_cyrillic)
save.image(paste0("lit_izrk_", Sys.Date(), ".RData"))
lit <- lit %>% 
    mutate_if(is.character, str_squish) %>% 
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
        
        decimalLatitude = as.character(decimalLatitude),
        decimalLongitude = as.character(decimalLongitude),
        
        decimalLatitude = case_when(nchar(decimalLatitude) > 8 ~ substr(decimalLatitude, 1, 8), TRUE ~ decimalLatitude),
        decimalLongitude = case_when(nchar(decimalLongitude) > 8 ~ substr(decimalLongitude, 1, 8), TRUE ~ decimalLongitude)
    )


for(i in 1:nrow(lit)){
    if(is.na(lit$decimalLatitude[i])){
        
    } else if (nchar(lit$decimalLatitude[i]) < nchar(lit$decimalLongitude[i])){
        # decimalLatitude
        
        lit$decimalLatitude[i] <- paste0(
            lit$decimalLatitude[i], 
            paste0(rep("0", nchar(lit$decimalLongitude[i]) - nchar(lit$decimalLatitude[i])), collapse = "")
        )
    } else if(nchar(lit$decimalLongitude[i]) < nchar(lit$decimalLatitude[i])){
        # decimalLongitude
        lit$decimalLongitude[i] <- paste0(
            lit$decimalLongitude[i], 
            paste0(rep("0", nchar(lit$decimalLatitude[i]) - nchar(lit$decimalLongitude[i])), collapse = "")
        )
    }
}

if(lit %>% 
    select(starts_with("decimal")) %>% 
    mutate_all(as.character) %>% 
    flatten_chr() %>% 
    nchar() %>% 
    max(na.rm = TRUE) <= 8) {
    
    readr::write_delim(
        lit, 
        "data/part1/occurrences_lit.txt", 
        delim = "\t", 
        na = "")   
    cli::cli_alert_success("The literature dataset export is successful")
} else {
    cli::cli_abort("Too high coordinates precision is detected!")
}

izrk <- izrk %>% 
    mutate_if(is.character, str_squish) %>% 
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
        
        decimalLatitude = as.character(decimalLatitude),
        decimalLongitude = as.character(decimalLongitude),
        
        decimalLatitude = case_when(nchar(decimalLatitude) > 8 ~ substr(decimalLatitude, 1, 8), TRUE ~ decimalLatitude),
        decimalLongitude = case_when(nchar(decimalLongitude) > 8 ~ substr(decimalLongitude, 1, 8), TRUE ~ decimalLongitude),
    )

for(i in 1:nrow(izrk)){
    if(is.na(izrk$decimalLatitude[i])){
        
    } else if (nchar(izrk$decimalLatitude[i]) < nchar(izrk$decimalLongitude[i])){
        # decimalLatitude
        izrk$decimalLatitude[i] <- paste0(
            izrk$decimalLatitude[i], 
            paste0(rep("0", nchar(izrk$decimalLongitude[i]) - nchar(izrk$decimalLatitude[i])), collapse = "")
        )
    } else if(nchar(izrk$decimalLongitude[i]) < nchar(izrk$decimalLatitude[i])){
        # decimalLongitude
        izrk$decimalLongitude[i] <- paste0(
            izrk$decimalLongitude[i], 
            paste0(rep("0", nchar(izrk$decimalLatitude[i]) - nchar(izrk$decimalLongitude[i])), collapse = "")
        )
    }
}

if(lit %>% 
    select(starts_with("decimal")) %>% 
    mutate_all(as.character) %>% 
    flatten_chr() %>% 
    nchar() %>% 
    max(na.rm = TRUE) <= 8) {
    
    readr::write_delim(
        izrk, 
        "data/part2/occurrences_coll.txt", 
        delim = "\t", 
        na = "")
    cli::cli_alert_success("The collection dataset export is successful")
} else {
    cli::cli_abort("Too high coordinates precision is detected!")
} 
    
full_join(
    tibble(dwc = colnames(lit),  lit = TRUE),
    tibble(dwc = colnames(izrk), izrk = TRUE),
    by = "dwc") %>% 
    writexl::write_xlsx("data/dwc_columns.xlsx")


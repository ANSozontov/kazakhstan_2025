# LOAD --------------------------------------------------------------------
library(tidyverse)
library(sf)
library(leaflet)
library(rgbif)
kaz_bnd <- list(
    ele = raster::raster("elevation.tiff"),
    focus = st_read("kaz.gpkg", "focus"),
    kaz0 = st_read("kaz.gpkg", "kaz0"), 
    kaz1 = st_read("kaz.gpkg", "kaz1"), 
    kaz2 = st_read("kaz.gpkg", "kaz2_alm")
)

path <- dir() %>% 
    str_subset("^data.*\\.xlsx$") %>% 
    sort(decreasing = TRUE) %>% 
    `[`(1)

izkn <- readxl::read_excel(path, sheet = "raw-data") %>% 
    select(RECORD:taxonRemarks) %>% 
    select(-RECORD, -OCCURRENCE, -EVENT, -LOCATION, -TAXON) %>% 
    mutate(
        scientificName = str_replace_all(scientificName, "L. ", "L."),
        scientificName = str_replace_all(scientificName, "C. ", "C."),
        scientificName = str_replace_all(scientificName, "O. ", "O."))

gen <- izkn %>% 
    split(.$taxonRank) %>% 
    map(~.x %>% 
        pull(genus) %>% 
        unique %>% 
        sort
    )

lit.pln <- readxl::read_excel(path, sheet = "lib-data") %>% 
    select(RECORD:taxonRemarks) %>% 
    select(-RECORD, -OCCURRENCE, -EVENT, -LOCATION, -TAXON)

lit.mnt <- filter(lit.pln, str_detect(dynamicProperties, "mountain"))
lit.pln <- filter(lit.pln, str_detect(dynamicProperties, "plain"))

gbif <- "0086127-250717081556266" %>% 
    occ_download_get(path = ".", overwrite = F) %>% 
    occ_download_import() %>% 
    filter(
        taxonRank %in% c("GENUS", "SPECIES", "SUBSPECIES"),
        !is.na(decimalLatitude) | !is.na(decimalLongitude)) %>%
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
             crs = 4326, remove = FALSE)

gbif <- st_drop_geometry(gbif[st_intersects(gbif, kaz_bnd$focus, sparse = FALSE), ])
inat <- gbif

{
sp.to.remove <- 
            "Aelurillus lutosus|Berlandina saraevi|Heriaeus horridus|
            |Heriaeus oblongus|Heterotheridion nigrovariegatum|
            |Hypsosinga kazachstanica|Larinia phthisica|Neoscona spasskyi|
            |Pardosa falcata|Philodromus emarginatus|Platnickina tincta|
            |Poecilochroa variana|Pseudomogrus bucharaensis|
            |Runcinia tarabayevi|Tetragnatha nigrita|Thanatus fabricii|
            |Thanatus mongolicus|Eresus kollari|Euryopis flavomaculata|
            |Araneus miquanensis"

inat <- inat %>% 
    filter(datasetName == "iNaturalist research-grade observations") %>% 
    mutate(
        eventDate = substr(eventDate, 1, 10), 
        eventDate = as.Date(eventDate)) %>%
    filter(eventDate <= as.Date("2025-04-18")) %>% 
    filter(
        # remove doubtful (orange) 
        str_detect(scientificName, negate = TRUE, pattern = sp.to.remove)
    ) %>% 
    mutate(
        # replace Spiracme mongolica -> Xysticus mongolicus 
        scientificName = case_when(
            str_detect(scientificName, "Spiracme mongolica") ~ "Xysticus mongolicus", 
            TRUE ~ scientificName),
        species = case_when(
            str_detect(species, "Spiracme mongolica") ~ "Xysticus mongolicus", 
            TRUE ~ species),
        # replace Xysticus cristatus -> X. pseudocristatus
        scientificName = case_when(
            str_detect(scientificName, "Xysticus cristatus") ~ "Xysticus pseudocristatus", 
            TRUE ~ scientificName),
        species = case_when(
            str_detect(species, "Xysticus cristatus") ~ "Xysticus pseudocristatus", 
            TRUE ~ species),
        # replace F. mongolica -> Fedotovia uzbekistanica
        scientificName = case_when(
            str_detect(scientificName, "Fedotovia mongolica") ~ "Fedotovia uzbekistanica", 
            TRUE ~ scientificName),
        species = case_when(
            str_detect(species, "Fedotovia mongolica") ~ "Fedotovia uzbekistanica", 
            TRUE ~ species),
        # genreralize to genera (yellow)
        taxonRank = case_when(
            str_detect(scientificName, "Alopecosa fedotovi|Alopecosa hui") ~ "GENUS", 
            TRUE ~ taxonRank
        ),
        scientificName = case_when(
            str_detect(scientificName, "Alopecosa fedotovi|Alopecosa hui") ~ "Alopecosa", 
            TRUE ~ scientificName
        ),
        species = case_when(
            str_detect(species, "Alopecosa fedotovi|Alopecosa hui") ~ "Alopecosa", 
            TRUE ~ species
        )
        
    )
}
gbif <- gbif %>% 
    filter(datasetName != "iNaturalist research-grade observations")
cat("data are here")
# MAP ---------------------------------------------------------------------
geo_plot <- lst(izkn, lit.pln, lit.mnt, gbif, inat) %>%
    map(~.x %>% 
        separate(scientificName, c("g", "sp"), sep = " ", extra = "drop") %>% 
        mutate(taxa = case_when(
            taxonRank != "GENUS" ~ paste(g, sp),
            TRUE ~ g
        )) %>% 
        select(occurrenceID, taxa, taxonRank, decimalLatitude, decimalLongitude) %>% 
        filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
        mutate(
            decimalLongitude = round(as.numeric(decimalLongitude), 3), 
            decimalLatitude = round(as.numeric(decimalLatitude), 3)) %>% 
        group_by(decimalLongitude, decimalLatitude) %>% 
        summarise(
            taxa = paste0(taxa, collapse = ", "), 
            occurrenceID = paste0(occurrenceID, collapse = ", "), 
            .groups = "drop") %>% 
        st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
                     crs = 4326, remove = FALSE))

leaflet() %>% 
    addTiles(group = "OSM") %>%
    addRasterImage(
        kaz_bnd$ele, 
        group = "ELEVATION",
        opacity = 1, colors = terrain.colors(10)) %>% 
    addPolygons(
        data = kaz_bnd$kaz2, 
        fillOpacity = 0, opacity = 0.5, 
        color = "darkgray", weight = 1.5) %>% 
    addPolygons(
        data = kaz_bnd$kaz1, 
        fillOpacity = 0, opacity = 0.8, 
        color = "black", weight = 2.5) %>% 
    addPolygons(
        data = kaz_bnd$kaz0, 
        fillOpacity = 0, opacity = 1, 
        color = "black", weight = 3) %>% 
    addPolygons(
        data = kaz_bnd$focus, 
        fillOpacity = 0, opacity = 1, 
        color = "blue", weight = 3) %>% 
    addCircleMarkers(
        data = geo_plot$inat,
        color = "cyan",  fillOpacity = 0,
        label = ~taxa, popup = ~occurrenceID,
        group = "iNaturalist") %>%
    addCircleMarkers(
        data = geo_plot$lit.mnt,
        color = "brown", fillOpacity = 0,
        weight = 2,
        label = ~taxa, popup = ~occurrenceID,
        group = "literature (mountain)") %>% 
    addCircleMarkers(
        data = geo_plot$lit.pln,
        color = "black", fillOpacity = 0,
        opacity = 1, weight = 2.5,
        label = ~taxa, popup = ~occurrenceID,
        group = "literature (plain)") %>%
    addCircleMarkers(
        data = geo_plot$izkn,
        fillColor = "green", fillOpacity = 0.3, 
        color = "white",  opacity = 1,
        label = ~taxa, popup = ~occurrenceID,
        group = "IZKN") %>%
    addCircleMarkers(
        data = geo_plot$gbif,
        color = "pink",  fillOpacity = 0,
        label = ~taxa, popup = ~occurrenceID,
        group = "GBIF") %>%
    addLayersControl(
        overlayGroups = c(
            "OSM", "ELEVATION",
            "iNaturalist", "GBIF", "IZKN",
            "literature (plain)",
            "literature (mountain)"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addScaleBar(position = "bottomright")

# counts ------------------------------------------------------------------
sp_list_list <- lst(
    izkn = mutate(izkn, taxonRemarks = paste0(identificationRemarks, ", ", taxonRemarks)),
    lit.pln, gbif, inat) %>% 
    map(~.x %>% 
            st_drop_geometry %>% 
            filter(taxonRank != "FAMILY") %>% 
            transmute(family, taxonRank, 
                      taxonRemarks = as.character(taxonRemarks),
                      scientificName #= case_when(taxonRank == "GENUS"~ genus, TRUE ~species)
            ))

sp_list_df <- sp_list_list %>% 
    map_dfr(rbind, .id = "dataset") %>% 
    filter(taxonRank != "GENUS" | str_detect(scientificName, "Pritha|Segestria|Pirata|Hylyphantes")) %>% 
    distinct() %>% 
    mutate(status = 1) %>% 
    arrange(family, scientificName) %>% 
    transmute(
        taxonRank, family, scientificName, dataset, status, #izkn, lit.pln, gbif, inat, 
        taxonRemarks = str_replace_all(taxonRemarks, ", NA", ""),
        taxonRemarks = case_when(taxonRemarks == "NA" ~ NA, TRUE ~ taxonRemarks))

remarks <- sp_list_df %>% 
    group_by(scientificName) %>% 
    summarise(taxonRemarks = paste0(na.omit(taxonRemarks), collapse = ", "), .groups = "drop")

sp_list_df <- sp_list_df %>% 
    select(-taxonRemarks) %>% 
    distinct() %>% 
    pivot_wider(names_from = dataset, values_from = status, values_fill = NA, values_fn = sum) %>% 
    left_join(remarks, by = "scientificName")

sum(izkn$individualCount)

# IZKN collectors
IZKN_collectors <- izkn %>% 
    pull(recordedBy) %>% 
    str_split(", ") %>% 
    flatten_chr() %>% 
    table %>% 
    tibble(name = names(.), n = as.numeric(.)) %>% 
    select(name, n) %>% 
    arrange(desc(n))

# izkn %>% 
#     filter(taxonRank == "SPECIES" | genus %in% gen$GENUS[!(gen$GENUS %in% gen$SPECIES)]) %>% 
#     select(scientificName) %>% 
#     distinct() %>% 
#     arrange(scientificName) 

# Figures -----------------------------------------------------------------
# Vienn Diagram
library(VennDiagram)
sp_list_list %>% 
    map(~.x %>% 
        distinct %>% 
        pull(scientificName) %>% 
        unique %>% 
        sort
    ) %>% 
    venn.diagram(
        x = .,
        category.names = c("IZKN" , "Liter." , 
                           "GBIF", "iNat"),
        filename = paste0('venn.diagramm_', Sys.Date(), '.png'),
        disable.logging = TRUE,
        output=TRUE,
        
        # Output features
        imagetype="png" ,
        height = 880 , 
        width = 880 , 
        resolution = 300,
        compression = "lzw",
        
        # Circles
        lwd = 2,
        lty = 'blank',
        fill = c("red", "blue", "green", "yellow"),
        
        # Numbers
        cex = .6,
        fontface = "bold",
        fontfamily = "sans",
        
        # Set names
        cat.cex = 0.6, 
        cat.fontface = "bold", 
        cat.default.pos = "outer",
        # cat.pos = c(-27, 27, 135),
        # cat.dist = c(0.055, 0.055, 0.085),
        cat.fontfamily = "sans"
        # rotation = 1
    ) 

sp_list_list %>% 
    map(distinct) %>% 
    map_df(rbind, .id = "id") %>% 
    select(id, family, scientificName) %>% 
    rbind(mutate(., id = "total")) %>% 
    distinct() %>% 
    count(id, family) %>%  
    mutate(family  = case_when(family %in% c(
        "Salticidae", "Gnaphosidae", "Lycosidae", "Thomisidae", 
        "Araneidae", "Philodromidae", "Theridiidae", "Linyphiidae"
        ) ~ family, TRUE ~ "x.others"
    )) %>% 
    group_by(id, family) %>% 
    summarise(nn = sum(n), .groups = "drop_last") %>% 
    mutate(
        nnn = nn / sum(nn) * 100, 
        nnnn = round(nnn, 1)
        ) %>% 
    ungroup %>% 
    mutate(
        id = toupper(id),
        family = factor(family, ordered = TRUE)) %>% 
    ggplot(aes(x = "", y = nnn, fill = family, label = nnnn)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    # geom_label(
    #            alpha = 0.7,
    #            nudge_x = 0.6,
    #            size = 2.2) +
    scale_fill_brewer(palette = "Set3") +
    labs(fill = "Family", x = NULL, y = NULL) +
    # guides(fill = guide_legend(override.aes = list(label = NULL, colour = NA), reverse = TRUE)) +
    coord_polar("y") +
    scale_y_reverse() +
    facet_wrap(~ id) + 
    theme_void() 
            

# export ------------------------------------------------------------------
lst(IZKN_collectors)
writexl::write_xlsx()

writexl::write_xlsx(sp_list_df, "species_list.xlsx")

contains_cyrillic <- function(text) {
    text <- paste0(text, collapse = "")
    str_detect(text, "[\\u0400-\\u04FF]")
}

lit <- rbind(lit.mnt, lit.pln)
lit %>% 
    #убирать перед проверкой на кириллицу
    select(-starts_with("verba"), -associatedReferences, -bibliographicCitation) %>% 
    sapply(contains_cyrillic)
     
readr::write_delim(lit, "occ_literature.csv", delim = "\t")

# arj ---------------------------------------------------------------------
x <- unzip("0086127-250717081556266.zip", "occurrence.txt") %>% 
    readr::read_delim() %>% 
    filter(taxonRank %in% c("GENUS", "SPECIES", "SUBSPECIES"))

# inat
inat <- x %>% 
    filter(datasetName == "iNaturalist research-grade observations") %>% 
    mutate(
        eventDate = substr(eventDate, 1, 10), 
        eventDate = as.Date(eventDate)) %>%
    filter(eventDate  < as.Date("2025-04-30")) %>% 
    select(scientificName, taxonRank, decimalLatitude, decimalLongitude) %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
# non-inat GBIF
gbif_all <- x %>% 
    filter(datasetName != "iNaturalist research-grade observations") %>% 
    select(scientificName, taxonRank, decimalLatitude, decimalLongitude)
# gbif <- gbif_all %>% 
#     filter(
#         !is.na(decimalLongitude), 
#         !is.na(decimalLatitude)) %>% 
#     st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

DF <- list(
    own = readxl::read_excel("data_2025-08-07.xlsx", sheet = "raw-data"),
    lit_mnt = "data_2025-08-07.xlsx" %>% 
        readxl::read_excel(sheet = "lib-data") %>% 
        filter(str_detect(dynamicProperties, "mountain")),
    lit_pln = "data_2025-08-07.xlsx" %>% 
        readxl::read_excel(sheet = "lib-data") %>% 
        filter(str_detect(dynamicProperties, "plain"))
    ) %>% 
    map(~.x %>% 
        filter(taxonRank %in% c("GENUS", "SPECIES", "SUBSPECIES")) %>% 
        separate(occurrenceID, c("tmp", "ID"), "spiders_") %>% 
        transmute(scientificName,
                  # ID, 
                  taxonRank, decimalLatitude, decimalLongitude) %>% 
        ###
        distinct %>% 
        group_by(decimalLatitude, decimalLongitude) %>% 
        summarise(#ID = paste0(ID, collapse = "; "),
                  scientificName = paste0(scientificName, collapse = "; "),
                  .groups = "drop") %>% 
        ###    
        st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
            crs = 4326, remove = FALSE)
        )

leaflet() %>% 
    addTiles() %>% 
    addPolygons(data = kz0, 
                fillOpacity = 0, opacity = 1) %>% 
    addPolygons(data = focus, color = "red",
                fillOpacity = 0, opacity = 1) %>% 
    addCircleMarkers(data = DF$own,
                     label = ~species, popup = ~ID,
                     color = "darkgreen")

own <- readxl::read_excel("data_2025-08-07.xlsx", sheet = "raw-data") %>% 
    # filter()
    select(-starts_with("verba")) %>% 
    select(RECORD:taxonRemarks) %>% 
    transmute(ID = occurrenceID, species, decimalLatitude, decimalLongitude) %>% 
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
             crs = 4326, remove = FALSE)

lit <- readxl::read_excel("data_2025-08-07.xlsx", sheet = "lib-data") %>% 
    select(RECORD:taxonRemarks) %>% 
    select(-starts_with("verba")) %>% 
    transmute(ID = as.character(ID), species, decimalLatitude, decimalLongitude) %>% 
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
    # filter(str_detect(species, "Pseudicius encarpatus")) %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
             crs = 4326, remove = FALSE) 

leaflet() %>% 
    addTiles() %>% 
    addPolygons(data = kz0, 
                fillOpacity = 0, opacity = 1) %>% 
    addPolygons(data = focus, color = "darkgreen",
                fillOpacity = 0, opacity = 1) %>% 
    addCircleMarkers(data = slice(own, 427),
                     label = ~species, popup = ~ID,
                     color = "darkgreen") 
    # addCircleMarkers(data = lit, 
    #                  color = "blue",
    #                  label = ~species, popup = ~ID)



st_covered_by(geo$inat, focus) 
i2 <- geo$inat[st_intersects(geo$inat, focus, sparse = FALSE), ]


result <- st_join(b, kz2, join = st_within)

result %>% 
    st_drop_geometry() %>% 
    select(1:3, COUNTRY, NAME_1, NAME_2, ENGTYPE_2) %>% 
    writexl::write_xlsx("tmp.xlsx")

focus <- st_read("kaz.gpkg", "focus")

ggplot()+ 
    geom_sf(data = kz0, fill = "white") + 
    # geom_sf(data = ak, fill = "red") +
    geom_sf(data = own, color = "darkgreen") + 
    geom_sf(data = lit_yes, color = "blue") + 
    geom_sf(data = lit_no, color = "red")


leaflet() %>% 
    addTiles() %>% 
    addPolygons(data = k1, fillOpacity = 0, color = "darkgray", opacity = 1) %>% 
    addPolygons(data = filter(kz2, NAME_1 == "Almaty"), 
                fillOpacity = 0, color = "white", opacity = 1) %>% 
    addPolygons(data = k0, fillOpacity = 0, color = "black", opacity = 1) %>%
    addPolygons(data = focus, fillOpacity = 0, color = "darkgreen", opacity = 1)


all <- DF %>% 
    append(list(inat = i2, gbif_all = gbif_all)) %>% 
    map(~.x %>% 
            st_drop_geometry %>% 
            select(scientificName, taxonRank)
    ) %>% 
    map_dfr(rbind, .id = "dataset") %>% 
    mutate(i = "+") %>% 
    distinct() %>% 
    pivot_wider(names_from = dataset, values_from = i) %>% 
    arrange(scientificName)

all %>% 
    separate(1, "gen", sep = " ", extra = "drop", remove = F) %>% 
    filter(gen %in% c("Hylyphantes", "Pirata", "Pritha", "Segestria", "Sitticus") | 
               taxonRank != "GENUS") %>% 
    select(-gen)



b <- sapply(a, contains_cyrillic)
b[b]

b <- sort(unique(a$occurrenceRemarks))
b <- sapply(b, contains_cyrillic)
b[b]

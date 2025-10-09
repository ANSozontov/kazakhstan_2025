# LOAD --------------------------------------------------------------------
library(tidyverse)
library(sf)
library(leaflet)
library(rgbif)
gis_data <- list(
    ele = raster::raster("gis/elevation.tiff"),
    focus = st_read("gis/kaz_focus.shp"),
    kaz0  = st_read("gis/kaz0.shp"), 
    kaz1  = st_read("gis/kaz1.shp"), 
    kaz2  = st_read("gis/kaz2_alm.shp")
)

path <- dir(pattern = ".RData") %>% 
    # str_subset("^data.*\\.xlsx$") %>% 
    # str_subset("~", negate = TRUE) %>% 
    sort(decreasing = TRUE) %>% 
    `[`(1)
load(path)
izrk <- mutate(izrk, species = scientificName)
lit  <- mutate(lit,  species = acceptedNameUsage)

lit.mnt <- filter(lit, str_detect(dynamicProperties, "mountain"))
lit.pln <- filter(lit, str_detect(dynamicProperties, "plain"))

gbif <- "0086127-250717081556266" %>% 
    occ_download_get(path = "./data", overwrite = F) %>% 
    occ_download_import()

gbif <- gbif %>% 
    select(
        datasetName, datasetKey, eventDate, 
        family, genus, species, #scientificName, 
        taxonRank, occurrenceID,
        identificationRemarks, taxonRemarks,
        decimalLatitude, decimalLongitude, 
        ) %>% 
    filter(
        taxonRank %in% c("GENUS", "SPECIES", "SUBSPECIES"),
        !is.na(decimalLatitude) | !is.na(decimalLongitude)) %>%
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
             crs = 4326, remove = FALSE) %>% 
    # 4023 -> 2686
    filter(st_intersects(geometry, gis_data$focus, sparse = FALSE)[,1]) %>% 
    st_drop_geometry() %>% 
    mutate(
        # Spiracme mongolica -> Xysticus mongolicus 
        # scientificName = case_when(
        #     str_detect(scientificName, "Spiracme mongolica") ~ "Xysticus mongolicus", 
        #     TRUE ~ scientificName),
        species = case_when(
            str_detect(species, "Spiracme mongolica") ~ "Xysticus mongolicus", 
            TRUE ~ species),
        # Xysticus cristatus -> X. pseudocristatus
        # scientificName = case_when(
        #     str_detect(scientificName, "Xysticus cristatus") ~ "Xysticus pseudocristatus", 
        #     TRUE ~ scientificName),
        species = case_when(
            str_detect(species, "Xysticus cristatus") ~ "Xysticus pseudocristatus", 
            TRUE ~ species),
        # Fedotovia mongolica -> Fedotovia uzbekistanica
        # scientificName = case_when(
        #     str_detect(scientificName, "Fedotovia mongolica") ~ "Fedotovia uzbekistanica", 
        #     TRUE ~ scientificName),
        species = case_when(
            str_detect(species, "Fedotovia mongolica") ~ "Fedotovia uzbekistanica", 
            TRUE ~ species),
        # Xysticus tristrami -> Bassanoides tristrami
        species = case_when(
            str_detect(species, "Xysticus tristrami") ~ "Bassanoides tristrami", 
            TRUE ~ species),
        # Dysdera 
        species = case_when(
            genus == "Dysdera" ~ "Dysdera sp.", 
            TRUE ~ species),
        # Evippa beschkentica -> Evippa caucasica
        species = case_when(
            str_detect(species, "Evippa beschkentica") ~ "Evippa caucasica", 
            TRUE ~ species),
        # Psammitis sp. -> Ps. marmorata (txR = SPECIES)
        species = case_when(
            genus == "Psammitis" & taxonRank == "GENUS" ~ "Psammitis marmorata", 
            TRUE ~ species),
        taxonRank = case_when(
            genus == "Psammitis" ~ "SPECIES", 
            TRUE ~ taxonRank),
        # genreralize to genera (yellow)
        taxonRank = case_when(
            str_detect(species, "Alopecosa fedotovi|Alopecosa hui") ~ "GENUS",
            TRUE ~ taxonRank
        ),
        # scientificName = case_when(
        #     str_detect(scientificName, "Alopecosa fedotovi|Alopecosa hui") ~ "Alopecosa", 
        #     TRUE ~ scientificName
        # ),
        species = case_when(
            str_detect(species, "Alopecosa fedotovi|Alopecosa hui") ~ "Alopecosa sp.", 
            TRUE ~ species
        )
        
    )

{
sp.to.remove1 <- 
            "Aelurillus lutosus|Heriaeus horridus|
            |Heriaeus oblongus|Heterotheridion nigrovariegatum|
            |Hypsosinga kazachstanica|Larinia phthisica|Neoscona spasskyi|
            |Pardosa falcata|Philodromus emarginatus|Platnickina tincta|
            |Poecilochroa variana|Pseudomogrus bucharaensis|
            |Runcinia tarabayevi|Tetragnatha nigrita|Thanatus fabricii|
            |Thanatus mongolicus|Eresus kollari|Euryopis flavomaculata|
            |Araneus miquanensis"
    
sp.to.remove2 <- 
        "Aculepeira armida|Alopecosa cursor|Archaeodictyna consecuta|
        |Cheiracanthium punctorium|Dictyna arundinacea|Drassodes lapidosus|
        |Larinioides ixobolus|Larinioides patagiatus|Micaria formicaria|
        |Microlinyphia pusilla|Metleucauge dentipalpis|Pardosa zonsteini|
        |Phlegra obscurimagna|Talavera aperta|Tetragnatha montana|
        |Trochosa ruricola|Theridion melanurum"

inat <- gbif %>% 
    mutate(
            eventDate = substr(eventDate, 1, 10), 
            eventDate = as.Date(eventDate)) %>% 
    filter(
        datasetName == "iNaturalist research-grade observations",
        eventDate <= as.Date("2025-04-18"),
        # remove doubtful (orange) 
        str_detect(species, negate = TRUE, pattern = sp.to.remove1),
        str_detect(species, negate = TRUE, pattern = sp.to.remove2)
    ) 
}

gbif <- gbif %>% 
    filter(datasetName != "iNaturalist research-grade observations")
cat("data are here")

# counts ------------------------------------------------------------------
# Xysticus tristrami ?? Есть в gbif в c47e7a5b-692d-4e26-a32f-74b0188eb594 -> Bassanoides
# Yllenus dalaensis, Y. pseudovalidus, Y. zhilgaensis пришел из GBIF (зенкенберг), у Ани -> Pseudomogrus 
# Dysdera из iNat не удалять! Оставить D. sp.
# Alopecosa из iNat не удалять! Оставить A. sp.
#! iNat: Psammitis sp. -> Ps. marmorata (txR = SPECIES)
#! iNAT: Evippa beschkentica -> Evippa caucasica

sp_list_list <- lst(
    `4.izrk` = mutate(izrk, taxonRemarks = paste0(identificationRemarks, ", ", taxonRemarks)),
    `1.lit.pln` = lit.pln, `3.gbif` = gbif, `2.inat` = inat) %>% 
    map(~.x %>% 
            st_drop_geometry %>% 
            filter(taxonRank == "SPECIES" | str_detect(species, " sp")) %>% 
            transmute(family, taxonRank, 
                      taxonRemarks = as.character(taxonRemarks),
                      scientificName = species
                      #case_when(taxonRank == "GENUS"~genus, TRUE~species)
            ))

sp_list_df <- sp_list_list %>% 
    map_dfr(rbind, .id = "dataset") %>% 
    # filter(taxonRank != "GENUS" | str_detect(scientificName, "Pritha|Segestria|Pirata|Hylyphantes")) %>% 
    distinct() %>% 
    mutate(status = 1) %>% 
    arrange(dataset, family, scientificName) %>% 
    transmute(
        taxonRank, family, scientificName, dataset, status, 
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

# IZRK collectors
IZRK_collectors <- izrk %>% 
    pull(recordedBy) %>% 
    str_split(", ") %>% 
    flatten_chr() %>% 
    table %>% 
    tibble(name = names(.), n = as.numeric(.)) %>% 
    select(name, n) %>% 
    arrange(desc(n))

# References table
bib_table <- lst(lit.pln, lit.mnt) %>% 
    map(~.x %>% 
        filter(taxonRank == "SPECIES" | str_detect(species, " sp")) %>% 
        select(bibliographicCitation, species) %>% 
        distinct()
    ) %>% 
    map_dfr(rbind, .id = "dataset") %>% 
    split(.$bibliographicCitation) %>% 
    lapply(function(a){tibble(
        n_total = length(unique(a$species)),
        n_focus = a %>% filter(dataset == "lit.pln") %>% pull(species) %>% unique %>% length,
        reference = a$bibliographicCitation[1]
    )}) %>% 
    map_dfr(rbind) 

piecharts_table <- sp_list_list %>% 
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
    summarise(n = sum(n), .groups = "drop_last") %>% 
    mutate(
        part = n / sum(n) * 100, 
        part_round = round(part, 1)
    ) %>% 
    ungroup %>% 
    mutate(
        id = toupper(id),
        family = factor(family, ordered = TRUE))

datasets <- tibble(
        sp_list_df[,1:7],
        n = apply(sp_list_df[,4:7], 1, function(a){sum(a, na.rm = TRUE)}) 
    ) %>% 
    filter(n == 1)

results <- c(
    paste("The literature dataset includes", nrow(rbind(lit.mnt, lit.pln)), "occurrences"),
    paste(nrow(lit.pln), "of which belong to the plain part of the studied region"), 
    paste("The remaining", nrow(lit.mnt), "occurrences come from the mountainous part"),
    "",
    paste("The IZRK collection dataset includes", nrow(izrk), "occurrences"),
    paste("The total number of adult spiders collected during this period was", sum(izrk$individualCount), "specimens"),
    paste("among them", nrow(distinct(izrk["species"])), "species were identified"),
    "", 
    paste("In total, we processed", nrow(bib_table), "references "), 
    paste(nrow(filter(bib_table, n_focus != 0)), "of them contain occurrences from the studied region"), 
    paste("All literature data contain", nrow(lit.pln), "records of", nrow(distinct(lit.pln["species"])), "species"),
    "", 
    paste("iNaturalist data:", nrow(inat), "occurrences"),
    paste("GBIF.org data:", nrow(gbif), "occurrences"),
    "",
    paste("Thus, at least", nrow(sp_list_df), "spider species from", 
          length(unique(map_chr(strsplit(sp_list_df$scientificName, " "), ~.x[1]))), 
          "genera and", length(unique(sp_list_df$family)), 
          "families are known from the region"), 
    "", 
    paste("More than half of the recorded species (", 
          nrow(datasets), 
          round(nrow(datasets)/nrow(sp_list_df)*100),
          "%) were found exclusively in a single dataset:"),
    map(datasets[,4:7], ~sum(.x, na.rm = TRUE)) %>% 
        paste0(names(.), ": ", ., collapse = ";  ")
)
cat("Results:", results, sep = "\n")
# MAP ---------------------------------------------------------------------
geo_plot <- lst(
    izrk,
    lit.pln,
    lit.mnt,
    gbif,
    inat
    ) %>%
    map(~.x %>%
            filter(taxonRank == "SPECIES" | str_detect(species, " sp\\.")) %>% 
            # separate(scientificName, c("g", "sp"), sep = " ", extra = "drop") %>% 
            # mutate(taxa = case_when(
            #     taxonRank != "GENUS" ~ paste(g, sp),
            #     TRUE ~ g
            # )) %>% 
            transmute(occurrenceID, taxa = species, taxonRank, decimalLatitude, decimalLongitude) %>% 
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
        gis_data$ele, 
        group = "ELEVATION",
        opacity = 1, colors = terrain.colors(10)) %>% 
    addPolygons(
        data = gis_data$kaz2, 
        fillOpacity = 0, opacity = 0.5, 
        color = "darkgray", weight = 1.5) %>% 
    addPolygons(
        data = gis_data$kaz1, 
        fillOpacity = 0, opacity = 0.8, 
        color = "black", weight = 2.5) %>% 
    addPolygons(
        data = gis_data$kaz0, 
        fillOpacity = 0, opacity = 1, 
        color = "black", weight = 3) %>% 
    addPolygons(
        data = gis_data$focus, 
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
        data = geo_plot$izrk,
        fillColor = "green", fillOpacity = 0.3, 
        color = "white",  opacity = 1,
        label = ~taxa, popup = ~occurrenceID,
        group = "IZRK") %>%
    addCircleMarkers(
        data = geo_plot$gbif,
        color = "pink",  fillOpacity = 0,
        label = ~taxa, popup = ~occurrenceID,
        group = "GBIF") %>%
    addLayersControl(
        overlayGroups = c(
            "OSM", "ELEVATION",
            "iNaturalist", "GBIF", "IZRK",
            "literature (plain)",
            "literature (mountain)"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addScaleBar(position = "bottomright")

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
        category.names = c("IZRK" , "Liter." , 
                           "GBIF", "iNat"),
        filename = paste0('figures/venn.diagramm_', Sys.Date(), '.png'),
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

piecharts_table %>% 
    ggplot(aes(x = "", y = part, fill = family, label = part_round)) +
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
ggsave(paste0('figures/pie.chart_', Sys.Date(), '.pdf'))

# export ------------------------------------------------------------------
lst(
    `species list` = sp_list_df, 
    IZRK_collectors, 
    referenes = bib_table, 
    `families ratio` = piecharts_table) %>% 
    writexl::write_xlsx(paste0("tables/tables_", Sys.Date(), ".xlsx"))
readr::write_lines(results, paste0("tables/summary_",  Sys.Date(), ".txt"))
cat("Results:", results, sep = "\n")

# arj ----------------------------------------------------------------

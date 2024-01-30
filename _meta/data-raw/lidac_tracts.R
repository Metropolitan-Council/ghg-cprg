source("R/_load_pkgs.R")

# read in data
comm <- readr::read_csv("_meta/data-raw/1.0-communities.csv",
  name_repair = "universal"
)

# filter to our 11 counties
comm_11 <- dplyr::filter(
  comm,
  (County.Name %in% c(
    "Hennepin County", "Ramsey County", "Anoka County", "Dakota County", "Carver County",
    "Sherburne County", "Chisago County", "Scott County", "Washington County"
  ) &
    State.Territory == "Minnesota") |
    County.Name %in% c("St. Croix County", "Pierce County") & State.Territory == "Wisconsin"
)

### look at distribution of LIDACs in 11 county area
comm_lidac <- dplyr::filter(comm_11, Identified.as.disadvantaged == TRUE)
tapply(comm_lidac$County.Name, comm_lidac$County.Name, length)

### grab spatial data for tracts and make quick map
mn_tracts <- tigris::tracts(state = "MN", county = c("Hennepin", "Ramsey", "Anoka", "Dakota", "Carver", "Sherburne", "Chisago", "Scott", "Washington"), year = 2010)
wi_tracts <- tigris::tracts(state = "WI", county = c("St. Croix", "Pierce"), year = 2010)
our_tracts <- dplyr::rows_append(mn_tracts, wi_tracts) %>%
  dplyr::left_join(., comm_11 %>%
    dplyr::select(Census.tract.2010.ID, Identified.as.disadvantaged),
  by = c("GEOID10" = "Census.tract.2010.ID")
  )
## basic map
ggplot(our_tracts, aes(fill = Identified.as.disadvantaged)) +
  geom_sf() +
  theme_void()

#### save data needed for PCAP

# row bind for output data
lidac_out <- comm_lidac %>%
  dplyr::select(Census.tract.2010.ID, County.Name, Identified.as.disadvantaged)

names(lidac_out)

lidac_out_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "Tract_ID", class(lidac_out$Census.tract.2010.ID), "Census tract ID",
    "County", class(lidac_out$County.Name), "Name of county",
    "Identified as disadvantaged", class(lidac_out$Identified.as.disadvantaged), "Identified as LIDAC by EPA"
  )

saveRDS(lidac_out, "_meta/data/lidac.RDS")
saveRDS(lidac_out_meta, "_meta/data/lidac_meta.RDS")

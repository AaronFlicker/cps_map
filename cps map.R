library(tidycensus)
library(leaflegend)
library(leaflet)
library(leafpop)
library(sf)
library(stringdist)
library(tidygeocoder)
library(tidyverse)
library(tigris)
library(htmlwidgets)
library(cincy)
options(tigris_use_cache = TRUE)

school <- read_delim(
  "school.txt",
  col_names = c("SchoolID", "School", "SchoolType", "SchoolStateCode"),
  skip = 1
) |>
  filter(!is.na(SchoolType)) 

school_adds <- read_csv(
  "cps.csv",
  col_names = c(
    LETTERS[1:5], 
    LETTERS[7:9], 
    "StreetNum", 
    LETTERS[10:13], 
    "City", 
    "State", 
    "Zip"
    )
  ) |>
  mutate(
    School = NA,
    Street = NA
    )

for (i in 1:nrow(school_adds)){
  for(j in 1:8){
    if(!is.na(school_adds[i, j])){
      school_adds$School[i] <- paste(school_adds$School[i], school_adds[i, j])
    }
  }
  for(k in 10:12){
    if(!is.na(school_adds[i, k])){
      school_adds$Street[i] <- paste(school_adds$Street[i], school_adds[i, k])
    }
  }
}
school_adds$School <- str_remove(school_adds$School, "NA ")
school_adds$Street <- str_remove(school_adds$Street, "NA ")
school_adds$Address <- paste(
  school_adds$StreetNum, 
  school_adds$Street, 
  school_adds$M, 
  sep = " "
  )
school_adds <- select(school_adds, School, Address, City:Zip)

geocoded_schools <- geocode(
  school_adds,
  street = Address,
  city = City,
  state = State,
  postalcode = Zip,
  method = "census"
) |>
  st_as_sf(coords = c("long", "lat"), crs = "NAD83")

schools2 <- full_join(geocoded_schools, school) |>
  filter(!is.na(SchoolType)) |>
  mutate(SchoolID = as.character(SchoolID))

source("~/cchmc_colors.R")

student <- read_delim(
  "student.txt",
  col_select = c(3, 5, 7, 8, 14, 15, 19, 23, 37),
  col_names = c(
    NA, 
    NA, 
    "StudentID", 
    NA, 
    "Gender", 
    NA, 
    "Ethnicity", 
    "Race", 
    rep(NA, 5), 
    "SchoolID", 
    "GradeLevel", 
    rep(NA, 3), 
    "Address", 
    rep(NA, 3), 
    "zip", 
    rep(NA, 13), 
    "Remote"
    ),
  skip = 1
  ) |>
  mutate_if(is.numeric, as.character)

attend <- read_delim(
  "attend_pct.txt",
  col_select = 1:7,
  col_names = c(
    "StudentID", 
    "SchoolID", 
    "AHRS", 
    "EHRS", 
    "UHRS", 
    "AEHHRS", 
    "PCNT", 
    NA
    ),
  skip = 1
  ) |>
  mutate(
    StudentID = as.character(StudentID),
    SchoolID = as.character(SchoolID)
  ) |>
  inner_join(student)

adds <- student |>
  filter(!is.na(Address)) |>
  mutate(
    Address2 = Address,
    Zip = str_trunc(zip, 5, "right", ellipsis = "")
    ) |>
  separate_wider_delim(
    Address2,
    delim = ",",
    names = "Address2",
    too_many = "drop",
    too_few = "align_start"
  ) 
  
adds_unique <- adds |>
  distinct(Address2, zip, Zip) |>
  mutate(AddID = row_number())

adds <- left_join(adds, adds_unique) 
student <- left_join(student, adds)

to_geocode <- adds_unique |>
  mutate(address = paste(Address2, "Cincinnati", "OH", Zip, sep = ", ")) |>
  rename(ID = AddID) |>
  select(ID, address)
#write_csv(to_geocode, "for_degauss.csv")

#cd "C:\Users\FLI6SH\OneDrive - cchmc\Documents\CPS\Neighborhood\cps_map"
#docker run --rm -v ${PWD}:/tmp ghcr.io/degauss-org/geocoder:3.0.2 for_degauss.csv

geocoded <- read.csv("for_degauss_geocoded_v3.0.2.csv") |>
  rename(AddID = ID)

uncoded <- filter(geocoded, matched_state != "OH" | is.na(lat))

coded <- anti_join(geocoded, uncoded, join_by(AddID))

hooded1 <- uncoded |>
  filter(
    geocode_result == "po_box" | 
      (!is.na(matched_state) & matched_state %in% c("IN", "KY"))
    ) |>
  mutate(Neighborhood = NA) |>
  select(AddID, Neighborhood)

cleaned <- anti_join(uncoded, hooded1, join_by(AddID)) |>
  mutate(
    address = str_to_title(address),
    address = str_replace(address, "Eastknoll", "E Knoll"),
    address = str_replace(address, "Sixty-Fourth", "64th"),
    address = str_replace(address, "Thirty Fourth", "34th"),
    address = str_replace(address, "Seventieth", "70th"),
    address = str_replace(address, "St Albans", "Saint Albans"),
    address = str_replace(address, "St Leger", "Saint Leger"),
    address = str_replace(address, "Mistyoak", "Misty Oak"),
    address = str_replace(address, "Northglen", "N Glen"),
    address = str_replace(address, "O Bryan", "Obryan"),
    address = str_replace(address, "Toronto", " Toronto"),
    address = str_replace(address, "St Elmo", "Saint Elmo"),
    address = str_replace(address, "St Charles", "Saint Charles"),
    address = str_replace(address, "St Catherine", "Saint Catherine"),
    address = str_replace(address, "St Michael", "Saint Michael")
    ) |>
  select(AddID, address)

geocoded2 <- geocode(cleaned, address = address) |>
  rename(lon = long)

coded2 <- filter(geocoded2, !is.na(lat)) |>
  select(AddID, lat, lon) |>
  rbind(select(coded, AddID, lat, lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 'NAD83', remove = FALSE)

hooded2 <- filter(geocoded2, is.na(lat)) |>
  mutate(
    Neighborhood = case_when(
      str_detect(address, "Taft") ~ "East Walnut Hills",
      str_detect(address, "Misty") ~ "Bond Hill",
      str_detect(address, "Albans") ~ "Golf Manor",
      str_detect(address, "Vista") ~ "Hyde Park",
      str_detect(address, "Geyer") | str_detect(address, "Mclellan") ~ 
        "Westwood",
      str_detect(address, "Forestview") | str_detect(address, "Glen") ~ 
        "Green Township",
      str_detect(address, "Sweetbay") ~ "Colerain Township",
      str_detect(address, "toronto") ~ "Anderson Township",
      TRUE ~ NA
      )
    ) |>
  select(AddID, Neighborhood)

muni_lines <- county_subdivisions(state = "OH", county = "Hamilton") |>
  mutate(
    Municipality = str_remove(NAMELSAD, " city"),
    Municipality = str_remove(Municipality, " village"),
    Municipality = str_remove(Municipality, " CCD"),
    Municipality = str_remove(Municipality, "The Village of "),
    Municipality = str_to_title(Municipality)
  ) |>
  st_as_sf() 

munis <- st_join(muni_lines, coded2) |>
  filter(!is.na(AddID)) |>
  group_by(AddID) |>
  mutate(count = n()) 

hooded3 <- anti_join(as_tibble(coded2), munis, join_by(AddID)) |>
  mutate(Neighborhood = NA) |>
  select(AddID, Neighborhood)

single <- filter(munis, count == 1)

hooded4 <- filter(munis, count > 1) |>
  inner_join(adds_unique) |>
  mutate(
    Neighborhood = case_when(
     str_detect(Address2, "Mitchell") ~ "St. Bernard",
     str_detect(Address2, "Puhlman") | str_detect(Address2, "Harrison") ~ 
       "Cheviot",
     str_detect(Address2, "Section") ~ "Norwood",
     str_detect(Address2, "Murray") ~ "Columbia Township",
     str_detect(Address2, "Plainfield") ~ "Kennedy Heights",
     str_detect(Address2, "Arbre") ~ "Sycamore Township",
     str_detect(Address2, "Mayfair") ~ "Springfield Township",
     TRUE ~ NA
      )
    ) |>
  distinct(AddID, Neighborhood)

hooded5 <- filter(single, Municipality != "Cincinnati") |>
  rename(Neighborhood = Municipality) |>
  as_tibble() |>
  select(AddID, Neighborhood)

hood_lines <- neigh_sna |>
  mutate(SHAPE = st_transform(SHAPE, crs = "NAD83"))

cinci <- filter(single, Municipality == "Cincinnati") |>
  as_tibble() |>
  select(AddID, lat, lon) |>
  st_as_sf(coords = c("lon", "lat"), crs = "NAD83") |>
  st_join(hood_lines)

hooded6 <- filter(cinci, !is.na(neighborhood)) |>
  as_tibble() |>
  select(AddID, Neighborhood = neighborhood) 

hooded7 <- filter(cinci, is.na(neighborhood)) |>
  inner_join(adds_unique) |>
  mutate(
    Neighborhood = case_when(
      str_detect(Address2, "Hillsdale") ~ "Wyoming",
      str_detect(Address2, "Englewood") |
        str_detect(Address2, "Elbrook") |
        str_detect(Address2, "Montgomery") ~ "Pleasant Ridge",
      str_detect(Address2, "Caldwell") | str_detect(Address2, "Marley") ~
        "Springfield Township",
      str_detect(Address2, "Everett") | str_detect(Address2, "Herbert") ~ 
        "Cheviot",
      str_detect(Address2, "Old Red Bank") ~ "Oakley",
      str_detect(Address2, "Plainfield") ~ "Kennedy Heights",
      TRUE ~ NA
    )
  ) |>
  as_tibble() |>
  select(AddID, Neighborhood)

hooded <- rbind(hooded1, hooded2) |>
  rbind(hooded3) |>
  rbind(hooded4) |>
  rbind(hooded5) |>
  rbind(hooded6) |>
  rbind(hooded7)

# hooded_schools <- st_join(bg_lines, st_as_sf(schools3), left = FALSE) |>
#   as_tibble() |>
#   select(Neighborhood:SchoolType) |>
#   right_join(schools3) |>
#   mutate(
#     Neighborhood = case_when(
#       School == "Cheviot School" ~ "Cheviot",
#       School == "Silverton Elementary" ~ "Silverton",
#       TRUE ~ Neighborhood
#     )
#   )


df <- left_join(student, hooded) |>
  mutate(
    Neighborhood = fct_na_value_to_level(Neighborhood),
    StudentID = as.character(StudentID),
    SchoolID = as.character(SchoolID)
    ) |>
  inner_join(attend) |>
  mutate(CA = ifelse(PCNT < 90, "Chronically absent", "Not chronically absent")) |>
  left_join(schools2, join_by(SchoolID)) |>
  select(StudentID, Neighborhood, CA, School, SchoolID, SchoolType)

boundaries <- filter(muni_lines, Municipality != "Cincinnati") |>
  select(Neighborhood = Municipality, geometry) |>
  rbind(hood_lines |> rename(Neighborhood = neighborhood, geometry = SHAPE)) |>
  mutate(centroid = st_centroid(geometry))

hoods <- sort(unique(df$Neighborhood[df$Neighborhood %in% boundaries$Neighborhood]))

hood_graph <- function(a, b){
  x <- df |>
    filter(
      SchoolType == a,
      Neighborhood == b
      ) |>
      group_by(School, CA) |>
      summarise(Students = n()) |>
      ungroup() |>
      mutate(
        Total = sum(Students),
        Share = Students/Total
      ) |>
      group_by(School) |>
      mutate(SchoolShare = sum(Share))

  small <- filter(x, SchoolShare < .1)
  
  small <- small |>
    mutate(School = paste(length(unique(small$School)), "other schools")) |>
    group_by(CA, School) |>
    summarise(
      Students = sum(Students),
      Share = sum(Share),
      Total = mean(Total)
    ) |>
    ungroup() |>
    mutate(SchoolShare = sum(Share))
  
  x <- filter(x, SchoolShare >= .1) |>
    rbind(small)
  
  ggplot(x, aes(x = reorder(School, Share), y = Share, fill = CA)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = NULL, y = "%", title = b, fill = NULL) +
    scale_fill_manual(
      values = c(cchmcdarkblue, cchmclightblue)
    ) +
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, .2), 
      labels = seq(0, 100, 20)
    ) +
    geom_text(
      aes(label = Students), 
      y = ifelse(x$CA == "Not chronically absent", .03, x$SchoolShare+.03)
    ) +
    annotate(
      "text", 
      label = paste(format(mean(x$Total), big.mark = ","), "total students"), 
      x = .75, 
      y = .5
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = .5),
      legend.position = "bottom"
    )
}

hood_es_popups <- lapply(hoods, function(b){
  hood_graph("es", b)
})

hood_hs_popups <- lapply(hoods, function(b){
  hood_graph("hs", b)
})

schools3 <- inner_join(schools2, df, multiple = "all") |>
  distinct(School, Address, geometry) |>
  filter(!is.na(Address)) |>
  arrange(str_to_title(School))

school_list <- sort(unique(schools3$School))

school_graph <- function(k){
  x <- df |>
    filter(
      School == k,
      !is.na(Neighborhood)
      ) |>
    group_by(Neighborhood, CA) |>
    summarise(Students = n()) |>
    ungroup() |>
    mutate(
      Total = sum(Students),
      Share = Students/Total
    ) |>
    group_by(Neighborhood) |>
    mutate(HoodShare = sum(Share))
  
  small <- filter(x, HoodShare < .1)
  small <- small |>
    mutate(
      Neighborhood = paste(
        length(unique(small$Neighborhood)), 
        "other neighborhoods"
        )
      ) |>
    group_by(CA, Neighborhood) |>
    summarise(
      Students = sum(Students),
      Share = sum(Share),
      Total = mean(Total)
    ) |>
    ungroup() |>
    mutate(HoodShare = sum(Share))
  
  if (nrow(small) > 0){
    x <- filter(x, HoodShare >= .1) |>
      rbind(small)
  }
  
  ggplot(x, aes(x = reorder(Neighborhood, Share), y = Share, fill = CA)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = NULL, y = "%", title = k, fill = NULL) +
    scale_fill_manual(
      values = c(cchmcdarkblue, cchmclightblue)
    ) +
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, .2), 
      labels = seq(0, 100, 20)
    ) +
    geom_text(
      aes(label = Students), 
      y = ifelse(x$CA == "Not chronically absent", .03, x$HoodShare+.03)
    ) +
    annotate(
      "text", 
      label = paste(format(mean(x$Total), big.mark = ","), "total students"), 
      x = .75, 
      y = .5
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = .5),
      legend.position = "bottom"
      )
}

school_popups <- lapply(school_list, function(k){
  school_graph(k)
})

centroids <- st_coordinates(boundaries$centroid) |>
  as_tibble() |>
  mutate(
    Neighborhood = boundaries$Neighborhood,
    X = case_when(
      Neighborhood == "Sycamore Township" ~ -84.3788,
      Neighborhood == "Columbia Township" ~ -84.3976,
      TRUE ~ X
      ),
    Y = case_when(
      Neighborhood == "Sycamore Township" ~ 39.20386,
      Neighborhood == "Columbia Township" ~ 39.17114,
      Neighborhood == "East End" ~ 39.122,
      TRUE ~ Y
      )
    ) |>
  st_as_sf(coords = c("X", "Y"), crs = "NAD83") |>
  rename(centroid = geometry)

boundaries <- as_tibble(select(boundaries, -centroid)) |>
  inner_join(as_tibble(centroids)) |>
  st_as_sf()

ca <- df |>
  group_by(Neighborhood) |>
  summarise(CA = mean(CA == "Chronically absent")) |>
  inner_join(boundaries) |>
  mutate(
    Tier = case_when(
      CA >= .5 ~ "50%+",
      CA >= .4 ~ "40-49%",
      CA >= .3 ~ "30-39%",
      CA >= .2 ~ "20-29%",
      TRUE ~ "<20%"
    ),
    Tier = factor(Tier, levels = c("<20%", "20-29%", "30-39%", "40-49%", "50%+")),
    CApct = paste0(round(CA*100, 1), "%")
  ) |>
  arrange(str_to_title(Neighborhood)) |>
  st_as_sf()

pal <- colorFactor(
  c(
    cchmcdarkgreen, 
    cchmclightgreen, 
    cchmclightblue, 
    cchmclightpurple, 
    cchmcdarkpurple
    ), 
  domain = ca$Tier
)

labels <- sprintf(
  "<strong>%s</strong><br/>%s chronically absent",
  ca$Neighborhood, ca$CApct
) |>
  lapply(htmltools::HTML)

cinci_icons <- icons(
  iconUrl = ifelse(
    schools3$SchoolType == "es", 
    "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
    "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-yellow.png"
  ),
  iconWidth = 20,
  iconHeight = 30
)

school_map <- leaflet() |>
  addTiles() |>
  addPolygons(
    data = ca,
    stroke = TRUE,
    weight = 1,
    smoothFactor = .5,
    opacity = 1,
    fillOpacity = .7,
    fillColor = ~pal(Tier),
    highlightOptions = highlightOptions(
      weight = 5,
      color = cchmcdarkblue,
      fillOpacity = .7
    ),
    label = labels
  ) |>
  addLegendFactor(
    pal = pal,
    values = ca$Tier,
    title = "Chronic absence",
    position = "bottomleft"
  ) |>
  addLayersControl(
    baseGroups = c(
      "High schools by neighborhood", 
      "Elementary schools by neighborhood",
      "Neighborhoods by school"
    ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = ca$centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_hs_popups, height = 400, width = 500),
    group = "High schools by neighborhood"
  ) |>
  addCircleMarkers(
    data = ca$centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_es_popups, height = 400, width = 500),
    group = "Elementary schools by neighborhood"
  ) |>
  addCircleMarkers(
    data = schools3$geometry,
    color = cchmcdarkblue,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(school_popups, height = 400, width = 500),
    group = "Neighborhoods by school"
  ) 

school_map

count <- df |>
  mutate(
    CA = str_replace_all(CA, " ", "_"),
    Neighborhood = fct_na_value_to_level(Neighborhood, level = "NA")
    ) |>
  group_by(School, Neighborhood, CA) |>
  summarise(Students = n()) |>
  pivot_wider(
    id_cols = c(School, Neighborhood),
    names_from = CA,
    values_from = Students
  ) |>
  mutate(
    across(Chronically_absent:Not_chronically_absent, \(x) coalesce(x, 0)),
    Students = Chronically_absent + Not_chronically_absent
    ) |>
  select(-Not_chronically_absent) |>
  arrange(School, Neighborhood)

saveWidget(school_map, "school_map.html")
write_csv(count, "chronic attendance.csv")

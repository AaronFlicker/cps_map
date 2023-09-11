library(leaflegend)
library(leaflet)
library(leafpop)
library(sf)
library(stringdist)
library(tidygeocoder)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)

schools <- read_delim("cps.txt", delim = " ")
colnames(schools) <- c(LETTERS[1:5], LETTERS[7:13])
schools2 <- schools |>
  mutate(
    Num1 = parse_number(C),
    Num2 = parse_number(D),
    Num3 = parse_number(E),
    Num4 = parse_number(G),
    Num5 = parse_number(H),
    Num6 = parse_number(I),
    Num7 = parse_number(J),
    Number = coalesce(Num1, Num2),
    Number = coalesce(Number, Num3),
    Number = coalesce(Number, Num4),
    Number = coalesce(Number, Num5),
    Number = coalesce(Number, Num6),
    Number = coalesce(Number, Num7),
    Number = ifelse(B == "Promise", "5425", Number),
    Name = paste(A, B),
    Name = ifelse(C == Number, Name, paste(Name, C)),
    Name = ifelse(Number == C | Number == D, Name, paste(Name, D)),
    Name  = ifelse(
      Number == C | Number == D | Number == E,
      Name,
      paste(Name, E)
    ),
    Name  = ifelse(
      Number == C | Number == D | Number == E | Number == G,
      Name,
      paste(Name, G)
    ),
    Name  = ifelse(
      Number == C | Number == D | Number == E | Number == G | Number == H,
      Name,
      paste(Name, H)
    ),
    Combo = paste(A, B, C, D, E, G, H, I, J, K, L, M),
    Combo = str_remove(Combo, Name),
    Combo = str_remove(Combo, Number),
    Combo = str_trim(Combo)
  ) |>
  separate_wider_delim(
    Combo, 
    delim = ",", 
    names = c("Street", "City", "Rest"), 
    too_few = "align_start"
  ) |>
  mutate(Rest = str_trim(Rest)) |>
  separate_wider_delim(
    Rest, 
    delim = " ", 
    names = c("State", "Zip"), 
    too_many = "drop"
  ) |>
  mutate(
    City = "Cincinnati",
    State = "OH",
    Street = ifelse(B == "Promise", "Winton Ridge Lane", Street),
    Zip = ifelse(Name == "Mt. Washington School", "45230", Zip),
    Name = ifelse(B == "Promise", "The Promise Center", Name),
    Name = ifelse(
      str_detect(Name, "Gamble Montessori Elementary"),
      "Gamble Montessori Elementary School",
      Name
    ),
    Name = ifelse(str_detect(Name, "Aiken"), "Aiken High School", Name)
  ) |>
  distinct(Name, Number, Street, City, State, Zip) |>
  filter(
    !Name %in% c(
      "Cincinnati Digital Academy", 
      "Virtual High School",
      "Hospital/Satellite Program Office"
    )
  ) |>
  mutate(
    Address = paste(Number, Street),
    Type = "School",
    Name = 
      case_when(
        str_detect(Name, "AMIS") ~ "AMIS School",
        Name == "Rising Stars at Vine" ~ "RS at Vine",
        Name == "Clifton Area Neighborhood School" ~ "CANS School",
        TRUE ~ Name
      )
  ) |>
  filter(!Name %in% c(
    "Juvenile Detention Center", 
    "The Promise Center"
    )
    ) |>
  select(-c(Street, Number))

geocoded_schools <- geocode(
  schools2,
  street = Address,
  city = City,
  state = State,
  postalcode = Zip,
  method = "census"
) |>
  st_as_sf(coords = c("long", "lat"), crs = "NAD83")

school <- read_delim(
  "school.txt",
  col_names = c("SchoolID", "School", "SchoolType", "SchoolStateCode"),
  skip = 1
) |>
  mutate(SchoolID = as.character(SchoolID)) |>
  filter(!is.na(SchoolType)) 

schools3 <- cross_join(school, geocoded_schools) |>
  mutate(dist = stringdist(School, Name, method = "jw")) |>
  group_by(Name) |>
  filter(dist == min(dist)) |>
  ungroup() |>
  select(SchoolID:SchoolType, geometry) |>
  arrange(str_to_title(School))

allocations <- read.csv("~/neighborhood bg allocations.csv")
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
    "Zip", 
    rep(NA, 13), 
    "Remote"
    ),
  skip = 1
  )

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
  )

adds <- student |>
  filter(!is.na(Address)) |>
  mutate(Address2 = Address) |>
  separate_wider_delim(
    Address2,
    delim = ",",
    names = "Address2",
    too_many = "drop",
    too_few = "align_start"
  ) 
  
adds_unique <- adds |>
  distinct(Address2, Zip) |>
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

uncoded <- geocoded |>
  filter(
    matched_state != "OH" |
      is.na(lat)
  )

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
    address = str_replace(address, "Seventieth", "70th")
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
      AddID == 5969 ~ "Springfield Township",
      AddID %in% c(15110, 17136) ~ "Green Township",
      AddID == 18173 ~ "Anderson Township",
      str_detect(address, "St Albans") ~ "Golf Manor",
      AddID == 11493 ~ "College Hill",
      AddID %in% c(1407, 7477) ~ "Westwood",
      str_detect(address, "Mclelland") ~ "Westwood",
      AddID == 12409 ~ "Avondale",
      AddID == 18013 ~ "Hyde Park",
      str_detect(address, "Misty") ~ "Bond Hill",
      AddID == 12530 ~ "Walnut Hills",
      AddID == 18568 ~ "Bond Hill",
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

multi <- filter(munis, count > 1) |>
  inner_join(adds_unique) |>
  mutate(
    Neighborhood = case_when(
      AddID == 14696 ~ "St. Bernard",
      AddID %in% c(3898, 8048) ~ "Cheviot",
      AddID == 7462 ~ "Norwood",
      str_detect(Address2, "Murray") ~ "Columbia Township",
      AddID == 6061 ~ "Sycamore Township",
      AddID == 13424 ~ "Springfield Township"
      )
    ) |>
  distinct(AddID, Neighborhood)

hooded4 <- filter(multi, !is.na(Neighborhood))

hooded5 <- filter(single, Municipality != "Cincinnati") |>
  rename(Neighborhood = Municipality) |>
  as_tibble() |>
  select(AddID, Neighborhood)

cinci <- single |>
  rbind(
    inner_join(munis, filter(multi, is.na(Neighborhood)) 
               |> select(-Neighborhood))
    ) |>
  filter(Municipality == "Cincinnati") |>
  as_tibble() |>
  select(AddID, lat, lon) |>
  st_as_sf(coords = c("lon", "lat"), crs = 'NAD83', remove = FALSE)

bg_lines <- block_groups(state = "OH", county = "Hamilton") |>
  select(GEOID, geometry) |>
  mutate(GEOID = as.numeric(GEOID)) |>
  inner_join(allocations, multiple = "all") |>
  filter(Municipality == "Cincinnati") |>
  st_as_sf() 
  
bg <- st_join(bg_lines, cinci) |>
  filter(!is.na(AddID))

bg2 <- bg |>
  distinct(AddID, Neighborhood) |>
  group_by(AddID) |>
  mutate(count = n())

hooded6 <- filter(bg2, count == 1) |>
  select(-count) 

hooded7 <- filter(bg2, count > 1) |>
  distinct(AddID) |>
  inner_join(bg, multiple = "all") |>
  inner_join(adds_unique) |>
  mutate(
    Neighborhood = case_when(
      AddID == 2073 ~ "Avondale",
      AddID %in% c(5990, 5903) ~ "College Hill",
      AddID %in% c(5554, 18917, 2893, 6505) ~ "CUF",
      AddID == 1766 ~ "East Price Hill",
      AddID %in% c(18400, 6359, 13652) ~ "East Walnut Hills",
      AddID == 11080 ~ "East Westwood",
      AddID == 16354 ~ "Hyde Park",
      AddID == 2978 ~ "Millvale",
      AddID %in% c(10216, 9335, 3228, 9787) ~ "Mt. Airy",
      AddID %in% c(10289, 2222, 13849) ~ "Mt. Auburn",
      AddID %in% c(17470, 19793) ~ "Mt. Lookout",
      AddID == 19607 ~ "North Avondale",
      AddID %in% c(1014, 19699, 2312) ~ "North Fairmount",
      AddID %in% c(920, 19379) ~ "Paddock Hills",
      AddID %in% c(14900, 297) ~ "Pleasant Ridge",
      AddID == 19104 ~ "Sayler Park",
      AddID %in% c(11805, 9389) ~ "South Fairmount",
      AddID == 7223 ~ "Walnut Hills",
      AddID == 5872 ~ "West Price Hill",
      AddID %in% c(3788, 8952, 3852) ~ "Westwood",
      str_detect(Address2, "Seymour") ~ "Roselawn",
      str_detect(Address2, "St Leo") ~ "North Fairmount",
      TRUE ~ NA
    )
  ) |>
  distinct(AddID, Neighborhood)

hooded <- rbind(hooded1, hooded2) |>
  rbind(hooded3) |>
  rbind(hooded4) |>
  rbind(hooded5) |>
  rbind(hooded6) |>
  rbind(hooded7)

student <- left_join(student, hooded)

student_add <- select(student, StudentID, Neighborhood)

# student2 <- read_delim("student.txt") |>
#   inner_join(student_add, by = c("Student ID" = "StudentID"))
# write_csv(student2, "student with neighborhood.csv")  

df <- student |>
  mutate(
    StudentID = as.character(StudentID),
    SchoolID = as.character(SchoolID)
    ) |>
  left_join(attend) |>
  mutate(CA = PCNT < 90) |>
  left_join(schools3) |>
  select(StudentID:GradeLevel, Neighborhood, CA:SchoolType)

hoods <- sort(unique(df$Neighborhood))

hood_es <- function(k){
  x <- df |>
    filter(
      Neighborhood == k,
      SchoolType == "es"
      ) |>
    group_by(School) |>
    summarise(Students = n()) |>
    ungroup() |>
    mutate(
      Total = sum(Students),
      Share = Students/Total
      )
  
  small <- filter(x, Share < .01) |>
    summarise(
      Students = sum(Students),
      Share = sum(Share),
      Total = mean(Total)
      ) |>
    mutate(School = "All other schools")
  if (small$Students > 0){
    x <- filter(x, Share >= .01) |>
      rbind(small)
  }
  
  ggplot(x, aes(x = reorder(School, Share), y = Share)) +
    geom_bar(stat = "identity", fill = cchmclightblue) +
    coord_flip() +
    labs(x = NULL, y = "%", title = k) +
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, .2), 
      labels = seq(0, 100, 20)
      ) +
    geom_text(aes(label = Students), hjust = -.5) +
    annotate(
      "text", 
      label = paste(mean(x$Total), "total students"), 
      x = .75, 
      y = .5
      ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5))
}

hood_es_popups <- lapply(hoods, function(k){
  hood_es(k)
})

hood_hs <- function(k){
  x <- df |>
    filter(
      Neighborhood == k,
      SchoolType == "hs"
    ) |>
    group_by(School) |>
    summarise(Students = n()) |>
    ungroup() |>
    mutate(
      Total = sum(Students),
      Share = Students/Total
    )
  
  small <- filter(x, Share < .01) |>
    summarise(
      Students = sum(Students),
      Share = sum(Share),
      Total = mean(Total)
    ) |>
    mutate(School = "All other schools")
  if (small$Students > 0){
    x <- filter(x, Share >= .01) |>
      rbind(small)
  }

  ggplot(x, aes(x = reorder(School, Share), y = Share)) +
    geom_bar(stat = "identity", fill = cchmclightblue) +
    coord_flip() +
    labs(x = NULL, y = "%", title = k) +
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, .2), 
      labels = seq(0, 100, 20)
    ) +
    geom_text(aes(label = Students), hjust = -.5) +
    annotate(
      "text", 
      label = paste(mean(x$Total), "total students"), 
      x = .75, 
      y = .5
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5))
}

hood_hs_popups <- lapply(hoods, function(k){
  hood_hs(k)
})

school_list <- sort(unique(df$School))

school_graph <- function(k){
  x <- df |>
    filter(
      School == k,
      !is.na(Neighborhood)
      ) |>
    group_by(Neighborhood) |>
    summarise(Students = n()) |>
    ungroup() |>
    mutate(
      Total = sum(Students),
      Share = Students/Total
    )
  
  small <- filter(x, Share < .01) |>
    summarise(
      Students = sum(Students),
      Share = sum(Share),
      Total = mean(Total)
    ) |>
    mutate(Neighborhood = "All other neighborhoods")
  
  if (small$Students > 0){
    x <- filter(x, Share >= .01) |>
      rbind(small) 
  }
  
  ggplot(x, aes(x = reorder(Neighborhood, Share), y = Share)) +
    geom_bar(stat = "identity", fill = cchmclightblue) +
    coord_flip() +
    labs(x = NULL, y = "%", title = k) +
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, .2), 
      labels = seq(0, 100, 20)
    ) +
    geom_text(aes(label = Students), hjust = -.5) +
    annotate(
      "text", 
      label = paste(mean(x$Total), "total students"), 
      x = .75, 
      y = .5
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5))
}

school_popups <- lapply(school_list, function(k){
  school_graph(k)
})

hood_lines <- bg_lines |>
  group_by(Neighborhood) |>
  summarise(geometry = st_union(geometry))

all_lines <- select(muni_lines, Municipality, geometry) |>
  filter(Municipality != "Cincinnati") |>
  rename(Neighborhood = Municipality) |>
  rbind(hood_lines) |>
  mutate(centroid = st_centroid(geometry))

ca <- df |>
  group_by(Neighborhood) |>
  summarise(CA = mean(CA, na.rm = TRUE)) |>
  inner_join(all_lines) |>
  mutate(
    Tier = case_when(
      CA >= .5 ~ "50%+",
      CA >= .4 ~ "40-49%",
      CA >= .3 ~ "30-39%",
      CA >= .2 ~ "20-29%",
      TRUE ~ "<20%"
    ),
    Tier = factor(Tier, levels = c("<20%", "20-29%", "30-39%", "40-49%", "50%+"))
  ) |>
  arrange(str_to_title(Neighborhood)) |>
  st_as_sf()

pal <- colorFactor(
  c("#76BC44", "#A1CA3C", "#9BD3DD", "#CA5699", "#83286B"), 
  domain = ca$Tier
)

leaflet() |>
  addTiles() |>
  addPolygons(
    data = ca,
    stroke = TRUE,
    weight = 1,
    smoothFactor = .5,
    opacity = 1,
    fillOpacity = .7,
    fillColor = ~pal(Tier)
  ) |>
  addLegendFactor(
    pal = pal,
    values = ca$Tier,
    title = "Chronic absence",
    position = "bottomleft"
  ) |>
  addLayersControl(
    baseGroups = c(
      "High schools", 
      "Elementary schools",
      "Neighborhoods"
    ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = ca$centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_hs_popups, height = 400, width = 500),
    group = "High schools"
  ) |>
  addCircleMarkers(
    data = ca$centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_es_popups, height = 400, width = 500),
    group = "Elementary schools"
  ) |>
  addCircleMarkers(
    data = schools3$geometry,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(school_popups, height = 400, width = 500),
    group = "Neighborhoods"
  )

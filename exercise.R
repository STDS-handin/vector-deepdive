library(tidyverse)
library(sf)
library(tmap)
library(units)
library(tictoc)
library(duckdb)



# lets now read in the canton polynomials
canton_polygons <- st_read(
  "./data/swissBOUNDARIES3D_1_5_LV95_LN02.gpkg",
  layer = "tlm_kantonsgebiet"
  )

# Lets have a look at this to have a visual understanding
tm_shape(canton_polygons) +
  tm_polygons()

# yes we got the canton polynomials


# Next we need the forest polygons from swisstlm3d.
PATH[2] %>% st_layers()
# we need the layer "tlm_bb_bodenbedeckung". Here, forest is found in 3 different categories according to the official
# catalogue: wald, wald offen, gehölzfläche


ground_cover_polys <- st_read(
  ".data/SWISSTLM3D_2025.gpkg", 
  layer = "tlm_bb_bodenbedeckung"
  )

ground_cover_polys %>%
  pull("objektart") %>%
  unique()

# the polygons have codes. We can simply filter by code to optain the polygons
# (tidyverse functions can also by applied to sf_objects since these are dataframes too)
# now wheter we consider "Gehölzfläche" also to be Forest or not is in a way arbitrary and will change the final result.
# I decide here to also include it, however it may aswell also be excluded


forest_polys <- ground_cover_polys %>%
  filter(objektart == "Wald")

# what is realy important here, is to accnoledge that the different area types can overlap
# according to the manual. Therefore we need to get the union of all polygons first, so that
# we later do not count surfaces double


# to avoid memory spikes, it is better to carry out the computations sequentially for every
# canton
calculate_area_per_canton <- function(canton_name, canton_polys, forest_polys) {
  # get the entry of the canton from the sf_object (this also contains the polynomial)
  target_canton <- canton_polys %>%
    filter(name == canton_name)

  # intersect the forest polynomials of all of switzerland with the canton to get new poylnomails of
  # all forest surfaces that cover the canton
  forest_polys_intersected <- st_intersection(forest_polys, target_canton)

  # as stated in the swissTLM3D manual, the different land covers considered as forest, can overlap
  # so first we need to get the union of all surfaces to not sum any surface double
  # then we can summ up the area
  forested_area_per_canton <- forest_polys_intersected %>%
    st_area() %>%
    sum()

  # lastly we return a named list with the canton name and the summed area
  return(
    list(name = canton_name, forested_area = forested_area_per_canton)
  )
}

# we get all the canton names
canton_names <- canton_polygons %>%
  pull(name) %>%
  unique()



# this takes for ever even tough the curvature of the earth is ignored (to speed up)

# and then we apply the calculation to every canton sequentially to get the forested area


results_list <- map(
  canton_names,
  ~ calculate_area_per_canton(.x, canton_polygons, forest_polys),
  .progress = "Calculation Progress"
)




# the resulting named lists can be combined to a dataframe
results_df <- results_list %>%
  bind_rows()

# then we can join the caluclated areas with the geometries of the canton
canton_forest_sf <- canton_polygons %>%
  left_join(results_df, by = "name") %>%
  mutate(
    # we also calculate the total area of forest now to get percentage values later
    total_canton_area = st_area(geom),

    # Calculate the percentage (Forest Area / Total Area * 100)
    forest_pct = as.numeric(forested_area / total_canton_area) * 100
  )

# 3. Plot the PERCENTAGE on the map
tm_shape(canton_forest_sf) +
  tm_polygons(
    col = "forest_pct", # Point to the new percentage column
    style = "jenks",
    palette = "Greens",
    title = "Forest Cover (%)" # Update legend title
  ) +
  tm_layout(
    main.title = "Percentage of Forest Cover by Swiss Canton",
    frame = FALSE
  )

canton_forest_sf %>%
  select(name, forest_pct) %>%
    arrange(desc(forest_pct))


con <- dbConnect(
  duckdb(),
  dbdir = "./data/wald-kantone.duckdb",
  read_only = TRUE
)




dbExecute(con, "INSTALL spatial;")
dbExecute(con, "LOAD spatial;")

tictoc::tic()

canton_frac <- dbReadTable(con, "cantonal_forest_fractions")

tictoc::toc()

print(canton_frac)




tictoc::tic()


results_list <- map(
  canton_names,
  ~ calculate_area_per_canton(.x, canton_polygons, forest_polys),
  .progress = "Calculation Progress"
)


results_df <- results_list %>%
  bind_rows()


canton_forest_sf <- canton_polygons %>%
  left_join(results_df, by = "name") %>%
  mutate(
    # we also calculate the total area of forest now to get percentage values later
    total_canton_area = st_area(geom),

    # Calculate the percentage (Forest Area / Total Area * 100)
    forest_pct = as.numeric(forested_area / total_canton_area) * 100
  )


canton_forest_sf %>%
  select(name, forest_pct) %>%
    arrange(desc(forest_pct))
    

tictoc::toc()


dbGetQuery(con, "SELECT sql FROM duckdb_indexes();")


dbDisconnect(con)

getwd()

grid_orig <- st_read(
  "./data/chessboard.gpkg",
   layer = "grid_orig"
  )

grid_dest <- st_read(
  "./data/chessboard.gpkg",
   layer = "grid_dest"

)


st_rook <- \(x, y) st_relate(x, y, pattern = "F***1****")

grid_rook <- grid_dest[grid_orig, , op = st_rook] |> 
  st_sample(1000, type = "hexagonal",by_polygon = TRUE)


st_bishop <- \(x, y) st_relate(x, y, pattern = "F***0****")

grid_bishop <- grid_dest[grid_orig, , op = st_bishop] |> 
  st_sample(1000, type = "hexagonal",by_polygon = TRUE)


st_queen <- \(x, y) st_relate(x, y, pattern = "F***T****")

grid_queen <- grid_dest[grid_orig, , op = st_queen] |> 
  st_sample(1000, type = "hexagonal",by_polygon = TRUE)



# --- 1. BISHOP VISUALIZATION ---
plot(st_geometry(grid_dest), col = "#8fbc8f")
plot(st_geometry(grid_orig), col = "#c8bfe7", add = TRUE)

# Generate dots for Bishop (corner touches)
dots_bishop <- grid_dest[grid_orig, , op = st_bishop] |> 
  st_sample(1000, type = "hexagonal", by_polygon = TRUE)

plot(st_geometry(dots_bishop), pch = 20, cex = 0.5, add = TRUE)
text(st_coordinates(st_centroid(grid_orig)), labels = "♗", cex = 10)

# --- 2. QUEEN VISUALIZATION ---
plot(st_geometry(grid_dest), col = "#8fbc8f")
plot(st_geometry(grid_orig), col = "#c8bfe7", add = TRUE)

# Generate dots for Queen (all touches)
dots_queen <- grid_dest[grid_orig, , op = st_queen] |> 
  st_sample(1000, type = "hexagonal", by_polygon = TRUE)

plot(st_geometry(dots_queen), pch = 20, cex = 0.5, add = TRUE)
text(st_coordinates(st_centroid(grid_orig)), labels = "♕", cex = 10)

# install.packages("tidyverse")
# install.packages("sf")
# install.packages("tmap")

library(tidyverse)
library(sf)
library(tmap)

FILES <- list.files("./data")
PATH <- file.path("./data", FILES)


# First we need to look at what layers are present to pick the right one. We are interested in the canton boundaries
PATH[1] %>% st_layers()
# therefore tlm_kantonsgebiete is the layer of interest for us


# lets now read in the canton polynomials
canton_polygons <- PATH[1] %>%
  st_read(layer = "tlm_kantonsgebiet")

# Lets have a look at this to have a visual understanding
tm_shape(canton_polygons) +
  tm_polygons()

# yes we got the canton polynomials


# Next we need the forest polygons from swisstlm3d.
PATH[2] %>% st_layers()
# we need the layer "tlm_bb_bodenbedeckung". Here, forest is found in 3 different categories according to the official
# catalogue: wald, wald offen, gehölzfläche


ground_cover_polys <- PATH[2] %>%
  st_read(layer = "tlm_bb_bodenbedeckung")


# the polygons have codes. We can simply filter by code to optain the polygons
# (tidyverse functions can also by applied to sf_objects since these are dataframes too)
# now wheter we consider "Gehölzfläche" also to be Forest or not is in a way arbitrary and will change the final result.
# I decide here to also include it, however it may aswell also be excluded


forest_polys <- ground_cover_polys %>%
  filter(objektart %in% c("Wald", "Gehoelzflaeche", "Wald offen"))

# what is realy important here, is to accnoledge that the different area types can overlap
# according to the manual. Therefore we need to get the union of all polygons first, so that
# we later do not count surfaces double


## scraped idea

## forest_polys %>% colnames()
##
##
## joined_polys <- st_join(
##         x = canton_polygons,
##         y = forest_polys,
##         join = st_intersects
##
##     )
##
##
##
## joined_polys %>%
##     mutate(
##         area_of_polys = st_area(geom)
##     )
##
##
# this strategy is to memmory hungry since the join generates a huge
# dataframe. I gues its better to perform this in a map sequentially


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
    st_union() %>%
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

sf_use_s2(FALSE)

# and then we apply the calculation to every canton sequentially to get the forested area


results_list <- map(
  canton_names,
  ~ calculate_area_per_canton(.x, canton_polygons, forest_polys),
  .progress = "Calculation Progress"
)

sf_use_s2(TRUE)

library(units)

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

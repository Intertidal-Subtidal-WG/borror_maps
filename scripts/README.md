### Code for "Notes from the Past Show How Local Variability can Stymie Urchins and the Rise of the Reds in the Gulf of Maine"

**Files**

- 1_clean_polygons.R - Loads polygons from digitized shapefiles. Cleans and makes them valid. Then splits polygons with mutiple habitat types into separate overlapping polygons, but with a percentage of 1/no. of species for future analyses of cover. Writes out cleaned shapefile of polygons. Also takes a shapefile of Maine state lines and generates a shapefile of Appledore for plotting.

- 2_find_polygon_overlap_with_line.R - Turns larger polygons of habitats into lines along 1.5m depth bathymetry line of entire island. Also calculates total % of island covered by each habitat type and outputs figures.

- 2a_plot_all_species_over_time_map.R - Uses lines along 1.5m depth to make a plot of maps of all habitat types around the island over time.

- 3_exposure_splits.R - Takes line shapefile and splits it up by quadrants of the island, adding relevant information. Generates output file for analysis and makes plot of habitat types over time in different quadrats of the island.

- 4_exposure_analysis.R - Beta regression models looking at whether change in habitat types over time depended on quadrant of the island. Outputs stats tables and posthoc figures.

- 5_dominant_stand_analysis.R - generates maps of change in grouped habitat types (i.e., reds, urchin barrens, kelp, red-kelp mixes) over time
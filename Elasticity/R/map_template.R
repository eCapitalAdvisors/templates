input_illinois_map <-
function(illinois_map_path) {
  illinois_map <- geojson_read(illinois_map_path, what = "sp")
  
  saveRDS(object = illinois_map, file = "../R/illinois_map.rds")
  
  return(illinois_map)
}

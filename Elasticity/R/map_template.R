input_illinois_map <-
function(illinois_map_path) {
  illinois_map <- geojson_read(illinois_map_path, what = "sp")
  return(illinois_map)
}

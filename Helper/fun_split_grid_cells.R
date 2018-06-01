print("Have an error in the meta data I need to fix, short term fix is checking the bbox")
bbox = data.frame(xll = 113.338953078, yll = -43.6345972634, xur = 153.569469029, yur = -10.6681857235)
# can fix this by using grep

# inputs
library(ggplot2)
meta_data = readRDS("Data/AS_meta_data.rds")
max_stns = 25
min_stns = 2
dx = dy = 2
min_years = 50


coord <- meta_data %>%
  # Have an error in the meta data I need to fix, short term fix is checking the bbox
  filter(longitude >= bbox$xll & longitude <= bbox$xur) %>%
  filter(latitude >= bbox$yll & latitude <= bbox$yur) %>%
  # Restrict the years
  mutate(num_years = pmin(last_year, end_year) - pmax(first_year, start_year) + 1) %>%
  filter(num_years >= min_years) %>%
  select(longitude, latitude)

# Get lower corners
lower_corners = coord %>%
  mutate(xll = floor(longitude/dx)*dx) %>%
  mutate(yll = floor(latitude/dy)*dy) %>%
  mutate(corner = paste(xll, yll))

# count lower corners
count_corners <- lower_corners %>%
  select(corner) %>%
  count(corner)

# join the count
lower_corners <- lower_corners %>%
  left_join(count_corners, by = "corner") %>%
  # only keep corners with a minimum number of stations
  filter(n >= min_stns) %>%
  mutate(cell_dx = dx) %>%
  mutate(cell_dy = dy)

# keep corners
keep_lower_corners <- lower_corners %>%
  filter(n <= max_stns) %>%
  select(-n)

#split corners
temp_split_lower_corners <- lower_corners %>%
  filter(n > max_stns) %>%
  select(-n)

while(nrow(temp_split_lower_corners) > 0){

  dx = dx/2
  dy = dy/2

  # get the lower corners
  temp_lower_corners <- temp_split_lower_corners %>%
    mutate(new_xll = floor(longitude/dx)*dx) %>%
    mutate(new_yll = floor(latitude/dy)*dy) %>%
    mutate(new_corner = paste(new_xll, new_yll)) %>%
    mutate(new_dx = dx) %>%
    mutate(new_dy = dy)

  # count lower corners
  temp_count_corners <- temp_lower_corners %>%
    select(new_corner) %>%
    count(new_corner) %>%
    rename(new_n = n)

  # join the count
  temp_lower_corners <- temp_lower_corners %>%
    left_join(temp_count_corners, by = "new_corner")

  # do we need to revert to old corner for any cells ?
  check_lower_corners <- temp_lower_corners %>%
    filter(new_n < min_stns)
  if(nrow(check_lower_corners) > 0){
    old_corners <- unique(check_lower_corners$corner)
    temp_keep_lower_corners <- temp_lower_corners %>%
      filter(corner %in% old_corners) %>%
      select(longitude, latitude, xll, yll, corner, cell_dx, cell_dy)
    keep_lower_corners <- rbind(keep_lower_corners, temp_keep_lower_corners)
    temp_lower_corners <- temp_lower_corners %>%
      filter(!(corner %in% old_corners))
  }

  # update the corners
  temp_lower_corners <- temp_lower_corners %>%
    select(longitude, latitude, new_xll, new_yll, new_corner, new_dx, new_dy, new_n) %>%
    rename(xll = new_xll, yll = new_yll, corner = new_corner, cell_dx = new_dx, cell_dy = new_dy)

  # keep corners
  temp_keep_lower_corners <- temp_lower_corners %>%
    filter(new_n <= max_stns) %>%
    select(-new_n)
  keep_lower_corners <- rbind(keep_lower_corners, temp_keep_lower_corners)

  #split corners
  temp_split_lower_corners <- temp_lower_corners %>%
    filter(new_n > max_stns) %>%
    select(-new_n)

}

lower_corners <- keep_lower_corners

# plot the grid
plot_locations <- ggplot() +
  geom_point(data = coord, aes(x = longitude, y = latitude),
             size = 0.01, alpha = 0.2, shape = 20, col = "blue") +
  geom_point(data = lower_corners, aes(x = xll, y = yll), size = 0.1, shape = 3) +
  theme_bw()
plot_locations


library(tidyverse)
library(rgdal)

# Contains a file for every cell in a grid of The Netherlands
# every such file has an elevation level for every 5 meter
feed <- read_file("http://geodata.nationaalgeoregister.nl/ahn2/atom/ahn2_5m.xml")

project <- function(spdf, axis) {
  # Convert a SpatialGridDataFrame to a dataframe
  # by projecting the points onto an axis
  
  center <- spdf@grid@cellcentre.offset[[axis]] # cell center wrt bottom left 
  dim <- spdf@grid@cells.dim[[axis]] # number of cells on an axis
  size <- spdf@grid@cellsize[[axis]] # size of a cell
  
  # Calculate start and end indices (lat or long, depending on `axis`)
  start <- center - (size/2)
  end <- start + (size*(dim-1))
  
  # Convert to matrix, take max per row/column
  if (axis == 1) {
    y <- matrix(spdf$band1, nrow = dim)
  } else {
    y <- matrix(spdf$band1, ncol = dim)
  }
  y <- apply(y, axis, function(x) max(c(-Inf, x), na.rm = T))
  
  # Convert to dataframe
  newdf <- data.frame("x" = seq(start, end, length.out = dim), "elevation" = y)
  return(newdf)
}

url_shp_to_df <- function(url, axis) {
  # Given a url, downloads its contents as a .zip, convertes to GDAL,
  # projects onto axis
  
  # Create temporary file to store the download in
  temp <- tempfile(fileext = ".zip")
  # Download the file and unzip it
  download.file(url, temp, quiet = TRUE)
  unzip(temp)
  # Obtain .tif files contained in .zip
  shp <- dir(tempdir(), "*.tif$")
  
  # Convert to GDAL and project onto axis
  y <- readGDAL(shp, silent = TRUE)
  y <- project(y, axis)
  
  # Remove temporary files
  file.remove(temp)
  file.remove(shp)
  
  return(y)
}

urls_shp_to_df <- function(urls, axis) {
  # Get temporary directory to store the files in
  wd <- getwd()
  td <- tempdir()
  setwd(td)
  
  # For every url (corresponding to a grid cell),
  # find its maximum over the axis 
  # Use llply because of its progress bar
  y <- plyr::llply(urls, url_shp_to_df, 
                   axis = axis,
                   .progress = 'text')
  y <- bind_rows(y)
  
  # Set the working directory back to the old working directory
  unlink(dir(td))
  setwd(wd)
  
  y <- y %>%
    arrange(x) %>%
    group_by(x) %>%
    summarise(elevation = max(elevation)) %>%
    ungroup()
  
  return(y)
}

# Obtain links to these files
links <- str_extract_all(feed, "http[:/.\\w]+\\.zip")[[1]] %>% unique

# Get elevation levels 
df <- urls_shp_to_df(links, axis = 2)

ggplot(df, aes(x = x, y = elevation)) + geom_line()

p <- ggplot(df, aes(x = x, y = elevation)) +
  geom_smooth(se = FALSE) +
  theme_void()

# Save plot as an svg
ggsave(filename = "elevation.svg", plot = p)

library(sf)
library(transformr)

create_interpolation <- function(square_side_length, circle_radius, num_interpolations, which = NULL) {
  # Create a square
  create_square <- function(center, side_length) {
    x <- center[1]
    y <- center[2]
    half_side <- side_length / 2
    square_coords <- matrix(c(x - half_side, y - half_side,
                              x + half_side, y - half_side,
                              x + half_side, y + half_side,
                              x - half_side, y + half_side,
                              x - half_side, y - half_side), ncol = 2, byrow = TRUE)
    square <- st_polygon(list(square_coords)) %>% st_sfc() %>% st_as_sf()
    return(square)
  }

  # Create a circle
  create_circle <- function(center, radius) {
    point <- st_point(center) %>% st_sfc()
    circle <- st_buffer(point, dist = radius, nQuadSegs = 100) %>% st_as_sf()
    return(circle)
  }

  # Generate circle and square points
  n_points <- 100
  center <- c(square_side_length / 2, square_side_length / 2)
  circle <- create_circle(center, circle_radius)
  square <- create_square(centre, square_side_length)

  # Interpolate between circle and square using transformr
  interpolated_shapes <- tween_sf(circle, square, ease = "linear", nframes = num_interpolations)

  if(!is.null(which)) {
    interpolated_shapes <- interpolated_shapes[which,]
  }

  return(interpolated_shapes)
}

# Example usage
square_side_length <- 10
circle_radius <- 2
num_interpolations <- 5
interpolated_shapes <- create_interpolation(square_side_length, circle_radius, num_interpolations)



plot(interpolated_shapes[[1]]) # Change the index to see other interpolated shapes
plot(interpolated_shapes[[2]])
plot(interpolated_shapes[[3]])


library(sf)

# Create a square
create_square <- function(center, side_length) {
  x <- center[1]
  y <- center[2]
  half_side <- side_length / 2
  square_coords <- matrix(c(x - half_side, y - half_side,
                            x + half_side, y - half_side,
                            x + half_side, y + half_side,
                            x - half_side, y + half_side,
                            x - half_side, y - half_side), ncol = 2, byrow = TRUE)
  square <- st_polygon(list(square_coords)) %>% st_sfc() %>% st_as_sf()
  return(square)
}

# Create a circle
create_circle <- function(center, radius) {
  point <- st_point(center) %>% st_sfc()
  circle <- st_buffer(point, dist = radius, nQuadSegs = 100) %>% st_as_sf()
  return(circle)
}

# Example usage
square_center <- c(5, 5)
square_side_length <- 10
square <- create_square(square_center, square_side_length)

circle_center <- c(5, 5)
circle_radius <- 3
circle <- create_circle(circle_center, circle_radius)

# Plot the square and circle
plot(square, col = "blue")
plot(circle, col = "red", add = TRUE)

test <- tween_sf(circle, square, ease = "linear", nframe = 100)


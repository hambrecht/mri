# Tests for check_survey_data() and check_dsm_data().
# Run from the project root: Rscript workflow/R/test-check_survey_data.R

suppressMessages({
  library(here)
  library(sf)
  library(terra)
})
source(here("workflow", "R", "check_survey_data.R"))

quiet <- function(expr) { out <- NULL; capture.output(out <- expr); invisible(out) }
status_of <- function(res, check) res$status[res$check == check]
expect_fail <- function(expr, check) {
  res <- quiet(expr)
  stopifnot(status_of(res, check) == "FAIL")
  invisible(res)
}

LOCAL <- 3979
set.seed(1)

# --- Synthetic survey: 10 x 10 km square, three north-south lines -----------
area <- st_sf(geometry = st_sfc(st_polygon(list(rbind(c(0, 0), c(1e4, 0), c(1e4, 1e4), c(0, 1e4), c(0, 0))))),
              crs = LOCAL)
transects <- st_sf(ID = 1:3, geometry = st_sfc(lapply(c(2000, 5000, 8000), function(x)
  st_linestring(rbind(c(x, 0), c(x, 1e4)))), crs = LOCAL))
xy <- cbind(sample(c(2000, 5000, 8000), 40, TRUE) + rnorm(40, 0, 100), runif(40, 0, 1e4))
ll <- st_coordinates(st_transform(st_as_sf(data.frame(xy), coords = 1:2, crs = LOCAL), 4326))
sightings <- data.frame(Species = sample(c("Moose", "Deer"), 40, TRUE), size = rpois(40, 1) + 1,
                        Longitude = ll[, 1], Latitude = ll[, 2])
cov <- rast(ext(-1000, 11000, -1000, 11000), res = 100, crs = paste0("EPSG:", LOCAL))
values(cov) <- runif(ncell(cov), 0, 100)
names(cov) <- "canopy"

check_raw <- function(s = sightings, t = transects, a = area, crs = LOCAL, covariates = cov) {
  check_survey_data(s, t, a, crs = crs, covariates = covariates, stop_on_fail = FALSE)
}

# Clean data: nothing fails, and stop_on_fail = TRUE does not error
res <- quiet(check_raw())
stopifnot(!any(res$status == "FAIL"))
quiet(check_survey_data(sightings, transects, area, crs = LOCAL, covariates = cov))

# Missing column
expect_fail(check_raw(s = sightings[, -2]), "sightings: required columns")
# Missing group size
s <- sightings; s$size[3] <- NA
expect_fail(check_raw(s = s), "sightings: group size present")
# Group size stored as text
s <- sightings; s$size <- as.character(s$size)
expect_fail(check_raw(s = s), "sightings: group size numeric")
# Missing coordinates
s <- sightings; s$Latitude[5] <- NA
expect_fail(check_raw(s = s), "sightings: coordinates present")
# Swapped longitude and latitude
s <- sightings; s[, c("Longitude", "Latitude")] <- s[, c("Latitude", "Longitude")]
expect_fail(check_raw(s = s), "sightings: longitude/latitude in range")
# Geographic target CRS
expect_fail(check_raw(crs = 4326), "target CRS projected, in metres")
# Transects as polygons instead of lines
expect_fail(check_raw(t = st_buffer(transects, 10)), "transects: sf line layer")
# Zero-length transect
t <- rbind(transects, st_sf(ID = 4, geometry = st_sfc(st_linestring(rbind(c(1, 1), c(1, 1))), crs = LOCAL)))
expect_fail(check_raw(t = t), "transects: no zero-length lines")
# A far-away sighting is a warning, not a failure
s <- sightings; s$Longitude[1] <- s$Longitude[1] + 0.5
res <- quiet(check_raw(s = s))
stopifnot(status_of(res, "sightings: inside study area") == "WARN",
          status_of(res, "distances: no extreme outliers") == "WARN")
# stop_on_fail = TRUE turns a failure into an error
stopifnot(inherits(try(quiet(check_survey_data(sightings, transects, area, crs = 4326)), silent = TRUE),
                   "try-error"))

# --- Synthetic formatted tables ---------------------------------------------
segs <- data.frame(Sample.Label = paste0("s", 1:30), Effort = 500,
                   x = rep(c(2000, 5000, 8000), 10), y = rep(seq(250, 9750, length.out = 10), each = 3),
                   canopy = runif(30, 0, 100))
dist <- data.frame(object = 1:20, distance = runif(20, 0, 250), size = 1)
obs <- data.frame(object = 1:20, Sample.Label = sample(segs$Sample.Label, 20, TRUE), size = 1,
                  distance = dist$distance)
pred <- data.frame(x = runif(100, 0, 1e4), y = runif(100, 0, 1e4), area = 250000, canopy = runif(100, 0, 100))

check_tabs <- function(d = dist, s = segs, o = obs, p = pred) {
  check_dsm_data(d, s, o, p, covariates = "canopy", truncation = 262, stop_on_fail = FALSE)
}

res <- quiet(check_tabs())
stopifnot(!any(res$status == "FAIL"))

# Effort left as a units object
s <- segs; s$Effort <- units::set_units(s$Effort, "m")
expect_fail(check_tabs(s = s), "no units-class columns")
# Duplicated segment label
s <- segs; s$Sample.Label[2] <- s$Sample.Label[1]
expect_fail(check_tabs(s = s), "segs: Sample.Label unique")
# Observation pointing to a segment that does not exist
o <- obs; o$Sample.Label[1] <- "nope"
expect_fail(check_tabs(o = o), "obs -> segs: every Sample.Label exists")
# Observation without a detection
o <- obs; o$object[1] <- 999
expect_fail(check_tabs(o = o), "obs -> dist: every object has a detection")
# Missing covariate in the prediction grid
p <- pred; p$canopy[1] <- NA
expect_fail(check_tabs(p = p), "pred: covariates complete")
# Prediction grid in longitude/latitude while segments are in metres
p <- pred; p$x <- runif(100, -101, -99); p$y <- runif(100, 50, 51)
expect_fail(check_tabs(p = p), "segs and pred in the same coordinate system")
# Zero effort
s <- segs; s$Effort[1] <- 0
expect_fail(check_tabs(s = s), "segs: Effort > 0")

# --- Real data: the RMNP example passes -------------------------------------
if (file.exists(here("data", "RMNP", "RMNPsightings.csv"))) {
  obs_raw <- read.csv(here("data", "RMNP", "RMNPsightings.csv"))
  tr <- st_read(here("data", "RMNP", "SimpliedFlightTrack.shp"), quiet = TRUE)
  ar <- st_read(here("data", "RMNP", "RMNPArea.shp"), quiet = TRUE)
  lidar <- rast(here("data", "RMNP", "lidar", "lidar_pabove2_zmax_10m.tif"))
  names(lidar) <- c("pabove2", "zmax")
  # The raw file flags two double counts with group size 0 ...
  res <- quiet(check_survey_data(obs_raw, tr, ar, crs = LOCAL, covariates = lidar, stop_on_fail = FALSE))
  stopifnot(status_of(res, "sightings: group size > 0") == "FAIL")
  # ... and passes once they are removed
  res <- quiet(check_survey_data(obs_raw[obs_raw$size > 0, ], tr, ar, crs = LOCAL, covariates = lidar,
                                 stop_on_fail = FALSE))
  stopifnot(!any(res$status == "FAIL"))
}
if (file.exists(here("workflow", "outputs", "model_data.rds"))) {
  md <- readRDS(here("workflow", "outputs", "model_data.rds"))
  res <- quiet(check_dsm_data(md$dist, md$segs, md$obs, md$pred, covariates = c("pabove2", "zmax"),
                              truncation = md$TRUNCATION_M, stop_on_fail = FALSE))
  stopifnot(!any(res$status == "FAIL"))
}

cat("All check_survey_data tests passed.\n")

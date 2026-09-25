# R workflow: density surface models for aerial ungulate surveys
# https://hambrecht.github.io/mri/workflow/
#
# The code of the five core articles in one script. Run it from a project
# folder (with a .here file, an RStudio project or a Git repository); the
# example data are downloaded into data/ if missing. Each section saves its
# results to workflow/outputs/ and the next section reloads them.
# Generated from the articles by workflow/R/make_script.R; do not edit by hand.

##############################################################################
# data-formatting.qmd
##############################################################################
## -----------------------------------------------------------------------------
library(here)    # file paths relative to the project root
library(sf)      # vector spatial data
library(dplyr)   # data manipulation
library(units)   # unit-aware lengths
library(ggplot2) # plotting


## -----------------------------------------------------------------------------
if (!file.exists(here("data", "RMNP", "RMNPsightings.csv"))) {
  dir.create(here("data"), showWarnings = FALSE)
  zip_file <- here("data", "rmnp_example_data.zip")
  if (!file.exists(zip_file)) {
    download.file("https://hambrecht.github.io/mri/data/rmnp_example_data.zip",
                  zip_file, mode = "wb")
  }
  unzip(zip_file, exdir = here("data"))
}


## -----------------------------------------------------------------------------
LOCAL <- 3979  # projected CRS (metres) used throughout the workflow
GLOBAL <- 4326 # WGS84 longitude/latitude, as recorded in the field


## -----------------------------------------------------------------------------
sightings_raw <- read.csv(here("data", "RMNP", "RMNPsightings.csv"))
transects_raw <- st_read(here("data", "RMNP", "SimpliedFlightTrack.shp"), quiet = TRUE)
area_raw <- st_read(here("data", "RMNP", "RMNPArea.shp"), quiet = TRUE)
lidar <- terra::rast(here("data", "RMNP", "lidar", "lidar_pabove2_zmax_10m.tif"))
names(lidar) <- c("pabove2", "zmax")
head(sightings_raw)


## -----------------------------------------------------------------------------
SPECIES_KEEP <- c("Moose", "Elk", "Deer", "Wolf", "Bison")
sightings_raw <- filter(sightings_raw, Species %in% SPECIES_KEEP)
nrow(sightings_raw)


## -----------------------------------------------------------------------------
# Compatibility checks for distance sampling / density surface model data.
#
# check_survey_data(): raw inputs (sightings, transects, study area, rasters)
#                      before any processing.
# check_dsm_data():    the formatted detection, segment, observation and
#                      prediction tables before fitting ds() and dsm().
#
# Both return a data.frame with one row per check (status PASS, WARN, FAIL or
# INFO), print it, and stop if any check FAILs (unless stop_on_fail = FALSE).

library(sf)

# Collects check results; `record(check, ok, detail)` adds a row.
new_checklist <- function() {
  rows <- list()
  record <- function(check, ok, detail = "", level = "FAIL") {
    status <- if (isTRUE(ok)) "PASS" else level
    if (status == "PASS") detail <- "" # details only where something needs attention
    rows[[length(rows) + 1]] <<- data.frame(check = check, status = status, detail = detail)
    invisible(isTRUE(ok))
  }
  info <- function(check, detail) record(check, FALSE, detail, level = "INFO")
  result <- function(stop_on_fail) {
    out <- do.call(rbind, rows)
    cat(sprintf("%-4s  %s%s\n", out$status, out$check,
                ifelse(out$detail == "", "", paste0(": ", out$detail))), sep = "")
    n_fail <- sum(out$status == "FAIL")
    if (stop_on_fail && n_fail > 0) {
      stop(n_fail, " check(s) failed; see the table above.", call. = FALSE)
    }
    invisible(out)
  }
  list(record = record, info = info, result = result)
}

check_survey_data <- function(sightings, transects, area, crs,
                              species = "Species", size = "size",
                              coords = c("Longitude", "Latitude"), coords_crs = 4326,
                              covariates = NULL, stop_on_fail = TRUE) {
  cl <- new_checklist()
  rec <- cl$record

  # --- Sightings -------------------------------------------------------------
  rec("sightings: has rows", nrow(sightings) > 0, sprintf("%d rows", nrow(sightings)))
  needed <- c(species, size, if (!inherits(sightings, "sf")) coords)
  missing_cols <- setdiff(needed, names(sightings))
  if (!rec("sightings: required columns", length(missing_cols) == 0,
           if (length(missing_cols)) paste("missing:", paste(missing_cols, collapse = ", ")) else "")) {
    return(cl$result(stop_on_fail))
  }

  if (inherits(sightings, "sf")) {
    n_empty <- sum(st_is_empty(sightings))
    rec("sightings: no empty geometries", n_empty == 0, sprintf("%d empty", n_empty))
    rec("sightings: CRS defined", !is.na(st_crs(sightings)))
    pts <- sightings[!st_is_empty(sightings), ]
  } else {
    xy <- sightings[, coords]
    bad_xy <- !stats::complete.cases(xy)
    rec("sightings: coordinates present", !any(bad_xy), sprintf("%d rows without coordinates", sum(bad_xy)))
    keep <- !bad_xy
    if (identical(coords_crs, 4326)) {
      out_range <- !bad_xy & (abs(xy[[1]]) > 180 | abs(xy[[2]]) > 90)
      rec("sightings: longitude/latitude in range", !any(out_range),
          sprintf("%d rows out of range (are longitude and latitude swapped or projected?)", sum(out_range)))
      keep <- keep & !out_range
    }
    if (!any(keep)) return(cl$result(stop_on_fail))
    pts <- st_as_sf(sightings[keep, ], coords = coords, crs = coords_crs, remove = FALSE)
  }
  if (nrow(pts) == 0) return(cl$result(stop_on_fail))

  sz <- sightings[[size]]
  rec("sightings: group size numeric", is.numeric(sz), class(sz)[1])
  if (is.numeric(sz)) {
    rec("sightings: group size present", !anyNA(sz), sprintf("%d missing", sum(is.na(sz))))
    rec("sightings: group size > 0", all(sz > 0, na.rm = TRUE), sprintf("%d <= 0", sum(sz <= 0, na.rm = TRUE)))
    rec("sightings: group size whole numbers", all(sz == round(sz), na.rm = TRUE),
        sprintf("%d non-integer", sum(sz != round(sz), na.rm = TRUE)), level = "WARN")
  }

  sp <- as.character(sightings[[species]])
  n_nosp <- sum(is.na(sp) | trimws(sp) == "")
  rec("sightings: species recorded", n_nosp == 0, sprintf("%d without species", n_nosp), level = "WARN")
  counts <- sort(table(sp[!is.na(sp) & trimws(sp) != ""]), decreasing = TRUE)
  cl$info("sightings: per species", paste(names(counts), counts, sep = " = ", collapse = ", "))

  dup <- duplicated(data.frame(pts[[species]], round(st_coordinates(pts), 6)))
  rec("sightings: no duplicate records", !any(dup),
      sprintf("%d with the same species and location", sum(dup)), level = "WARN")

  # --- Transects and study area ---------------------------------------------
  line_types <- c("LINESTRING", "MULTILINESTRING")
  rec("transects: sf line layer",
      inherits(transects, "sf") && all(st_geometry_type(transects) %in% line_types),
      if (inherits(transects, "sf")) paste(unique(st_geometry_type(transects)), collapse = ", ") else class(transects)[1])
  rec("area: sf polygon layer",
      inherits(area, c("sf", "sfc")) && all(st_geometry_type(area) %in% c("POLYGON", "MULTIPOLYGON")),
      if (inherits(area, c("sf", "sfc"))) paste(unique(st_geometry_type(area)), collapse = ", ") else class(area)[1])
  if (!inherits(transects, "sf") || !inherits(area, c("sf", "sfc"))) return(cl$result(stop_on_fail))

  rec("transects: CRS defined", !is.na(st_crs(transects)))
  rec("area: CRS defined", !is.na(st_crs(area)))
  rec("transects: valid geometries", all(st_is_valid(transects)), sprintf("%d invalid", sum(!st_is_valid(transects))))
  rec("area: valid geometry", all(st_is_valid(area)), "fix with sf::st_make_valid()")

  target <- st_crs(crs)
  projected_m <- !is.na(target) && !isTRUE(target$IsGeographic) && identical(target$units_gdal, "metre")
  rec("target CRS projected, in metres", projected_m,
      if (is.na(target)) "undefined" else paste0(target$Name, " (", target$units_gdal, ")"))
  if (!projected_m || is.na(st_crs(transects)) || is.na(st_crs(area)) || is.na(st_crs(pts))) {
    return(cl$result(stop_on_fail))
  }

  pts <- st_transform(pts, target)
  transects <- st_transform(transects, target)
  area <- st_union(st_transform(st_geometry(area), target))

  len <- as.numeric(st_length(transects))
  rec("transects: no zero-length lines", all(len > 0), sprintf("%d of %d", sum(len == 0), length(len)))
  cl$info("transects: total length", sprintf("%.1f km in %d features", sum(len) / 1000, length(len)))

  inside <- lengths(st_intersects(pts, area)) > 0
  rec("sightings: inside study area", all(inside), sprintf("%d outside", sum(!inside)), level = "WARN")
  len_in <- sum(as.numeric(st_length(st_intersection(st_geometry(transects), area))))
  rec("transects: inside study area", len_in / sum(len) >= 0.95,
      sprintf("%.1f%% of length inside", 100 * len_in / sum(len)), level = "WARN")

  d <- as.numeric(st_distance(pts, transects[st_nearest_feature(pts, transects), ], by_element = TRUE))
  q95 <- stats::quantile(d, 0.95)
  cl$info("distances: median / 95% / max (m)", sprintf("%.0f / %.0f / %.0f", stats::median(d), q95, max(d)))
  rec("distances: no extreme outliers", max(d) <= 3 * q95,
      sprintf("%d sightings > 3x the 95th percentile (off-effort, or wrong transect?)", sum(d > 3 * q95)),
      level = "WARN")

  # --- Covariate rasters (optional) -----------------------------------------
  if (!is.null(covariates)) {
    rec("covariates: CRS defined", terra::crs(covariates) != "")
    samp <- terra::vect(st_transform(st_sample(area, 2000), terra::crs(covariates)))
    na_area <- colMeans(is.na(terra::extract(covariates, samp, ID = FALSE)))
    rec("covariates: cover study area", all(na_area <= 0.05),
        paste(sprintf("%s %.1f%% missing", names(na_area), 100 * na_area), collapse = "; "), level = "WARN")
    na_pts <- colSums(is.na(terra::extract(covariates, terra::vect(st_transform(pts, terra::crs(covariates))), ID = FALSE)))
    rec("covariates: values at sightings", all(na_pts == 0),
        paste(sprintf("%s %d missing", names(na_pts), na_pts), collapse = "; "), level = "WARN")
  }

  cl$result(stop_on_fail)
}

check_dsm_data <- function(dist, segs, obs, pred = NULL, covariates = NULL,
                           truncation = NULL, stop_on_fail = TRUE) {
  cl <- new_checklist()
  rec <- cl$record

  required <- list(
    dist = c("object", "distance", "size"),
    segs = c("Sample.Label", "Effort", "x", "y"),
    obs = c("object", "Sample.Label", "size", "distance"),
    pred = c("x", "y", "area")
  )
  tables <- list(dist = dist, segs = segs, obs = obs, pred = pred)
  tables <- tables[!vapply(tables, is.null, logical(1))]
  for (nm in names(tables)) {
    miss <- setdiff(required[[nm]], names(tables[[nm]]))
    rec(paste0(nm, ": required columns"), length(miss) == 0,
        if (length(miss)) paste("missing:", paste(miss, collapse = ", ")) else "")
  }
  if (any(vapply(names(tables), function(nm) !all(required[[nm]] %in% names(tables[[nm]])), logical(1)))) {
    return(cl$result(stop_on_fail))
  }

  for (nm in intersect(names(tables), c("dist", "segs", "obs"))) {
    rec(paste0(nm, ": plain data.frame (not sf)"), !inherits(tables[[nm]], "sf"),
        "drop the geometry with sf::st_drop_geometry()", level = "WARN")
  }
  unit_cols <- unlist(lapply(names(tables), function(nm) {
    cols <- names(tables[[nm]])[vapply(tables[[nm]], inherits, logical(1), "units")]
    if (length(cols)) paste0(nm, "$", cols)
  }))
  rec("no units-class columns", length(unit_cols) == 0,
      paste(c(unit_cols, if (length(unit_cols)) "(convert with as.numeric())"), collapse = " "))

  rec("dist: object IDs unique", !anyDuplicated(dist$object), sprintf("%d duplicated", sum(duplicated(dist$object))))
  rec("obs: object IDs unique", !anyDuplicated(obs$object), sprintf("%d duplicated", sum(duplicated(obs$object))))
  rec("segs: Sample.Label unique", !anyDuplicated(segs$Sample.Label),
      sprintf("%d duplicated", sum(duplicated(segs$Sample.Label))))
  rec("obs -> dist: every object has a detection", all(obs$object %in% dist$object),
      sprintf("%d not in dist", sum(!obs$object %in% dist$object)))
  rec("dist -> obs: every detection is linked to a segment", all(dist$object %in% obs$object),
      sprintf("%d not in obs (they only inform the detection function)", sum(!dist$object %in% obs$object)),
      level = "WARN")
  rec("obs -> segs: every Sample.Label exists", all(obs$Sample.Label %in% segs$Sample.Label),
      sprintf("%d not in segs", sum(!obs$Sample.Label %in% segs$Sample.Label)))

  eff <- as.numeric(segs$Effort)
  rec("segs: Effort > 0", !anyNA(eff) && all(eff > 0), sprintf("%d missing or <= 0", sum(is.na(eff) | eff <= 0)))
  dd <- as.numeric(dist$distance)
  rec("dist: distances >= 0", !anyNA(dd) && all(dd >= 0), sprintf("%d missing or negative", sum(is.na(dd) | dd < 0)))
  if (!is.null(truncation)) {
    rec("dist: distances within truncation", all(dd <= truncation, na.rm = TRUE),
        sprintf("%d beyond %.0f m (will be excluded)", sum(dd > truncation, na.rm = TRUE), truncation), level = "WARN")
  }
  rec("segs: coordinates present", !anyNA(segs$x) && !anyNA(segs$y),
      sprintf("%d missing", sum(is.na(segs$x) | is.na(segs$y))))

  if (!is.null(covariates)) {
    miss <- setdiff(covariates, names(segs))
    rec("segs: covariate columns", length(miss) == 0, paste(miss, collapse = ", "))
    na <- vapply(intersect(covariates, names(segs)), function(v) sum(is.na(segs[[v]])), numeric(1))
    rec("segs: covariates complete", all(na == 0), paste(names(na)[na > 0], na[na > 0], sep = " = ", collapse = ", "))
  }

  if (!is.null(pred)) {
    p <- if (inherits(pred, "sf")) st_drop_geometry(pred) else pred
    rec("pred: coordinates and area complete", stats::complete.cases(p[, c("x", "y", "area")]) |> all())
    if (!is.null(covariates)) {
      miss <- setdiff(covariates, names(p))
      rec("pred: covariate columns", length(miss) == 0, paste(miss, collapse = ", "))
      na <- vapply(intersect(covariates, names(p)), function(v) sum(is.na(p[[v]])), numeric(1))
      rec("pred: covariates complete", all(na == 0), paste(names(na)[na > 0], na[na > 0], sep = " = ", collapse = ", "))
    }
    # Segments and grid must share a coordinate system: their extents must overlap.
    overlap <- function(a, b) max(min(a), min(b)) < min(max(a), max(b))
    rec("segs and pred in the same coordinate system", overlap(segs$x, p$x) && overlap(segs$y, p$y),
        sprintf("segs x %.0f to %.0f, pred x %.0f to %.0f", min(segs$x), max(segs$x), min(p$x), max(p$x)))
  }

  cl$result(stop_on_fail)
}


## -----------------------------------------------------------------------------
checks <- check_survey_data(
  sightings_raw, transects_raw, area_raw, crs = LOCAL,
  species = "Species", size = "size", coords = c("Longitude", "Latitude"),
  covariates = lidar, stop_on_fail = FALSE
)


## -----------------------------------------------------------------------------
sightings_raw[sightings_raw$size <= 0, ]


## -----------------------------------------------------------------------------
sightings_raw <- filter(sightings_raw, size > 0)
checks <- check_survey_data(sightings_raw, transects_raw, area_raw, crs = LOCAL,
                            covariates = lidar)


## -----------------------------------------------------------------------------
sightings <- sightings_raw |>
  select(Species, size, Latitude, Longitude) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = GLOBAL, remove = FALSE) |>
  st_transform(LOCAL)
sightings$object <- seq_len(nrow(sightings))

transects <- transects_raw |>
  rename(Transect = ID) |>
  st_transform(LOCAL)

area <- area_raw |>
  st_transform(LOCAL) |>
  st_union() |>
  st_as_sf()

table(sightings$Species)
sum(st_length(transects)) |> set_units("km")


## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = area, fill = NA) +
  geom_sf(data = transects, colour = "grey60", linewidth = 0.3) +
  geom_sf(data = sightings, aes(colour = Species), size = 1) +
  theme_minimal()


## -----------------------------------------------------------------------------
nearest_line <- st_nearest_feature(sightings, transects)
sightings$distance <- as.numeric(
  st_distance(sightings, transects[nearest_line, ], by_element = TRUE)
)
summary(sightings$distance)


## -----------------------------------------------------------------------------
hist(sightings$distance, breaks = 20, main = "", xlab = "Distance (m)")


## -----------------------------------------------------------------------------
TRUNCATION_M <- as.numeric(quantile(sightings$distance, probs = 0.95))
TRUNCATION_M


## -----------------------------------------------------------------------------
# Split lines into vertex-to-vertex pieces.
# From https://dieghernan.github.io/201905_Cast-to-subsegments/
stdh_cast_substring <- function(x, to = "MULTILINESTRING") {
  ggg <- st_geometry(x)
  if (!unique(st_geometry_type(ggg)) %in% c("POLYGON", "LINESTRING")) {
    stop("Input should be LINESTRING or POLYGON")
  }
  geom_list <- vector("list", length(ggg))
  for (k in seq_along(ggg)) {
    sub <- ggg[k]
    geom_list[[k]] <- lapply(
      1:(length(st_coordinates(sub)[, 1]) - 1),
      function(i) rbind(
        as.numeric(st_coordinates(sub)[i, 1:2]),
        as.numeric(st_coordinates(sub)[i + 1, 1:2])
      )
    ) |>
      st_multilinestring() |>
      st_sfc()
  }
  endgeom <- do.call(rbind, geom_list) |> st_sfc(crs = st_crs(x))
  if (class(x)[1] == "sf") endgeom <- st_set_geometry(x, endgeom)
  if (to == "LINESTRING") endgeom <- st_cast(endgeom, "LINESTRING")
  endgeom
}


## -----------------------------------------------------------------------------
SEGMENT_LENGTH_M <- 2 * TRUNCATION_M

segs <- transects |>
  st_segmentize(dfMaxLength = set_units(SEGMENT_LENGTH_M, "m")) |>
  stdh_cast_substring(to = "LINESTRING")

segs$Effort <- as.numeric(st_length(segs))
segs <- segs |>
  group_by(Transect) |>
  mutate(Sample.Label = paste("2025-RMNP", Transect, row_number(), sep = "-")) |>
  ungroup()

nrow(segs)
summary(segs$Effort)


## -----------------------------------------------------------------------------
nearest_seg <- st_nearest_feature(sightings, segs)
sightings$Sample.Label <- segs$Sample.Label[nearest_seg]

dist <- sightings |>
  filter(distance <= TRUNCATION_M) |>
  mutate(X = st_coordinates(geometry)[, 1], Y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  as.data.frame()

nrow(dist)


## -----------------------------------------------------------------------------
seg_xy <- st_coordinates(st_centroid(segs))
segs$x <- seg_xy[, "X"]
segs$y <- seg_xy[, "Y"]


## -----------------------------------------------------------------------------
obs <- dist[, c("object", "Sample.Label", "Species", "size", "distance")]
head(obs)


## -----------------------------------------------------------------------------
checks <- check_dsm_data(dist, st_drop_geometry(segs), obs, truncation = TRUNCATION_M)


## -----------------------------------------------------------------------------
dir.create(here("workflow", "outputs"), recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(dist = dist, obs = obs, segs = segs, area = area,
       TRUNCATION_M = TRUNCATION_M, LOCAL = LOCAL),
  here("workflow", "outputs", "survey_data.rds")
)


## -----------------------------------------------------------------------------
# NA


##############################################################################
# covariates.qmd
##############################################################################
## -----------------------------------------------------------------------------
library(here)
library(sf)
library(dplyr)
library(terra)         # raster data
library(exactextractr) # fast zonal statistics over polygons
library(ggplot2)


## -----------------------------------------------------------------------------
survey <- readRDS(here("workflow", "outputs", "survey_data.rds"))
list2env(survey, envir = environment())


## -----------------------------------------------------------------------------
lidar <- rast(here("data", "RMNP", "lidar", "lidar_pabove2_zmax_10m.tif"))
names(lidar) <- c("pabove2", "zmax")
stopifnot(same.crs(lidar, paste0("EPSG:", LOCAL)))
lidar


## -----------------------------------------------------------------------------
plot(lidar, nc = 2)


## -----------------------------------------------------------------------------
pts <- vect(dist, geom = c("X", "Y"), crs = paste0("EPSG:", LOCAL))
dist <- cbind(dist, extract(lidar, pts, ID = FALSE))
summary(dist[, c("pabove2", "zmax")])


## -----------------------------------------------------------------------------
strips <- st_buffer(segs, dist = TRUNCATION_M)
seg_means <- exact_extract(lidar, strips, fun = "mean", progress = FALSE)
names(seg_means) <- names(lidar)

segs <- segs |>
  bind_cols(seg_means) |>
  st_drop_geometry() |>
  as.data.frame()

summary(segs[, names(lidar)])


## -----------------------------------------------------------------------------
segs <- segs[complete.cases(segs[, names(lidar)]), ]
stopifnot(all(obs$Sample.Label %in% segs$Sample.Label))


## -----------------------------------------------------------------------------
CELL_SIZE_M <- 500

cells <- st_make_grid(area, cellsize = CELL_SIZE_M, what = "polygons")
centres <- st_centroid(cells)
inside <- lengths(st_intersects(centres, area)) > 0

pred <- st_sf(geometry = cells[inside])
xy <- st_coordinates(centres[inside])
pred$x <- xy[, 1]
pred$y <- xy[, 2]
pred$area <- CELL_SIZE_M^2
nrow(pred)


## -----------------------------------------------------------------------------
pred_means <- exact_extract(lidar, pred, fun = "mean", progress = FALSE)
names(pred_means) <- names(lidar)
pred <- bind_cols(pred, pred_means)

pred <- pred[complete.cases(st_drop_geometry(pred)), ]
nrow(pred)


## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = pred, aes(fill = pabove2), colour = NA) +
  geom_point(data = segs, aes(x, y), size = 0.1, colour = "white") +
  scale_fill_viridis_c(name = "pabove2 (%)") +
  theme_minimal()


## -----------------------------------------------------------------------------
checks <- check_dsm_data(dist, segs, obs, pred, covariates = names(lidar),
                         truncation = TRUNCATION_M)


## -----------------------------------------------------------------------------
saveRDS(
  list(dist = dist, obs = obs, segs = segs, pred = pred, area = area,
       TRUNCATION_M = TRUNCATION_M, LOCAL = LOCAL, CELL_SIZE_M = CELL_SIZE_M),
  here("workflow", "outputs", "model_data.rds")
)


##############################################################################
# detection-function.qmd
##############################################################################
## -----------------------------------------------------------------------------
library(here)
library(Distance)
library(ggplot2)


## -----------------------------------------------------------------------------
model_data <- readRDS(here("workflow", "outputs", "model_data.rds"))
list2env(model_data, envir = environment())

dist$Species <- relevel(factor(dist$Species), ref = "Moose")


## -----------------------------------------------------------------------------
ggplot(dist, aes(distance)) +
  geom_histogram(binwidth = 25, boundary = 0) +
  facet_wrap(~Species, scales = "free_y") +
  labs(x = "Distance (m)", y = "Count") +
  theme_minimal()


## -----------------------------------------------------------------------------
ggplot(dist, aes(pabove2, distance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Canopy cover, pabove2 (%)", y = "Distance (m)") +
  theme_minimal()


## -----------------------------------------------------------------------------
df_hn <- ds(dist, truncation = TRUNCATION_M, key = "hn", adjustment = NULL)
df_hr <- ds(dist, truncation = TRUNCATION_M, key = "hr", adjustment = NULL)
df_hn_cos <- ds(dist, truncation = TRUNCATION_M, key = "hn", adjustment = "cos",
                max_adjustments = 2)


## -----------------------------------------------------------------------------
summary(df_hr)


## -----------------------------------------------------------------------------
par(mfrow = c(1, 2))
plot(df_hn, main = "Half-normal")
plot(df_hr, main = "Hazard-rate")


## -----------------------------------------------------------------------------
covariates <- c("Species", "size", "pabove2", "zmax")
formulas <- c(
  covariates,
  combn(covariates, 2, paste, collapse = " + ")
)

df_cov <- list()
for (f in formulas) {
  for (key in c("hn", "hr")) {
    fit <- tryCatch(
      ds(dist, truncation = TRUNCATION_M, key = key, adjustment = NULL,
         formula = as.formula(paste("~", f))),
      error = function(e) NULL
    )
    if (!is.null(fit)) df_cov[[paste(key, f)]] <- fit
  }
}
length(df_cov)


## -----------------------------------------------------------------------------
all_models <- c(list(df_hn, df_hr, df_hn_cos), unname(df_cov))
model_table <- do.call(summarize_ds_models, c(all_models, list(output = "plain")))
head(model_table[, -1], 10)


## -----------------------------------------------------------------------------
model_table[model_table[["Delta AIC"]] < 2, c("Key function", "Formula", "Delta AIC")]


## -----------------------------------------------------------------------------
df_best <- df_cov[["hr Species + pabove2"]]
summary(df_best)


## -----------------------------------------------------------------------------
gof_ds(df_best)


## -----------------------------------------------------------------------------
plot(df_best, showpoints = TRUE, pch = 20, cex = 0.5)


## -----------------------------------------------------------------------------
saveRDS(df_best, here("workflow", "outputs", "detection_function.rds"))


##############################################################################
# dsm.qmd
##############################################################################
## -----------------------------------------------------------------------------
library(here)
library(dsm)      # density surface models (loads mgcv)
library(dsmextra) # extrapolation checks
library(sf)
library(ggplot2)


## -----------------------------------------------------------------------------
model_data <- readRDS(here("workflow", "outputs", "model_data.rds"))
list2env(model_data, envir = environment())
df_best <- readRDS(here("workflow", "outputs", "detection_function.rds"))


## -----------------------------------------------------------------------------
obs_moose <- obs[obs$Species == "Moose", ]
nrow(obs_moose)
mean(!segs$Sample.Label %in% obs_moose$Sample.Label)


## -----------------------------------------------------------------------------
dsm_xy <- dsm(abundance.est ~ s(x, y, bs = "ts"), df_best, segs, obs_moose,
              family = tw(), gamma = 1.4, method = "REML")
summary(dsm_xy)


## -----------------------------------------------------------------------------
vis.gam(dsm_xy, view = c("x", "y"), plot.type = "contour", too.far = 0.05,
        main = "", asp = 1)


## -----------------------------------------------------------------------------
dsm_p2 <- dsm(abundance.est ~ s(pabove2, bs = "ts", k = 5), df_best, segs, obs_moose,
              family = tw(), gamma = 1.4, method = "REML")
dsm_zmax <- dsm(abundance.est ~ s(zmax, bs = "ts", k = 5), df_best, segs, obs_moose,
                family = tw(), gamma = 1.4, method = "REML")
dsm_both <- dsm(abundance.est ~ s(pabove2, bs = "ts", k = 5) + s(zmax, bs = "ts", k = 5),
                df_best, segs, obs_moose,
                family = tw(), gamma = 1.4, method = "REML")


## -----------------------------------------------------------------------------
cor(segs$pabove2, segs$zmax)
concurvity(dsm_both, full = FALSE)$estimate


## -----------------------------------------------------------------------------
data.frame(
  model = c("s(pabove2)", "s(zmax)"),
  dev_expl = c(summary(dsm_p2)$dev.expl, summary(dsm_zmax)$dev.expl),
  AIC = c(AIC(dsm_p2), AIC(dsm_zmax))
)


## -----------------------------------------------------------------------------
dsm_hab <- if (summary(dsm_p2)$dev.expl >= summary(dsm_zmax)$dev.expl) dsm_p2 else dsm_zmax
summary(dsm_hab)


## -----------------------------------------------------------------------------
plot(dsm_hab, shade = TRUE, rug = TRUE, residuals = FALSE, pages = 1)


## -----------------------------------------------------------------------------
gam.check(dsm_hab)


## -----------------------------------------------------------------------------
rqgam_check(dsm_hab)


## -----------------------------------------------------------------------------
covs <- c("pabove2", "zmax")
extrap <- compute_extrapolation(
  samples = segs[, covs],
  covariate.names = covs,
  prediction.grid = st_drop_geometry(pred)[, c("x", "y", covs)],
  coordinate.system = sp::CRS(SRS_string = paste0("EPSG:", LOCAL))
)
summary(extrap)


## -----------------------------------------------------------------------------
pred$ExDet <- extrap$data$all$ExDet
ggplot(pred) +
  geom_sf(aes(fill = ExDet), colour = NA) +
  scale_fill_gradient2(low = "#b2182b", mid = "grey95", high = "#2166ac", midpoint = 0.5) +
  theme_minimal()


## -----------------------------------------------------------------------------
fit_species <- function(species, covariates = c("pabove2", "zmax")) {
  obs_sp <- obs[obs$Species == species, ]
  fit <- function(rhs) {
    dsm(as.formula(paste("abundance.est ~", rhs)), df_best, segs, obs_sp,
        family = tw(), gamma = 1.4, method = "REML")
  }
  if (nrow(obs_sp) < 20) return(fit("1"))

  # One covariate (they are concurve): the one with most deviance explained
  singles <- lapply(covariates, function(v) fit(sprintf("s(%s, bs = 'ts', k = 5)", v)))
  best <- singles[[which.max(sapply(singles, function(m) summary(m)$dev.expl))]]

  # Keep it only if significant and not shrunk away
  st <- summary(best)$s.table
  if (st[1, "p-value"] <= 0.05 && st[1, "edf"] >= 0.85) best else fit("1")
}

species_list <- c("Moose", "Deer", "Elk", "Bison", "Wolf")
dsm_species <- lapply(setNames(species_list, species_list), fit_species)
sapply(dsm_species, function(m) deparse(formula(m)))


## -----------------------------------------------------------------------------
saveRDS(list(dsm_species = dsm_species, dsm_xy = dsm_xy),
        here("workflow", "outputs", "dsm_models.rds"))


##############################################################################
# abundance.qmd
##############################################################################
## -----------------------------------------------------------------------------
library(here)
library(dsm)
library(sf)
library(ggplot2)


## -----------------------------------------------------------------------------
model_data <- readRDS(here("workflow", "outputs", "model_data.rds"))
list2env(model_data, envir = environment())
models <- readRDS(here("workflow", "outputs", "dsm_models.rds"))
list2env(models, envir = environment())

pred_df <- st_drop_geometry(pred)
area_km2 <- sum(pred_df$area) / 1e6
area_km2


## -----------------------------------------------------------------------------
dsm_moose <- dsm_species$Moose
pred$N_moose <- predict(dsm_moose, pred_df, off.set = pred_df$area)
sum(pred$N_moose)


## -----------------------------------------------------------------------------
pred$D_moose <- pred$N_moose / (pred_df$area / 1e6)
ggplot(pred) +
  geom_sf(aes(fill = D_moose), colour = NA) +
  scale_fill_viridis_c(name = "Moose / km²") +
  theme_minimal()


## -----------------------------------------------------------------------------
var_moose <- dsm_var_gam(dsm_moose, pred.data = pred_df, off.set = pred_df$area)
summary(var_moose)


## -----------------------------------------------------------------------------
var_cells <- dsm_var_gam(dsm_moose, pred.data = split(pred_df, seq_len(nrow(pred_df))),
                         off.set = pred_df$area)
pred$CV_moose <- sqrt(var_cells$pred.var) / pred$N_moose


## -----------------------------------------------------------------------------
ggplot(pred) +
  geom_sf(aes(fill = CV_moose), colour = NA) +
  scale_fill_viridis_c(name = "CV", option = "magma") +
  theme_minimal()


## -----------------------------------------------------------------------------
abundance_row <- function(species, model) {
  s <- summary(dsm_var_gam(model, pred.data = pred_df, off.set = pred_df$area))
  N <- s$pred.est
  cv <- s$cv
  C <- exp(qnorm(0.975) * sqrt(log(1 + cv^2)))
  rhs <- sub(" + offset(off.set)", "", deparse(formula(model)[[3]]), fixed = TRUE)
  data.frame(Species = species, Model = rhs,
             N = round(N), Density_km2 = round(N / area_km2, 3),
             SE = round(s$se), CV = round(cv, 3),
             Lower95 = round(N / C), Upper95 = round(N * C))
}

abundance <- do.call(rbind, Map(abundance_row, names(dsm_species), dsm_species))
abundance


## -----------------------------------------------------------------------------
pred$N_moose_xy <- predict(dsm_xy, pred_df, off.set = pred_df$area)
s_xy <- summary(dsm_var_gam(dsm_xy, pred.data = pred_df, off.set = pred_df$area))
data.frame(Model = c("habitat", "spatial s(x, y)"),
           N = round(c(summary(var_moose)$pred.est, s_xy$pred.est)),
           CV = round(c(summary(var_moose)$cv, s_xy$cv), 3))


## -----------------------------------------------------------------------------
maps <- rbind(
  data.frame(model = "habitat", D = pred$D_moose, geometry = pred$geometry),
  data.frame(model = "spatial s(x, y)", D = pred$N_moose_xy / (pred_df$area / 1e6),
             geometry = pred$geometry)
) |> st_as_sf()
ggplot(maps) +
  geom_sf(aes(fill = D), colour = NA) +
  facet_wrap(~model) +
  scale_fill_viridis_c(name = "Moose / km²") +
  theme_minimal()


## -----------------------------------------------------------------------------
saveRDS(pred[, c("x", "y", "area", "N_moose", "D_moose", "CV_moose")],
        here("workflow", "outputs", "moose_density.rds"))
write.csv(abundance, here("workflow", "outputs", "abundance.csv"), row.names = FALSE)


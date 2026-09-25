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

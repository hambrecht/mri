import rasterio
from rasterio.mask import mask
from rasterio.warp import calculate_default_transform, reproject, Resampling
import geopandas as gpd
import numpy as np
import os


# === USER INPUTS ===
raster_path = "mapLayers\Hansen_GFC-2024-v1.12_gain_50N_090W.tif"            # Path to the input raster
shapefile_path = "mapLayers\studyAreaOntario.shp"    # Path to the shapefile
output_path = "mapLayers\Hansen_GFC-2024-v1.12_gain_50N_090WMASKED.tif"   # Output raster path


# === CONSTANTS ===
WGS84_EPSG = "EPSG:4326"
NODATA_VAL = 0  # Set output NoData value

# === LOAD SHAPEFILE AND ENSURE CRS ===
print("Reading shapefile...")
shapefile = gpd.read_file(shapefile_path)

if shapefile.crs != WGS84_EPSG:
    print(f"Reprojecting shapefile from {shapefile.crs} to WGS84...")
    shapefile = shapefile.to_crs(WGS84_EPSG)

geometries = shapefile.geometry.values

# === OPEN AND REPROJECT RASTER IF NEEDED ===
with rasterio.open(raster_path) as src:
    raster_crs = src.crs
    print(f"Raster CRS: {raster_crs}")
    
    # Reproject raster to WGS84 if necessary
    if raster_crs != WGS84_EPSG:
        print(f"Reprojecting raster to WGS84...")
        transform, width, height = calculate_default_transform(
            src.crs, WGS84_EPSG, src.width, src.height, *src.bounds
        )
        kwargs = src.meta.copy()
        kwargs.update({
            'crs': WGS84_EPSG,
            'transform': transform,
            'width': width,
            'height': height
        })

        reprojected_path = "temp_reprojected.tif"
        with rasterio.open(reprojected_path, 'w', **kwargs) as dst:
            for i in range(1, src.count + 1):
                reproject(
                    source=rasterio.band(src, i),
                    destination=rasterio.band(dst, i),
                    src_transform=src.transform,
                    src_crs=src.crs,
                    dst_transform=transform,
                    dst_crs=WGS84_EPSG,
                    resampling=Resampling.nearest
                )
        raster_to_use = reprojected_path
    else:
        raster_to_use = raster_path

# === CLIP AND MASK ===
with rasterio.open(raster_to_use) as src:
    print("Clipping and masking raster...")
    out_image, out_transform = mask(src, geometries, crop=True)
    
    print("Replacing 0s with NaN...")
    data = out_image.astype('float32')
    data[data == 0] = np.nan

    print("Scaling data to uint16 range...")
    data_min = np.nanmin(data)
    data_max = np.nanmax(data)
    print(f"Data range: min={data_min}, max={data_max}")

    # Avoid division by zero
    # if data_max - data_min == 0:
    #     print("Warning: No data range to scale. Using zeros.")
    #     scaled = np.zeros_like(data, dtype='uint16')
    # else:
    #     scaled = (data - data_min) / (data_max - data_min) * 65535
    #     scaled = np.where(np.isnan(scaled), NODATA_VAL, scaled)
    #     scaled = scaled.astype('uint16')

    out_meta = src.meta.copy()
    out_meta.update({
        "driver": "GTiff",
        "height": data.shape[1],
        "width": data.shape[2],
        "transform": out_transform,
        "dtype": 'uint16',
        "crs": WGS84_EPSG,
        "nodata": NODATA_VAL,
        "compress": "lzw"
    })

# === SAVE FINAL OUTPUT ===
with rasterio.open(output_path, "w", **out_meta) as dest:
    print(f"Saving final raster to {output_path}")
    dest.write(data)

# === CLEAN UP TEMP FILE ===
if raster_to_use != raster_path:
    print("Cleaning up temporary files...")
    os.remove(raster_to_use)

print("All done.")
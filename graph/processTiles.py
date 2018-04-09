import quadkey
import math
import os

def deg2num(lat_deg, lon_deg, zoom):
    lat_rad = math.radians(lat_deg)
    n = 2.0 ** zoom
    xtile = int((lon_deg + 180.0) / 360.0 * n)
    ytile = int((1.0 - math.log(math.tan(lat_rad) + (1 / math.cos(lat_rad))) / math.pi) / 2.0 * n)
    return (xtile, ytile)

def num2deg(xtile, ytile, zoom):
    n = 2.0 ** zoom
    lon_deg = xtile / n * 360.0 - 180.0
    lat_rad = math.atan(math.sinh(math.pi * (1 - 2 * ytile / n)))
    lat_deg = math.degrees(lat_rad)
    return (lat_deg, lon_deg)

def tile2quadkey(fname):
    before_zoom = fname.find("_")
    after_zoom = fname.find("_", before_zoom+1)
    after_x = fname.find("_", after_zoom+1)
    after_y = fname.find(".", after_x+1)
    tilex = int(fname[after_zoom+1:after_x])
    tiley = int(fname[after_x+1:after_y])
    zoom = int(fname[before_zoom+1:after_zoom])
    qk = quadkey.from_tile((tilex, tiley), zoom)
    return qk.key[12:]+".png"


files = os.listdir(".")
for file in files:
    if file != "processTile.py":
        os.rename(filename, tile2quadkey(filename))

# print tile2quadkey("tile_12_9416_12108.png")

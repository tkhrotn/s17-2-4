######################################################################
#
# GLOBAL VARIABLES
#
######################################################################

SECONDS_ONE_DAY <- 24 * 60 * 60

# MAP OPTIONS
MAP_NAME_OPTIONS <- list('OpenStreetMap'='OPEN_STREET_MAP', 'OpenTopoMap'='OPEN_TOPO_MAP', 'GSJ Standard'='GSJ_S', 'GSJ Hillshade'='GSJ_H', 'ECORIS Vegetation'='ECORIS') # 'Mapbox'='MAP_BOX')
MAP_TILES_URLS <- list(
    OPEN_STREET_MAP='http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
    # GOOGLE_S='http://www.google.cn/maps/vt?lyrs=s@189&gl=cn&x={x}&y={y}&z={z}',
    OPEN_TOPO_MAP='http://c.tile.opentopomap.org/{z}/{x}/{y}.png',
    GSJ_S='https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png',
    GSJ_H='http://cyberjapandata.gsi.go.jp/xyz/hillshademap/{z}/{x}/{y}.png',
    ECORIS='https://map.ecoris.info/tiles/vegehill/{z}/{x}/{y}.png'
    # MAP_BOX='//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png'
)
MAP_TILES_DESCRIPTION <- list(
    OPEN_STREET_MAP='Maps by <a href="https://www.openstreetmap.org/">OSM</a>',
    # GOOGLE_S='Maps by <a href="http://www.google.com/">Google</a>',
    OPEN_TOPO_MAP='Maps by <a href="https://www.openstreetmap.org/">OpenStreetMap contributors</a>',
    GSJ_S='Maps by <a href="https://maps.gsi.go.jp/development/ichiran.html">GSJ</a>',
    GSJ_H='Maps by <a href="https://maps.gsi.go.jp/development/ichiran.html">GSJ</a>',
    ECORIS='Maps by <a href="https://www.gsi.go.jp/top.html">国士地理院</a>'
    # MAP_BOX='Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
)
CIRCLE_STYLES <- list(
    "color" = "yellow",
    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
    "font-size" = "20px",
    "border-color" = "rgba(0,0,0,0.5)",
    "background-color" = "black",
    "opacity" = 0.65
)

SECONDS_ONE_DAY = 86400

# HOTLINE SETTINGS
DEFAULT_OUTLINE_WIDTH <- 0
DEFAULT_WEIGHT <- 12
DEFAULT_SMOOTH <- 8
DEFAULT_OUTLINE_COLOR <- '#000000'
DEFAULT_PALLET_COLOR <- c("#00FFFF", "#00FF00", "#FFFF00", "#FF7F00", "#FF0000")

# Action Method Options
LOCATION_NAMES = list("河川流量", "汚染濃度")
ACTION_OPTIONS = list("移流分散", "到達時間")
ALL_ACTION_OPTIONS = c(LOCATION_NAMES, ACTION_OPTIONS)

# CONTOUR GW
CONTOUR_COLORS = c('#0000C8', '#0019FF', '#0098FF', '#2CFF96', '#97FF00', '#FFEA00', '#FF6F00', '#FF0000')

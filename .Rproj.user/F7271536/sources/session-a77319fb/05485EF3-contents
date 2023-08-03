
# Generarción de Mapas Web para presentaciones

# Opciones Generales ------------------------------------------------------
options(browser="/usr/bin/open -a 'Google Chrome'")
setwd("../../book-change-detection/")

# Librerias ---------------------------------------------------------------
library(raster)
library(mapview)

# Recursos ----------------------------------------------------------------
source("R/DI.R")
source("R/graphics.R")
source("R/functions.R")
source("R/utils_image.R")
source("R/Douglas-Peucker.R")
source("R/log_mean_ratio.R")


# Paletas -----------------------------------------------------------------
palette_grey <- grey(1:100/100)
palette_rainbow <- rainbow(100)

make_map_web <- function(map, name, path_out, show=T){
  path_out_html <- paste0(path_out, "/", name,".html") %>% 
    gsub(patter="//", replacement = "/", .)
  mapview::mapshot(map, url = path_out_html)
  if(isTRUE(show)){
    utils::browseURL(path_out_html)
  }
  print(paste0("Mapa web guardado en: ", path_out_html))
}

delete_black <- function(path_tif){
  img_rbg <- terra::rast(path_tif) %>% 
    raster::brick() %>%
    subset(c(1, 2, 3))
  # set all values with 0 to NA
  vals = values(img_rbg)
  idx = which(rowSums(vals) == 0)
  vals[idx, ] = cbind(NA, NA, NA)
  
  img_rbg = setValues(img_rbg, vals)
  return(img_rbg)
}


# Área de Estudio ---------------------------------------------------------


# RGB
st_2017 <- delete_black("results/ST_001/ST_001-2017_reproy.tif")
st_2022 <- delete_black("results/ST_001/ST_001-2022_reproy.tif")

m_2017 <- viewRGB(st_2017, r = 1, g = 2, b = 3, layer.name = "RBG_2017",
                  na.color = "transparent", quantiles = NULL)
m_2022 <- viewRGB(st_2022, r = 1, g = 2, b = 3, layer.name = "RBG_2022",
                  na.color = "transparent", quantiles = NULL)

rgb_img <- m_2017 + m_2022
make_map_web(map = rgb_img, name = "RGB_roi", path_out = "results/ST_001/")


# Radar -------------------------------------------------------------------


img1 <- raster("data/turberas/tif/ST_001/S1_GRD_SAR_VH_2017.tif")
img2 <- raster("data/turberas/tif/ST_001/S1_GRD_SAR_VH_2023.tif")

sar_map <- mapview(img1) + mapview(img2)
make_map_web(map = sar_map, name = "SAR_roi", path_out = "results/ST_001/")

plot(img1, col=palette_grey)
plot(img2, col=palette_grey)
# 


# ViewRGB -----------------------------------------------------------------
st_2017 <- stack("results/ST_001/ST_001-2017_reproy.tif")
st_2022 <- stack("results/ST_001/ST_001-2022_reproy.tif")
m_2017 <- viewRGB(st_2017, r = 1, g = 2, b = 3)
m_2022 <- viewRGB(st_2023, r = 1, g = 2, b = 3)


# MC: Diferencia Directa --------------------------------------------------
dd <- raster("results/ST_001/DD.tif")
dd_th <- raster("results/ST_001/DD_PAD_GTE1.tif")
dd_th[dd_th ==0] <- NA
mdd_th <- mapview(dd_th, na.color =NA, alpha = 0.5, legend=F)

map_dd <- rgb_img + mapview(dd)+mdd_th
make_map_web(map = map_dd, name = "DD_roi", path_out = "results/ST_001/")



# Log Ratio ---------------------------------------------------------------

lr <- raster("results/ST_001/DLR.tif")

map_lr <- rgb_img + mapview(lr)
make_map_web(map = map_lr, name = "LR_roi", path_out = "results/ST_001/")



# Relación de verosimilitud logarítmica (LLR) -----------------------------


llr <- raster("results/ST_001/LLR.tif")

map_llr <- rgb_img + mapview(llr)
make_map_web(map = map_llr, name = "LLR_roi", path_out = "results/ST_001/")


# Enhanced Difference Image (EDI) -----------------------------------------

edi <- raster("results/ST_001/EDI.tif")
edi_out <- raster("results/ST_001/EDI_Outliers.tif")

map_edi <- rgb_img + mapview(edi)+mapview(edi_out,  na.color = NA)
make_map_web(map = map_edi, name = "EDI_roi", path_out = "results/ST_001/")


# MC: Triangular Threshold Segmentation (Douglas-Peucker) -----------------

tts_edi <- raster("results/ST_001/TTS_EDI.tif")
tts_dd <- raster("results/ST_001/TTS_DD.tif")

map_tts <- rgb_img + mapview(tts_edi, na.color = NA,  col.regions ="yellow")+
  mapview(tts_dd, na.color =NA, col.regions ="orange")
make_map_web(map = map_tts, name = "TTS_roi", path_out = "results/ST_001/")


# Log Mean Ratio ----------------------------------------------------------


lmr <- raster("results/ST_001/LMR.tif")
lmr_th <- raster("results/ST_001/LMR_th.tif")

map_lmr <- rgb_img + mapview(lmr, na.color = NA)+ mapview(lmr_th, na.color = NA)
make_map_web(map = map_lmr, name = "LMR_roi", path_out = "results/ST_001/")



#  PCA K-Means ------------------------------------------------------------

pcak_edi <- raster("results/ST_001/PCAK_EDI.tif")

map_pcak_edi <- rgb_img + mapview(pcak_edi, na.color = NA, col.region="orange") 
make_map_web(map = map_pcak_edi, name = "PCAK_EDI_roi", path_out = "results/ST_001/")



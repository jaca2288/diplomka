# Stažení měsíčních nočních světel NASA Black Marble (VNP46A3, NearNadir_Composite_Snow_Free)
# pro Prahu. Ukládá jen výřezy do ntl/prague_YYYY_MM.tif, velké HDF5 dlaždice maže.
#
# Spuštění:  Rscript ntl_download.R 2022-01 2025-10
# Hotové měsíce přeskakuje -> při chybách stačí spustit znovu.
# Vyžaduje NASA Earthdata token v ~/.Renviron (NASA_BEARER_TOKEN) a autorizovanou aplikaci LAADS.

suppressPackageStartupMessages({
  library(sf); library(terra); library(httr2); library(jsonlite)
})
setwd("/Users/jachym.bielesz/diplomka")

args  <- commandArgs(trailingOnly = TRUE)
from  <- as.Date(paste0(if (length(args) >= 1) args[1] else "2022-01", "-01"))
to    <- as.Date(paste0(if (length(args) >= 2) args[2] else "2025-10", "-01"))

TOKEN <- Sys.getenv("NASA_BEARER_TOKEN")
stopifnot(nchar(TOKEN) > 0)
LAADS <- "https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5200/VNP46A3"
TILES <- c("h19v03", "h19v04")   # 10–20° E, 50–60° N a 40–50° N: jih Prahy leží pod 50° N
LAYER <- "//HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data_Fields/NearNadir_Composite_Snow_Free"
# Stahuje se postupně: při souběžných spojeních server přenosy zastavuje

dir.create("ntl", showWarnings = FALSE)
prague <- readRDS("cache/prague_boundary.rds")
crop_ext <- ext(vect(prague)) + 0.05

months <- seq(from, to, by = "month")
out_of <- function(d) sprintf("ntl/prague_%s.tif", format(d, "%Y_%m"))
months <- months[!file.exists(out_of(months))]
cat("months to download:", length(months), "\n")
if (length(months) == 0) quit(save = "no")

auth <- function(req) req_headers(req, Authorization = paste("Bearer", TOKEN))

# 1) názvy a velikosti souborů (verze zpracování je součástí názvu)
info <- lapply(months, function(d) {
  doy <- sprintf("%03d", as.integer(format(d, "%j")))
  lst <- request(sprintf("%s/%s/%s.json", LAADS, format(d, "%Y"), doy)) |>
    auth() |> req_retry(max_tries = 5) |> req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  k <- vapply(TILES, function(t) { i <- grep(t, lst$content$name); if (length(i) == 1) i else NA_integer_ }, 0L)
  if (anyNA(k)) return(NULL)
  list(month = d,
       url   = sprintf("%s/%s/%s/%s", LAADS, format(d, "%Y"), doy, lst$content$name[k]),
       size  = as.numeric(lst$content$size[k]))
})
cat("missing in archive:", sum(vapply(info, is.null, TRUE)), "\n")
info <- Filter(Negate(is.null), info)

# 2) stažení přes curl s navazováním přerušeného přenosu a kontrolou velikosti,
#    výřez Prahy, smazání dlaždice
cfg <- tempfile()
writeLines(sprintf('header = "Authorization: Bearer %s"', TOKEN), cfg)
Sys.chmod(cfg, "600")

download_file <- function(url, size, path) {
  for (k in 1:30) {
    system2("curl", c("-sS", "-L", "-K", cfg, "-C", "-", "--retry", "5", "--retry-all-errors",
                      "--speed-limit", "20000", "--speed-time", "60",
                      "-o", shQuote(path), shQuote(url)), stdout = FALSE, stderr = FALSE)
    if (file.exists(path) && file.size(path) >= size) break
  }
  tryCatch(rast(path, subds = LAYER), error = function(e) NULL)
}

process_month <- function(it) {
  tmp <- file.path(tempdir(), sprintf("vnp46a3_%s_%s.h5", format(it$month, "%Y_%m"), TILES))
  xs  <- Map(download_file, it$url, it$size, tmp)
  if (any(vapply(xs, is.null, TRUE))) {
    unlink(tmp)
    return(sprintf("FAILED %s", format(it$month, "%Y-%m")))
  }
  x <- do.call(merge, unname(lapply(xs, crop, y = crop_ext, snap = "out")))
  writeRaster(x, out_of(it$month), overwrite = TRUE)
  unlink(tmp)
  sprintf("done %s %s", format(it$month, "%Y-%m"), format(Sys.time(), "%H:%M:%S"))
}

for (it in info) cat(process_month(it), "\n")
unlink(cfg)

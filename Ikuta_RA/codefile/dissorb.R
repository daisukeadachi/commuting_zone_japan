library(tidyverse)
library(sf)

#' @description Dissolve geometries in an sf object based on a specified grouping variable.
#' @param sf_object An sf object containing geometries with CZ data (in column `cluster`) to be dissolved.
#' @param group_var A variable in the sf object to group by for dissolving geometries.
#' @return An sf object with dissolved geometries.
#' @examples
#' # Assuming 'sf_data' is an sf object with a column 'region'
#' dissolved_sf <- dissorb(sf_data, region)
dissorb <- function(sf_object, group_var = "cluster") {
    sf_object <- sf_object %>%
        sf::st_make_valid() %>%
        dplyr::select(-NO, -DATE) %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::summarise() %>%
        dplyr::ungroup()
    return(sf_object)
}

#' @description run func of `dissorb` for each year
#' @param MMMpath A vector of sf file paths for each year.
#' @param CZpath A vector of CSV file paths for each year.
#' @param h A char of height threshold.
#' @param outdir A char of output directory name.
#' @return None. Writes dissolved shapefiles to specified output directory.
byYear <- function(
    MMMpath = "mapdata/mmm20151001/mmm20151001.shp",
    CZpath = "output/clustered/harmonized",
    outdir,
    h = "0.98") {
    czlist <- stringr::str_glue("{CZpath}/{h}")|>
        list.files(full.names = TRUE, recursive = TRUE)|>
        purrr::set_names(~ stringr::str_sub(basename(.x), start = 1, end = 4))
    sf_obj  <- sf::read_sf(MMMpath, options = "ENCODING=CP932")
    purrr::iwalk(
        czlist,
        ~ {
            # 1. 保存したいパスを先に変数で作る
            save_path <- stringr::str_glue("output/shpfiles/{outdir}/{.y}/CZ_{.y}.shp")
            
            # 2. そのパスのディレクトリ（フォルダ）部分が存在しなければ作成する
            # recursive = TRUE にすることで、途中のフォルダ(outputやshpfilesなど)も一気に作ります
            dir_name <- dirname(save_path)
            if (!dir.exists(dir_name)) {
                dir.create(dir_name, recursive = TRUE)
            }
            sf_obj %>%
            dplyr::left_join(readr::read_csv(.x, show_col_types = FALSE), by = c("JISCODE" = "i")) %>%
            dissorb() %>% 
            sf::st_write(
                obj = .,
                dsn = stringr::str_glue("output/shpfiles/{outdir}/{.y}/CZ_{.y}.shp"),
                delete_layer = TRUE,
                layer_options = c("ENCODING=CP932")
            )}
    )
}

tree_height <- c("0.98", "0.97", "0.99")
purrr::walk(
    tree_height,
    ~{
        if(.x == "0.98"){
            outdir <- "main"
        }else {
            outdir <- stringr::str_glue("appendix/{.x}")
        }
        byYear(
        MMMpath = "mapdata/mmm20151001/mmm20151001.shp",
        CZpath = "output/clustered/harmonized",
        outdir = outdir,
        h = .x
    )}
)
beepr::beep()

library(testthat)
library(sf)
source("../../codefile/color_assignment.R")

# helper to create square polygon by its lower-left corner
square <- function(x, y, id){
  sf::st_polygon(list(rbind(c(x,y), c(x+1,y), c(x+1,y+1), c(x,y+1), c(x,y)))) %>% sf::st_sfc(crs = 4612) %>% sf::st_sf(geometry = ., JISCODE = id)
}

# Test 1: adjacency uniqueness
test_that("Adjacent groups get different colors", {
  a <- square(0,0, 1) %>% dplyr::mutate(cluster = 1)
  b <- square(1,0, 2) %>% dplyr::mutate(cluster = 2)
  sf <- dplyr::bind_rows(a,b)
  res <- assign_group_colors(sf, "cluster", colors = c("#FF0000", "#00FF00"))
  expect_true(all(c("cluster","color","signature") %in% names(res)))
  expect_false(res$color[1] == res$color[2])
})

# Test 2: fixed color override
test_that("Fixed color is applied and preserved", {
  a <- square(0,0, 10) %>% dplyr::mutate(cluster = 10)
  b <- square(1,0, 11) %>% dplyr::mutate(cluster = 11)
  sf <- dplyr::bind_rows(a,b)
  res <- assign_group_colors(sf, "cluster", colors = c("#AA0000", "#00AA00"), fixed = list(value = 10, color = "#123456"))
  expect_equal(res$color[res$cluster == 10], "#123456")
})

# Test 3: prev_colors reuse by signature
test_that("prev_colors reuses colors for identical membership signature", {
  a1 <- square(0,0, 100) %>% dplyr::mutate(cluster = 1)
  b1 <- square(1,0, 101) %>% dplyr::mutate(cluster = 2)
  sf1 <- dplyr::bind_rows(a1,b1)
  map1 <- assign_group_colors(sf1, "cluster", colors = c("#A1","#B1"))
  prev_map <- setNames(map1$color, map1$signature)

  # construct identical groups again but swapped order
  a2 <- square(0,0, 100) %>% dplyr::mutate(cluster = 10)
  b2 <- square(1,0, 101) %>% dplyr::mutate(cluster = 20)
  sf2 <- dplyr::bind_rows(a2,b2)
  map2 <- assign_group_colors(sf2, "cluster", prev_colors = prev_map)

  # both groups should reuse colors from prev_map
  expect_true(all(map2$signature %in% names(prev_map)))
  expect_true(all(map2$color == prev_map[map2$signature]))
})

# Test 4: signature column content
test_that("signature is deterministic and contains JISCODEs", {
  a <- square(0,0, 201) %>% dplyr::mutate(cluster = 1)
  b <- square(1,0, 202) %>% dplyr::mutate(cluster = 2)
  sf <- dplyr::bind_rows(a,b)
  map <- assign_group_colors(sf, "cluster")
  expect_match(map$signature[1], "201")
  expect_match(map$signature[2], "202")
})

# Test 5: conflict detection when fixed neighbors have same color
test_that("Error when fixed colors force adjacent conflict", {
  a <- square(0,0, 301) %>% dplyr::mutate(cluster = 1)
  b <- square(1,0, 302) %>% dplyr::mutate(cluster = 2)
  sf <- dplyr::bind_rows(a,b)
  expect_error(assign_group_colors(sf, "cluster", fixed = list(value = 1, color = "#FF00FF"), prev_colors = setNames(c("#FF00FF"), c("302"))))
})

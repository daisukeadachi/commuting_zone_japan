# LaTeX table fragments for the coauthor-meeting deck.
#
# The deck follows the source paper's presentation: each table in the body compares the
# 2010 and the 2020 delineation in two columns, at the cutoff of 0.977 that the source
# paper uses. The appendix tables carry every census year at that same cutoff, and one
# table sets the two cutoff anchors against each other for the two headline years.
# Fragments are written to validation/slides/tables and are input by the deck.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

slide_cutoff <- baseline_cutoff
earlier_year <- 2010
later_year <- 2020
table_dir <- file.path("validation/slides", "tables")
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

# Romanized names for the municipalities the reassignment table lists. The scope table
# carries Japanese names only and every reader-facing label in this project is English.
romanized <- c(
  "01463" = "Shimukappu", "11100" = "Saitama", "19212" = "Uenohara",
  "21219" = "Gujo", "21604" = "Shirakawa", "29427" = "Kawai"
)

count <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
share <- function(x) sprintf("%.1f\\%%", 100 * x)
ratio <- function(x) sprintf("%.2f", x)
br <- " \\\\"

pick <- function(data, year) {
  data %>% filter(year == !!year, abs(cutoff - slide_cutoff) < 1e-9)
}

# The whole tabular is written out, not only its rows. A fragment holding bare rows has
# to be pulled in from inside a tabular, where the last row's line break runs into the
# rule that follows it.
write_table <- function(rows, path, spec, header) {
  writeLines(c(sprintf("\\begin{tabular}{%s}", spec), "\\toprule", header, "\\midrule",
               rows, "\\bottomrule", "\\end{tabular}"),
             file.path(table_dir, path))
  message("wrote ", path, " (", length(rows), " rows)")
}

write_rows <- function(rows, path) {
  writeLines(rows, file.path(table_dir, path))
  message("wrote ", path, " (", length(rows), " lines)")
}

diagnostics <- read_csv(output_path("diagnostics_table.csv"), show_col_types = FALSE)
containment <- read_csv(output_path("containment_table.csv"), show_col_types = FALSE)
core_table <- read_csv(output_path("core_table.csv"), show_col_types = FALSE)
core <- core_table %>% filter(core_definition == "Urban Employment Area central city")

# ---------------------------------------------------------------------------
# Body tables

a <- pick(diagnostics, earlier_year)
b <- pick(diagnostics, later_year)
rows <- c(
  sprintf("Municipalities in the delineation & %s & %s%s", count(a$municipalities), count(b$municipalities), br),
  sprintf("Commuting zones & %s & %s%s", count(a$commuting_zones), count(b$commuting_zones), br),
  sprintf("Single-municipality zones & %s & %s%s", count(a$single_municipality_zones), count(b$single_municipality_zones), br),
  sprintf("Municipalities in the largest zone & %s & %s%s", count(a$municipalities_in_largest_zone), count(b$municipalities_in_largest_zone), br),
  sprintf("Non-contiguous zones & %s & %s%s[2pt]", count(a$noncontiguous_zones_paper_test), count(b$noncontiguous_zones_paper_test), br),
  sprintf("Smallest zone, resident labour force & %s & %s%s", count(a$min_population), count(b$min_population), br),
  sprintf("Average zone, resident labour force & %s & %s%s", count(a$mean_population), count(b$mean_population), br),
  sprintf("Largest zone, resident labour force & %s & %s%s[2pt]", count(a$max_population), count(b$max_population), br),
  sprintf("Smallest zone area (sq.\\,km) & %s & %s%s", count(a$min_area_km2), count(b$min_area_km2), br),
  sprintf("Average zone area (sq.\\,km) & %s & %s%s", count(a$mean_area_km2), count(b$mean_area_km2), br),
  sprintf("Largest zone area (sq.\\,km) & %s & %s%s[2pt]", count(a$max_area_km2), count(b$max_area_km2), br),
  sprintf("Compactness of zones & %s & %s%s", ratio(a$compactness), ratio(b$compactness), br)
)
write_table(rows, "diagnostics.tex", "lrr", paste0("& 2010 & 2020", br))

ca <- pick(containment, earlier_year); cb <- pick(containment, later_year)
ka <- pick(core, earlier_year); kb <- pick(core, later_year)
rows <- c(
  paste0("\\multicolumn{3}{l}{\\emph{Core}}", br),
  sprintf("\\quad Urban areas split across zones & %s & %s%s", count(ka$urban_areas_split_across_zones), count(kb$urban_areas_split_across_zones), br),
  sprintf("\\quad Zones containing a core & %s & %s%s", share(ka$share_of_zones_with_a_core), share(kb$share_of_zones_with_a_core), br),
  sprintf("\\quad Residents working in a core of their zone & %s & %s%s", share(ka$share_working_in_a_core_of_own_zone), share(kb$share_working_in_a_core_of_own_zone), br),
  sprintf("\\quad Workforce living in a core of their zone & %s & %s%s[3pt]", share(ka$share_living_in_a_core_of_own_zone), share(kb$share_living_in_a_core_of_own_zone), br),
  paste0("\\multicolumn{3}{l}{\\emph{Connection}}", br),
  paste0("\\quad Minimum wage correlation & \\multicolumn{2}{c}{pending}", br),
  paste0("\\quad Mean wage correlation & \\multicolumn{2}{c}{pending}", br),
  paste0("\\quad Mean wage correlation, multi-municipality zones & \\multicolumn{2}{c}{pending}", br, "[3pt]"),
  paste0("\\multicolumn{3}{l}{\\emph{Containment}}", br),
  sprintf("\\quad Minimum contained & %s & %s%s", share(ca$min_contained), share(cb$min_contained), br),
  sprintf("\\quad Mean contained & %s & %s%s", share(ca$mean_contained), share(cb$mean_contained), br),
  sprintf("\\quad Share of the labour force contained & %s & %s%s", share(ca$share_of_labour_force_contained), share(cb$share_of_labour_force_contained), br)
)
write_table(rows, "fit.tex", "lrr", paste0("& 2010 & 2020", br))

reassignments <- read_csv(output_path("reassignment_table.csv"),
                          col_types = cols(code = col_character(), .default = col_guess())) %>%
  filter(year == later_year, abs(cutoff - slide_cutoff) < 1e-9)
rows <- vapply(seq_len(nrow(reassignments)), function(k) {
  r <- reassignments[k, ]
  name <- if (r$code %in% names(romanized)) romanized[[r$code]] else r$code
  action <- if (grepl("^move", r$suggestion)) "Reassign" else "Retain"
  sprintf("%s & %d & %s & %d & %s & %s%s", name, r$assigned_zone,
          share(r$share_commuting_to_assigned_zone), r$nearest_adjacent_zone,
          share(r$share_commuting_to_that_zone), action, br)
}, character(1))
write_table(rows, "reassignments.tex", "lrrrrl",
            paste0("Municipality & Zone & To own zone & Nearest zone & To that zone & Suggestion", br))

# ---------------------------------------------------------------------------
# Appendix tables

diagnostics_all <- diagnostics %>% filter(abs(cutoff - slide_cutoff) < 1e-9) %>% arrange(year)
rows <- sprintf("%d & %s & %s & %s & %s & %s%s", diagnostics_all$year,
                count(diagnostics_all$municipalities), count(diagnostics_all$commuting_zones),
                count(diagnostics_all$single_municipality_zones),
                count(diagnostics_all$municipalities_in_largest_zone),
                count(diagnostics_all$noncontiguous_zones_paper_test), br)
write_table(rows, "appendix_diagnostics_counts.tex", "lrrrrr",
            paste0("Year & Municipalities & Commuting & Single- & Largest & Non-", br,
                   "& & zones & municipality & zone & contiguous", br))

rows <- sprintf("%d & %s & %s & %s & %s & %s & %s & %s%s", diagnostics_all$year,
                count(diagnostics_all$min_population), count(diagnostics_all$mean_population),
                count(diagnostics_all$max_population), count(diagnostics_all$min_area_km2),
                count(diagnostics_all$mean_area_km2), count(diagnostics_all$max_area_km2),
                ratio(diagnostics_all$compactness), br)
write_table(rows, "appendix_diagnostics_size.tex", "lrrrrrrr",
            paste0("Year & \\multicolumn{3}{c}{Resident labour force} & ",
                   "\\multicolumn{3}{c}{Area (sq.\\,km)} & Compactness", br,
                   "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} ",
                   "& Smallest & Average & Largest & Smallest & Average & Largest &", br))

containment_all <- containment %>% filter(abs(cutoff - slide_cutoff) < 1e-9) %>% arrange(year)
rows <- sprintf("%d & %s & %s & %s & %s & %s%s", containment_all$year,
                share(containment_all$min_contained), share(containment_all$mean_contained),
                share(containment_all$labour_weighted_mean_contained),
                share(containment_all$share_of_labour_force_contained),
                share(containment_all$mean_work_contained), br)
write_table(rows, "appendix_containment.tex", "lrrrrr",
            paste0("Year & Minimum & Mean & Labour-force & Share of the & Mean work", br,
                   "& & & weighted mean & labour force & contained", br))

core_all <- core_table %>% filter(abs(cutoff - slide_cutoff) < 1e-9)
uea <- core_all %>% filter(core_definition == "Urban Employment Area central city")
district <- core_all %>% filter(core_definition == "densely inhabited district population at least 10,000")
rows <- vapply(sort(unique(core_all$year)), function(y) {
  u <- uea %>% filter(year == y)
  d <- district %>% filter(year == y)
  sprintf("%d & %s & %s & %s & %s & %s & %s & %s%s", y,
          if (nrow(u)) share(u$share_of_zones_with_a_core) else "--",
          if (nrow(u)) share(u$share_working_in_a_core_of_own_zone) else "--",
          if (nrow(u)) share(u$share_living_in_a_core_of_own_zone) else "--",
          if (nrow(u)) count(u$urban_areas_split_across_zones) else "--",
          share(d$share_of_zones_with_a_core), share(d$share_working_in_a_core_of_own_zone),
          share(d$share_living_in_a_core_of_own_zone), br)
}, character(1))
write_table(rows, "appendix_core.tex", "lrrrrrrr",
            paste0("Year & \\multicolumn{4}{c}{Urban Employment Area} & ",
                   "\\multicolumn{3}{c}{District population 10,000 or more}", br,
                   "\\cmidrule(lr){2-5} \\cmidrule(lr){6-8} ",
                   "& With core & Work in & Live in & Split & With core & Work in & Live in", br))

grid <- expand.grid(year = c(earlier_year, later_year),
                    cutoff = c(baseline_cutoff, setdiff(cutoff_anchors, baseline_cutoff)))
rows <- vapply(seq_len(nrow(grid)), function(k) {
  y <- grid$year[k]; cut <- grid$cutoff[k]
  d <- diagnostics %>% filter(year == y, abs(cutoff - cut) < 1e-9)
  c2 <- containment %>% filter(year == y, abs(cutoff - cut) < 1e-9)
  k2 <- core %>% filter(year == y, abs(cutoff - cut) < 1e-9)
  sprintf("%d & %s & %s & %s & %s & %s & %s & %s%s", y, format(cut, nsmall = 3),
          count(d$commuting_zones), count(d$noncontiguous_zones_paper_test),
          share(c2$min_contained), share(c2$mean_contained),
          share(c2$share_of_labour_force_contained),
          count(k2$urban_areas_split_across_zones), br)
}, character(1))
write_table(rows, "appendix_cutoff_comparison.tex", "llrrrrrr",
            paste0("Year & Cutoff & Commuting & Non- & Minimum & Mean & Share of the & Areas", br,
                   "& & zones & contiguous & contained & contained & labour force & split", br))

pairs_all <- read_csv(output_path("similarity_table.csv"), show_col_types = FALSE) %>%
  filter(abs(cutoff_earlier - slide_cutoff) < 1e-9) %>%
  arrange(earlier_year, later_year)
rows <- sprintf("%d against %d & %s & %s & %s & %s%s",
                pairs_all$earlier_year, pairs_all$later_year, pairs_all$role,
                count(pairs_all$municipalities), ratio(pairs_all$mean_similarity),
                ratio(pairs_all$labour_weighted_similarity_later), br)
write_table(rows, "appendix_pairs.tex", "llrrr",
            paste0("Pair & Role & Municipalities & Mean & Labour-force weighted", br))

# ---------------------------------------------------------------------------
# Numbers quoted in the deck's own prose.

similarity <- read_csv(output_path("similarity_table.csv"), show_col_types = FALSE) %>%
  filter(earlier_year == !!earlier_year, later_year == !!later_year,
         abs(cutoff_earlier - slide_cutoff) < 1e-9)
sweep <- read_csv(output_path("cutoff_sweep.csv"), show_col_types = FALSE) %>%
  filter(earlier_year == !!earlier_year, later_year == !!later_year,
         abs(anchor - slide_cutoff) < 1e-9)
best <- sweep %>% arrange(cutoff_later) %>% slice_max(mean_similarity, n = 1, with_ties = FALSE)
plateau <- sweep %>% filter(mean_similarity >= best$mean_similarity - 0.005)
macros <- c(
  sprintf("\\newcommand{\\meanSimilarity}{%s}", ratio(similarity$mean_similarity)),
  sprintf("\\newcommand{\\weightedSimilarity}{%s}", ratio(similarity$labour_weighted_similarity_later)),
  sprintf("\\newcommand{\\bestCutoff}{%s}", format(best$cutoff_later, nsmall = 3)),
  sprintf("\\newcommand{\\bestSimilarity}{%s}", ratio(best$mean_similarity)),
  sprintf("\\newcommand{\\plateauLow}{%s}", format(min(plateau$cutoff_later), nsmall = 3)),
  sprintf("\\newcommand{\\plateauHigh}{%s}", format(max(plateau$cutoff_later), nsmall = 3)),
  sprintf("\\newcommand{\\plateauZonesHigh}{%s}", count(max(plateau$commuting_zones))),
  sprintf("\\newcommand{\\plateauZonesLow}{%s}", count(min(plateau$commuting_zones)))
)
write_rows(macros, "numbers.tex")

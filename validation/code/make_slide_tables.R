# LaTeX table fragments for the coauthor-meeting deck.
#
# The deck follows the source paper's presentation: each table compares the 2010 and the
# 2020 delineation in two columns, at the cutoff of 0.977 that the source paper uses.
# Fragments are written to validation/slides/tables and are input by the deck; nothing
# here formats a table that the deck does not use.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

slide_cutoff <- 0.977
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

pick <- function(data, year) {
  data %>% filter(year == !!year, abs(cutoff - slide_cutoff) < 1e-9)
}

# The whole tabular is written out, not only its rows. A fragment holding bare rows has
# to be pulled in with \input from inside a tabular, where the last row's line break
# runs into the rule that follows it.
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

# Diagnostic statistics, the counterpart of Table 1.
diagnostics <- read_csv(file.path(output_dir, "diagnostics_table.csv"), show_col_types = FALSE)
a <- pick(diagnostics, earlier_year)
b <- pick(diagnostics, later_year)
rows <- c(
  sprintf("Municipalities in the delineation & %s & %s \\\\", count(a$municipalities), count(b$municipalities)),
  sprintf("Commuting zones & %s & %s \\\\", count(a$commuting_zones), count(b$commuting_zones)),
  sprintf("Single-municipality zones & %s & %s \\\\", count(a$single_municipality_zones), count(b$single_municipality_zones)),
  sprintf("Municipalities in the largest zone & %s & %s \\\\", count(a$municipalities_in_largest_zone), count(b$municipalities_in_largest_zone)),
  sprintf("Non-contiguous zones & %s & %s \\\\[2pt]", count(a$noncontiguous_zones_paper_test), count(b$noncontiguous_zones_paper_test)),
  sprintf("Smallest zone, resident labour force & %s & %s \\\\", count(a$min_population), count(b$min_population)),
  sprintf("Average zone, resident labour force & %s & %s \\\\", count(a$mean_population), count(b$mean_population)),
  sprintf("Largest zone, resident labour force & %s & %s \\\\[2pt]", count(a$max_population), count(b$max_population)),
  sprintf("Smallest zone area (sq.\\,km) & %s & %s \\\\", count(a$min_area_km2), count(b$min_area_km2)),
  sprintf("Average zone area (sq.\\,km) & %s & %s \\\\", count(a$mean_area_km2), count(b$mean_area_km2)),
  sprintf("Largest zone area (sq.\\,km) & %s & %s \\\\[2pt]", count(a$max_area_km2), count(b$max_area_km2)),
  sprintf("Compactness of zones & %s & %s \\\\", ratio(a$compactness), ratio(b$compactness))
)
write_table(rows, "diagnostics.tex", "lrr", "& 2010 & 2020 \\\\")

# Fit statistics, the counterpart of Table 2, in the same three groups.
containment <- read_csv(file.path(output_dir, "containment_table.csv"), show_col_types = FALSE)
core <- read_csv(file.path(output_dir, "core_table.csv"), show_col_types = FALSE) %>%
  filter(core_definition == "Urban Employment Area central city")
ca <- pick(containment, earlier_year); cb <- pick(containment, later_year)
ka <- pick(core, earlier_year); kb <- pick(core, later_year)
rows <- c(
  "\\multicolumn{3}{l}{\\emph{Core}} \\\\",
  sprintf("\\quad Urban areas split across zones & %s & %s \\\\", count(ka$urban_areas_split_across_zones), count(kb$urban_areas_split_across_zones)),
  sprintf("\\quad Zones containing a core & %s & %s \\\\", share(ka$share_of_zones_with_a_core), share(kb$share_of_zones_with_a_core)),
  sprintf("\\quad Residents working in a core of their zone & %s & %s \\\\", share(ka$share_working_in_a_core_of_own_zone), share(kb$share_working_in_a_core_of_own_zone)),
  sprintf("\\quad Workforce living in a core of their zone & %s & %s \\\\[3pt]", share(ka$share_living_in_a_core_of_own_zone), share(kb$share_living_in_a_core_of_own_zone)),
  "\\multicolumn{3}{l}{\\emph{Connection}} \\\\",
  "\\quad Minimum wage correlation & \\multicolumn{2}{c}{pending} \\\\",
  "\\quad Mean wage correlation & \\multicolumn{2}{c}{pending} \\\\",
  "\\quad Mean wage correlation, multi-municipality zones & \\multicolumn{2}{c}{pending} \\\\[3pt]",
  "\\multicolumn{3}{l}{\\emph{Containment}} \\\\",
  sprintf("\\quad Minimum contained & %s & %s \\\\", share(ca$min_contained), share(cb$min_contained)),
  sprintf("\\quad Mean contained & %s & %s \\\\", share(ca$mean_contained), share(cb$mean_contained)),
  sprintf("\\quad Share of the labour force contained & %s & %s \\\\", share(ca$share_of_labour_force_contained), share(cb$share_of_labour_force_contained))
)
write_table(rows, "fit.tex", "lrr", "& 2010 & 2020 \\\\")

# Reassignments that would remove non-contiguity, the counterpart of Table 3.
reassignments <- read_csv(file.path(output_dir, "reassignment_table.csv"),
                          col_types = cols(code = col_character(), .default = col_guess())) %>%
  filter(year == later_year, abs(cutoff - slide_cutoff) < 1e-9)
rows <- vapply(seq_len(nrow(reassignments)), function(k) {
  r <- reassignments[k, ]
  name <- if (r$code %in% names(romanized)) romanized[[r$code]] else r$code
  action <- if (grepl("^move", r$suggestion)) "Reassign" else "Retain"
  sprintf("%s & %d & %s & %d & %s & %s \\\\", name, r$assigned_zone,
          share(r$share_commuting_to_assigned_zone), r$nearest_adjacent_zone,
          share(r$share_commuting_to_that_zone), action)
}, character(1))
write_table(rows, "reassignments.tex", "lrrrrl",
            "Municipality & Zone & To own zone & Nearest zone & To that zone & Suggestion \\\\")

# Figures the deck refers to by number in its own prose, and the similarity it reports.
similarity <- read_csv(file.path(output_dir, "similarity_table.csv"), show_col_types = FALSE) %>%
  filter(earlier_year == !!earlier_year, later_year == !!later_year,
         abs(cutoff_earlier - slide_cutoff) < 1e-9)
sweep <- read_csv(file.path(output_dir, "cutoff_sweep.csv"), show_col_types = FALSE) %>%
  filter(earlier_year == !!earlier_year, later_year == !!later_year,
         abs(anchor - slide_cutoff) < 1e-9)
best <- sweep %>% slice_max(mean_similarity, n = 1)
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

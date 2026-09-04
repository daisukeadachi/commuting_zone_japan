# LaTeX table fragments for the paper.
#
# The deck's fragments under validation/slides/tables are written for a projector: no
# notes, few rows, two columns of headline years. The paper needs the same numbers set out
# for a reader who can stop and look, so the fragments are written separately rather than
# shared. Each holds the tabular alone; the caption and the notes are written where the
# table is used.
#
# Two scopes appear. The published delineation covers every municipality the census
# reports, offshore islands included, and that is what the table of the delineation
# itself reports. The diagnostic and fit statistics that replicate Fowler (2024) are
# computed on the four main islands, the Okinawa main island and everything joined to one
# of them by a permanent road link, which is the like-for-like comparison with the source
# paper and leaves out half a percent of the labour force. Every note says which.
#
# Writes validation/output/paper_tables/*.tex.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

table_dir <- file.path(output_dir, "paper_tables")
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

paper_cutoff <- baseline_cutoff
headline_years <- c(2010, 2020)

count <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
share <- function(x) sprintf("%.1f\\%%", 100 * x)
share_fine <- function(x) sprintf("%.2f\\%%", 100 * x)
ratio <- function(x) sprintf("%.2f", x)
br <- " \\\\"

at_cutoff <- function(data) data %>% filter(abs(cutoff - paper_cutoff) < 1e-9) %>% arrange(year)
pick <- function(data, y) data %>% filter(year == y, abs(cutoff - paper_cutoff) < 1e-9)

write_table <- function(rows, path, spec, header) {
  writeLines(c(sprintf("\\begin{tabular}{%s}", spec), "\\hline\\hline", header, "\\hline",
               rows, "\\hline\\hline", "\\end{tabular}"),
             file.path(table_dir, path))
  message("wrote ", path, " (", length(rows), " rows)")
}

full <- at_cutoff(read_csv(output_path("full_coverage_zones.csv"), show_col_types = FALSE))
diagnostics <- at_cutoff(read_csv(output_path("diagnostics_table.csv"), show_col_types = FALSE))
containment <- at_cutoff(read_csv(output_path("containment_table.csv"), show_col_types = FALSE))
core_all <- at_cutoff(read_csv(output_path("core_table.csv"), show_col_types = FALSE))
core <- core_all %>% filter(core_definition == "Urban Employment Area central city")
comparison <- at_cutoff(read_csv(file.path(output_dir, "constrained_comparison.csv"),
                                 show_col_types = FALSE))
us <- read_csv(file.path(output_dir, "us_compactness.csv"), show_col_types = FALSE)

# ---------------------------------------------------------------------------
# The published delineation, every census year.

rows <- sprintf("%d & %s & %s & %s & %s & %s & %s & %s%s", full$year,
                count(full$municipalities), count(full$zones), count(full$island_zones),
                count(diagnostics$municipalities), count(diagnostics$commuting_zones),
                count(diagnostics$single_municipality_zones),
                count(diagnostics$municipalities_in_largest_zone), br)
write_table(rows, "delineation.tex", "lrrrrrrr",
            paste0("& \\multicolumn{3}{c}{Whole country} & \\multicolumn{4}{c}{Main islands}", br,
                   "\\cline{2-4} \\cline{5-8}",
                   "Year & Municipality & Commuting & Offshore & Municipality & Commuting & ",
                   "Single-unit & Largest", br,
                   "& units & zones & zones & units & zones & zones & zone", br))

# ---------------------------------------------------------------------------
# Diagnostic statistics for the two headline years, the counterpart of Table 1 of the
# source paper, with the compactness of the units the zones are built from beside the
# compactness of the zones and the United States figures below them.

a <- pick(diagnostics, headline_years[1]); b <- pick(diagnostics, headline_years[2])
us_county <- us$compactness[us$unit == "county"]
us_zone <- us$compactness[us$unit == "commuting zone"]
rows <- c(
  sprintf("Municipalities in the delineation & %s & %s%s", count(a$municipalities), count(b$municipalities), br),
  sprintf("Commuting zones & %s & %s%s", count(a$commuting_zones), count(b$commuting_zones), br),
  sprintf("Single-municipality zones & %s & %s%s", count(a$single_municipality_zones), count(b$single_municipality_zones), br),
  sprintf("Municipalities in the largest zone & %s & %s%s", count(a$municipalities_in_largest_zone), count(b$municipalities_in_largest_zone), br),
  sprintf("Non-contiguous zones & %s & %s%s[3pt]", count(a$noncontiguous_zones_paper_test), count(b$noncontiguous_zones_paper_test), br),
  sprintf("Smallest zone, resident labour force & %s & %s%s", count(a$min_population), count(b$min_population), br),
  sprintf("Average zone, resident labour force & %s & %s%s", count(a$mean_population), count(b$mean_population), br),
  sprintf("Largest zone, resident labour force & %s & %s%s[3pt]", count(a$max_population), count(b$max_population), br),
  sprintf("Smallest zone area (sq.\\,km) & %s & %s%s", count(a$min_area_km2), count(b$min_area_km2), br),
  sprintf("Average zone area (sq.\\,km) & %s & %s%s", count(a$mean_area_km2), count(b$mean_area_km2), br),
  sprintf("Largest zone area (sq.\\,km) & %s & %s%s[3pt]", count(a$max_area_km2), count(b$max_area_km2), br),
  sprintf("Compactness of zones & %s & %s%s", ratio(a$compactness), ratio(b$compactness), br),
  sprintf("Compactness of municipalities & %s & %s%s[3pt]", ratio(a$compactness_municipalities), ratio(b$compactness_municipalities), br),
  sprintf("United States, compactness of commuting zones & \\multicolumn{2}{c}{%s}%s", ratio(us_zone), br),
  sprintf("United States, compactness of counties & \\multicolumn{2}{c}{%s}%s", ratio(us_county), br))
write_table(rows, "diagnostics.tex", "lrr",
            paste0("& ", headline_years[1], " & ", headline_years[2], br))

# ---------------------------------------------------------------------------
# Fit statistics for the two headline years, the counterpart of Table 2 of the source
# paper. The Connection block waits on wage microdata and is left out rather than shown
# empty.

ca <- pick(containment, headline_years[1]); cb <- pick(containment, headline_years[2])
ka <- pick(core, headline_years[1]); kb <- pick(core, headline_years[2])
rows <- c(
  paste0("\\multicolumn{3}{l}{\\emph{Core}}", br),
  sprintf("\\quad Urban areas split across zones & %s & %s%s", count(ka$urban_areas_split_across_zones), count(kb$urban_areas_split_across_zones), br),
  sprintf("\\quad Zones containing a core & %s & %s%s", share(ka$share_of_zones_with_a_core), share(kb$share_of_zones_with_a_core), br),
  sprintf("\\quad Residents working in a core of their zone & %s & %s%s", share(ka$share_working_in_a_core_of_own_zone), share(kb$share_working_in_a_core_of_own_zone), br),
  sprintf("\\quad Workforce living in a core of their zone & %s & %s%s[3pt]", share(ka$share_living_in_a_core_of_own_zone), share(kb$share_living_in_a_core_of_own_zone), br),
  paste0("\\multicolumn{3}{l}{\\emph{Containment}}", br),
  sprintf("\\quad Minimum contained & %s & %s%s", share(ca$min_contained), share(cb$min_contained), br),
  sprintf("\\quad Fifth percentile contained & %s & %s%s", share(ca$p5_contained), share(cb$p5_contained), br),
  sprintf("\\quad Median contained & %s & %s%s", share(ca$median_contained), share(cb$median_contained), br),
  sprintf("\\quad Mean contained & %s & %s%s", share(ca$mean_contained), share(cb$mean_contained), br),
  sprintf("\\quad Share of the labour force contained & %s & %s%s", share(ca$share_of_labour_force_contained), share(cb$share_of_labour_force_contained), br),
  sprintf("\\quad Mean work contained & %s & %s%s", share(ca$mean_work_contained), share(cb$mean_work_contained), br))
write_table(rows, "fit.tex", "lrr",
            paste0("& ", headline_years[1], " & ", headline_years[2], br))

# ---------------------------------------------------------------------------
# Appendix: the same three blocks over every census year, and what the contiguity
# constraint changes.

rows <- sprintf("%d & %s & %s & %s & %s & %s & %s & %s & %s%s", diagnostics$year,
                count(diagnostics$min_population), count(diagnostics$mean_population),
                count(diagnostics$max_population), count(diagnostics$min_area_km2),
                count(diagnostics$mean_area_km2), count(diagnostics$max_area_km2),
                ratio(diagnostics$compactness), ratio(diagnostics$compactness_municipalities), br)
write_table(rows, "appendix_size.tex", "lrrrrrrrr",
            paste0("Year & \\multicolumn{3}{c}{Resident labour force} & ",
                   "\\multicolumn{3}{c}{Area (sq.\\,km)} & \\multicolumn{2}{c}{Compactness}", br,
                   "\\cline{2-4} \\cline{5-7} \\cline{8-9}",
                   "& Smallest & Average & Largest & Smallest & Average & Largest & ",
                   "Zones & Municipalities", br))

rows <- sprintf("%d & %s & %s & %s & %s & %s & %s & %s%s", containment$year,
                share(containment$min_contained), share(containment$p5_contained),
                share(containment$median_contained), share(containment$mean_contained),
                share(containment$labour_weighted_mean_contained),
                share(containment$share_of_labour_force_contained),
                share(containment$mean_work_contained), br)
write_table(rows, "appendix_containment.tex", "lrrrrrrr",
            paste0("Year & Minimum & Fifth & Median & Mean & Labour-force & Share of the & ",
                   "Mean work", br,
                   "& & percentile & & & weighted mean & labour force & contained", br))

district <- core_all %>%
  filter(core_definition == "densely inhabited district population at least 10,000")
rows <- vapply(sort(unique(core_all$year)), function(y) {
  u <- core %>% filter(year == y)
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
                   "\\cline{2-5} \\cline{6-8}",
                   "& With core & Work in & Live in & Split & With core & Work in & Live in", br))

rows <- sprintf("%d & %s & %s & %s & %s & %s & %s & %s%s", comparison$year,
                count(comparison$zones_unconstrained), count(comparison$zones_constrained),
                count(comparison$noncontiguous_zones_unconstrained),
                count(comparison$noncontiguous_zones_constrained),
                share(comparison$mean_contained_unconstrained),
                share(comparison$mean_contained_constrained),
                ratio(comparison$mean_similarity), br)
write_table(rows, "appendix_constraint.tex", "lrrrrrrr",
            paste0("Year & \\multicolumn{2}{c}{Commuting zones} & ",
                   "\\multicolumn{2}{c}{Non-contiguous zones} & ",
                   "\\multicolumn{2}{c}{Mean contained} & Jaccard", br,
                   "\\cline{2-3} \\cline{4-5} \\cline{6-7}",
                   "& Without & With & Without & With & Without & With & similarity", br))

# ---------------------------------------------------------------------------
# Numbers quoted in the paper's own prose.

similarity <- read_csv(output_path("similarity_table.csv"), show_col_types = FALSE) %>%
  filter(earlier_year == 2010, later_year == 2020,
         abs(cutoff_earlier - paper_cutoff) < 1e-9, abs(cutoff_later - paper_cutoff) < 1e-9)
sweep <- read_csv(output_path("cutoff_sweep.csv"), show_col_types = FALSE) %>%
  filter(earlier_year == 2010, later_year == 2020, abs(anchor - paper_cutoff) < 1e-9)
best <- sweep %>% arrange(cutoff_later) %>% slice_max(mean_similarity, n = 1, with_ties = FALSE)
plateau <- sweep %>% filter(mean_similarity >= best$mean_similarity - 0.005)
latest <- full %>% filter(year == max(year))
base <- full %>% filter(year == min(year))
b2015 <- full %>% filter(year == 2015)

macros <- c(
  sprintf("\\newcommand{\\zonesLatest}{%s}", count(latest$zones)),
  sprintf("\\newcommand{\\unitsLatest}{%s}", count(latest$municipalities)),
  sprintf("\\newcommand{\\zonesFirst}{%s}", count(base$zones)),
  sprintf("\\newcommand{\\unitsFirst}{%s}", count(base$municipalities)),
  sprintf("\\newcommand{\\zonesFifteen}{%s}", count(b2015$zones)),
  sprintf("\\newcommand{\\unitsFifteen}{%s}", count(b2015$municipalities)),
  sprintf("\\newcommand{\\islandShare}{%s}", share_fine(latest$share_of_labour_force_on_islands)),
  sprintf("\\newcommand{\\meanSimilarity}{%s}", ratio(similarity$mean_similarity)),
  sprintf("\\newcommand{\\bestCutoff}{%s}", format(best$cutoff_later, nsmall = 3)),
  sprintf("\\newcommand{\\bestSimilarity}{%s}", ratio(best$mean_similarity)),
  sprintf("\\newcommand{\\plateauLow}{%s}", format(min(plateau$cutoff_later), nsmall = 3)),
  sprintf("\\newcommand{\\plateauHigh}{%s}", format(max(plateau$cutoff_later), nsmall = 3)),
  sprintf("\\newcommand{\\plateauZonesLow}{%s}", count(min(plateau$commuting_zones))),
  sprintf("\\newcommand{\\plateauZonesHigh}{%s}", count(max(plateau$commuting_zones))),
  sprintf("\\newcommand{\\usCountyCompactness}{%s}", ratio(us_county)),
  sprintf("\\newcommand{\\usZoneCompactness}{%s}", ratio(us_zone)),
  sprintf("\\newcommand{\\usZones}{%s}", count(us$units[us$unit == "commuting zone"])),
  sprintf("\\newcommand{\\constraintSimilarity}{%s}",
          ratio(min(comparison$mean_similarity))),
  sprintf("\\newcommand{\\constraintDetached}{%s}",
          count(max(comparison$noncontiguous_zones_unconstrained))))
writeLines(macros, file.path(table_dir, "numbers.tex"))
message("wrote numbers.tex (", length(macros), " macros)")

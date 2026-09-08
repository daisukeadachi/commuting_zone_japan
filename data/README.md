# Inputs

The delineation is built from data the authors may not redistribute. This folder therefore holds only what the project itself produced, and this README says where each of the other inputs comes from. Point `CZ_SHARED_ROOT` at the folder holding the commuting matrices and the boundary layer before running anything in `code/`.

## What this folder holds

`municipality_scope.csv` lists every municipality unit of 2020 and marks the ones inside the main-island scope, which is the four main islands, the Okinawa main island and every municipality joined to one of them by a permanent road link. A municipality unit is a municipality with the wards of a government-designated city, and the 23 special wards of Tokyo, each merged into one unit.

`adjacency_edges.csv` is the municipality adjacency graph the constrained clustering runs on: one row per pair of units that share a boundary, plus the permanent road links across water that `code/config.R` lists.

`original/` holds the same two tables for each census date on the municipality codes in force at that date.

All three are produced by `code/build_scope_and_adjacency.R` and `code/build_original_scope_and_adjacency.R` from the boundary layers below.

## What this folder does not hold

**Census commuting matrices, 1980 to 2020.** One matrix per census year, giving the number of residents of each municipality who work in each municipality. They are aggregates of the Population Census microdata, which the Ministry of Internal Affairs and Communications holds and the authors cannot redistribute. `code/config.R` reads them from `CZ_SHARED_ROOT`, one subfolder per labor-force sample, as `commute_<year>_<harmonized|original>.csv`.

**Municipality boundary polygons.** One layer per census date, from Municipality Map Maker for Web, <http://tkirimura.com/mmm/>. `code/fetch_boundaries.py` drives the tool's own endpoints and asks for designated cities as single cities, the Tokyo special wards kept as wards, on the lightweight world-geodetic output. The tool covers 1 January 1970 to 1 May 2019, so the 2020 census is read on the 2015 layer; no municipal boundary moved after 2015, and the two towns that took new codes on incorporation are renamed on read.

**Municipality code crosswalks between census dates.** From the same tool, fetched by `code/fetch_crosswalks.py` and extended to the 2020 and 2025 dates by `code/build_recent_crosswalks.py`.

**Urban Employment Area membership, 1980 to 2020.** The Metropolitan and Micropolitan Employment Area suburb and central-city lists of Kanemoto and Tokuoka, <https://www.csis.u-tokyo.ac.jp/UEA/uea_code.htm>. There is no 1985 delineation; the series runs 1980 then 1990. The archives are encoded in CP932, and the number of suburb blocks per row varies by year, four from 2010 onward and three before. The 1995 and 2000 metropolitan files and the 2000 micropolitan file repeat some rows, so deduplicate on the full row before use.

**Densely inhabited district population and area, by municipality.** For 1980 to 2015 at five-year intervals, dataset A16 of the National Land Numerical Information series, <https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A16.html>, assembled by `code/build_did_table.py`. For 2020, table 1-2 of the basic tabulation of the 2020 census on e-Stat, which has to be fetched through a browser: the series above stops at 2015, and the e-Stat result list is driven by JavaScript, from which no stable file URL can be derived.

**United States county polygons with the 2020 commuting zone of each.** The layer released with Fowler (2024), from the Output Data of <https://github.com/csfowler/CommutingZones2020>, fetched on first use by `code/build_us_compactness.R`. It carries that paper's county vintage, its zone assignment and its projection rather than a reconstruction of any of the three.

## Two checks worth repeating

The 2015 boundary layer the fetch script returns is identical, code for code and area for area, to the layer the project has used throughout. That is the check that the tool's parameters are the intended ones.

The mean Polsby-Popper ratio over the dissolved zones of the United States layer is 0.397 against the 0.40 that Fowler (2024) reports, over 593 zones against its 593.

## Where the two district sources disagree

Validating the central-city district populations in the Urban Employment Area files against the independently built district table gives an exact match for 2005, 2010 and 2015, one discrepancy in 2015 at Matsumoto, and sixteen and twelve discrepancies in 2000 and 1990. The older files carry values that belong to other rows, so where the two sources disagree before 2005, prefer the district table.

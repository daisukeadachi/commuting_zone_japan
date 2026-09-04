# Provenance of the external data

Everything listed in issue #13 is here. Most of it was downloaded by script and is either committed or rebuildable with `validation/code/build_did_table.py`; one file had to be fetched through a browser and is recorded below.

## Outstanding

Nothing. The one item that needed a browser has been supplied.

## Resolved

| What | Year | Source | File | How it was checked |
| --- | --- | --- | --- | --- |
| Densely inhabited district population and area, by municipality | 2020 | e-Stat, 2020 census, basic tabulation, table 1-2 「男女別人口，世帯の種類別世帯数及び面積－全国，都道府県，市区町村（人口集中地区）」 | `validation/data/did_municipality_2020_raw.xlsx` | District population sums to 88,285,927, matching the published national figure exactly. Counting the special wards of Tokyo as one unit gives 793 municipalities with a district, again an exact match. District area sums to 13,245.8 against a published 13,250.4, a gap of 0.03 percent arising from rounding the ward figures. |

The National Land Numerical Information series that supplied 1980 to 2015 stops at 2015, and on e-Stat the equivalent table sits behind a JavaScript-driven result list from which no stable file URL could be derived, so this one file was fetched by hand.

## Already collected, for contrast

| What | Years | Files | Source |
| --- | --- | --- | --- |
| Metropolitan Employment Area, suburb lists | 1980, 1990, 1995, 2000, 2005, 2010, 2015, 2020 | `validation/data/uea/MEA*.csv` | <https://www.csis.u-tokyo.ac.jp/UEA/uea_code.htm> |
| Metropolitan Employment Area, central-city lists | same | `validation/data/uea/MEA*C*.csv` | same |
| Micropolitan Employment Area, suburb lists | same | `validation/data/uea/M[Cc]EA*.csv` | same |
| Micropolitan Employment Area, central-city lists | same | `validation/data/uea/M[Cc]EA*C*.csv` | same |
| Densely inhabited district population and area, by municipality | 1980 to 2015 at five-year intervals | `validation/data/did_municipality.csv` | National Land Numerical Information dataset A16, <https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A16.html> |
| Densely inhabited district population and area, by municipality | 2020 | same file, appended | e-Stat table 1-2 of the 2020 basic tabulation |
| Municipality boundary polygons, one layer per census date | 1980 to 2015 at five-year intervals | `validation/data/boundaries/mmm<date>/` | Municipality Map Maker for Web, <http://tkirimura.com/mmm/>, fetched by `validation/code/fetch_boundaries.py` |
| United States county polygons with the 2020 commuting zone of each | 2020 | `validation/data/us/county20.shp` | Output Data of <https://github.com/csfowler/CommutingZones2020>, fetched by `validation/code/build_us_compactness.R` |

There is no 1985 Urban Employment Area delineation; the series runs 1980 then 1990. This does not affect the present design, since 1985 appears in none of the comparison pairs.

## Notes on the collected files

The Urban Employment Area archives are encoded in CP932 and the number of suburb blocks per row varies by year, four from 2010 onward and three before.

The central-city files report the district population from 1990 onward and, from 2000 onward, also the central-city population and the employment-residence ratio. The 1980 and 1995 files carry names and codes only.

The 1995 and 2000 metropolitan files and the 2000 micropolitan file repeat some rows, so deduplicate on the full row before use. Validating the central-city district populations against the independently built district table gives an exact match for 2005, 2010 and 2015, one discrepancy in 2015 at Matsumoto, and sixteen and twelve discrepancies in 2000 and 1990 respectively. Inspection of the latter shows the older files carrying values that belong to other rows, so where the two sources disagree before 2005, prefer the district table.

## The two layers fetched by script

Both are gitignored and are re-fetched by the script that uses them.

The boundary layers are generated on demand, so they carry no stable URL; `fetch_boundaries.py` drives the tool's own endpoints and asks for designated cities as single cities with the Tokyo special wards kept as wards, on the lightweight world-geodetic output. The 2015 layer it returns is identical to `mmm20151001_ku_aggregate` in the shared project folder, code for code and area for area, which is the check that the parameters are the ones the project has been using all along. The tool covers 1 January 1970 to 1 May 2019, so the 2020 census is read on the 2015 layer; no municipal boundary moved after 2015 and the two towns that took new codes are renamed on read.

The United States layer is the one Fowler (2024) released, so it carries that paper's county vintage, its zone assignment and its projection rather than a reconstruction of any of the three. Computing the mean Polsby-Popper ratio over its dissolved zones gives 0.397 against the 0.4 the paper reports, and 593 zones against its 593.

# Data that could not be fetched automatically

Everything else listed in issue #13 was downloaded by script and is either committed here or rebuildable with `validation/code/build_did_table.py`. What follows is the remainder, which needs a browser.

## Outstanding

| # | What | Year | Where | Why it needs a browser | Save as |
| --- | --- | --- | --- | --- | --- |
| 1 | Densely inhabited district population and area, by municipality | 2020 | e-Stat, 令和2年国勢調査, 人口等基本集計, table 1-2 「男女別人口，世帯数及び面積－全国，都道府県，市区町村（人口集中地区）」. Entry point: <https://www.e-stat.go.jp/stat-search/files?toukei=00200521&tstat=000001136464> | The National Land Numerical Information series that supplied 1980 to 2015 stops at 2015. On e-Stat the equivalent table is reachable only through a JavaScript-driven result list, so no stable file URL could be derived. | `validation/data/did_municipality_2020_raw.xlsx` |

Check figures for that file once it is in hand, from the Statistics Bureau summary of the 2020 round: 1,276 districts, set in 793 of the 1,719 municipalities, holding 88,285,927 residents, which is 70.0 percent of the national population of 126,146,099.

The same three quantities computed from the 1980 to 2015 table already built here give 825 municipalities and 86,985,899 residents in 2015, so the 2020 figures should sit slightly below on municipality count and slightly above on population.

## Scope of the gap

This blocks only the secondary version of the Core measures, the mechanical rule on district population, and only in 2020. The primary version, which reads the Urban Employment Area, is unaffected: the 2020 central-city files carry the district population of every central city, so the Core block and the map of split areas can be produced for every year in the sample without this file.

## Already collected, for contrast

| What | Years | Files | Source |
| --- | --- | --- | --- |
| Metropolitan Employment Area, suburb lists | 1980, 1990, 1995, 2000, 2005, 2010, 2015, 2020 | `validation/data/uea/MEA*.csv` | <https://www.csis.u-tokyo.ac.jp/UEA/uea_code.htm> |
| Metropolitan Employment Area, central-city lists | same | `validation/data/uea/MEA*C*.csv` | same |
| Micropolitan Employment Area, suburb lists | same | `validation/data/uea/M[Cc]EA*.csv` | same |
| Micropolitan Employment Area, central-city lists | same | `validation/data/uea/M[Cc]EA*C*.csv` | same |
| Densely inhabited district population and area, by municipality | 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015 | `validation/data/did_municipality.csv` | National Land Numerical Information dataset A16, <https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A16.html> |

There is no 1985 Urban Employment Area delineation; the series runs 1980 then 1990. This does not affect the present design, since 1985 appears in none of the comparison pairs.

## Notes on the collected files

The Urban Employment Area archives are encoded in CP932 and the number of suburb blocks per row varies by year, four from 2010 onward and three before.

The central-city files report the district population from 1990 onward and, from 2000 onward, also the central-city population and the employment-residence ratio. The 1980 and 1995 files carry names and codes only.

The 1995 and 2000 metropolitan files and the 2000 micropolitan file repeat some rows, so deduplicate on the full row before use. Validating the central-city district populations against the independently built district table gives an exact match for 2005, 2010 and 2015, one discrepancy in 2015 at Matsumoto, and sixteen and twelve discrepancies in 2000 and 1990 respectively. Inspection of the latter shows the older files carrying values that belong to other rows, so where the two sources disagree before 2005, prefer the district table.

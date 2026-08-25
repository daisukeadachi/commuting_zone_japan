"""Put the district table on harmonized municipality codes.

Harmonized means the boundaries in force on 1 October 2015, the base used
throughout the project: where several municipalities of an earlier year were
later consolidated, their figures are added together and attributed to the
successor. Because the mapping is many-to-one everywhere it matters, adding a
count is exact rather than an approximation.

The 2015 codes need no mapping. The 2020 codes lie beyond the coverage of
Municipality Map Maker, but nothing merged between the two censuses: the count
of municipalities stood at 1,718 plus the 23 special wards on both dates, and
the only changes were two towns incorporated as cities, each without any
boundary change. Those two are reversed by hand.

One municipality in the crosswalk splits between two successors, Kamikuishiki
village in Yamanashi, and it holds no district in any year, so the split never
has to be resolved here. The draft assigns it whole to Fujikawaguchiko town for
the commuting data.

Codes 01695, 01696 and 01698 appear as 2015 targets but never in the census;
they are the Northern Territories, and they hold no district either.
"""
import csv
import os

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, os.pardir, "data")
SRC = os.path.join(DATA, "did_municipality.csv")
OUT = os.path.join(DATA, "did_municipality_harmonized.csv")
XWALK = os.path.join(DATA, "crosswalk")

MAPPED_YEARS = ["1980", "1985", "1990", "1995", "2000", "2005", "2010"]

# Towns incorporated as cities between the 2015 and 2020 censuses, without any
# boundary change. Mapping the 2020 code back gives the 2015 code.
CITY_INCORPORATIONS_2020 = {"04216": "04423", "40231": "40305"}


def load_crosswalk(year):
    path = os.path.join(XWALK, f"codelist_{year}1001and20151001.csv")
    mapping = {}
    with open(path, encoding="cp932") as f:
        for row in csv.DictReader(f):
            src = row["JISCODE1"].zfill(5)
            dst = row["JISCODE2"].zfill(5)
            try:
                weight = float(row["WEIGHT"])
            except (TypeError, ValueError):
                weight = 1.0
            mapping.setdefault(src, []).append((dst, weight))
    return mapping


def main():
    rows = list(csv.DictReader(open(SRC, encoding="utf-8")))
    crosswalks = {y: load_crosswalk(y) for y in MAPPED_YEARS}

    agg = {}
    unmapped = {}
    for r in rows:
        year, code = r["year"], r["muni_code"]
        if year in crosswalks:
            targets = crosswalks[year].get(code)
            if not targets:
                unmapped.setdefault(year, []).append(code)
                continue
            # Keep the successor holding the largest share. Every municipality
            # that holds a district maps one-to-one, so this only ever picks the
            # single available target.
            dst = max(targets, key=lambda t: t[1])[0]
        elif year == "2020":
            dst = CITY_INCORPORATIONS_2020.get(code, code)
        else:
            dst = code
        k = (year, dst)
        if k not in agg:
            agg[k] = {"year": year, "muni_code": dst, "muni_name": r["muni_name"],
                      "did_population": 0, "did_area_km2": 0.0,
                      "n_districts": 0, "n_source_municipalities": 0,
                      "_blank_count": False}
        agg[k]["did_population"] += int(r["did_population"])
        agg[k]["did_area_km2"] += float(r["did_area_km2"])
        if r["n_districts"] == "":
            agg[k]["_blank_count"] = True
        else:
            agg[k]["n_districts"] += int(r["n_districts"])
        agg[k]["n_source_municipalities"] += 1

    for year, codes in sorted(unmapped.items()):
        print(f"{year}: {len(codes)} codes absent from the crosswalk: {sorted(codes)}")

    fields = ["year", "muni_code", "muni_name", "did_population", "did_area_km2",
              "n_districts", "n_source_municipalities"]
    with open(OUT, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        for k in sorted(agg):
            row = dict(agg[k])
            row["did_area_km2"] = round(row["did_area_km2"], 2)
            if row.pop("_blank_count"):
                row["n_districts"] = ""
            w.writerow({k2: row[k2] for k2 in fields})

    print(f"wrote {OUT}: {len(agg)} municipality-year rows")
    print(f"{'year':6s} {'harmonized units':>17s} {'source units':>13s} "
          f"{'population':>12s} {'>=50,000':>9s}")
    for y in sorted({k[0] for k in agg}):
        sub = [v for k, v in agg.items() if k[0] == y]
        print(f"{y:6s} {len(sub):17d} {sum(v['n_source_municipalities'] for v in sub):13d} "
              f"{sum(v['did_population'] for v in sub):12d} "
              f"{sum(1 for v in sub if v['did_population'] >= 50000):9d}")


if __name__ == "__main__":
    main()

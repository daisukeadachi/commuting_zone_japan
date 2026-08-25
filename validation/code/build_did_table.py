"""Build a municipality-level densely inhabited district table for 1980-2015.

Source: National Land Numerical Information dataset A16, published by the
Ministry of Land, Infrastructure, Transport and Tourism. National archives
exist for 2010 and 2015; earlier years are published one archive per
prefecture. Only the .dbf inside each archive is read; the geometry is
discarded.

Two corrections are applied to the raw records.

A district whose polygon comes in several disjoint pieces is written as one
record per piece, each repeating the district's full population and area, so
the records are deduplicated on the district identifier before summing.

The raw codes are at ward level for the special wards of Tokyo and for the
wards of ordinance-designated cities. The designated-city wards are collapsed
to their city, and the Tokyo wards are left alone, which is the convention the
commuting matrices use.

The dataset stops at 2015. The 2020 figures come instead from table 1-2 of the
basic tabulation of the 2020 census, saved alongside as
did_municipality_2020_raw.xlsx, which reports the same two quantities for the
same units.
"""
import csv
import os
import struct
import time
import urllib.request
import zipfile

try:
    import openpyxl
except ImportError:
    openpyxl = None

BASE = "https://nlftp.mlit.go.jp/ksj/gml/data/A16"
HERE = os.path.dirname(os.path.abspath(__file__))
CACHE = os.path.join(HERE, os.pardir, "data", "a16_cache")
OUT = os.path.join(HERE, os.pardir, "data", "did_municipality.csv")
XLSX_2020 = os.path.join(HERE, os.pardir, "data", "did_municipality_2020_raw.xlsx")

NATIONAL = ["10", "15"]
PER_PREF = ["80", "85", "90", "95", "00", "05"]
PREFS = [f"{i:02d}" for i in range(1, 48)]

# Wards of ordinance-designated cities collapse to the city. Ranges follow the
# treatment already used to build the commuting matrices. The special wards of
# Tokyo are deliberately absent: they stay separate.
WARD_RANGES = [
    (1100, 1199, 1100, "札幌市"), (4100, 4199, 4100, "仙台市"),
    (11100, 11199, 11100, "さいたま市"), (12100, 12199, 12100, "千葉市"),
    (14100, 14129, 14100, "横浜市"), (14130, 14149, 14130, "川崎市"),
    (14150, 14199, 14150, "相模原市"), (15100, 15199, 15100, "新潟市"),
    (22100, 22129, 22100, "静岡市"), (22130, 22199, 22130, "浜松市"),
    (23100, 23199, 23100, "名古屋市"), (26100, 26199, 26100, "京都市"),
    (27100, 27139, 27100, "大阪市"), (27140, 27199, 27140, "堺市"),
    (28100, 28199, 28100, "神戸市"), (33100, 33199, 33100, "岡山市"),
    (34100, 34199, 34100, "広島市"), (40100, 40129, 40100, "北九州市"),
    (40130, 40199, 40130, "福岡市"), (43100, 43199, 43100, "熊本市"),
]
CITY_NAME = {f"{t:05d}": n for _, _, t, n in WARD_RANGES}


def collapse_wards(code):
    n = int(code)
    for lo, hi, target, _ in WARD_RANGES:
        if lo <= n <= hi:
            return f"{target:05d}"
    return f"{n:05d}"


def fetch(url, name):
    os.makedirs(CACHE, exist_ok=True)
    path = os.path.join(CACHE, name)
    if os.path.exists(path) and os.path.getsize(path) > 0:
        return path
    for attempt in range(3):
        try:
            with urllib.request.urlopen(url, timeout=120) as r:
                data = r.read()
            with open(path, "wb") as f:
                f.write(data)
            return path
        except Exception as e:
            print(f"    retry {attempt + 1} for {name}: {e}")
            time.sleep(2)
    return None


def read_dbf(raw):
    nrec, hlen, rlen = struct.unpack("<IHH", raw[4:12])
    nfields = (hlen - 33) // 32
    fields = []
    for i in range(nfields):
        fd = raw[32 + i * 32: 64 + i * 32]
        fields.append((fd[:11].split(b"\x00")[0].decode("cp932", "replace"), fd[16]))
    pos = hlen
    for _ in range(nrec):
        rec = raw[pos: pos + rlen]
        pos += rlen
        if not rec or rec[:1] == b"*":
            continue
        p = 1
        row = {}
        for name, ln in fields:
            row[name] = rec[p: p + ln].decode("cp932", "replace").strip()
            p += ln
        yield row


def districts_from_zip(path):
    with zipfile.ZipFile(path) as z:
        for name in z.namelist():
            if name.lower().endswith(".dbf"):
                yield from read_dbf(z.read(name))


def num(v, cast=float, default=0):
    try:
        return cast(float(v))
    except (TypeError, ValueError):
        return default


def rows_2020():
    """Read the 2020 figures from table 1-2 of the basic tabulation.

    The sheet carries one row per district plus a total row per unit, marked
    DID00 in the district-symbol column, and lists the wards of designated
    cities both individually and aggregated to the city. The city-level row is
    taken and the ward rows dropped, except for Tokyo, where the wards are kept
    and the aggregate row for the special wards is dropped instead.
    """
    if openpyxl is None:
        print("openpyxl missing; skipping 2020")
        return
    if not os.path.exists(XLSX_2020):
        print(f"{XLSX_2020} not present; skipping 2020")
        return
    ws = openpyxl.load_workbook(XLSX_2020, read_only=True)["b01_02"]
    totals, counts = [], {}
    for r in ws.iter_rows(min_row=16, values_only=True):
        kind, area_name, symbol = r[0], r[2], r[3]
        if kind not in ("0", "1", "2", "3"):
            continue
        raw_code = str(area_name).split("_")[0].zfill(5)
        if raw_code == "13100":        # aggregate of the special wards
            continue
        code = collapse_wards(raw_code)
        pop = num(r[4], int)
        if pop <= 0:
            continue
        if str(symbol).startswith("DID00"):
            # The population and area totals are read at city level for a
            # designated city, so its ward rows are skipped here.
            if kind == "0" and code != raw_code:
                continue
            totals.append((code, str(area_name).split("_", 1)[-1], pop, num(r[11])))
        elif kind not in ("0", "1"):
            # Districts are listed individually only for units that are not
            # wards. For a designated city and for the special wards of Tokyo
            # the listing is per ward, and a district spanning two wards is
            # listed under both, so the count is left missing there rather than
            # reported wrong. The population and area totals are unaffected.
            counts[code] = counts.get(code, 0) + 1

    for code, name, pop, area in totals:
        yield {"year": "2020", "muni_code": code, "muni_name": name,
               "did_population": pop, "did_area_km2": area,
               "n_districts": counts.get(code, "")}
    print(f"2020: {len(totals)} municipalities with a district")


def main():
    seen = {}          # (year, district id) -> record, deduplicating polygon pieces
    duplicates = 0

    for yy in NATIONAL:
        path = fetch(f"{BASE}/A16-{yy}/A16-{yy}_GML.zip", f"A16-{yy}.zip")
        if path is None:
            print(f"FAILED national {yy}")
            continue
        n = 0
        for r in districts_from_zip(path):
            key = (str(num(r.get("A16_011"), int)), r.get("A16_001", ""))
            if key in seen:
                duplicates += 1
                continue
            seen[key] = r
            n += 1
        print(f"national {yy}: {n} districts")

    for yy in PER_PREF:
        n = 0
        missing = []
        for pp in PREFS:
            path = fetch(f"{BASE}/A16-{yy}/A16-{yy}_{pp}_GML.zip", f"A16-{yy}_{pp}.zip")
            if path is None:
                missing.append(pp)
                continue
            for r in districts_from_zip(path):
                key = (str(num(r.get("A16_011"), int)), r.get("A16_001", ""))
                if key in seen:
                    duplicates += 1
                    continue
                seen[key] = r
                n += 1
        print(f"year {yy}: {n} districts, missing prefectures {missing}")

    print(f"deduplicated {duplicates} repeated polygon pieces")

    agg = {}
    for (year, _), r in seen.items():
        raw_code = r.get("A16_002", "").strip()
        if not raw_code or not year:
            continue
        code = collapse_wards(raw_code)
        k = (year, code)
        if k not in agg:
            # A collapsed city would otherwise inherit the name of whichever
            # ward happened to be read first.
            agg[k] = {"year": year, "muni_code": code,
                      "muni_name": CITY_NAME.get(code, r.get("A16_003", "")),
                      "did_population": 0, "did_area_km2": 0.0, "n_districts": 0}
        agg[k]["did_population"] += num(r.get("A16_005"), int)
        agg[k]["did_area_km2"] += num(r.get("A16_006"))
        agg[k]["n_districts"] += 1

    for row in rows_2020():
        agg[(row["year"], row["muni_code"])] = row

    with open(OUT, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=["year", "muni_code", "muni_name",
                                          "did_population", "did_area_km2", "n_districts"])
        w.writeheader()
        for k in sorted(agg):
            row = dict(agg[k])
            row["did_area_km2"] = round(row["did_area_km2"], 2)
            w.writerow(row)

    print(f"wrote {OUT}: {len(agg)} municipality-year rows")
    print(f"{'year':6s} {'with district':>14s} {'>=50,000':>9s} {'>=10,000':>9s}")
    for y in sorted({k[0] for k in agg}):
        sub = [v for k, v in agg.items() if k[0] == y]
        print(f"{y:6s} {len(sub):14d} "
              f"{sum(1 for v in sub if v['did_population'] >= 50000):9d} "
              f"{sum(1 for v in sub if v['did_population'] >= 10000):9d}")


if __name__ == "__main__":
    main()

"""Municipality code crosswalks from 1 October 2015 to the 2020 and 2025 census dates.

The crosswalks for 1980 to 2010 are fetched from Municipality Map Maker by
fetch_crosswalks.py, whose coverage ends on 1 May 2019. The two later dates are
built here instead, from the 2015 side of the 2010 crosswalk, because what
happened after 2015 is small enough to write down and check.

Between 1 October 2015 and 1 October 2020 no municipalities merged, and two towns
were incorporated as cities, each taking a new code: Tomiya in Miyagi on 10
October 2016 and Nakagawa in Fukuoka on 1 October 2018. Every other code is
unchanged.

Between 1 October 2020 and 1 October 2025 nothing changed at all. Japan held 792
cities, 743 towns and 183 villages on both dates, so no municipality merged, none
was incorporated as a city and none was promoted from village to town; any of
those would move one of the three counts. A change of name or of a boundary
between two municipalities leaves the code alone. The 2025 crosswalk is therefore
the identity on the 2020 codes.

Source for the 2025 composition: <https://uub.jp/pjn/pn.html>.

Writes validation/data/crosswalk/codelist_20151001and20201001.csv and
validation/data/crosswalk/codelist_20151001and20251001.csv, in the layout and the
CP932 encoding of the fetched files, so that every crosswalk reads the same way.
"""
import csv
import os

HERE = os.path.dirname(os.path.abspath(__file__))
CROSSWALK_DIR = os.path.join(HERE, os.pardir, "data", "crosswalk")
BASE = os.path.join(CROSSWALK_DIR, "codelist_20101001and20151001.csv")
FIELDS = ["NO", "DATE1", "JISCODE1", "PNAME1", "GNAME1", "CNAME1",
          "WEIGHT", "DATE2", "JISCODE2", "PNAME2", "GNAME2", "CNAME2"]

# Incorporations as cities between the 2015 and the 2020 census dates, as
# (2015 code, 2020 code, 2020 gun name, 2020 municipality name).
INCORPORATIONS = [
    ("04423", "04216", "", "富谷市"),
    ("40305", "40231", "", "那珂川市"),
]


def read_2015_universe():
    """The municipalities in force on 1 October 2015, from the 2015 side of a crosswalk."""
    seen = {}
    with open(BASE, encoding="cp932", newline="") as f:
        for row in csv.DictReader(f):
            code = row["JISCODE2"]
            if code not in seen:
                seen[code] = (row["PNAME2"], row["GNAME2"], row["CNAME2"])
    return dict(sorted(seen.items()))


def write_crosswalk(path, target_date, changes):
    """One row per 2015 municipality, mapped onto the codes in force at target_date.

    changes maps a 2015 code to the (code, gun name, municipality name) it carries at
    the target date; every code absent from it is unchanged.
    """
    universe = read_2015_universe()
    with open(path, "w", encoding="cp932", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=FIELDS)
        writer.writeheader()
        for n, (code, (prefecture, gun, name)) in enumerate(universe.items(), start=1):
            target_code, target_gun, target_name = changes.get(code, (code, gun, name))
            writer.writerow({
                "NO": n,
                "DATE1": "20151001", "JISCODE1": code,
                "PNAME1": prefecture, "GNAME1": gun, "CNAME1": name,
                "WEIGHT": 1,
                "DATE2": target_date, "JISCODE2": target_code,
                "PNAME2": prefecture, "GNAME2": target_gun, "CNAME2": target_name,
            })
    print(f"{path}: {len(universe)} rows, {len(changes)} code changes")


if __name__ == "__main__":
    changes_2020 = {old: (new, gun, name) for old, new, gun, name in INCORPORATIONS}
    write_crosswalk(os.path.join(CROSSWALK_DIR, "codelist_20151001and20201001.csv"),
                    "20201001", changes_2020)
    write_crosswalk(os.path.join(CROSSWALK_DIR, "codelist_20151001and20251001.csv"),
                    "20251001", changes_2020)

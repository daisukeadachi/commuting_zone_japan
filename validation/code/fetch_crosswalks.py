"""Fetch municipality code crosswalks from Municipality Map Maker.

Each file maps the municipality codes in force at a census date onto the codes
in force on 1 October 2015, which is the harmonization base used throughout the
project. Settings match the crosswalks already held for other dates: the wards
of ordinance-designated cities are collapsed to the city while the special wards
of Tokyo stay separate, the layout is one pair per row, and the weights are
expressed with the earlier date as the base, so each weight is the share of that
municipality's area falling inside the 2015 unit.

Source: <http://tkirimura.com/mmm/>, which covers 1 January 1970 to 1 May 2019.
Cite Kirimura, Nakaya and Yano (2011), <https://doi.org/10.5638/thagis.19.139>.
"""
import json
import os
import time
import urllib.parse
import urllib.request

BASE = "http://tkirimura.com/mmm"
HERE = os.path.dirname(os.path.abspath(__file__))
OUTDIR = os.path.join(HERE, os.pardir, "data", "crosswalk")

# 2015 is the base, so it needs no crosswalk. 2020 lies beyond the tool's
# coverage and is handled by hand: no municipalities merged between the two
# censuses, and the only code changes are two towns incorporated as cities.
YEARS = [1980, 1985, 1990, 1995, 2000, 2005, 2010]

DESIGNATED_CITY_AS_ONE_TOKYO_WARDS_SEPARATE = 2
ONE_ROW_PER_PAIR = 0
EARLIER_DATE_AS_BASE = 0


def fetch_one(year):
    out = os.path.join(OUTDIR, f"codelist_{year}1001and20151001.csv")
    if os.path.exists(out) and os.path.getsize(out) > 0:
        print(f"{year}: already present")
        return out
    params = urllib.parse.urlencode({
        "y1": year, "m1": 10, "d1": 1,
        "y2": 2015, "m2": 10, "d2": 1,
        "s": DESIGNATED_CITY_AS_ONE_TOKYO_WARDS_SEPARATE,
        "l": ONE_ROW_PER_PAIR,
        "b": EARLIER_DATE_AS_BASE,
        "t": int(time.time() * 1000),
    }).encode()
    req = urllib.request.Request(
        f"{BASE}/citycodelist.php", data=params,
        headers={"content-type": "application/x-www-form-urlencoded;charset=UTF-8"})
    with urllib.request.urlopen(req, timeout=180) as r:
        answer = json.loads(r.read().decode())
    if str(answer.get("result")) != "1":
        raise RuntimeError(f"{year}: generation failed, {answer}")
    url = f"{BASE}/{answer['url'].lstrip('./')}"
    with urllib.request.urlopen(url, timeout=180) as r:
        body = r.read()
    os.makedirs(OUTDIR, exist_ok=True)
    with open(out, "wb") as f:
        f.write(body)
    print(f"{year}: {len(body)} bytes")
    return out


if __name__ == "__main__":
    for y in YEARS:
        fetch_one(y)
        time.sleep(2)

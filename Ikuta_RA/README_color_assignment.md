# 色分け（Color Assignment）の詳細

このドキュメントは、通勤圏（CZ）と都市雇用圏（UEA）の地図作成における色分けのアルゴリズムと、各コードが生成する地図について詳しく説明します。

## 概要

色分けは、隣接するポリゴンに異なる色を割り当てる**グラフ着色問題**です。目的は：

1.  隣接する通勤圏/都市雇用圏に異なる色を割り当てる
2.  年を跨いで同じ構成の地域は同じ色を使用する（色の再利用）
3.  特定地域（例：東京）の色を固定する
4.  視認性を高めるため、最小限の色数で着色する

## グラフ着色アルゴリズムの詳細

### 問題の定義

**入力：** - ポリゴン集合（各ポリゴンはJISCODEでラベル付け） - グループ情報（各ポリゴンをグループに割り当て） - 色パレット

**出力：** - グループごとの色割り当て

**制約：** - 隣接するポリゴンは異なる色を持つ必要がある - 必要な色数を最小化する（4色定理により、平面グラフは最大4色で着色可能）

### アルゴリズムの流れ

```         
┌─────────────────────────────────────────────────────────┐
│ 1. メンバーシップシグネチャの計算                        │
│    各グループの構成市町村をソートして結合                │
└──────────────┬──────────────────────────────────────────┘
               ↓
┌─────────────────────────────────────────────────────────┐
│ 2. ポリゴンの統合と隣接関係の計算                        │
│    同じグループのポリゴンをマージ                        │
│    → spdep::poly2nb()で隣接リスト作成                    │
└──────────────┬──────────────────────────────────────────┘
               ↓
┌─────────────────────────────────────────────────────────┐
│ 3. 前年の色を適用（Color Reuse）                         │
│    - シグネチャが一致するグループ → 前年の色を検討      │
│    - 隣接チェック付きで衝突回避                         │
└──────────────┬──────────────────────────────────────────┘
               ↓
┌─────────────────────────────────────────────────────────┐
│ 4. 固定色を適用                                         │
│    特定グループ（東京など）に色を強制                  │
│    この色は他のグループに使用されない                 │
└──────────────┬──────────────────────────────────────────┘
               ↓
┌─────────────────────────────────────────────────────────┐
│ 5. Welsh-Powell法で残りを着色                           │
│    度の高い順にグループを処理                           │
│    各グループに隣接グループで未使用の色を割り当て      │
└──────────────┬──────────────────────────────────────────┘
               ↓
┌─────────────────────────────────────────────────────────┐
│ 6. 検証：隣接グループの色が異なるか確認                │
│    衝突があればエラーを投げる                           │
└─────────────────────────────────────────────────────────┘
```

## コア関数：`assign_group_colors()`

### 関数シグネチャ

``` r
assign_group_colors(
  sf_obj,              # sf オブジェクト（ポリゴン）
  group_col,           # グループ列名（"cluster"または"UEA"）
  colors = RColorBrewer::brewer.pal(5, "Set2"),  # 色パレット
  fixed = NULL,        # 固定する色 list(value = <グループID>, color = <16進色コード>)
  prev_colors = NULL   # 前年の色情報（シグネチャ→色マッピング）
)
```

### 処理の詳細

#### 1. メンバーシップシグネチャの計算

**目的：** 年を跨いで同じ構成のグループを識別

``` r
# 各グループに属するJISCODEを取得・ソート・結合
signatures <- vapply(group_vals, function(g) {
  mem <- grp$JISCODE[grp[[group_col]] == g]
  mem <- sort(unique(as.character(mem)))
  paste(mem, collapse = ",")
}, character(1))
```

**例：**

```         
グループ50：市町村 [13102, 13101, 13103] で構成
↓ 処理
signature = "13101,13102,13103"

10年後にグループ75が同じ市町村で構成された場合
→ signature = "13101,13102,13103" で一致
→ 前年と同じ色を使用可能
```

#### 2. ポリゴンの統合と隣接関係の計算

``` r
# グループごとにポリゴンを統合
grp <- dplyr::group_by_at(grp, group_col)
grp <- dplyr::summarise(grp)  # geomを統合
grp <- sf::st_make_valid(grp) # 無効なジオメトリを修正

# 隣接関係を計算
neighbors <- spdep::poly2nb(grp)
# neighbors[[i]] = グループi に隣接するグループのインデックスリスト
```

**重要：** 統合後のポリゴンで隣接関係を計算することで、複数の市町村で構成されるグループの隣接判定が正確になります。

#### 3. 前年の色を適用（隣接チェック付き）

**新機能：** 隣接グループとの衝突をチェックしてから前年の色を適用

``` r
if (!is.null(prev_colors) && !is.null(grp_signature)) {
  # シグネチャが前年に存在するグループを検出
  matched <- which(grp_signature %in% names(prev_map))
  
  for (idx in matched) {
    proposed_color <- prev_map[grp_signature[idx]]
    neighbor_colors <- color_assignment[neighbors[[idx]]]
    neighbor_colors <- neighbor_colors[!is.na(neighbor_colors)]
    
    # 隣接グループと衝突しない場合のみ色を適用
    if (!proposed_color %in% neighbor_colors) {
      color_assignment[idx] <- proposed_color
    }
    # 衝突する場合はスキップ → Welsh-Powell法で後に処理
  }
}
```

**効果：** - 同じシグネチャのグループが隣接していても異なる色が割り当てられる - 衝突エラーが大幅に減少

#### 4. 固定色の適用

``` r
if (!is.null(fixed) && !is.null(fixed$value) && !is.null(fixed$color)) {
  idx <- which(grp[[group_col]] == fixed$value)
  if (length(idx) > 0) {
    color_assignment[idx] <- fixed$color
  }
}
```

**用途：** 東京都市圏（13100）など重要な地域の色を固定

#### 5. Welsh-Powell法による色割り当て

**アルゴリズム：**

```         
1. 各グループの度（隣接グループ数）を計算
2. 度の大きい順にソート
3. ソート順にグループを処理：
   a. 隣接グループで使用されている色を取得
   b. パレット内で未使用の色を探す
   c. 見つからない場合、パレットを拡張
```

**コード：**

``` r
deg <- vapply(neighbors, length, integer(1))
order_idx <- order(deg, decreasing = TRUE)  # 度が大きい順

for (v in order_idx) {
  if (!is.na(color_assignment[v])) next  # 既に割り当て済みならスキップ
  
  nbcols <- color_assignment[neighbors[[v]]]
  nbcols <- nbcols[!is.na(nbcols)]  # NA を除去
  
  # 固定色も除外
  available <- setdiff(palette, c(nbcols, fixed_color))
  
  # パレット拡張
  while (length(available) == 0) {
    new_n <- max(4, length(palette) * 2)
    palette <- c(palette, 
                 grDevices::colorRampPalette(palette)(new_n)[(length(palette) + 1):new_n])
    available <- setdiff(palette, c(nbcols, fixed_color))
  }
  
  color_assignment[v] <- available[1]  # 最初の利用可能な色を使用
}
```

**最適性：** - 度の高い順に処理することで、パレットサイズを最小化 - 平面グラフの場合、理論値（4色以下）に近い結果を得やすい

#### 6. 検証

``` r
conflicts <- character(0)
for (i in seq_along(neighbors)) {
  for (j in neighbors[[i]]) {
    if (i < j &&
        !is.na(color_assignment[i]) && 
        !is.na(color_assignment[j]) &&
        color_assignment[i] == color_assignment[j]) {
      conflicts <- c(conflicts, 
                     paste0(grp[[group_col]][i], "-", 
                            grp[[group_col]][j], ":", 
                            color_assignment[i]))
    }
  }
}
if (length(conflicts) > 0) {
  stop("Coloring conflict detected between adjacent groups: ", 
       paste(conflicts, collapse = "; "))
}
```

## 実装の詳細

### 固定色の予約（Fixed Color Reservation）

`fixed`パラメータで指定した色は、他のグループには割り当てられません：

``` r
fixed_color <- NULL
if (!is.null(fixed) && !is.null(fixed$color)) {
  fixed_color <- fixed$color
  # 色の選択時に固定色を除外
  available <- setdiff(palette, c(nbcols, fixed_color))
}
```

**用途：** 東京都市圏（13100）など、重要な地域の色を一貫性を保つ

### 色の永続化（Color Map Persistence）

`save_color_map()`と`load_color_map()`で、シグネチャ→色マッピングをCSVで保存・読み込み：

-   **ディレクトリ：** `output/color_map/`
-   **ファイル形式：** `{kind}_signature_color.csv`
    -   例：`CZ_signature_color.csv`, `UEA_signature_color.csv`
-   **カラム：** `signature`, `color`

``` r
# 1980年の色情報を読み込み
prev_CZ_map <- load_color_map("CZ", dir = "output/color_map")

# 2015年で新しい色を割り当てた後、保存
save_color_map(CZ_color %>% select(signature, color), "CZ", dir = "output/color_map")
```

## 地図生成コードの詳細

### 1. `CZandUEA.R` - 基本的なCZ/UEA比較地図

**目的：** 関東地方のCZとUEAを並べて表示し、年による変化を比較

**処理内容：**

``` r
for (y in c(1980, 2015)) {
  # UEAデータを読み込み
  UEA <- bind_rows(McEA_center, McEA_sub1, McEA_sub2, McEA_sub3,
                   MEA_center, MEA_sub1, MEA_sub2, MEA_sub3)
  
  # CZデータを読み込み
  CZ <- read_csv(paste0("output/", y, "_harmonized.csv"))
  
  # 色分け
  UEA_color <- assign_group_colors(UEA.sf, "UEA", 
                                   fixed = list(value = 13100, color = 固定色),
                                   prev_colors = prev_UEA_map)
  
  CZ_color <- assign_group_colors(CZ.sf, "cluster",
                                  fixed = list(value = Gohunai, color = 固定色),
                                  prev_colors = prev_CZ_map)
  
  # 出力
  CZmap + UEAmap → output/map_image/CZandUEA/{y}_UEAandCZmap_eng.png
}
```

- 出力ファイル:`output/map_image/CZandUEA/1980_UEAandCZmap_eng.png` - `output/map_image/CZandUEA/2015_UEAandCZmap_eng.png`
  - 東京都市圏の色を固定（全年で統一）
  - UEAとCZの同年での比較が可能

### 2. `TimeSeriesCZ_kanto_multi.R` - 時系列CZ地図（関東ズーム）

**目的：** 1980年から2020年の5年ごとのCZ変化を1つのpng（複数パネル）に表示

**処理内容：**

``` r
for (y in seq(1980, 2020, 5)) {
  # CZを読み込み・色分け
  CZ_color <- assign_group_colors(CZ.sf, "cluster",
                                  colors = c(Set2, "#377EB8"),  # 6色パレット
                                  fixed = list(value = Gohunai, color = 固定色),
                                  prev_colors = prev_CZ_map)
  
  # 関東地方にズーム
  # lim_y = c(34.7, 37.1), lim_x = c(138, 140.9)
  
  # 前年の色情報を保存
  prev_CZ_map <- update
}

# 全9枚のパネルをpatchworkで結合
output/map_image/ts_CZ/1980to2020_kanto_harmonized_CZmap_eng.png
```

- 出力ファイル：`output/map_image/ts_CZ/1980to2020_kanto_harmonized_CZmap_eng.png` 
  - 3行3列の9パネル（1980, 1985, 1990, ..., 2020）
  - 関東地域のみ表示


### 3. `CZ_2015.R` - 2015年CZ地図（複数ズームレベル）

**目的：** 2015年のCZを全国規模と関東ズーム、異なるビューで表示

**処理内容：**

``` r
# 2015年の単一年度のデータを処理
CZ_color <- assign_group_colors(CZ.sf, "cluster",
                                colors = Set2,
                                fixed = list(value = Gohunai, color = 固定色),
                                prev_colors = prev_CZ_map)

# 複数のビューを生成：
1. enlarge:  全国表示（北海道は北西に移動）
2. kanto:    関東地方ズーム
```

- 出力ファイル:
  - `output/map_image/CZ2015/2015_CZmap_enlarge.png` - 全国 
  - `output/map_image/CZ2015/2015_CZmap_kanto.png` - 関東ズーム

### 4. `tree-height_harmonized.R` / `tree-height_original.R` - 複数クラスタリング結果の地図

**目的：** 異なるクラスタリング結果（異なるツリー高さ）によるCZの比較

**処理内容：**

``` r
# output/clustered/harmonized/ や output/clustered/original/ 内の
# 複数のCSVファイル（異なるツリー高さのクラスタリング結果）を読み込み

for (each_clustering_result in czlist) {
  CZ_color <- assign_group_colors(CZ.sf, "cluster",
                                  fixed = list(value = Gohunai, color = 固定色))
  
  # 各結果を個別に出力
  output/map_image/tree-height/harmonized/{parameter}/{filename}.png
}
```

**出力ディレクトリ構造：**

```         
output/map_image/tree-height/
├── harmonized/
│   ├── 0.5/
│   │   ├── {result1}.png
│   │   └── {result2}.png
│   └── 0.8/
│       ├── {result1}.png
│       └── {result2}.png
└── original/
    └── ... (同様の構造)
```

**特徴：** - 異なるパラメータでのクラスタリング結果を視覚的に比較 - 最適なツリー高さを決定するのに有用

### 5. `tree-height_harmonized_kanto.R` / `tree-height_original_kanto.R` - 複数結果の関東ズーム版

**目的：** tree-height\_\*.Rと同じ処理を関東地域に限定して表示

**差異：** - `coord_sf(ylim = c(34.7, 37.1), xlim = c(138, 140.9))` で関東にズーム - より詳細な比較が可能

### 6. `withRail.R` - 鉄道インフラ付きCZ地図

**目的：** CZ地図に新幹線・在来線を重ねて表示

**処理内容：**

``` r
# 鉄道データを読み込み
Rail.row <- read_sf("data/N05-23_GML/N05-23_RailroadSection2.shp")

# 新幹線と在来線を分離
HSR <- Rail.row %>% filter(str_detect(N05_002, "新幹線"))
Rail <- Rail.row %>% filter(str_detect(N05_002, "新幹線", negate = TRUE))

# CZ地図に鉄道を重ねる
CZ.sf %>% ggplot() +
  geom_sf(aes(fill = color)) +
  geom_sf(data = HSR, color = "#333333", linetype = "dashed") +
  geom_sf(data = Rail, color = "black")
```

- 出力ファイル：
  - `output/map_image/withRail/2015_kanto_CZwithRailmap_eng.png` - 関東
  - `output/map_image/withRail/2015_kinki_CZwithRailmap_eng.png` - 近畿
  - `output/map_image/withRail/2015_nagoya_CZwithRailmap_eng.png` - 名古屋
  - `output/map_image/withRail/2015_whole_CZwithRailmap_eng.png` - 全国

    - 新幹線（破線）
    - 在来線（実線）

## 使用パラメータの推奨値

| パラメータ | 用途 | 推奨値 |
|-------------------------------|------------------|-----------------------|
| `colors` | 基本パレット | `RColorBrewer::brewer.pal(5, "Set2")` |
|  | 多くのグループが必要 | `c(RColorBrewer::brewer.pal(5, "Set2"), "#377EB8")` |
| `fixed$value` | 東京都市圏の中心市町村 | `13100` |
| `fixed$color` | 東京の固定色 | `RColorBrewer::brewer.pal(6, "Set2")[6]` |
| `linewidth` | 市町村境界線の幅 | `0.05 ~ 0.1` |


## トラブルシューティング

### 備考: 2015年時点の地図について
- 福島第一原子力発電所の事故の影響で、福島県沿岸部の一部市町村はCZを構成するためのデータが欠けている。
  - 該当地域はグレー(`#a9a9a9`)で塗っている。

### エラー：「Coloring conflict detected between adjacent groups」

隣接するグループに同じ色が割り当てられた場合に発生します。

**原因：** 1. パレットサイズが小さすぎてグラフが着色不可能 2. 隣接チェック付き`prev_colors`の適用時に処理順序の問題

**診断方法：**

``` r
# エラーメッセージから衝突ペアを確認
# "Coloring conflict: 75-79:#66C2A5; 79-80:#66C2A5"
# グループ75と79が同じ色、グループ79と80が同じ色
```

**解決方法：**

1.  **パレットサイズを増やす**

``` r
colors <- c(RColorBrewer::brewer.pal(8, "Set2"),
            RColorBrewer::brewer.pal(8, "Set3"))
```

2.  **前年の色情報をリセット**

``` r
# output/color_map/CZ_signature_color.csv を削除
# 新規に割り当てた色で再実行
```

3.  **固定色の指定を解除（テスト用）**

``` r
# 一時的に fixed = NULL とする
CZ_color <- assign_group_colors(CZ.sf, "cluster", 
                                colors = colors,
                                prev_colors = prev_CZ_map)
# 衝突がないか確認後、固定色を復活
```

### 問題：色が毎年変わってしまう

色の永続化がうまくいっていない可能性があります。

**原因：** 1. `output/color_map/` ディレクトリが存在しない 2. `load_color_map()`が失敗している 3. `prev_colors` が正しく渡されていない

**診断方法：**

``` r
# 色マップが読み込まれているか確認
prev_CZ_map <- load_color_map("CZ", dir = "output/color_map")
print(prev_CZ_map)  # NULL ではなくデータフレームが返されるか確認
```

**解決方法：**

``` r
# 色マップディレクトリを手動作成
dir.create("output/color_map", recursive = TRUE, showWarnings = FALSE)

# 前年の結果から色マップを手動エクスポート
prev_CZ_map <- CZ_color_previous_year %>% select(signature, color)
save_color_map(prev_CZ_map, "CZ", dir = "output/color_map")
```


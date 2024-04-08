# 都道府県版ジェンダー・ギャップ指数(2024年版)

ダウンロードは[こちら](https://github.com/kyodo-official/gender-gap-index/archive/refs/heads/master.zip)

> [!CAUTION]
>行政指標の「都道府県庁の大卒程度採用の男女比」に関し、島根県に大きな報告ミスがあり、内閣府が24年4月4日付で調査報告書サイトを修正した。
>
>このため、24年の行政分野は同日時点の掲載データで統計処理をやり直し、3月8日に新聞等で公表した結果とは異なっている。 

内閣府などの統計から計30指標を選出し、4分野で各都道府県の女性の地位を分析した指数。スイスのシンクタンク、世界経済フォーラム(WEF)が毎年公表するジェンダー・ギャップ指数と同様の手法で統計処理した。指数は1に近いほど平等であることを示す。

項目の選定については三浦まり・上智大教授(ジェンダー政治論)が、統計処理については竹内明香・上智大准教授(経済統計学)が担当した。

https://digital.kyodonews.jp/gender2024/

```
gender-gap-index/
│
├ README.md .. この説明
├ README_2022.md .. 2022年版の都道府県版ジェンダーギャップ指数に関する説明
├ gender-gap-index.R .. 算出スクリプト
│
├ data/2022/
├ data/2023/
├ data/2024_0308/ .. 3月8日に新聞等で公表したデータ(旧)
├ data/2024_0404/ .. 項目ごとの男女比データ(最新版)
│　├ 政治/
│　│　└ ○○.csv
│　├ 行政/
│　│　└ ○○.csv
│　├ 教育/
│　│　└ ○○.csv
│　└ 経済/
│　 　└ ○○.csv
├ data/JIS_X_0401.csv .. 都道府県コード(総務省)
│
├ result/2022/
├ result/2023/
├ result/2024_0308/ .. 3月8日に新聞等で公表した算出結果(旧)
└ result/2023_0404/ .. 算出結果(最新版)
　　├ index.csv .. 都道府県版ジェンダーギャップ指数
　　├ rankings.csv .. 指数のランキング
　　├ ratio_○○.csv .. 各分野ごとの男女比
　　└ rankings_○○.csv .. 各分野ごとのランキング
```


## 算出手順

本指数は、世界経済フォーラムによるジェンダーギャップ指数の算出方法に基本的に従っている。計算手順は大きく2段階となり、1段階目に30項目の男女比を算出し、2段階目に各分野ごとの加重平均を算出する。

ただし教育分野に関しては、学生関連の項目と教育業界就業者関連の項目のウェイトを50%ずつにするため、2段階目で「学生関連」「教育業界就業者」それぞれの加重平均を求め、3段階目にそれら2項目の加重平均を求める。

以下では、計算手順の詳細を示す。

### 1段階目

項目ごとの男女比を算出する。男性値と女性値を![f1](./img/f1.svg)とする。ここで![f2](./img/f2.svg)は都道府県を表す。まず、比率をとり

![f3](./img/f3.svg)

項目によっては![f4](./img/f4.svg)とする。この比率が1である場合、平等である。逆に1から離れるほど、不平等となる。項目によっては、一部の都道府県で1以上の値をとるものがある。この1以上の値は、1という平等状態よりも良い状態を表すとはいえないため1以上の値を1に変換する。

![f5](./img/f5.svg)

この変換した男女比![f6](./img/f6.svg)を項目ごとの男女比とする。

2段階目の加重平均で標準偏差を使用するため、平均、分散、標準偏差を

![f7](./img/f7.svg)

とする。この平均、分散、標準偏差の算出で世界経済フォーラムでは人口比によるウェイトを使用しているが、本指数では、都道府県の扱いを平等とするため、人口比は用いなかった。

### 2段階目・3段階目(教育分野のみ)
各分野ごとに男女比の加重平均を算出する。例として、行政分野の9項目の男女比を用いた加重平均を説明する。男女比![f6](./img/f6.svg)と標準偏差![f8](./img/f8.svg)に項目を表す![f9](./img/f9.svg)を追加し、![f10](./img/f10.svg)とする。ウェイトは、標準偏差の逆数から次の値

![f11](./img/f11.svg)

を使用する。ウェイト![f12](./img/f12.svg)は、項目ごとに一つの値が求まり、9項目であれば、9つのウェイトが算出される。行政分野の指数は、9項目の加重平均として以下の式で算出した。

![f13](./img/f13.svg)


## データ出典

省庁などの統計は2024年1月9日時点の公表データを使用した。ただし島根県の数値の大幅修正があった都道府県庁の大卒程度採用の男女比は、4月4日時点の公表データを使用した。

### 政治分野

- 歴代知事の在職年数の男女比 (2024年1月1日時点)
    - [全国知事会「知事ファイル」](http://www.nga.gr.jp/app/chijifile/)
        - 母数の年数として、沖縄は51年、そのほかの都道府県は76年を用いている
        - 男性の在職年数は、母数の年数から女性在職年数を引いて算出した

- 衆参両院選挙区選出議員の男女比(2024年1月1日時点)
    - 共同通信選挙データベース
        - 参院の合区選出議員は、すべて自県選出議員として扱う 

- 都道府県議会の男女比(2023年12月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算
        
- 女性ゼロ議会(2023年6月1日時点)
    - 市川房枝記念女性と政治センター、京都女子大の協力による
        - 女性ゼロ議会数については、女性ゼロの議会数を市町村ごとにカウントし、女性が在職している議会数を女性値とした。男性値は、男性が在職する議会は全議会なので、市町村の議会数を使用している
        - 本データは女性議員の人数ではなく、議会数であることに注意されたい

- 市区町村議会の男女比(2023年12月31日時点)
    - [総務省「地方公共団体の議会の議員及び長の所属党派別人員調等」](https://www.soumu.go.jp/senkyo/senkyo_s/data/ninki/touhabetsu.html)

- 市区町村長の男女比(2024年1月1日時点)
    - 全国市長会、全国町村会への取材による

### 行政分野

- 副知事の男女比(2024年1月1日時点)
    - 共同通信調べ

- 都道府県の管理職の男女比(2023年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 他指標の内数である教育委員管理職を控除した
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算

- 都道府県の審議会の男女比(2023年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 他指標の内数である都道府県防災会議委員を控除した
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算

- 都道府県防災会議の男女比(2023年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算

- 地方自治法180条の5に基づく委員会の男女比(2023年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 選挙管理委員会、人事委員会、公安委員会など
        - 他指標の内数である教育委員を控除した
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算

- 都道府県庁採用(大卒程度)の男女比(2023年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算
        - 本指標のみ2023年3月1日時点の公表データを使用した。1月5日時点で公表されていた福岡県の数値が申告ミスによるものであることが判明し、国に対しても修正申告済みであることから、本データは既に修正されていたものとみなした

- 都道府県職員の育休取得率における男女格差(2022年度)
    - [総務省「地方公共団体の勤務条件調査」](https://www.e-stat.go.jp/stat-search/files?tclass=000001212700&cycle=0)
        - ![f4](./img/f4.svg) で計算している

- 市区町村の管理職の男女比(2023年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算

- 市区町村の審議会の男女比(2023年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 他指標の内数である防災会議総委員数を控除した
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算

- 市区町村の防災会議の男女比(2023年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算

### 教育分野

- 大学進学率の男女差(2023年5月1日時点)
    - [文部科学省「学校基本調査」](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00400001&tstat=000001011528)
        - 大学進学率は、大学進学者数/中学等卒業者数で算出した
        - 「中学等」は中学校+義務教育学校+特別支援学校中学部+中等教育学校前期課程

- 小学校の校長の男女比(2023年5月1日時点)
    - [文部科学省「学校基本調査」](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00400001&tstat=000001011528)

- 中学校の校長の男女比(2023年5月1日時点)
    - [文部科学省「学校基本調査」](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00400001&tstat=000001011528)

- 高校の校長の男女比(2023年5月1日時点)
    - [文部科学省「学校基本調査」](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00400001&tstat=000001011528)

- 小中高校の副校長・教頭の男女比(2023年5月1日時点)
    - [文部科学省「学校基本調査」](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00400001&tstat=000001011528)

- 都道府県教育委員会委員の男女比(2023年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算

- 都道府県教育委員会事務局の管理職における男女比(2023年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 出典元では「総数」「うち女性数」となっているため、総数―女性数＝男性数として便宜的に計算

 ### 経済分野
- 就業率の男女差(2022年10月1日時点)
    - [総務省「就業構造基本調査」](https://www.stat.go.jp/data/shugyou/2022/index.html)

- フルタイムの仕事に従事する割合の男女比(2022年7月)
    - [厚生労働省「賃金構造基本統計調査」](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00450091&tstat=000001011429)

- 家事・育児・介護・看護・買い物に使用する時間の男女格差(2022年10月)
    - [総務省「社会生活基本調査」](https://www.e-stat.go.jp/stat-search/database?page=1&toukei=00200533&tstat=000001158160)
        - ![f4](./img/f4.svg) で計算している

- 社長数の男女比(2023年)
    - [東京商工リサーチ「全国女性社長調査」](https://www.tdb.co.jp/report/watching/press/p231111.html)

- 企業や法人の役員・管理職の男女比(2020年10月)
    - [総務省「国勢調査」](https://www.e-stat.go.jp/stat-search/database?page=1&layout=datalist&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136467&cycle_facet=cycle&tclass2val=0&metadata=1&data=1)

- 農協役員の男女比(農協2023年7月、漁協2022年3月31日)
    - [JA全中「JA都道府県⼥性役員等調査」](https://women.ja-group.jp/about-us/gender-equality/)
    - [農林水産省「水産業協同組合統計表」](https://www.maff.go.jp/j/tokei/kouhyou/suisan_kumiai_toukei/)

## 算出結果の再現

共同通信社による[R言語](https://cran.r-project.org/)スクリプト(gender-gap-index.R)を実行することで、本指数の算出結果を再現できる。

本スクリプトはR >= 4.1.0でのみ動作する。

# 都道府県版ジェンダー・ギャップ指数

内閣府などの統計から計28指標を選出し、4分野で各都道府県の女性の地位を分析した指数。スイスのシンクタンク、世界経済フォーラム(WEF)が毎年公表するジェンダー・ギャップ指数と同様の手法で統計処理した。指数は1に近いほど平等であることを示す。

項目の選定については三浦まり・上智大教授(ジェンダー政治論)が、統計処理については竹内明香・上智大准教授(経済統計学)が担当した。

https://digital.kyodonews.jp/gender2022/

```
gender-gap-index/
│
├ README.md .. この説明
├ gender-gap-index.R .. 算出スクリプト
│
├ data/2022/ .. 項目ごとの男女比データ
│　├ 政治/
│　│　└ ○○.csv
│　├ 行政/
│　│　└ ○○.csv
│　├ 教育/
│　│　└ ○○.csv
│　├ 経済/
│　│　└ ○○.csv
│　└ JIS_X_0401.csv .. 都道府県コード(総務省)
│
└ result/2022/ .. 算出結果
　　├ index.csv .. 都道府県版ジェンダーギャップ指数
　　├ rankings.csv .. 指数のランキング
　　└ ratio_○○.csv .. 各分野各項目ごとの男女比
```


## 算出手順

本指数は、世界経済フォーラムによるジェンダーギャップ指数の算出方法に基本的に従っている。計算手順は大きく2段階となり、1段階目に28項目の男女比を算出し、2段階目に4分野ごとの加重平均を算出する。以下では、計算手順の詳細を示す。

### 1段階目

項目ごとの男女比を算出する。男性値と女性値を![f1](./img/f1.svg)とする。ここで![f2](./img/f2.svg)は都道府県を表す。まず、比率をとり

![f3](./img/f3.svg)

項目によっては![f4](./img/f4.svg)とする。この比率が1である場合、平等である。逆に1から離れるほど、不平等となる。項目によっては、一部の都道府県で1以上の値をとるものがある。この1以上の値は、1という平等状態よりも良い状態を表すとはいえないため1以上の値を1に変換する。

![f5](./img/f5.svg)

この変換した男女比![f6](./img/f6.svg)を項目ごとの男女比とする。

2段階目の加重平均で標準偏差を使用するため、平均、分散、標準偏差を

![f7](./img/f7.svg)

とする。この平均、分散、標準偏差の算出で世界経済フォーラムでは人口比によるウェイトを使用しているが、本指数では、都道府県の扱いを平等とするため、人口比は用いなかった。

### 2段階目
4分野ごとに男女比の加重平均を算出する。例として、行政分野の9項目の男女比を用いた加重平均を説明する。男女比![f6](./img/f6.svg)と標準偏差![f8](./img/f8.svg)に項目を表す![f9](./img/f9.svg)を追加し、![f10](./img/f10.svg)とする。ウェイトは、標準偏差の逆数から次の値

![f11](./img/f11.svg)

を使用する。ウェイト![f12](./img/f12.svg)は、項目ごとに一つの値が求まり、9項目であれば、9つのウェイトが算出される。行政分野の指数は、9項目の加重平均として以下の式で算出した。

![f13](./img/f13.svg)


## データ出典（2022年版）

### 政治分野

- 衆参両院選挙区選出議員の男女比(2022年1月1日時点)
    - 共同通信選挙データベース
        - 参院の合区選出議員は、すべて自県選出議員として扱う 
- 都道府県議会の男女比(2021年8月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
- 市区町村議会の男女比(2020年12月31日時点)
    - [総務省「党派別人員調べ」](https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200231&tstat=000001154346&cycle=0&year=20201&tclass1val=0)
- 女性ゼロ議会(2020年12月31日時点)
    - [総務省「党派別人員調べ」](https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200231&tstat=000001154346&cycle=0&year=20201&tclass1val=0)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 女性ゼロ議会数については、女性ゼロの議会数を市町村ごとにカウントし、女性が在職している議会数を女性値とした。男性値は、男性が在職する議会は全議会なので、市町村の議会数を使用している
        - 本データは女性議員の人数ではなく、議会数であることに注意されたい
- 歴代知事の在職年数の男女比 (2022年1月1日時点)
    - [全国知事会「知事ファイル」](http://www.nga.gr.jp/app/chijifile/)
        - 母数の年数として、沖縄は49年、そのほかの都道府県は74年を用いている
        - 男性の在職年数は、母数の年数から女性在職年数をひいて算出した
- 市区町村長の男女比(2022年1月1日時点)
    - 全国市長会、全国町村会への取材による

### 行政分野

- 都道府県の管理職の男女比(2021年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 内数である教育委員管理職を除いたものに置き換えた
- 都道府県の審議会の男女比(2021年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 内数である都道府県防災会議委員総数を引いたものを、都道府県審議会委員延総委員等数として使用した
- 都道府県防災会議の男女比(2021年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
- 地方自治法180条の5に基づく委員会の男女比(2021年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 内数の調整をおこない教育委員総数を引いた値に置き換えている
- 都道府県庁採用(大卒程度)の男女比(2021年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
- 都道府県職員の育休取得率における男女格差(2020年度)
    - [総務省「地方公共団体の勤務条件調査」](https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200213&tstat=000001161808&cycle=0&stat_infid=000032159550&tclass1val=0)
        - ![f4](./img/f4.svg) で計算している
- 市区町村の管理職の男女比(2021年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
- 市区町村の審議会の男女比(2021年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
        - 内数である市町村防災会議総委員数を引いたものに置き換えている
- 市区町村の防災会議の男女比(2021年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)

### 教育分野
- 大学進学率の男女格差(2021年5月1日時点)
    - [文部科学省「学校基本調査」](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00400001&tstat=000001011528)
        - 大学進学率は、大学進学者数/中学等卒業者数で算出した
        - 「中学等」は中学校+義務教育学校+特別支援学校中学部+中等教育学校前期課程
- 小学校の校長の男女比(2021年5月1日時点)
    - [文部科学省「学校基本調査」](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00400001&tstat=000001011528)
- 中学・高校の校長の男女比(2021年5月1日時点)
    - [文部科学省「学校基本調査」](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00400001&tstat=000001011528)
- 小中高校の副校長・教頭の男女比(2021年5月1日時点)
    - [文部科学省「学校基本調査」](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00400001&tstat=000001011528)
- 都道府県教育委員会委員の男女比(2021年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)
- 都道府県教育委員会事務局の管理職における男女比(2021年4月1日時点)
    - [内閣府男女共同参画局](https://www.gender.go.jp/policy/mieruka/government.html)

### 経済分野
- フルタイムの仕事に従事する男女間の賃金格差(2020年7月)
    - [厚生労働省「賃金構造基本統計調査」](https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00450091&tstat=000001011429&cycle=0&tclass1=000001152186&tclass2=000001152187&tclass3=000001152197&tclass4val=0)
- フルタイム以外の仕事に従事する男女間の賃金格差(2020年7月)
    - [厚生労働省「賃金構造基本統計調査」](https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00450091&tstat=000001011429&cycle=0&tclass1=000001152186&tclass2=000001152187&tclass3=000001152197&tclass4val=0)
- フルタイムの仕事に従事する割合の男女比(2020年7月)
    - [厚生労働省「賃金構造基本統計調査」](https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00450091&tstat=000001011429&cycle=0&tclass1=000001152186&tclass2=000001152187&tclass3=000001152197&tclass4val=0)
        - フルタイムの仕事に従事する労働者数 / すべての労働者数で算出した
- 共働き家庭の家事・育児等に使用する時間の男女格差(2016年10月)
    - [総務省「社会生活基本調査」](https://www.e-stat.go.jp/stat-search/database?page=1&layout=datalist&toukei=00200533&tstat=000001095335&cycle=0&tclass1=000001095377&tclass2=000001095393&tclass3=000001095396&tclass4val=0)
        - ![f4](./img/f4.svg) で計算している
- 社長数の男女比(2021年7月)
    - [東京商工リサーチ「全国女性社長調査」](https://www.tsr-net.co.jp/news/analysis/20211102_04.html)
- 企業や法人の役員・管理職の男女比(2015年10月)
    - [総務省「国勢調査」](https://www.e-stat.go.jp/stat-search/database?page=1&layout=datalist&toukei=00200521&tstat=000001080615&cycle=0&tclass1=000001104855&tclass2val=0)
- 農協役員の男女比(2021年7月)
    - [JA全中「JA都道府県⼥性役員等調査」](https://women.ja-group.jp/about-us/gender-equality/)

## 算出結果の再現

共同通信社による[R言語](https://cran.r-project.org/)スクリプト(gender-gap-index.R)を実行することで、本指数の算出結果を再現できる。

本スクリプトはR >= 4.1.0でのみ動作する。
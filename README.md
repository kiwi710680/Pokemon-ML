# Pokemon-ML：在瀏覽器裡跑 R，從預測走到因果效果

**線上版：https://kiwi710680.github.io/Pokemon-ML/**

六個互動 Lab，搭配 Moccia et al. (2024) *Machine learning in causal inference for epidemiology* 的導讀簡報。每一格程式都能改、能重跑，計算全部在瀏覽器裡用真正的 R（[webR](https://docs.r-wasm.org/webr/)）完成，不用安裝任何東西。

一條主線：plug-in 的誤差跟著 Q̂ 走；用 EIF 修正之後，只剩 ĝ 誤差 × Q̂ 誤差。六個 Lab，就是這句話在不同資料上的六種樣子。

| Lab | 資料 | 在看什麼 |
|---|---|---|
| 1 寶可夢 | Pokémon GO CP | regression 三步驟、overfitting、bias／variance、ridge、cross-validation、SuperLearner |
| 2 神奇糖果 | 模擬（真值已知） | plug-in 的誤差拆解、EIF、AIPW、TMLE、R₂、orthogonality、cross-fitting、positivity |
| 3 NHEFS | 戒菸與體重 | overlap／balance 診斷、full sample vs cross-fit、三種敏感度、和 Karim (2026) 對帳 |
| 4 RHC | 右心導管與死亡 | random forest 在 full sample「自己改考卷」，cross-fit 救回來 |
| 5 IHDP | 半合成（真值已知） | bias、RMSE、95% CI 覆蓋率 |
| 6 LaLonde | 職訓 vs PSID | overlap 不存在時，估計器能做什麼；trimming 改變的是題目 |

說明文字是中文；程式碼、輸出與圖表標籤是英文。

## 怎麼用

- **線上**：直接開上面的網址。第一次會從 webr.r-wasm.org 下載 R（約 30 MB，之後會快取）；Lab 4–6 還會下載 randomForest。
- **本機**：下載整個 repo，雙擊 `start_lab.bat`（需要 Python 3），或在資料夾裡執行 `python -m http.server 8765` 再開 http://localhost:8765 。直接雙擊 HTML 檔讀不到資料。
- **本機 R**：`R/` 裡的 `01`–`06` 腳本是同一套程式，在 repo 根目錄執行，例如 `Rscript R/04_rhc.R`。Lab 1–3 只需要 base R，Lab 4–6 需要 `randomForest`。估計器在 `R/helpers.R`，nuisance 引擎在 `R/engine.R`。

## 資料來源

| 資料 | 來源 |
|---|---|
| 寶可夢 | Pokémon GO 紀錄（OpenIntro `pokemon`）；課堂 train/test 切分依投影片重建 |
| 神奇糖果 | 虛構情境，`simulate_candy()` 模擬 |
| NHEFS | Hernán & Robins, *Causal Inference: What If*；CRAN `causaldata` |
| RHC | Connors et al. (1996) JAMA；Vanderbilt Biostatistics 公開資料（hbiostat.org/data） |
| IHDP | Hill (2011) 半合成 benchmark；CEVAE 公開版本 `ihdp_npci_1`–`10` |
| LaLonde | LaLonde (1986)；Dehejia & Wahba 整理版（users.nber.org/~rdehejia/data） |

分析方法參考 Karim (2026) *Three Routes to One Answer* 與其 companion repository（[ehsanx/dml-alternatives](https://github.com/ehsanx/dml-alternatives)）；程式碼為獨立撰寫。教學敘事受李宏毅教學風格啟發，非本人授課或背書。

## 限制

- 神奇糖果是虛構情境；它示範的是機制，不是任何真實介入的效果。
- random forest 有隨機性，瀏覽器（webR）和本機 R 的 RF 數字會差一點；頁面說明以瀏覽器跑出的數字為準。
- IHDP 只用 10 份資料，覆蓋率的蒙地卡羅誤差約 ±7%：看方向，不看小數點。

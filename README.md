# text-mining-testing


前言

        「文字探勘」(text mining)是資料探勘的一種延伸，顧名思義就是針對「文字」型態的資料進行分析。我們在此次報告使
        用的是文字探勘中的關鍵詞分析，藉由關鍵詞分析，我們可以藉由計算特定關鍵詞在各年出現的次數來分析此關鍵詞在該年
        對社會的影響力、重要性。我們使用的文本為民國100年至110年之施政計畫、政府出版品以及社會期刊，分別代表政府的施
        政方向、各部會的執行成果以及學者對各社會議題的重視程度。藉此分析在面對特定議題時，各部會的執行力以及政府的施
        政方向是否與學者提出的社會問題是否一致。


(一)讀取檔案並檢查是否都有讀到

        我們蒐集民國100年至110年之施政計畫、政府出版品以及社會期刊之PDF檔案，並依年份放入資料夾。由於我們僅需分析文
        字部分，圖片、表格或其餘版面配置將不利於我們的文字分析。而藉其僅需分析文字之特性，我們選擇將各檔案所出現之文
        字全數轉為TXT檔。

        我們利用R將PDF檔全數轉為文字檔，而其中因檔案眾多，可能出現無法讀取成功讀取之檔案，則由我們手動排除問題。並
        檢查頭尾的文字是否有成功讀取，若尾部有遺漏讀取的地方再回去看哪個PDF檔出現問題並修正。


(二)清理資料(空白&換行)

        在成功轉檔後因PDF檔中出現許多圖文重疊或圖片與文字交錯之情況，不利R辨認，且由於我們轉檔時為保留檔案完整性，並未
        刪除封面、封底等頁數，導致R轉檔時亦同時將這些頁面中的空白部分轉入文字檔中。圖片部份則可能無法被R成功讀取而成為
        亂碼或空白部分。

        我們利用TXT檔內建之尋找功能尋找常見錯誤並取代之，並手動再次檢查是否遺漏未更改之空白與亂碼。 

(三)斷詞並計算次數

        1.分析資料(三個類別32個主題，重複多次)
        	由於R內建中文詞彙量較少，我們所分析之關鍵詞有許多都未出現，所以需要製作各類別之關鍵詞辭庫。
                
        	在分析時，若遇到全形符號或空格會出現錯誤，此時R會提示發生前數個字詞，藉由TXT檔之「尋找」功能，複製並尋
        	找R所提示之字詞，將可以快速找到發生錯誤的位置並手動排除。
        
        2.輸出資料
        	藉由(三)可以以計數的方式計算個關鍵詞在三個類別於各年所出現隻次數，藉此得知各關鍵詞於各年度的重要性。
                
        	我們將次數以表格方式輸出excel檔之中，並利用ggplot套件，將各個主題之關鍵詞出現次數的多寡以圓形面積大小
		呈現，x軸為時間順序(以民國年份)。
		
        3.分析與解釋
		利用表格與圖表，我們分析各主題出現最多次之關鍵詞為何，並分析個關鍵詞十年間的出現次數消長，佐以各年份所
        	發生與討論之重大事件、議題，可得知政府執行者、學界、政府決策者三界於各年度所重視之議題是否一致。
                
        	將三大類別依施政計畫、政府出版品、學術期刊順序寫出「110社會重要議題與關鍵字報告」，其中各類別皆有四大
		領域共32個重大範疇之關鍵詞報告。


(四)標準化:

        在比較分析兩組資料時，可能會因單位的不同或數字大小的代表性不同，產生各變數間變化的程度不一，影響統計分析的結果。
        可利用資料的正規化（normalization）或標準化(standarization)解決此類的問題，藉由將原始資料轉換
        (transformation)後的量，進行資料間的比較及分析。
	
        因為各年的文本（施政計畫、政府刊物及學術期刊）的出版（或發表）數目、篇幅各不相同，各關鍵詞（議題）出現的頻次
        自然在各類文本中存在相當不同的差異；為進行各議題出現頻次之比較分 析，先分別對104年度到110年度各議題出現的次數
        的資料進行標準化。在機器學習演算法中，常用的資料正規化之一是將原始資料的數值，按比例縮放於 [0, 1] 區間中，
        且不改變其原本分佈；此即「最小值最大值正規化」方法。
	
        若X_(i,t)為議題i在某一類文本於第t年出現的次數，Z_(i,t)為其最小值最大值正規化結果如下：
	
						Z_(i,t)=(X_(i,t)-X_min)/(X_max-X_min )
						
        其中，X_max為該類文本各議題出現之最高總和次數；X_min為出現最少的總和次數。表格1為各議題出現頻次之標準化結果。

(標準化資料:[表格1.pdf](https://github.com/HenryLee1111/text-mining-testing/files/9554714/1.pdf))


 
(五)加權與排名:

        由於不同文本有其不同的意義，本解討論如何透過不同的權重，衡量議題的趨勢、重要性。表2為對表1標準化過後的施政計畫
        、政府刊物及學術期刊的次數之兩兩相關係數。

<img width="300" alt="表格2" src="https://user-images.githubusercontent.com/109747015/189841530-c253dd3d-a2f4-44d3-be90-3b0d78afc503.png">

        由於各標準化次數的相關係數皆為正值，因此可藉由對表1進行主成分分析(Principal Component Analysis, PCA)，再由
        其第一主成分所對應的因素負荷量(Factor Loading)作為各議題之權重，如表3所示。
	
<img width="300" alt="表格3" src="https://user-images.githubusercontent.com/109747015/189842720-9c3b4d9a-595c-48bd-974e-dba362202caf.png">

        對於表1標準化後的資料進行加權，得出議題指標(Issue Index，II)；並以此指標作為不同年度的排序，其結果如表格4。
	
(議題指標與名次:[表格4.pdf](https://github.com/HenryLee1111/text-mining-testing/files/9554706/4.pdf))

        對此我們也整理出不同議題下不同年度議題指標的折線圖:

![議題指標折線圖](https://user-images.githubusercontent.com/109747015/189844752-cd6f319f-0b93-42df-b4f2-77be5bbc74c7.png)

        表5為將7年的議題指標加總所得之排序，前五名分別是數位轉型與自動化發展、晚婚(不婚)化、能源轉型、長期照護、環境污染
        ；後三名分別世代資源不均、網路詐騙、電子廢棄物。
(議題指標總和與名次:[表格5.pdf](https://github.com/HenryLee1111/text-mining-testing/files/9554763/5.pdf))

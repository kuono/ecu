# Changelog for MyEcu

## Unreleased changes

ライブラリとはいえ，ほとんど本体プログラムと化している。もう少し整理しないとね。

## 動作の前提条件

   1) ECUと接続するシリアルケーブルがあること。なければ作ること。
   2) シリアル-USB変換ケーブルがあること。なければ作ること。
   3) 変換ケーブルのドライバをインストールすること。
 ちなみに魚野は FTDI Chips の変換基盤を使用。 VCPドライバをインストールした。　https://www.ftdichip.com/Drivers/VCP.htm
-- # インストールの途中，機能拡張がブロックされるので，セキュリティとプライバシーで実行を許可すること
-- # 

## Change history

-- #   2018.10.14 デバイスが存在しない場合にその旨表示する。unknown データをすべて表示する。
--                異常・正常結果を常に表示。ログファイル書き込みと画面表示の両方を用意。
-- #   2018.11.11 v.0.2.0.1 マルチスレッド化->logが書き込まれない。Chanにデータが入っていかない？
-- #   2018.11.12 v.0.2.0.2 例外処理が使いこなせずコンパイルが通らないのでEither Error DataQQに。
-- #   2018.11.16 v.0.2.0.3 tryなどを使ってみる 
-- #   2018.11.16 v.0.2.0.4 MVarなど導入。並列化。
-- #    なぜかputStrだと画面が更新されない->putStrLnに
-- #    単純にCommunicatorとLoggerをforkするとloggerが先に始まってしまうことの対策
-- #      -> Chan (MVar ECUStatus)で解決
-- #    E80, E7D 時のスレッド再起動処理を埋め込む
-- #    hscurses化 -> ウィンドウの大きさを超えて画面出力すると例外が起こるので，一旦利用中止。
-- #   2018.11.16 v.0.2.0.5 キー入力でrunwithecuに非同期例外を投げるとエンジンが瞬停する。
-- #    手抜きのモデルデータ表示をやめる
-- #   2018.12.13 v.0.2.0.6 累積データを表示させるようにするため，新たにデータ型を追加し，関数引数の変更等を行う。
-- #    突然電源ECUから反応がなくなった場合のエラー処理
-- #    ND , NC エラーときの自動待機・接続挑戦継続
-- #    error code reset command 
-- #    これらのエラー処理を，Monadを使ってログに書き込んでいく
-- #    画面表示でログを表示する
-- #    エラーを表示した後，復帰しない（catch処理を組み込み）
-- #   2018.12.25 v.0.2.0.7  不要な試行錯誤コード整理
-- #                         default log directory setting in command line arg
-- #   2019.01.02 v.0.2.0.8  最小・最大データを記録・表示するように                    
-- #   2019.01.27 v.0.2.0.9  通し番号も表示するように
-- #   2019.01.29 v.0.2.0.10 接続がおかしくなった。原因は？？？←１本カプラが外れていた。→ケーブルコネクタを固定できるカップラーに変更
-- #                         エンジン速度を基準に，エラーを画面ログ部分に蓄積表示していたが，上限値超えのみ表示に。将来的には機械学習におきかえる？
-- #                         一度エラーが出ると接続がおかしくなる。例外のマスキングを正しく行うこと。
-- #                         初期化が成功したら一旦画面をクリアする。
-- #                         状態遷移をきちんと制御する。-> 例外をやめ，メッセージ化した
-- #                         動作中にケーブルを引っこ抜くと，Index例外が発生。デバイスも見つけられなくなる。
-- #                             →標準長に満たないデータ来たときはエラー処理する
-- #                         未接続状態でqを押してもQuitできない？
-- #                         throttle pot metorの値がおかしい（0.28〜3.33）→100倍していたのを10倍に
-- #                         E80,E7D 直後に初期化できない。読み込みバッファにデータ残っていそう。
-- #                         
-- #                         Post process for average caliculation and defect detection
-- #                         ライブラリ化・絞り込み（Chan ECUDataを輸出）
-- #                         threepenny.gui を使ったデータ履歴表示
-- #                         データソースの属性を記述する方式に変更（Stream/Cell, Retry/STOP, ....) 
-- #                         ん？それってFRP？でも失敗時の振る舞いを加味する操作は入っていないかも
-- #                         q で抜ける時，logger にフラッシュさせる
-- #   2019.02.19 v.0.2.1.0  一旦，安定版とする。
-- #   2019.03.09 v.0.2.1.1  テストデータモード追加。以降，GitHubにプロジェクト登録
-- #
-- # GUI 準備 (Threepenny-guiの導入)　とりあえず動いたが，終了方法未詳。
-- #
-- # いきなりプラグを抜くと ecu-exe: discardData: does not exist (Device not configured)
-- #   これはどうやら Linux, macOSの場合である。 discardData関数がシステムコールでエラーを
-- #   受け取っている。
-- # -- http://hackage.haskell.org/package/unix-2.7.2.2/docs/src/System.Posix.Terminal.Common.html#discardData
-- # -- | @discardData fd queues@ calls @tcflush@ to discard
-- # --   pending input and\/or output for @Fd@ @fd@,
-- # --   as indicated by the @QueueSelector@ @queues@.

## 0.3.0.0

2019.03.10 v0.3.0.0

- TestModeを追加
- ./log/ECU...としていたが，ディレクトリが存在していないとランタイムエラーを起こすので変更

-- #   2019.03.15 v.0.3.0.0  TestMode追加 - メインループやスレッド構成を見直し→読み出しエラー頻発
-- #   2019.03.15 v.0.3.0.1  読み出しエラー対策のため割り込み対処構成見直し；依然エラー頻発。
-- #   2019.03.17 v.0.3.0.2  正格評価化，割り込み対策，メインループをEitherモナド化

## 0.4.0.0

2019.04.02 v0.4.0.0

- Elmに触発された枠組みに変更
- 抽象化をより進める

## 0.4.0.1

2019.04.14 v.0.4.0.1

- 起動時，ECUがつながっていないと例外が見えてしまっていることの対処
- 画面表示に単位を表示
- 冒頭表示サイズ通りでないバイト数しかとれなかった場合のランタイムエラー回避

## 0.4.0.2

2019.04.29 v.0.4.0.2

- HDMIモニターでの表示用に，若干行数を縮めた。
- 閉じたシリアルポートを再度閉じようとして起こしている例外の捕捉コードを追加

ecu-exe: Data.ByteString.index: index too large: 10, length = 9
CallStack (from HasCallStack):**        
  error, called at libraries/bytestring/Data/ByteString.hs:1877:23 in bytestring-0.10.8.2:Data.ByteString
ecu-exe: Data.ByteString.index: index too large: 10, length = 9
CallStack (from HasCallStack):*         
  error, called at libraries/bytestring/Data/ByteString.hs:1877:23 in bytestring-0.10.8.2:Data.ByteString
     map Sensor(kPa):     33  **        


-- # Future Plan
-- # 　冷却音表示の着色（出発準備完了表示）
-- #   進角等の設定関数の作成
-- #   Raspberry Pi への稼働移行（タッチスクリーン・車載用ケース手配）
-- #   ECUが停止した後QUITをするとhCloseが二回呼ばれ、例外が発生している。
-- #   ReaderT, StateT モナド導入
-- #   cef3を組み込み，GUIを内在化
-- #   CUIとGUIを選べるようにする
-- #   ゲージUIを組み込む
-- #   parse関数で-- 二バイトデータを無視しているので注意
-- #   - 最大値・最小値を，直近10秒程度の範囲に変更する？
-- #   - FRP化する

composeAndWriteDataWithTestDataFile::FilePath -> ECUDataCh -> IO ()
composeAndWriteDataWithTestDataFile p dc = do
  kaishijikoku <- currentTime
  writeChan dc ECUData { status = Connected mne00000 , at = [kaishijikoku] }
  withFile p ReadMode loop
  where
      loop ::Handle -> IO ()
      loop h = do
        eof <- hIsEOF h
        if eof
          then pure ()
          else do
            raw <- hGetLine h
            ima <- currentTime
            let d807d = compose raw
            case d807d of
              Right d  -> do
                writeChan dc $ ECUData { status = GotData d , at = [ima] }  
                threadDelay 400000 -- 0.4 sec
                loop h
              Left m   -> do
                -- print m
                writeChan dc $ ECUData { status = NotConnected m, at = [ima] } 
                threadDelay 400000 -- 0.4 sec
                loop h            

compose :: String -> Either String ECUData807d
compose d = -- Left m where m = show . length $ splitOn "," d 
        case splitOn "," d of
          [ d0, d00, d01, d02, d03, d04, d05, d06, d07, d08, d09,
            d10, d11, d12, d13, d14, d15, d16, d17, d18, d19,
            d20, d21, d22, d23, d24, d25, d26, d27, d28, d29 ]
                    -> Right $ ECUData807d {
                          d80 = BS.pack [
                            0x1c, -- 00
                            upperbyte ( read d01 ) :: Word8, -- 01 e speed
                            lowerbyte ( read d01 ) :: Word8, -- 02
                            ( readd d02 ) + 55 :: Word8, -- 03 cool t
                            ( readd d03 ) + 55 :: Word8, -- 04 amb t
                            ( readd d04 ) + 55 :: Word8, -- 05 int t
                            ( readd d05 ) + 55 :: Word8, -- 06 fuel t
                            ( readd ( d06 ) ) :: Word8,     -- 07 mapS
                            truncate (( read d07 ) * 10) :: Word8, -- 08 btv
                            truncate (( read d08 ) * 200) :: Word8, --09 thrott
                            if d09  == "T" then 16 :: Word8 else 0 :: Word8, --10 idlesw
                            ( readHex d10 ) :: Word8, -- 11 unknown 0b
                            if d11  == "T" then 0 :: Word8 else 0xff :: Word8, --12 pncls
                            (if d12 == "T" then 1 else 0 ) .|.
                            (if d13 == "T" then 2 else 0 ) :: Word8, -- 13 cool air
                            (if d14 == "T" then 1 else 0 ) .|.
                            (if d15 == "T" then 128 else 0 ) :: Word8, --14 fu / tn
                            ( readd d16 ) :: Word8 , -- 15
                            ( readHex d17 ) :: Word8 , -- 16
                            ( readHex d18 ) :: Word8 , -- 17
                            ( readHex d19 ) :: Word8 , -- 18 idleacmp
                            upperbyte ( read d20 ) :: Word8, -- 19
                            lowerbyte ( read d20 ) :: Word8, -- 20
                            ( readHex d21 ) :: Word8 , -- 21
                            ( readd d22 ) * 2 + 24 :: Word8 , -- 22
                            upperbyte . fromIntegral $ truncate ((read d23 ) * 500) :: Word8, -- coilt u
                            lowerbyte . fromIntegral $ truncate ((read d23 ) * 500) :: Word8,
                            ( readHex d24 ) :: Word8,
                            ( readHex d25 ) :: Word8,
                            ( readHex d26 ) :: Word8] ,
                          d7d = BS.pack [
                            0x20, -- 00
                            0x00,0x00,0x00,0x00,0x00,
                            ( readd d27 ), -- 06
                            0x00,0x00,0x00,
                            ( readd d28 ), -- 0a
                            0x00,
                            ( readd d29 ),  --0c
                            0x00,0x00,0x00,
                            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0,0,0,0,0,0,0
                          ]
                        }
                        where 
                          readd::String -> Word8
                          readd s = readd' $ reverse s
                            where 
                              readd'::String -> Word8
                              readd' []     = 0
                              readd' [u]    = hextoint u
                              readd' (f:r)  = hextoint f + 10 * (readd' r) 
                          upperbyte::Int -> Word8
                          upperbyte p = if p > 0xffff then 0xff else fromIntegral $ p `div` 256
                          lowerbyte::Int -> Word8
                          lowerbyte p = if p > 0xffff then 0xff else fromIntegral ( p - fromIntegral ( upperbyte p ))
                          readHex::String -> Word8
                          readHex i = 
                            case length i of 
                              0   -> 0
                              1   -> hextoint $ head i
                              2   -> 16 * ( hextoint $ head i) + (hextoint (head (tail i) ))
                              otherwise -> 0xFF
                          hextoint ::Char -> Word8
                          hextoint c = 
                            case c of 
                              '0' -> 0
                              '1' -> 1
                              '2' -> 2
                              '3' -> 3
                              '4' -> 4
                              '5' -> 5
                              '6' -> 6
                              '7' -> 7
                              '8' -> 8
                              '9' -> 9
                              'A' -> 10
                              'B' -> 11
                              'C' -> 12
                              'D' -> 13
                              'E' -> 14 
                              'F' -> 15
                              otherwise -> 0
          
          otherwise -> Left d

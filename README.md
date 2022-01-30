# Rover Mini MEMS Monitor

## preface

Copyright (c) 2019-2021 by Kentaro UONO

Released under [the MIT license](https://opensource.org/licenses/mit-license.php)

## Version history

- 0.13.0   by K.UONO on 2022.01.30 UI arrange was changed.
- 0.12.3   by K.UONO on 2021.11.01 bug fix
- 0.12.2   by K.UONO on 2021.09.11 bugs are fixed for rpi.
- 0.12.1   by K.UONO on 2021.08.29 MEMS type was changed.
- 0.12.0   by K.UONO on 2021.08.01 Dhall and configration file system.
- 0.11.2   by K.Uono on 2021.07.28 Adoption for low Display raw number.
- 0.11.1   by K.Uono on 2021.07.26 RaspberryPi OS adoption
- 0.11.0   by K.Uono on 2021.07.23 Fault Code bug fix
- 0.10     by K.Uono on 2017.11.19
- 0.9.3    by K.Uono on 2020.XX.XX added HTML treat function as UI driver
- 0.9.2    by K.Uono on 2020.09.20
- 0.9.0    by K.Uono on 2020.01.25
- 0.8.0    by K.Uono on 2019.09.07 ReaderT Version
- 0.7.0    by K.Uono on 2019.08.14 Brick Version
- 0.5.0    by K.Uono on 2019.08.03 UI ralated types and functions are now defined in UI module
- 0.4.2.0  by K.Uono on 2019.07.06
- 0.4.1.2  by K.Uono on 2019.07.05

## Basic usage <!-- 基本的な使い方 -->

You need a VT100 terminal because the App uses some Japanese fonts. 
<!-- 画面出力に日本語フォントを使っているので，日本語対応の端末エミュレータが必要です。-->
    cabal update
    cabal build
    cabal run

<!-- stack build --profile
stack exec -- <bin_name> +RTS -p -hb
stack exec -- hp2ps -e8in -c <proj_name>.hp
　例　hp2ps -e8in -c ecu-exe.hp
 プレビューでecu-exe.psを開ける

stack run +RTS -p -hc
stack test --profile --test-arguments "+RTS -hm" -- 引数の渡し方
stack exec -- <bin_name> +RTS -p -hc -->

## Development Related Memo

### Development Objective

### 不明点

- キーオンののち初回接続してしばらくしてエラーが出ると，まったくつながらない ←　いつの間にか繋がるようになった。ドライバのバグフィックスがあった？

### わかっている問題点

- Raspberry Pi 上で動作させると，テキスト端末エミュレーションソフトの解像度の関係で画面が入りきらない。
  - [参考画像をここにいれる](./)
- プロファイリングすると盛大なメモリリークあり（ギガ単位のメモリ利用）。Vtyモジュールのせい？
  - ただし少なくともCatalinaでモニタリングしている限りでは，数十Mバイト程度しかメモリは消費していない。
  - Big Surでも同様。また，Rapsberian でも同様。リソースモニタで見ていると，数１００MBくらいしか消費していないのでは。

### 近日対応したい機能

- 既存ログのビューア機能
- ヘルプキー（ESCキー）を押した時にダイアログ表示
- ポート値などをGUIを使って入力できるようにする
- 各種設定値（上下限値，平均値等）を表示
- グラフ表示項目の選択ダイアログ・初期設定
- 各センサー値に異常値が出た時に，異常記録を画面・ログ双方に残す
- アイドリングセンサ値の常時表示　← 異常検知アルゴリズムをどうするか検討
- Form Widget 実装開始。ただし，以下の問題がある：
- フォームイベントを扱っていない
- フォームが画面幅全体に出てしまっている
- フォームイベントを扱う間の通常のイベントの扱いをどうするか決めていない
- フォームまわりの宣言について，どのモジュールで扱えばいいのかよくわかっていない
- TonaTona ライブラリを使用し始める予定
- 設定ファイルに初期値を書き込んでおく（接続に成功したポート名くらい？）
- UI として新たにHTML生成する機能を加える予定
- モデルに，vector構造で保持するmutableな直近データを加える予定
- Status の dset :: [DataSet] を vector に <- ガベージコレクションがあるので対応不要かも
- 型定義や関数の置き場所を再度整理し，モジュールの独立性を高める予定


### 保留とする開発予定項目

- conduit ライブラリを導入 <- 手段が目的になっている？

### 追加したい機能/解決したい問題点/課題

-- #   ThreePennyGUI を使い，現場で手持ちの iPhone や iPad からデータをリアルタイムで見られるようにする
-- #   ゲージUIを組み込む
-- #   Raspberry Pi への稼働移行（タッチスクリーン・車載用ケース手配）
-- #   cef3を組み込み，GUIを内在化
-- #   CUIとGUIを選べるようにする
-- #   - 最大値・最小値を，直近10秒程度の範囲に変更する？

### いつ対応したかは忘れたが，解決済みのもの・無意味となったもの

-- #   ECUが停止した後QUITをするとhCloseが二回呼ばれ、例外が発生している。
-- #   キャラクタグラフのベースラインが移動する問題の原因究明。
-- #   parse関数で-- ２バイトデータを無視しているので注意
-- #   ReaderT, StateT モナド導入
-- #   - FRP化する

以下の情報は，本プログラムを作成する上で参考にしたデータや覚書です。

## ECUのデータについて

### 現在表示している内容

- ECUステータス（接続/断) ----------------------------------時刻
- ライブデータ------------------------------------------------
- Engine Speed (rpm)    : エンジン回転数
- Throttle Potent ( V ) : スロットル開度
- Map Sensor (kPa)      : MAPセンサ値
- Battery Voltage ( V ) : 蓄電池電圧
- Coolant Temp (dgC)    : 冷却液温度
- Ambient Temp (dgC)    : 環境温度
- Intake Air Temp (dgC) : 吸気温度
- Park or neutral? A/C? : AT車のインヒビタースイッチ（92年式マニュアル車はエアコンのon/off状態のようです）
- Idle switch           : アイドルスイッチ
- Idl Air Ctl M P(C/O)  :
- Idl Spd deviatn       :
- Ignition advnce (deg) : 進角
- Coil Time (msc)       : コイルタイム
- Lambda voltage ( mV)  : O2センサ電圧
- Closed/open loop      :
- Fuel trim ( % )       : フューエルトリム
- Unknown 80 data (0B 0F 10 11 15 19 1A 1B)
- フォールトコード

### MiniMoniの表示画面の意味

[メーカーさんのサイト](https://richeehauser.com/mini-moni/)参照

- マルチ画面   0000 I C / 0.00V 0  : 原動機回転数 アイドリング認識 O2フィードバック / TPot電圧 TPotステップ値
- 二次燃調面   SHORT FT / 100 % CL : Short Term Fuel Trim (%) 回路開閉（CL or OP？）

#### フォールトコードの意味

- [こちら](https://minkara.carview.co.jp/userid/2834887/car/2442400/4981106/6/note.aspx#title)参照
  - Status byte 1 (0x0d)
    - Bit 0 : Coolant Temp Sensor Error
    - Bit 1 : Inlet Air Temp Sensor Error
    - Bit 4 : Ambient Air Temp Sensor Error (But not installed on Mini)
    - Bit 5 : Fuel Temp Sensor Error (But not installed on Mini)
  - Status byte 2 (0x0e)
    - Bit 1 : Fuel pump cirkit Error
    - Bit 5 : ECU Vaccum Sensor Error
    - Bit 7 : T-Pot cirkit Error
- [解析サイト](https://colinbourassa.github.io/car_stuff/mems_interface/)による
  - 0x0D  Fault codes. On the Mini SPi, only two bits in this location are checked:
    - Bit 0: Coolant temp sensor fault (Code 1)
    - Bit 1: Inlet air temp sensor fault (Code 2)
  - 0x0E  Fault codes. On the Mini SPi, only two bits in this location are checked:
    - Bit 1: Fuel pump circuit fault (Code 10)
    - Bit 7: Throttle pot circuit fault (Code 16)
- 由来不明(ミニモニの画面表記のこと？) 
  - 01:Coolant 02:Crank NG
  - 01:T-Pot 02:ERROR 17 - 定番のエラー
  - 01:Air Temp 02:Map Sens 03:Crank NG
  - 01:Crank NG
  - 01:Air Temp 02:Map Sens

### ECUから返ってくるデータの意味

data Frame80  = Frame80 {
        size_80     :: Int , -- 0x00   Size of data frame, including this byte. This should be 0x1C (28 bytes) for the frame described here.
        engineSpeed :: Int , -- 0x01-2 Engine speed in RPM (16 bits)
        coolantTemp :: Int , -- 0x03   Coolant temperature in degrees C with +55 offset and 8-bit wrap
        ambientTemp :: Int , -- 0x04   Computed ambient temperature in degrees C with +55 offset and 8-bit wrap
        intakeATemp :: Int , -- 0x05   Intake air temperature in degrees C with +55 offset and 8-bit wrap
        fuelTemp    :: Int , -- 0x06   Fuel temperature in degrees C with +55 offset and 8-bit wrap. This is not supported on the Mini SPi, and always appears as 0xFF.
        mapSensor   :: Int , -- 0x07   MAP sensor value in kilopascals
        battVoltage :: Float , -- 0x08 Battery voltage, 0.1V per LSB (e.g. 0x7B == 12.3V)
        throttlePot :: Float , -- 0x09 Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
        idleSwitch  :: Bool ,-- 0x0A   Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
        *** It seems that Bit 0 will be set in spite of Bit 4 in case of MEMS 1.3J, at least. 31st Jusy 2021 by K.UONO ***
        unknown0B   :: Word8,-- 0x0B   Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
        pnClosed    :: Bool ,-- 0x0C   Park/neutral switch. Zero is closed, nonzero is open.
        -- Fault codes. On the Mini SPi, only two bits in this location are checked:
        faultCode1  :: Bool ,-- 0x0D  * Bit 0: Coolant temp sensor fault (Code 1)
        faultCode2  :: Bool ,--       * Bit 1: Inlet air temp sensor fault (Code 2)
        faultCode10 :: Bool ,-- 0x0E  * Bit 1: Fuel pump circuit fault (Code 10)
        faultCode16 :: Bool ,--       * Bit 7: Throttle pot circuit fault (Code 16)
        unknown0F   :: Word8,-- 0x0F   Unknown
        unknown10   :: Word8,-- 0x10   Unknown
        unknown11   :: Word8,-- 0x11   Unknown
        idleACMP    :: Int  ,-- 0x12   Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
        idleSpdDev  :: Int  ,-- 0x13-14 Idle speed deviation (16 bits)
        unknown15   :: Word8,-- 0x15    Unknown
        ignitionAd  :: Float,-- 0x16    Ignition advance, 0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
        coilTime    :: Float,-- 0x17-18 Coil time, 0.002 milliseconds per LSB (16 bits)
        unknown19   :: Word8,-- 0x19    Unknown
        unknown1A   :: Word8,-- 0x1A    Unknown
        unknown1B   :: Word8 -- 0x1B    Unknown
    } deriving Show

### 各種センサーの役割や信号の意味

[参考サイトはこちら](http://www.geocities.jp/dmxbd452/injection.html)

- クランク角センサー
約１，３～１，５KΩ　点火、燃料噴射の為の基本センサー　故障時　始動不良。
数Ωにショートしている場合、リレーモジュールよりカチャカチャ異音発生しアクセルONでも回転上がらず。

- MAPセンサー
ECU内部に有るセンサー。点火時期、噴射時間調整の為の最重要センサー。　インテークマニホールドの負圧で電圧変化。                大気圧時　４．５３V　　最大負圧時　６９．７mV(ミリボルト）を発生する。　から
ホース切れ等の場合(負圧無し）　黒煙発生　点火時期不良(遅れる）　加速不良
電圧を発生しない場合　始動性が極端に悪くなるが(セル１０回でも始動するかな？）エンストしない、パワーは無い。

- スロットルポジション　センサー(ポテンショメーター）
実測値(今回は電圧で表示しました。スロットルボディーに付いている状態で）
全閉時　約0.5V　　全開時　約3.0V　（これで　正常値です！故障だと思わないように！）
0V 又は 5V 出力は　センサー故障を意味するので　ECUは、故障モードになり、センサー故障がメモリーされる。
故障時　加速不良　ACコンプレッサー作動不良。

- λ(ラムダ)センサー　O2センサー
故障時　排気ガスが濃い　又は薄い　黒煙発生　エンジン不調　エンスト発生
内部にヒーター有り。国産に変更出来ますがそのままだと作動しません。

- 水温センサー,吸気温センサー Ω値特性は両方同じ

- 水温センサー
冷間時　約3.0V（点検日の気温 10℃。　通常20℃ の抵抗値で表示する）
温間時　約0.4V（電動ファン作動時の水温時）
0V 又は 5V 出力時は、センサー故障とECUがみなし故障モードになり、故障がメモリーにインプットされる。
水温センサー故障時は冷間時の始動不良になる。（キャブ車のチョーク作動不良と同じ）
故障モードは、　水温60℃　吸気温35℃に設定　　　年式により黒煙を発生するＥＣＵ有り

- ステッパーモーター
7.5度ステップのモーター。アイドリング回転数の調整用。約１５Ω。
AC作動時、AT車のR、D時（P、N以外）、電気負荷の大きい時等アイドル・アップさせる。
ハンダ割れ、水浸入による固着故障有り。

- PTCヒーター
インテーク・マニホールドを暖めるヒーター。水温が上昇するとOFFする。
時々焼損していて振るとカラカラ音の発生するPTC有り。
冷間時の始動性にはあまり関係無い。排気ガス対策と思われます。理由は、スイッチＯＮでは作動しません。
エンジン始動後ＰＴＣリレーがＯＮする。通電後数秒で手で触れない位熱くなる。

### what is ptc

Positive temperature coefficient (PTC) characteristics 自己温度制御ヒータ

### ラムダセンサの働き

[How do lambda sensors work](https://mechanics.stackexchange.com/questions/23933/how-do-lambda-sensors-work)
Lambda sensors work at elevated temperatures, around 300 °C (600 °F); many lambda sensors contain a resistive heater element to help get them up to temperature quickly.

[Rover Mini Electlic Fuel Injection について](http://www.tmsmini.com/cooper/sp_acr.htm)

- 各種データについて（１）-- Rover Pod 1より
-- RPM            : エンジン回転数
-- Idle Switch    : アイドル時，Closed 。93年以前の SPI では Idle Switch が設置されていないため，ECUはTPSを代わりに使用。
-- P/N Switch     : スタートアップ時，Closed. オートマのみ。マニュアル車は直結。
-- MAP Sensor     : Strategy manifold 圧（kPa)。期待値はキーオン時，100kPa, 高アイドル時30kPa
-- Coolant temp   : 摂氏温度
-- Inlet Air temp : 摂氏温度。エンジンが温まった時の期待値：20〜50度
-- Ambient temp   : 他のセンサ値を元にした計算値。摂氏温度。エンジンオフ時，200度。
-- Battery Voltage: バッテリ電圧
-- Throttle Pot   : 電圧
-- Lambda Volts   : 酸素センサ（触媒前のBank 1に設置）のO2電圧(mV)

- 各種データについて（２）（抜粋） -- ワークショップマニュアルより
-- Idle speed controlled by ECU   : 850 +/- 25 rpm
-- Exhaust gas CO content at idle : 05% max Not adjustable
-- Throttle potentionmeter voltage: Closed : 0 to 1V, Open : 4 to 5V

[インジェクションシステムのクローズドループについて]
<http://www.lightcycle.jp/old/blog/2011/1228-2039.php>
ナローバンドシステム⇒ナローバンドの０２センサーが使用されるシステムです。
ワ[イ]ドバンドにくらべて測定できる空燃比が限られており、走行の一部の領域でのみ
０２センサーの補正が入るものとなります。

０２センサーの補正が入れられない領域としては、アクセルを大きくあけて高回転に
加速していくなどの状態があげられます。
このような領域ではあらかじめコンピューターに入力されている固定データで制御される
ことになります。

この０２センサー制御の入らない状態をオープンループと呼びます。

Narrowband

These qualitatively detect whether the exhaust gases are rich or lean.

The most prevalent type of sensor is the zirconia-based narrowband, which generates its own voltage as signal output:

useful voltage range 0.1 - 0.9 V
stoichiometric AFR at 0.45 V
low voltage = lean condition, high voltage = rich condition
Narrowband Voltage Graph

The rarer titania-based variant does not generate its own voltage, but changes electrical resistance based on the oxygen concentration detected.

low resistance = rich condition, high resistance = lean condition

[Air–fuel ratio meter] wikipedia
<https://en.wikipedia.org/wiki/Air%E2%80%93fuel_ratio_meter#Zirconia_oxygen_sensor>


    -- 【ECU仕様の知識】
    -- <https://www.minimania.com/A_Basic_Guide_to_Electronic_Fuel_Injection_for_Minis> より
    -- For a typical Mini EFI conversion, the EFI ECU should receive information from the following sensors:
    -- * A throttle position sensor that tells the computer how hard your foot is on the accelerator pedal.
    -- * A crank angle sensor that basically tells the computer where the pistons are in their travels.
    -- * A knock sensor which detects any sign of detonation (pre-ignition or 'pinging') which is where the 
    --   air/fuel mixture is exploding violently inside the combustion chamber instead of as a controlled 
    --   progressive burning.
    -- * A Manifold Absolute Pressure (MAP) sensor which measures manifold vacuum (or boost!).
    -- * The engine speed in revolutions per minute, which, among other functions, informs the ECU whether 
    --   or not the rev limiter should be invoked.
    -- * An oil pressure sensor as an emergency input, if the oil pressure is too low, some EFI ECUs actually 
    --   turn the engine off after triggering a warning to the driver to restrict engine damage.
    -- * An engine temperature sensor to let the ECU know if a cold-start function is required where the idle 
    --   speed is increased along with different fuel and ignition settings are used until the engine has 
    --   reached a normal operating temperature.
    -- * An oxygen sensor that is plumbed into the exhaust manifold that examines the exhaust gases leaving 
    --   the engine and informs the ECU what the air/fuel ratio is.
    -- * Many other input sensors that can be used include the temperature of the intake air (especially 
    --   important for a Mini using forced induction), a sensor indicating which gear you have selected and 
    --   many more I don't have room to list.

## Rover Mini の ECUの型式

    -- https://blogs.yahoo.co.jp/dmxbd452/5751726.html
    -- http://www.minispares.com/product/Classic/MNE101070.aspx
    -- https://memsfcr.co.uk/ecu-versions/
    -- Part number	Manual / Automatic	Attributes	              VIN No. From - VIN No. To 
    -- MNE10026	 Automatic	SPI -         Except Cooper	                  - 59844
    -- MNE10027	 Manual    	SPI - Japan - Except Cooper	                  - 60487
    -- MNE10078  Manual     SPI - Japan - Cooper                          - 60487
    -- MNE10097	 Manual	    SPI -         Except Cooper - 1992-93	 	  - 059586
    --                                    Except Cooper                   - 059586
    -- MNE10090	 Automatic	SPI -         Except Cooper	            59845	68084
    -- MNE101060 Automatic	SPI -         Except Cooper	            68085	103112
    -- MNE10092	 Manual	    SPI -         Cooper	                60488	69492
    -- MNE10089	 Manual	    SPI -         Except Cooper	            59587	67377
    -- MNE101040 Manual	    SPI -         Except Cooper	            67378	103112
    -- MNE101150 Manual	    SPI -         Except Cooper	           103113	134454
    -- MNE101160 Automatic	SPI -         Except Cooper     	   103113	134454

    -- MNE10078  Cooper to 060487 JAPAN SPEC <- http://www.minispares.com/product/Classic/Electrics/MNE10078.aspx?09&ReturnUrl=/search/classic/MNE10078.aspx|Back%20to%20search ->
    -- MNE101070 Cooper Nov93 on 69493 <- http://www.minispares.com/product/Classic/Electrics/MNE101070.aspx?09&ReturnUrl=/search/classic/MNE.aspx|Back%20to%20search ->
    -- MNE101350 JAPAN SPEC 134455 ON <- http://www.minispares.com/product/Classic/Electrics/MNE101350.aspx?09&ReturnUrl=/search/classic/MNE.aspx|Back%20to%20search ->
    -- MNE101361 Automatic  SPI with Air Con <- http://www.minispares.com/product/Classic/Electrics/Ignition/MNE101361.aspx?0908&ReturnUrl=/search/classic/MNE.aspx|Back%20to%20search ->
    -- MNE101350 Manual	    SPI - Except UK - 1996 on	134455 - 	 
    -- MNE101351 Manual	    SPI - Except UK - 1996 on	134455 - 
    -- MNE101360 Automatic	SPI - Except UK - 1996 on	134455 - 	 
    
    -- MNE101361 Automatic	SPI - Air Con - Except UK - 1996 on	134455 - 	 
    
    -- ^ original number for size of Frame 7d = 32

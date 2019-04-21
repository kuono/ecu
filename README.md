# ecu

Rover Mini MEMS Monitor

programmed by Kentaro UONO

## 基本的な使い方
stack build --profile
stack exec -- <bin_name> +RTS -p -hb
hp2ps -e8in -c <proj_name>.hp
　例　hp2ps -e8in -c ecu-exe.hp
 プレビューでecu-exe.psを開ける

stack run +RTS -p -hc
stack test --profile --test-arguments "+RTS -hm" -- 引数の渡し方
stack exec -- <bin_name> +RTS -p -hc

返ってくるデータの意味
data Frame80  = Frame80 {
        size_80     :: Int , -- 0x00	Size of data frame, including this byte. This should be 0x1C (28 bytes) for the frame described here.
        engineSpeed :: Int , -- 0x01-2	Engine speed in RPM (16 bits)
        coolantTemp :: Int , -- 0x03	Coolant temperature in degrees C with +55 offset and 8-bit wrap
        ambientTemp :: Int , -- 0x04	Computed ambient temperature in degrees C with +55 offset and 8-bit wrap
        intakeATemp :: Int , -- 0x05	Intake air temperature in degrees C with +55 offset and 8-bit wrap
        fuelTemp    :: Int , -- 0x06	Fuel temperature in degrees C with +55 offset and 8-bit wrap. This is not supported on the Mini SPi, and always appears as 0xFF.
        mapSensor   :: Int , -- 0x07	MAP sensor value in kilopascals
        battVoltage :: Float , -- 0x08	Battery voltage, 0.1V per LSB (e.g. 0x7B == 12.3V)
        throttlePot :: Float , -- 0x09	Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
        idleSwitch  :: Bool ,-- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
        unknown0B   :: Word8,-- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
        pnClosed    :: Bool ,-- 0x0C	Park/neutral switch. Zero is closed, nonzero is open.
        -- Fault codes. On the Mini SPi, only two bits in this location are checked:             
        faultCode1  :: Bool ,-- 0x0D  * Bit 0: Coolant temp sensor fault (Code 1)
        faultCode2  :: Bool ,--       * Bit 1: Inlet air temp sensor fault (Code 2)
        faultCode10 :: Bool ,-- 0x0E  * Bit 1: Fuel pump circuit fault (Code 10)
        faultCode16 :: Bool ,--       * Bit 7: Throttle pot circuit fault (Code 16)
        unknown0F   :: Word8,-- 0x0F	Unknown
        unknown10   :: Word8,-- 0x10	Unknown
        unknown11   :: Word8,-- 0x11	Unknown
        idleACMP    :: Int  ,-- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
        idleSpdDev  :: Int  ,-- 0x13-14	Idle speed deviation (16 bits)
        unknown15   :: Word8,-- 0x15	Unknown
        ignitionAd  :: Float,-- 0x16	Ignition advance, 0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
        coilTime    :: Float,-- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
        unknown19   :: Word8,-- 0x19	Unknown
        unknown1A   :: Word8,-- 0x1A	Unknown
        unknown1B   :: Word8 -- 0x1B	Unknown
    } deriving Show




    --
    -- 【各種センサーの役割や信号の意味】  http://www.geocities.jp/dmxbd452/injection.html より
    -- クランク角センサー　約１，３～１，５KΩ　点火、燃料噴射の為の基本センサー　故障時　始動不良。
    --                　数Ωにショートしている場合、リレーモジュールよりカチャカチャ異音発生しアクセルONでも回転上がらず。
    -- MAPセンサー　　　　ECU内部に有るセンサー。点火時期、噴射時間調整の為の最重要センサー。　インテークマニホールドの負圧で電圧変化。
    --                 大気圧時　４．５３V　　最大負圧時　６９．７mV(ミリボルト）を発生する。　から
    --                 ホース切れ等の場合(負圧無し）　黒煙発生　点火時期不良(遅れる）　加速不良
    --                 電圧を発生しない場合　始動性が極端に悪くなるが(セル１０回でも始動するかな？）エンストしない、パワーは無い。　
    -- スロットルポジション　センサー(ポテンショメーター）
    --                 実測値(今回は電圧で表示しました。スロットルボディーに付いている状態で）
    --               　全閉時　約０，５V　　全開時　約３．０V　（これで　正常値です！故障だと思わないように！）
    --               　０V　又は　５V出力は　センサー故障を意味するので　ECUは、故障モードになり、センサー故障がメモリーされる。
    --               　故障時　加速不良　ACコンプレッサー作動不良。
    -- λ(ラムダ)センサー　O2センサー
    --                故障時　排気ガスが濃い　又は薄い　黒煙発生　エンジン不調　エンスト発生
    --                内部にヒーター有り。国産に変更出来ますがそのままだと作動しません。　
    -- 水温センサー,吸気温センサー Ω値特性は両方同じ　
    --   水温センサー　 冷間時　約３．０V（１０℃　点検日の気温。　通常２０℃の抵抗値で表示する）
    --                温間時　約０．４V（電動ファン作動時の水温時）
    --                ０V 　又は　５V出力時は、センサー故障とECUがみなし故障モードになり、故障がメモリーにインプットされる。
    --                水温センサー故障時は冷間時の始動不良になる。（キャブ車のチョーク作動不良と同じ）
    --                故障モードは、　水温６０℃　吸気温３５℃に設定　　　年式により黒煙を発生するＥＣＵ有り
    -- ステッパーモーター約１５Ω　　７．５度ステップのモーター　アイドリング回転数の調整用。　AC作動時、AT車のR、D時（P、N以外）、
    --                電気負荷の大きい時等アイドル・アップさせる。ハンダ割れ、水浸入による固着故障有り。
    -- PTCヒーター      インテーク・マニホールドを暖めるヒーター　水温が上昇するとOFFする。　時々焼損していて振るとカラカラ音の
    --                発生するPTC有り。冷間時の始動性にはあまり関係無い。排気ガス対策と思われます。理由は、スイッチＯＮでは作動
    --                しません。エンジン始動後ＰＴＣリレーがＯＮする。通電後数秒で手で触れない位熱くなる。
    
    -- [How do lambda sensors work]
    -- https://mechanics.stackexchange.com/questions/23933/how-do-lambda-sensors-work
    Lambda sensors work at elevated temperatures, around 300 °C (600 °F); many lambda sensors contain a resistive heater element to help get them up to temperature quickly.

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
https://en.wikipedia.org/wiki/Air%E2%80%93fuel_ratio_meter#Zirconia_oxygen_sensor


    -- 【ECU仕様の知識】
    -- https://www.minimania.com/A_Basic_Guide_to_Electronic_Fuel_Injection_for_Minis より
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
 

     -- https://blogs.yahoo.co.jp/dmxbd452/5751726.html
    -- http://www.minispares.com/product/Classic/MNE101070.aspx
    -- Part number	Manual / Automatic	Attributes	              VIN No. From - VIN No. To 
    -- MNE10026	 Automatic	SPI -         Except Cooper	 	          - 59844
    -- MNE10027	 Manual    	SPI - Japan - Except Cooper	          	- 60487
    -- MNE10097	 Manual	    SPI -         Except Cooper - 1992-93	 	- 59586
    -- MNE10090	 Automatic	SPI -         Except Cooper	            59845	68084
    -- MNE101060 Automatic	SPI -         Except Cooper	            68085	103112
    -- MNE10092	 Manual	    SPI -         Cooper	                  60488	69492
    -- MNE10089	 Manual	    SPI - Except Cooper	                    59587	67377
    -- MNE101040 Manual	    SPI - Except Cooper	67378	103112
    -- MNE101150 Manual	    SPI - Except Cooper	103113	134454
    -- MNE101160 Automatic	SPI - Except Cooper	103113	134454
    -- MNE101350 Manual	    SPI - Except UK - 1996 on	134455 - 	 
    -- MNE101351 Manual	    SPI - Except UK - 1996 on	134455 - 
    -- MNE101360 Automatic	SPI - Except UK - 1996 on	134455 - 	 
    -- MNE101361 Automatic	SPI - Air Con - Except UK - 1996 on	134455 - 	 
    
    -- ^ original number for size of Frame 7d = 32


module Meters 
(
m4_4,
m2_2,
m3_4,
m5_4,
m3_8,
m6_8,
m9_8,
m12_8,
commonTime,
cutTime,
assignMeter
,MMeter
)
where

import Types
import Tools

import Control.Lens

measure :: Music
measure = [InTime { _t=0, _dur=0, _val= emptyNote & pitch .~ Ly LyMeasureEvent }]

beat :: Music
beat = [InTime { _t=0, _dur=0, _val= emptyNote & pitch .~ Ly LyBeatEvent }]

-- some shortcuts:
beatAt :: Duration -> Music
beatAt d = startMusicAt d beat

beat2 = beatAt (1/4)
beat3 = beatAt (2/4)
beat4 = beatAt (3/4)
beat5 = beatAt (4/4)

type MMeter = (Music,Duration)

m4_4 :: MMeter
m4_4 = (concat [measure,beat,beat2,beat3,beat4], 4/4)
commonTime = m4_4

m2_2 :: MMeter
m2_2 = (concat [measure,beat,beat3], 2/2)
cutTime = m2_2

m3_4 :: MMeter
m3_4 = (concat [measure,beat,beat2,beat3], 3/4)

m2_4 :: MMeter
m2_4 = (concat [measure,beat,beat2], 2/4)

m5_4 :: MMeter
m5_4 = (concat [measure,beat,beat2,beat3,beat4,beat5], 5/4)

m3_8 :: MMeter
m3_8 = (concat [measure,beat], 3/8)

m6_8 :: MMeter
m6_8 = (concat [measure,beat,beatAt $ 3/8], 6/8)

m9_8 :: MMeter
m9_8 = (concat [measure,beat,beatAt $ 3/8,beatAt $ 6/8], 9/8)

m12_8 :: MMeter
m12_8 = (concat [measure,beat,beatAt $ 3/8,beatAt $ 6/8,beatAt $ 9/8], 12/8)

-- 5/8 possibilities
m3'2_8 :: MMeter
m3'2_8 = (concat [measure,beat,beatAt $ 3/8], 5/8)

m2'3_8 :: MMeter
m2'3_8 = (concat [measure,beat,beat2], 5/8)

-- 7/8 possibilities
m3'3'2_8 :: MMeter
m3'3'2_8 = (concat [measure,beat,beatAt $ 3/8,beatAt $ 6/8], 7/8)

m3'2'3_8 :: MMeter
m3'2'3_8 = (concat [measure,beat,beatAt $ 3/8,beatAt $ 5/8], 7/8)

m2'3'3_8 :: MMeter
m2'3'3_8 = (concat [measure,beat,beatAt $ 2/8,beatAt $ 5/8], 7/8)

assignMeter :: MMeter -> Music -> Music
assignMeter = assignMeterWithLine "all"

assignMeterWithLine :: String -> MMeter -> Music -> Music
assignMeterWithLine s (rawmeter,duration) music = music ++ (meterTrack $ getStartTime music) where
    meter = assignLine s rawmeter
    endTime = getEndTime music
    meterTrack :: PointInTime -> Music
    meterTrack p = 
        if p >= endTime
        then []
        else meter ++ meterTrack (p+duration)

-- to render changes in meter, we need to go from measure and beat
-- events to Meters

type MeterSignature = ([PointInTime],Duration)

getMeterSignature :: MMeter -> MeterSignature
getMeterSignature (m, d) = let
    beats = m^.ofType (lyBeatEventType)
    in (beats^..traverse.t,d)

meterTable :: [(MeterSignature,LyMeterEvent)]
meterTable = [
     (getMeterSignature m4_4, LyMeterEvent $ Meter 4 4 )
    ,(getMeterSignature m2_2, LyMeterEvent $ Meter 2 2 )
    ,(getMeterSignature m2_4, LyMeterEvent $ Meter 2 4 )
    ,(getMeterSignature m3_4, LyMeterEvent $ Meter 3 4 )
    ,(getMeterSignature m5_4, LyMeterEvent $ Meter 5 4 )
    ,(getMeterSignature m3_8, LyMeterEvent $ Meter 3 8 )
    ,(getMeterSignature m6_8, LyMeterEvent $ Meter 6 8 )
    ,(getMeterSignature m9_8, LyMeterEvent $ Meter 9 8 )
    ,(getMeterSignature m12_8, LyMeterEvent $ Meter 12 8 )
    ,(getMeterSignature m2'3_8, LyMeterEvent $ Meter 5 8 )
    ,(getMeterSignature m3'2_8, LyMeterEvent $ Meter 5 8 )
    ,(getMeterSignature m3'3'2_8, LyMeterEvent $ Meter 7 8 )
    ,(getMeterSignature m3'2'3_8, LyMeterEvent $ Meter 7 8 )
    ,(getMeterSignature m2'3'3_8, LyMeterEvent $ Meter 7 8 )
    ]


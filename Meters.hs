module Meters 
(
m44,
m22,
m34,
m54,
m38,
m68,
m98,
m128,
commonTime,
cutTime,
assignMeter
,MMeter
)
where

import Types
import Output.Render -- for the Renderable instances
import Tools

import Control.Lens

measure :: Music
measure = [InTime { _t=0, _dur=0, _val= emptyNote & pitch .~ Ly LyMeasureEvent }]

beat :: Music
beat = [InTime { _t=0, _dur=0, _val= emptyNote & pitch .~ Ly LyBeatEvent }]

-- some shortcuts:
beat2 = startMusicAt 1 beat
beat2dot = startMusicAt (3/2) beat
beat3 = startMusicAt 2 beat
beat3dot = beat4
beat4 = startMusicAt 3 beat
beat5 = startMusicAt 4 beat
beat4dot = startMusicAt (9/2) beat

type MMeter = (Music,Duration)

m44 :: MMeter
m44 = (concat [measure,beat,beat2,beat3,beat4], 4)
commonTime = m44

m22 :: MMeter
m22 = (concat [measure,beat,beat3], 4)
cutTime = m22

m34 :: MMeter
m34 = (concat [measure,beat,beat2,beat3], 3)

m24 :: MMeter
m24 = (concat [measure,beat,beat2], 2)

m54 :: MMeter
m54 = (concat [measure,beat,beat2,beat3,beat4,beat5], 5)

m38 :: MMeter
m38 = (concat [measure,beat], 3/2)

m68 :: MMeter
m68 = (concat [measure,beat,beat2dot], 3)

m98 :: MMeter
m98 = (concat [measure,beat,beat2dot,beat3dot], 9/2)

m128 :: MMeter
m128 = (concat [measure,beat,beat2dot,beat3dot,beat4dot], 6)

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
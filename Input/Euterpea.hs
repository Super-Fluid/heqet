module Input.Euterpea where

import Heqet
import qualified Euterpea as E

import Control.Lens

fromEu :: E.Music E.Note1 -> Music
fromEu (E.Prim (E.Note dur ((pc, oct), nas))) = let 
       (hegetPC,hegetAcc) = convertPC pc
       n = emptyNote 
       	   & pitch .~ Ly (LyPitch MakePitch { _pc = hegetPC, _oct = oct, _cents = 0 })
	   & acc .~ Just hegetAcc
       it = InTime { _val = n, _dur = dur, _t = 0 }
       in [it]
fromEu (E.Prim (E.Rest dur)) = []
fromEu (m1 E.:+: m2) = []
fromEu (m1 E.:=: m2) = fromEu m1 ++ fromEu m2 -- just smoosh them together
fromEu (E.Modify (E.Tempo tempo) m) = fromEu m
fromEu (E.Modify (E.Transpose p) m) = transpose (absPitch2Pitch p) (fromEu m)
fromEu (E.Modify (E.Instrument i) m) = (fromEu m) & traverse.val.inst .~ Just (getEuInst i)
fromEu (E.Modify (E.Phrase pas) m) = []
fromEu (E.Modify (E.Player s) m) = []
fromEu (E.Modify (E.KeySig pitchclass mode) m) = (fromEu m) & traverse.val.key .~ Just (convertKey pitchclass mode)

absPitch2Pitch :: E.AbsPitch -> Pitch
absPitch2Pitch = error "not implimented yet" -- TODO

convertKey :: E.PitchClass -> E.Mode -> (PitchClass, Mode, Maybe Accidental)
convertKey pitchclass mode = let
	   (pc', acc) = convertPC pitchclass
	   mode' = case mode of
	   	 E.Major -> MajorM
		 E.Minor -> MinorM
	   in (pc',mode',Just acc)

convertPC :: E.PitchClass -> (PitchClass, Accidental)
convertPC  E.Cff = (As,DoubleFlat)
convertPC  E.Cf = (B,Flat)
convertPC  E.C = (C,Natural)
convertPC  E.Dff = (C,DoubleFlat)
convertPC  E.Cs = (Cs,Sharp)
convertPC  E.Df = (Cs,Flat)
convertPC  E.Css = (D,DoubleSharp)
convertPC  E.D = (D,Natural)
convertPC  E.Eff = (D,DoubleFlat)
convertPC  E.Ds = (Ds,Sharp)
convertPC  E.Ef = (Ds,Flat)
convertPC  E.Fff = (Ds,DoubleFlat)
convertPC  E.Dss = (E,DoubleSharp)
convertPC  E.E = (E,Natural)
convertPC  E.Ff = (E,Flat)
convertPC  E.Es = (F,Sharp)
convertPC  E.F = (F,Natural)
convertPC  E.Gff = (F,DoubleFlat)
convertPC  E.Ess = (Fs,DoubleSharp)
convertPC  E.Fs = (Fs,Sharp)
convertPC  E.Gf = (Fs,Flat)
convertPC  E.Fss = (G,DoubleSharp)
convertPC  E.G = (G,Natural)
convertPC  E.Aff = (G,DoubleFlat)
convertPC  E.Gs = (Gs,Sharp)
convertPC  E.Af = (Gs,Flat)
convertPC  E.Gss = (A,DoubleSharp)
convertPC  E.A = (A,Natural)
convertPC  E.Bff = (A,DoubleFlat)
convertPC  E.As = (As,Sharp)
convertPC  E.Bf = (As,Flat)
convertPC  E.Ass = (B,DoubleSharp)
convertPC  E.B = (B,Natural)
convertPC  E.Bs = (C,Sharp)
convertPC  E.Bss = (Cs,DoubleSharp)

applicableAttribute :: E.NoteAttribute -> (Note a -> Note a)
applicableAttribute (E.Volume i) = (& dynamic .~ Just (fromIntegral i / 127))
applicableAttribute (E.Fingering i) = (& noteCommands %~ (("-"++show i):))
applicableAttribute (E.Dynamics s) = (& noteCommands %~ (("-"++show s):))
applicableAttribute (E.Params ds) = (& noteCommands %~ (("-\"Params:"++show ds++"\""):))

{-

data PhraseAttribute
  = Dyn Euterpea.Dynamic
  | Tmp Euterpea.Tempo
  | Art Articulation
  | Orn Ornament

data Euterpea.Dynamic
  = Euterpea.Accent Rational
  | Crescendo Rational
  | Diminuendo Rational
  | StdLoudness StdLoudness
  | Loudness Rational

data Euterpea.Tempo = Ritardando Rational | Accelerando Rational

Euterpea.Note Dur a | Euterpea.Rest Dur

data Ornament
  = Trill
  | Mordent
  | InvMordent
  | DoubleMordent
  | Turn
  | TrilledTurn
  | ShortTrill
  | Arpeggio
  | ArpeggioUp
  | ArpeggioDown
  | Instruction String
  | Head NoteHead
  | DiatonicTrans Int

data StdLoudness = PPP | PP | P | MP | SF | MF | NF | FF | FFF

data Articulation
  = Euterpea.Staccato Rational
  | Legato Rational
  | Slurred Rational
  | Euterpea.Tenuto
  | Euterpea.Marcato
  | Pedal
  | Fermata
  | FermataDown
  | Breath
  | DownBow
  | UpBow
  | Harmonic
  | Pizzicato
  | LeftPizz
  | BartokPizz
  | Swell
  | Wedge
  | Thumb
  | Euterpea.Stopped

data InstrumentName
  = 
  | 
  | 
  | 
  | 
  | ChorusedPiano
  | Harpsichord
  | Clavinet
  | Celesta
  | Glockenspiel
  | MusicBox
  | Vibraphone
  | Marimba
  | Xylophone
  | TubularBells
  | Dulcimer
  | HammondOrgan
  | PercussiveOrgan
  | RockOrgan
  | ChurchOrgan
  | ReedOrgan
  | Accordion
  | Harmonica
  | TangoAccordion
  | AcousticGuitarNylon
  | AcousticGuitarSteel
  | ElectricGuitarJazz
  | ElectricGuitarClean
  | ElectricGuitarMuted
  | OverdrivenGuitar
  | DistortionGuitar
  | GuitarHarmonics
  | AcousticBass
  | ElectricBassFingered
  | ElectricBassPicked
  | FretlessBass
  | SlapBass1
  | SlapBass2
  | SynthBass1
  | SynthBass2
  | 
  | 
  | 
  | 
 TremoloStrings
 PizzicatoStrings
 OrchestralHarp
 Timpani
  | StringEnsemble1
  | StringEnsemble2
  | SynthStrings1
  | SynthStrings2
  | ChoirAahs
  | VoiceOohs
  | SynthVoice
  | OrchestraHit
  | 
  | 
  | 
  | MutedTrumpet
  | 
  | BrassSection
  | SynthBrass1
  | SynthBrass2
  | 
  | 
  | 
  | Recorder
  | PanFlute
  | BlownBottle
  | Shakuhachi
  | Whistle
  | Ocarina
  | Lead1Square
  | Lead2Sawtooth
  | Lead3Calliope
  | Lead4Chiff
  | Lead5Charang
  | Lead6Voice
  | Lead7Fifths
  | Lead8BassLead
  | Pad1NewAge
  | Pad2Warm
  | Pad3Polysynth
  | Pad4Choir
  | Pad5Bowed
  | Pad6Metallic
  | Pad7Halo
  | Pad8Sweep
  | FX1Train
  | FX2Soundtrack
  | FX3Crystal
  | FX4Atmosphere
  | FX5Brightness
  | FX6Goblins
  | FX7Echoes
  | FX8SciFi
  | Sitar
  | Banjo
  | Shamisen
  | Koto
  | Kalimba
  | Bagpipe
  | Fiddle
  | Shanai
  | TinkleBell
  | Agogo
  | SteelDrums
  | Woodblock
  | TaikoDrum
  | MelodicDrum
  | SynthDrum
  | ReverseCymbal
  | GuitarFretNoise
  | BreathNoise
  | Seashore
  | BirdTweet
  | TelephoneRing
  | Helicopter
  | Applause
  | Gunshot
  | Percussion
  | Custom String


-}

getEuInst :: E.InstrumentName -> Instrument
getEuInst E.Violin = violin
getEuInst E.Viola = viola
getEuInst E.Cello = cello
getEuInst E.Contrabass = string_bass
getEuInst E.Trumpet = trumpet
getEuInst E.FrenchHorn = horn
getEuInst E.Trombone = trombone
getEuInst E.Tuba = tuba
getEuInst E.SopranoSax = soprano_sax
getEuInst E.AltoSax = alto_sax
getEuInst E.TenorSax = tenor_sax
getEuInst E.BaritoneSax = baritone_sax
getEuInst E.Oboe = oboe
getEuInst E.Bassoon = bassoon
--getEuInst E.EnglishHorn
getEuInst E.Clarinet = clarinet
--getEuInst E.Piccolo
getEuInst E.Flute = flute
getEuInst E.AcousticGrandPiano = piano
getEuInst E.BrightAcousticPiano = piano
getEuInst E.ElectricGrandPiano = piano
getEuInst E.HonkyTonkPiano = piano
getEuInst E.RhodesPiano = piano
getEuInst (E.Custom s) = melody & name .~ s
getEuInst _ = melody
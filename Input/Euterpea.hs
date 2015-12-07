module Input.Euterpea where

import Heqet
import qualified Euterpea as E
import qualified Dynamics

import Control.Lens

fromEu :: E.Music E.Pitch -> Music
fromEu (E.Prim (E.Note dur (pc, oct))) = let 
       (hegetPC,hegetAcc) = convertPC pc
       n = emptyNote 
       	   & pitch .~ Ly (LyPitch MakePitch { _pc = hegetPC, _oct = oct - 4, _cents = 0 })
	   & acc .~ Just hegetAcc
       it = InTime { _val = n, _dur = dur, _t = 0 }
       in [it]
fromEu (E.Prim (E.Rest dur)) = [InTime { _val = emptyNote & pitch .~ Ly (LyRest), _dur = dur, _t = 0}]
fromEu (m1 E.:+: m2) = (fromEu m1) `seqI` (fromEu m2)
fromEu (m1 E.:=: m2) = fromEu m1 ++ fromEu m2 -- just smoosh them together
fromEu (E.Modify (E.Tempo tempo) m) = fromEu m
fromEu (E.Modify (E.Transpose p) m) = transpose (absPitch2Pitch p) (fromEu m)
fromEu (E.Modify (E.Instrument i) m) = (fromEu m) & traverse.val.inst .~ Just (getEuInst i)
fromEu (E.Modify (E.Phrase pas) m) = foldl (&) (fromEu m) (map applyEuPhraseAttr pas)
-- apply all phrase attribute application functions
fromEu (E.Modify (E.Player s) m) = (fromEu m) & traverse.val.line .~ Just s
fromEu (E.Modify (E.KeySig pitchclass mode) m) = (fromEu m) & traverse.val.key .~ Just (convertKey pitchclass mode)

fromEu1 :: E.Music E.Note1 -> Music
fromEu1 (E.Prim (E.Note dur ((pc, oct), nas))) = let 
       (hegetPC,hegetAcc) = convertPC pc
       n = emptyNote 
       	   & pitch .~ Ly (LyPitch MakePitch { _pc = hegetPC, _oct = oct, _cents = 0 })
	   & acc .~ Just hegetAcc
       it = InTime { _val = n, _dur = dur, _t = 0 }
       in foldl (&) [it] (map applyEuNoteAttr nas) -- apply all note attributes to the note 
fromEu1 (E.Prim (E.Rest dur)) = [InTime { _val = emptyNote & pitch .~ Ly (LyRest), _dur = dur, _t = 0}]
fromEu1 (m1 E.:+: m2) = (fromEu1 m1) `seqI` (fromEu1 m2)
fromEu1 (m1 E.:=: m2) = fromEu1 m1 ++ fromEu1 m2 -- just smoosh them together
fromEu1 (E.Modify (E.Tempo tempo) m) = fromEu1 m
fromEu1 (E.Modify (E.Transpose p) m) = transpose (absPitch2Pitch p) (fromEu1 m)
fromEu1 (E.Modify (E.Instrument i) m) = (fromEu1 m) & traverse.val.inst .~ Just (getEuInst i)
fromEu1 (E.Modify (E.Phrase pas) m) = foldl (&) (fromEu1 m) (map applyEuPhraseAttr pas)
-- apply all phrase attribute application functions
fromEu1 (E.Modify (E.Player s) m) = (fromEu1 m) & traverse.val.line .~ Just s
fromEu1 (E.Modify (E.KeySig pitchclass mode) m) = (fromEu1 m) & traverse.val.key .~ Just (convertKey pitchclass mode)

applyEuNoteAttr :: E.NoteAttribute -> (Music -> Music)
applyEuNoteAttr (E.Volume i) = (& traverse.val.dynamic .~ Just (fromIntegral i / 128))
applyEuNoteAttr (E.Fingering _) = id -- not supported
applyEuNoteAttr (E.Dynamics s) = applyNoteCommand $ "^\\markup{dynamic: "++s++"}" -- make better!
applyEuNoteAttr (E.Params _) = id -- ???

absPitch2Pitch :: E.AbsPitch -> Pitch
absPitch2Pitch = error "not implimented yet" -- TODO

convertKey :: E.PitchClass -> E.Mode -> (PitchClass, Mode, Maybe Accidental)
convertKey pitchclass mode = let
	   (pc', acc) = convertPC pitchclass
	   mode' = case mode of
	   	 E.Major -> MajorM
		 E.Minor -> MinorM
	   in (pc',mode',Just acc)

applyEuPhraseAttr :: E.PhraseAttribute -> (Music -> Music)
applyEuPhraseAttr (E.Dyn dyn) = applyEuDynamic dyn
applyEuPhraseAttr (E.Tmp tmp) = applyEuTempo tmp
applyEuPhraseAttr (E.Art art) = applyEuArticulation art
applyEuPhraseAttr (E.Orn orn) = applyEuOrnament orn

applyEuDynamic :: E.Dynamic -> (Music -> Music)
applyEuDynamic (E.Accent _) = applyArt Accent
applyEuDynamic (E.Crescendo _) = id -- TODO
applyEuDynamic (E.Diminuendo _) = id -- TODO
applyEuDynamic (E.StdLoudness loud) = applyEuStdLoudness loud
applyEuDynamic (E.Loudness r) = let loudness = (1 `min` r) `max` 0 -- confine to interval [0,1]
	       in (& traverse.val.dynamic .~ Just (fromRational loudness))

applyEuStdLoudness :: E.StdLoudness -> (Music -> Music)
applyEuStdLoudness E.PPP = applyDynamic Dynamics.ppp
applyEuStdLoudness E.PP = applyDynamic Dynamics.pp
applyEuStdLoudness E.P = applyDynamic Dynamics.p
applyEuStdLoudness E.MP = applyDynamic Dynamics.mp
applyEuStdLoudness E.SF = applyDynamic Dynamics.f -- ????
applyEuStdLoudness E.MF = applyDynamic Dynamics.mf
applyEuStdLoudness E.NF = applyDynamic Dynamics.mf -- nf == mf ???
applyEuStdLoudness E.FF = applyDynamic Dynamics.ff
applyEuStdLoudness E.FFF = applyDynamic Dynamics.fff

applyEuTempo :: E.Tempo -> (Music -> Music)
applyEuTempo _ = id -- TODO

applyEuArticulation :: E.Articulation -> (Music -> Music)
applyEuArticulation (E.Staccato _) = applyArt Staccato
applyEuArticulation (E.Legato _) = id -------------------------------
applyEuArticulation (E.Slurred _) = id --------------------------------
applyEuArticulation (E.Tenuto) = applyArt Tenuto
applyEuArticulation (E.Marcato) = applyArt Marcato
applyEuArticulation (E.Pedal) = id -- TODO
applyEuArticulation (E.Fermata) = applyNoteCommand "\\fermata"
applyEuArticulation (E.FermataDown) = applyNoteCommand "\\fermata"
 -- same as fermata; orientation of symbols is not within the scope of Euterpea
applyEuArticulation (E.Breath) = id -- not sure how to apply a breath mark...
applyEuArticulation (E.DownBow) = applyNoteCommand "\\downbow"
applyEuArticulation (E.UpBow) = applyNoteCommand "\\upbow"
applyEuArticulation (E.Harmonic) = applyNoteCommand "\\flagolet"
applyEuArticulation (E.Pizzicato) = id -- TODO
applyEuArticulation (E.LeftPizz) = id -- TODO
applyEuArticulation (E.BartokPizz) = id -- TODO
applyEuArticulation (E.Swell) = applyNoteCommand "\\espressivo"
applyEuArticulation (E.Wedge)  = applyArt Marcato -- right?
applyEuArticulation (E.Thumb) = applyNoteCommand "\\thumb"
applyEuArticulation (E.Stopped) = applyArt Stopped

applyEuOrnament :: E.Ornament -> (Music -> Music)
applyEuOrnament (E.Trill) = applyNoteCommand "\\trill"
applyEuOrnament (E.Mordent) = applyNoteCommand "\\prall"
applyEuOrnament (E.InvMordent) = applyNoteCommand "\\mordent"
applyEuOrnament (E.DoubleMordent) = applyNoteCommand "\\prallprall"
applyEuOrnament (E.Turn) = applyNoteCommand "\\turn"
applyEuOrnament (E.TrilledTurn) = id -- TODO
applyEuOrnament (E.ShortTrill) = applyNoteCommand "\\trill" -- make different from trill?
applyEuOrnament (E.Arpeggio) = id -- TODO
applyEuOrnament (E.ArpeggioUp) = id -- TODO
applyEuOrnament (E.ArpeggioDown) = id --TODO
applyEuOrnament (E.Instruction s) = applyNoteCommand $ "\\markup{"++s++"}"
applyEuOrnament (E.Head head) = id -- probably not supported
applyEuOrnament (E.DiatonicTrans i) = id -- TODO

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
applicableAttribute (E.Volume i) = (& dynamic .~ Just (fromIntegral i / 128))
applicableAttribute (E.Fingering i) = (& noteCommands %~ (("-"++show i):))
applicableAttribute (E.Dynamics s) = (& noteCommands %~ (("-"++show s):))
applicableAttribute (E.Params ds) = (& noteCommands %~ (("-\"Params:"++show ds++"\""):))

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


{-

data Euterpea.Tempo = Ritardando Rational | Accelerando Rational

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

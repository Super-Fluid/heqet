module EuterpeaInterface where

import Heqet
import qualified Euterpea as E

import Control.Lens

fromEu :: E.Music E.Note1 -> Music
fromEu (Prim (E.Note dur a)) = []
fromEu (Prim (E.Rest dur)) = []
fromEu (m1 E.:+: m2) = []
fromEu (m1 E.:=: m2) = fromEu m1 ++ fromEu m2 -- just smoosh them together
fromEu (Modify (Tempo tempo) m) = []
fromEu (Modify (Transpose p) m) = []
fromEu (E.Instrument inst) = []
fromEu (Phrase pas m) = []
fromEu (Player s) = []
fromEu (KeySig pc mode m) = []
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
  = AcousticGrandPiano
  | BrightAcousticPiano
  | ElectricGrandPiano
  | HonkyTonkPiano
  | RhodesPiano
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
  | Violin
  | Viola
  | Cello
  | Contrabass
  | TremoloStrings
  | PizzicatoStrings
  | OrchestralHarp
  | Timpani
  | StringEnsemble1
  | StringEnsemble2
  | SynthStrings1
  | SynthStrings2
  | ChoirAahs
  | VoiceOohs
  | SynthVoice
  | OrchestraHit
  | Trumpet
  | Trombone
  | Tuba
  | MutedTrumpet
  | FrenchHorn
  | BrassSection
  | SynthBrass1
  | SynthBrass2
  | SopranoSax
  | AltoSax
  | TenorSax
  | BaritoneSax
  | Oboe
  | Bassoon
  | EnglishHorn
  | Clarinet
  | Piccolo
  | Flute
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
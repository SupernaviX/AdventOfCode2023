module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Activation = On | Off deriving (Eq, Show)
data Pulse = High | Low deriving (Eq, Show)

data ModuleState =
    FlipFlop Activation
  | Conjunction (M.Map T.Text Pulse)
  | Broadcaster
  | Receiver Activation
  | Untyped
  deriving (Show)

data Module = Module {
  state :: ModuleState,
  outputs :: [T.Text]
} deriving (Show)

type Modules = M.Map T.Text Module

parseModule :: T.Text -> (T.Text, Module)
parseModule line =
  let [descriptor, rawOutputs] = T.splitOn (T.pack " -> ") line
      (name, typ)
        | descriptor == T.pack "broadcaster" = (descriptor, Broadcaster)
        | T.index descriptor 0 == '%'        = (T.drop 1 descriptor, FlipFlop Off)
        | T.index descriptor 0 == '&'        = (T.drop 1 descriptor, Conjunction M.empty)
        | otherwise                          = (descriptor, Untyped)
      outputs = T.splitOn (T.pack ", ") rawOutputs
  in (name, Module typ outputs)

parseModules :: T.Text -> Modules
parseModules input =
  let modules = map parseModule (T.lines input)
      inputs = concatMap findInputs modules
      modules' = map (initialize inputs) modules
  in M.fromList modules'
  where
    findInputs (name, Module _ outputs) = map (,name) outputs
    initialize inputs (name, Module (Conjunction _) outputs) =
      let inputs' = M.fromList $ map (\mapping -> (snd mapping, Low)) $ filter (\mapping -> fst mapping == name) inputs
      in (name, Module (Conjunction inputs') outputs)
    initialize _ x = x

mashButton :: Modules -> [(Modules, Int, Int)]
mashButton modules = iterate pushButton (modules, 0, 0)

pushButton :: (Modules, Int, Int) -> (Modules, Int, Int)
pushButton (modules, highs, lows) = handlePulse modules highs lows [(T.pack "button", T.pack "broadcaster", Low)]
  where
    handlePulse modules highs lows [] = (modules, highs, lows)
    handlePulse modules highs lows ((from, to, pulse):pulses) =
      let modul = M.findWithDefault (Module Untyped []) to modules
          (modul', outPulses) = processPulse modul from pulse
          modules' = M.insert to modul' modules
          (highs', lows') = case pulse of
            High -> (highs + 1, lows)
            Low -> (highs, lows + 1)
          pulses' = pulses ++ map (\(t, p) -> (to, t, p)) outPulses
      in handlePulse modules' highs' lows' pulses'

processPulse :: Module -> T.Text -> Pulse -> (Module, [(T.Text, Pulse)])
processPulse (Module state outputs) from pulse =
  let (state', pulse') = processPulse' state from pulse
      pulses = case pulse' of
        Just p -> map (,p) outputs
        Nothing -> []
  in (Module state' outputs, pulses)

processPulse' :: ModuleState -> T.Text -> Pulse -> (ModuleState, Maybe Pulse)
processPulse' Broadcaster _ pulse = (Broadcaster, Just pulse)
processPulse' (FlipFlop state) _ High = (FlipFlop state, Nothing)
processPulse' (FlipFlop Off) _ Low = (FlipFlop On, Just High)
processPulse' (FlipFlop On) _ Low = (FlipFlop Off, Just Low)
processPulse' (Conjunction states) from pulse =
  let states' = M.insert from pulse states
      pulse' = if all (== High) (M.elems states') then Low else High
  in (Conjunction states', Just pulse')
processPulse' (Receiver _) _ High = (Receiver Off, Nothing)
processPulse' (Receiver _) _ Low = (Receiver On, Nothing) 
processPulse' Untyped _ pulse = (Untyped, Nothing)

part1 :: Modules -> Int
part1 modules =
  let (_, highs, lows) = mashButton modules !! 1000
  in highs * lows

main = do
  input <- TIO.readFile "input"
  let modules = parseModules input
  print $ part1 modules

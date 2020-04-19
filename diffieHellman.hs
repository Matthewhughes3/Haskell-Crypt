import PrimeGen (getPrime, randomBits, modExp)
import System.Random

defaultBits :: Integer
defaultBits = 256

-- EXAMPLE
-- g, p, gA, and gB are exchanged publicly, a and b are kept secret
-- by each party in the conversation. Since gAB and gBA are equal,
-- each party is able to arrive at a symmetric
-- key without exchanging private variables a and b.
-- To break, an attacker must derrive a from gA or b from gB.
-- Because of the modulus p, this is extremely difficult for large numbers.
main = do
  gen <- getStdGen
  let [g,a,b] = take 3 $ randomBits defaultBits gen
  let p = getPrime defaultBits gen
  let gA = modExp g a p
  let gB = modExp g b p
  let gBA = modExp gB a p
  let gAB = modExp gA b p
  putStrLn $ show gAB
  putStrLn $ show gBA

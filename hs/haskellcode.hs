 
import qualified Data.Time.Clock as C
import qualified System.Environment as SE
import qualified Data.Number.LogFloat as LF
import qualified Data.Vector.Unboxed as UV
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Read as TR  main :: IO ()
main = do
  twds <- SE.getArgs
  let [tps, wps, vf, wf, df, du, ouf] = twds
  let twd = Prelude.map read twds :: [Int]
  v_st <- TIO.readFile vf--"../hakaru-rktjit/arg3.csv"
  words_st <- TIO.readFile wf--"../hakaru-rktjit/arg4.csv"
  docs_st <- TIO.readFile df--"../hakaru-rktjit/arg5.csv"
  let topic_prior = (Prelude.map LF.logFloat (replicate (read tps) 1.0))
  let word_prior = (Prelude.map LF.logFloat (replicate (read wps) 1.0))
  let v = (Prelude.map (read . T.unpack) (T.split (== '
') v_st)) :: [Int]
  let words = (Prelude.map (read . T.unpack) (T.split (== '
') words_st)) :: [Int]
  let docs = (Prelude.map (read . T.unpack) (T.split (== '
') docs_st)) :: [Int]
  let docUpdate = (read du :: Int)
  start_time <- C.getCurrentTime
  let result = (UV.map LF.logFromLogFloat
             (prog
              (UV.fromList topic_prior)
              (UV.fromList word_prior)
              (UV.fromList v)
              (UV.fromList words)
              (UV.fromList docs)
               docUpdate))
  end_time <-C.getCurrentTime
  print "result:"
  print result
  TIO.writeFile ouf (T.unlines (Prelude.map T.pack (Prelude.map show (UV.toList result)))) 
  print "time:"
  print $ C.diffUTCTime end_time start_time

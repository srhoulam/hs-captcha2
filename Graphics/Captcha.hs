module Graphics.Captcha where

import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Char
import           GHC.Arr
import           Graphics.GD
import           System.Random

data Theme = Light | Dark | StripedHoriz | StripedVert

data CaptchaConfig = CaptchaConfig
  { ccChirpHorizontally :: Bool
  , ccChirpVertically   :: Bool
  , ccTheme             :: Theme
  , ccStripes           :: Int
  , ccLength            :: Int
  , ccMinChirpDepth     :: Float
  , ccMaxChirpDepth     :: Float
  , ccFontName          :: String
  , ccFontSize          :: Double
  , ccCaptchaSize       :: Int
  , ccFinalSize         :: (Int, Int)
  }

-- | Use this as a starting point in your own custom configuration. Then, in
-- future versions with new options, your code won't break, but use the defaults.
defaultConfig = CaptchaConfig
  { ccChirpHorizontally = False
  , ccChirpVertically = True
  , ccTheme = StripedHoriz
  , ccStripes = 2
  , ccLength = 6
  , ccMinChirpDepth = 1.0
  , ccMaxChirpDepth = 2.5
  , ccFontName = "Courier New"
  , ccFontSize = 22.0
  , ccCaptchaSize = 192
  , ccFinalSize = (128, 64)
  }

alphabet :: Array Int Char
alphabet = listArray (0, 57) "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

main :: IO ()
main = mainCfg defaultConfig

-- | This is the recommended way to test a desired captcha configuration before
-- using it in your app.
mainCfg :: CaptchaConfig -> IO ()
mainCfg cfg = do
  string <- makeRandomString $ ccLength cfg
  image <- createInitialImage cfg string
  chirpDoubleRandom cfg image
  chirpDoubleRandom cfg image
  image <- cropToFinalSize cfg image
  putStrLn string
  savePngFile "captcha.png" image

-- | Make a captcha using the provided configuration. Returns a pair: the
-- solution, and the captcha image as a PNG bytestring.
makeCaptcha :: CaptchaConfig -> IO (String, ByteString)
makeCaptcha cfg = do
  string <- makeRandomString $ ccLength cfg
  image <- createInitialImage cfg string
  chirpDoubleRandom cfg image
  chirpDoubleRandom cfg image
  image <- cropToFinalSize cfg image
  byteString <- savePngByteString image
  return (string, byteString)


makeRandomString :: Int -> IO String
makeRandomString len = mapM (const makeRandomLetter) [1..len]
  where makeRandomLetter = do
          n <- randomRIO (0, 57)
          return $ alphabet ! n


chirpDoubleRandom :: CaptchaConfig -> Image -> IO ()
chirpDoubleRandom cfg image = do
  when (ccChirpVertically cfg) $ do
    depth1 <- randomRIO (ccMinChirpDepth cfg, ccMaxChirpDepth cfg)
    period1 <- randomRIO (1, 6)
    period2 <- randomRIO (1, 6)
    let captchaSizeF = fromIntegral $ ccCaptchaSize cfg
    chirpVertically
      (ccCaptchaSize cfg)
      (makeChirpFunction
        (ccCaptchaSize cfg)
        depth1
        (captchaSizeF / period1)
        (captchaSizeF / period2))
      image

  when (ccChirpHorizontally cfg) $ do
    depth2 <- randomRIO (ccMinChirpDepth cfg, ccMaxChirpDepth cfg)
    period3 <- randomRIO (1, 6)
    period4 <- randomRIO (1, 6)
    let captchaSizeF = fromIntegral $ ccCaptchaSize cfg
    chirpHorizontally
      (ccCaptchaSize cfg)
      (makeChirpFunction
        (ccCaptchaSize cfg)
        depth2
        (captchaSizeF / period3)
        (captchaSizeF / period4))
      image


chirpHorizontally :: Int -> (Int -> Int) -> Image -> IO ()
chirpHorizontally captchaSize chirpFunction image = do
  withImage (newImage (captchaSize, captchaSize))
    (\temporaryImage -> do
        copyRegion
          (0, 0)
          (captchaSize, captchaSize)
          image
          (0, 0)
          temporaryImage

        forM_ [0 .. captchaSize - 1] $ \i -> copyRegion
          (0, i)
          (captchaSize, 1)
          image
          (chirpFunction i, i)
          temporaryImage

        copyRegion
          (0, 0)
          (captchaSize, captchaSize)
          temporaryImage
          (0, 0)
          image)


chirpVertically :: Int -> (Int -> Int) -> Image -> IO ()
chirpVertically captchaSize chirpFunction image = do
  withImage (newImage (captchaSize, captchaSize))
    (\temporaryImage -> do
        copyRegion
          (0, 0)
          (captchaSize, captchaSize)
          image
          (0, 0)
          temporaryImage

        forM_ [0 .. captchaSize - 1] $ \i -> copyRegion
          (i, 0)
          (1, captchaSize)
          image
          (i, chirpFunction i)
          temporaryImage

        copyRegion
          (0, 0)
          (captchaSize, captchaSize)
          temporaryImage
          (0, 0)
          image)


makeChirpFunction :: Int -> Float -> Float -> Float -> Int -> Int
makeChirpFunction captchaSize depth startingWaveLength endingWaveLength row =
  let waveLength = ((startingWaveLength * fromIntegral row)
                     + (endingWaveLength * fromIntegral (captchaSize - row)))
                   / fromIntegral captchaSize
  in floor $ depth * sin (fromIntegral row * ((2*pi) / waveLength))


createInitialImage :: CaptchaConfig -> String -> IO Image
createInitialImage cfg = case ccTheme cfg of
  Dark         -> createInitialImageSimple True cfg
  Light        -> createInitialImageSimple False cfg
  StripedHoriz -> createInitialImageStriped False cfg
  StripedVert  -> createInitialImageStriped True cfg

createInitialImageStriped :: Bool -> CaptchaConfig -> String -> IO Image
createInitialImageStriped vert cfg string = do
  darkImage <- createInitialImageSimple True cfg string
  lightImage <- createInitialImageSimple False cfg string
  let pxPerStripe = floor $ fromIntegral (ccCaptchaSize cfg) / fromIntegral (ccStripes cfg)
      captchaSize = ccCaptchaSize cfg

  if vert then forM_ (filter even [0 .. ccStripes cfg - 1]) $ \stripe ->
    copyRegion
      (stripe * pxPerStripe, 0)
      (pxPerStripe, captchaSize)
      lightImage
      (stripe * pxPerStripe, 0)
      darkImage
    else forM_ (filter even [0 .. ccStripes cfg - 1]) $ \stripe ->
      copyRegion
        (0, stripe * pxPerStripe)
        (captchaSize, pxPerStripe)
        lightImage
        (0, stripe * pxPerStripe)
        darkImage

  return darkImage

createInitialImageSimple :: Bool -> CaptchaConfig -> String -> IO Image
createInitialImageSimple dark cfg string = do
  let captchaSize = ccCaptchaSize cfg
      fontName = ccFontName cfg
      fontSize = ccFontSize cfg
      foregroundColor = if dark then 0xFFFFFF else 0x000000
      backgroundColor = if dark then 0x000000 else 0xFFFFFF
  image <- newImage (captchaSize, captchaSize)
  useFontConfig True
  drawFilledRectangle (0, 0) (captchaSize, captchaSize) backgroundColor image
  ((left, top), _, (right, bottom), _)
      <- measureString fontName fontSize 0.0 (0, 0) string foregroundColor

  let width = right - left
      height = top - bottom
      originX = (captchaSize - width) `div` 2
      originY = (captchaSize + height) `div` 2
  drawString fontName fontSize 0.0 (originX, originY) string foregroundColor image
  return image


cropToFinalSize :: CaptchaConfig -> Image -> IO Image
cropToFinalSize cfg image = do
  let captchaSize = ccCaptchaSize cfg
      finalSize = ccFinalSize cfg
  result <- newImage finalSize
  copyRegion ( (captchaSize - fst finalSize) `div` 2
             , (captchaSize - snd finalSize) `div` 2)
             finalSize
             image
             (0, 0)
             result
  return result

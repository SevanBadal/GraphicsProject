module Texture where
import Codec.Picture (readImage, generateImage, convertRGBA8, convertRGB8, DynamicImage(..), Image(..), PixelRGB8(..), PixelRGBA8(..))
import Codec.Picture.Types
import Data.Word
import qualified Data.Vector.Storable as VS
import Graphics.GL.Types
import Graphics.GL.Core33 -- ensure support for Version 330

loadImageTexture :: [Char] -> IO (Image PixelRGBA8)
loadImageTexture filePath = do
    eErrDI0 <- readImage filePath
    dyImage0 <- case eErrDI0 of
        Left e -> do
            putStrLn e
            return $ ImageRGBA8 $ generateImage (\x y ->
                let x' = fromIntegral x in (PixelRGBA8 x' x' x' x')) 500 400
        Right di -> return di
    let ipixelrgba80 = convertRGBA8 dyImage0
    return ipixelrgba80

isBlended :: Bool -> IO ()
isBlended True =  glEnable GL_BLEND
isBlended False = glDisable GL_BLEND

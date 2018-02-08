import Data.Complex
import Data.Word
import Codec.Picture

{-
This is my first Haskell project of any magnitude at all, so some of the coding here is not the best.
I will probably update it at some point.
-}

--A single value for use in the Mandelbrot set calculation.
data MPix =
        --The iteration at which the point escaped the r=2 circle
        --This is remembered for the purpose of colouring the picture
        Esc Int

        --The original, then current value of the point.
        --The original value is used as c in the calculations
        | Val {val_init :: Complex Double, val_current :: Complex Double}
        deriving Show

--A rectangle defined by two corners
data Rectangle a = Rectangle{
        x1 :: a,
        y1 :: a,
        x2 :: a,
        y2 :: a
        } deriving Show

--The Mandelbrot Set data is a grid of individual test points
type MSet = [[MPix]]

--Creates a grid of MPix points. Each will be used for one pixel
mSetInit :: Rectangle Double -> (Int, Int) -> MSet
mSetInit bounds squares =
        [ 
                [Val (x :+ y) (0 :+ 0) | x <- [x1 bounds, x1 bounds + fst spacing .. x2 bounds] ]
        | y <- [y2 bounds, y2 bounds - snd spacing .. y1 bounds] ]
        where spacing = ( (x2 bounds - x1 bounds) / (fromIntegral $ fst squares), (y2 bounds - y1 bounds) / (fromIntegral $ snd squares)  )

--The iterated function for the Mandelbrot set
mFunc :: MPix -> MPix
mFunc (Val init a) = Val init (a*a + init)


mIterate :: Int -> MPix -> MPix

--If a value has already escaped the r=2 circle, then do nothing with it!
mIterate _ (Esc a) = Esc a

--Otherwise iterate the function
mIterate iter mpix =
        if (magnitude . val_current $ result) > 2
                then Esc iter --Since it has escaped this iteration
                else result --The updated value
        where result = mFunc mpix



mSetIterateTo' :: Int -> MSet -> MSet
mSetIterateTo' end mset = map (map (mIterateTo 0 end)) mset

mIterateTo :: Int -> Int -> MPix -> MPix
mIterateTo start end mpix
        | start >= end = mpix
        | otherwise    = mIterateTo (start + 1) end (mIterate start mpix)



--Generates an image using the points
genImage :: Rectangle Double -> (Int, Int) -> Int -> Image PixelRGB16
genImage bounds size iter =
        generateImage (
                \ x y -> toPixel $ mset !! y !!x                
        ) (fst size) (snd size)

        where
                mset = mSetIterateTo' iter $ mSetInit bounds (fromIntegral $ fst size, fromIntegral $ snd size)

--Converts the MPix values to Pixels
toPixel :: MPix -> PixelRGB16

--Colour pixels that remain in the set black
toPixel (Val _ _) = PixelRGB16 0 0 0

--Cyclically colour the other pixels based on their mod 6
toPixel (Esc n)
        | cycle == 0 = PixelRGB16 0 0 65535
        | cycle == 1 = PixelRGB16 0 40000 40000
        | cycle == 2 = PixelRGB16 0 65535 0
        | cycle == 3 = PixelRGB16 40000 40000 0
        | cycle == 4 = PixelRGB16 65535 0 0
        | cycle == 5 = PixelRGB16 40000 0 40000
        where cycle = n `mod` 6
        

--The main function is called at the start of the program
--It handles the user input
main = do
        --Take the user input and store it in constants.
        putStrLn "Enter xMin:"
        xMinText <- getLine
        let xMin = read xMinText :: Double

        putStrLn "Enter xMax:"
        xMaxText <- getLine
        let xMax = read xMaxText :: Double

        putStrLn "Enter yMin:"
        yMinText <- getLine
        let yMin = read yMinText :: Double

        putStrLn "Enter yMax:"
        yMaxTest <- getLine
        let yMax = read yMaxTest :: Double

        putStrLn "Enter xSquares"
        xSquaresText <- getLine
        let xSquares = read xSquaresText :: Int

        putStrLn "Enter ySquares"
        ySquaresText <- getLine
        let ySquares = read ySquaresText :: Int

        putStrLn "Enter Iterations"
        iterText <- getLine
        let iter = read iterText :: Int

        putStrLn "Generating image ..."
        
        --Generate the image to the specified parameters
        let img = genImage (Rectangle xMin yMin xMax yMax) (xSquares, ySquares) iter
        savePngImage "TestImg1" (ImageRGB16 img)
        
        putStrLn "Done!"


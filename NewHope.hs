import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace


main :: IO ()
main = play (InWindow "Millenium Falcon" (600, 750) (20, 20)) skyc fps initial escape handleKeys update 


escape :: World-> Picture 
escape x = Pictures [titles, Translate (-250) (-175) $ canon 2 tt, Translate (posx x) (posy x) (falcon tt), houses tt, sky tt, credit]
    where
        tt = time x

-- sky time
-- Translate (-50) 0 $ polygon [(0,0), (75,0), (75,150), (48,150)],

fps = 10

data World= World{
posx :: Float,
posy :: Float,
time :: Float
 
}


initial :: World
initial = World{
posx = 150,
posy = (-275), 
time = 0 

}

handleKeys :: Event -> World-> World
handleKeys (EventKey (SpecialKey KeySpace) (Down) _ _) = boost
handleKeys (EventKey (SpecialKey KeyUp) (Down) _ _) = up
handleKeys (EventKey (SpecialKey KeyDown) (Down) _ _) =  down
handleKeys (EventKey (SpecialKey KeyLeft) (Down) _ _) = left
handleKeys (EventKey (SpecialKey KeyRight) (Down) _ _) =  right
handleKeys _ = id


up, down, left, right :: World-> World
boost game = game {posy = posy game + 200}
up game = game {posy = posy game + 25}
down game = game {posy = posy game - 25}
left game =  game {posx =  posx game - 25} 
right game = game {posx =  posx game + 25}



update :: Float -> World-> World
update dtime game = game {time = time game + dtime}













-- CUSTOM COLOUR PALLET

brownie, sand, burly, peach, skyc, jet1, jet2, jet3, silver, steel, sun1, sun2, dirty, falcol, fal2 :: Color

brownie  = makeColorI 199  111  32 255
sand = makeColorI 244 164 96 150
burly = makeColorI 222 184 135 255
peach = makeColorI 255 228 181 255
brickie = makeColorI 139 69 19 255

skyc = makeColorI 204 229 255 255
jet1 = makeColorI 0 204 204 255
jet2 = makeColorI 0 0 205 255
jet3 = makeColorI 30 144 255 255

steel = makeColorI 128 128 128 255
silver = makeColorI 192 192 192 255


sun1 = makeColorI 255 0 0 150
sun2 = makeColorI 255 255 0 150
dirty = makeColorI 0 102 102 255
falcol = makeColorI 224 224 224 175

fal2 = makeColorI  128 128 128 200


-- HELPER FUNCTIONS

range :: Int -> Int -> Float -> Bool
range lo hi y = lo <= x && x <= hi
               where x = round y
                                         
                      
lighter :: Float -> Color -> Color  
lighter time x | range 0 3 time = x
                | range 3 5 time = bright x
                | range 5 10 time = light x
                | range 10 15 time = light (light x)
                | range 15 20 time = bright (light (light x))
                | otherwise = bright (bright (bright (bright x)))
                        

timer :: Float -> Picture 
timer time = Translate (-50) 200 $ Scale 0.2 0.2 $ Color black $ Text (show (round time))

titles :: Picture 
titles = Translate 145 150 $ Pictures [title, subtitle1, subtitle2]

title = Translate (-150) 175 $ Scale 0.15 0.15 $ Color black $ Text "TATOOINE: Land Of Two Suns"
subtitle1 = Translate (-100) 150 $ Scale 0.1 0.1 $ Color black $ Text "Press space bar to launch "
subtitle2 = Translate (-100) 125 $ Scale 0.1 0.1 $ Color black $ Text "Use arrow keys to control"

credit = Translate (225) (-363) $ Scale 0.08 0.08 $ Color white $ Text "By Jai Sabnis"



-- THE EARTH 

ground :: Float -> Picture
ground n = Translate 0 n $ Color sand $ rectangleSolid 1500 100

wall :: Int -> Int -> Picture
wall 0 h = Blank 
wall b h =  Pictures [wallh h brick, Translate 20 0 $ wall (b-1) h]
       
wallh :: Int -> Picture -> Picture
wallh 0 x = Blank
wallh n x = Pictures [x , Translate 0 12 $ wallh (n-1) x]  


brick :: Picture
brick =  Pictures [
                   Color burly $ Translate 3 0  $ rectangleUpperSolid 20 10,
                   Color brickie $ Translate (-5) 0  $ rectangleUpperSolid 4 12,
                   Color brickie $ Translate 5 0  $ rectangleUpperSolid 2 12 ,
                   Color black $ Translate 3 (10)  $ rectangleUpperSolid 20 2 ,
                   Color brickie $ Translate 3 (0)  $ rectangleUpperSolid 20 2
                
                ]

-- brick dim x= 20  y = 14


door :: Picture 
door = Color black $ Pictures [ thickCircle 8 8 
                      ,  Translate 0 (- 30)  $ rectangleUpperSolid 24 35] 



house1 :: Picture
house1 = Pictures [ Color black $ thickCircle 42 100, 
                    Color peach $ thickCircle 40 100, 
                    Color black $ Translate 0 (-100)  $ rectangleUpperSolid 222 112,
                    Color peach $ Translate 0 (-100)  $ rectangleUpperSolid 220 110, 
                    Translate 0 (-70) $ door

                    ]

house2 :: Picture                    
house2 = Pictures [
                    Color black $ rectangleUpperSolid 102 302,
                    Color burly $ rectangleUpperSolid 100 300 
                   
                   ]
              
pod :: Float -> Picture 
pod time = Pictures [
                    Color silver $ rectangleUpperSolid 5 100,
                    Color steel $ rectangleUpperSolid 10 50, 
                    Translate 0 100 $ Color (flick time) $ rectangleUpperSolid 5 5 
                   ]                 
                       where flick time | even (round time) = red
                                         | otherwise = skyc
                                         
         
houses time = Pictures [ ground (-400), Translate (0) (-350) $ pod time, Translate (-320) (-350) $ wall 8 15, Translate 250 (-350) house2, Translate 150 (-250) house1, Translate (-75) (-354) (sakura 3 time), Translate (260) (-354) (sakura 4 time)]




leaf :: Picture 
leaf = Color dirty $ Scale 0.4 0.4 $ polygon [(0,100),(20, 145),(40,100), (30,75), (20, 60), (10, 75) ]

sakura :: Int -> Float -> Picture 
sakura 0 time = Blank 
sakura n time = Scale 0.75 0.75 $ Pictures 
                      [ Color brownie $ rectangleUpperSolid 10 24,
                      Translate 0 20 $ Color brownie $ rectangleUpperSolid 10 30
                      , Translate (-6) (10) $ leaf 
                      , Translate 0 20 
                         $ Rotate (150 * sin time / (fromIntegral n))
                         $ sakura (n-1) time
                         
                       , Translate 0 30 
                         $ Rotate (100 * sin time / (fromIntegral n))
                         $ sakura (n-1) time
                         
                        , Translate 0 30 
                         $ Rotate (50 * sin time / (fromIntegral n))
                         $ sakura (n-1) time
                        ]


-- THE SKY

sky :: Float -> Picture 
sky time = Pictures [suns time, Translate (-250) 100 $ fighters time 5]


suns :: Float -> Picture
suns time = Translate (-150) 250 $ Pictures [ Translate 10 30 $ Translate (-time * 2) time $ Color (lighter time sun1) $ thickCircle 10 20, 
                    Translate (time * 2) time $ Color (lighter time sun2) $ thickCircle 10 20]
 


fighters :: Float -> Int -> Picture 
fighters time n = Translate (40 * time) (10 * time) (tie n)

tie :: Int -> Picture 
tie 0 = Blank
tie n = Scale 0.5 0.5 $ Pictures [ Translate 0 (-10) $ Color steel $ rectangleUpperSolid 150 15,
                                  Color black $ thickCircle 30 30, 
                                  Color silver $ thickCircle 20 20, 
               
                 Color black $ thickCircle 17 20, 
                 Color silver $ thickCircle 15 10,
                 
                 Color red $ thickCircle 2 10,

                 Translate (-75) (-75) $ Color black $ rectangleUpperSolid 7 150,
                 Translate (75) (-75) $ Color black $ rectangleUpperSolid 7 150,
                 Translate (-20) (-22) $ Rotate 45 $ Color black $ rectangleUpperSolid 3 75,
                 Translate (20) (-22) $ Rotate (-45) $ Color black $ rectangleUpperSolid 3 75,
                 Translate (0) (-31) $ Color black $ rectangleUpperSolid 3 75,
                 Scale 0.95 0.95 $ Translate (-2000) (-200) $ Color silver $ tie (n-1)
                 ]


cloud :: Picture 
cloud = Pictures [ Translate 0 0 $ thickCircle 20 50 
                      ,  Translate 50 0  $ thickCircle 20 75  
                       ,Translate 100 0  $ thickCircle 20 50
                      
                      ]
                      
falcon :: Float -> Picture 
falcon time = Scale 0.75 0.75 $ Pictures [ Color black $ Translate (-80) 0 $ polygon [(-2,0), (77,0), (77,152), (46,152)],
                    Color silver $ Translate (-80) 0 $ polygon [(0,0), (75,0), (75,150), (48,150)] ,
                    Color black $ polygon [(2,0), (2,152), (29,152), (77,0)],
                    Color silver $ polygon [(0,0), (0,150), (27,150), (75,0)], 
                    
                    Color black $ thickCircle 32 100, 
                    Color falcol $ thickCircle 30 100, 
                                  
                    Color black $ Translate (-56) (-60) $ polygon [(-1,0), (112,0), (82,52), (38,52)],
                    Color steel $ Translate (-56) (-60) $ polygon [(1,0), (110,0), (80,50), (40,50)],
                    Color black $ thickCircle 16 20, 
                    Color silver $ thickCircle 15 10,
                    Color red $ rectangleUpperSolid 10 5,
                    Translate (-3) 19 $ wallh 5 chamber,
                    Translate (-3) (-78) $ wallh 5 chamber,
                    Rotate 162 $ Color (yo time) $ thickArc 0 150 84 5
                 ]

                 where yo time | even (round time) = jet1
                               | otherwise = jet3



canon :: Int -> Float -> Picture 
canon 0 time = Blank 
canon n time =  Pictures[  thickCircle 35 100 ,
                                                   Translate 0 10 $  Rotate (-60 * sin time) $ rectangleUpperSolid 7 160,
                                                   Translate 0 (-10) $ Color silver $ Scale 0.75 0.75 $ canon (n-1) time,
                                                   Translate 0 (-20) $ Color steel $ Scale 0.75 0.75 $ canon (n-1) time
                                                 ]
                       


chamber :: Picture
chamber =  Pictures [
                   Color silver $ Translate 3 0  $ rectangleUpperSolid 20 10,
                   Color black $ Translate (-5) 0  $ rectangleUpperSolid 4 12,
                   Color steel $ Translate 5 0  $ rectangleUpperSolid 2 12 ,
                   Color black $ Translate 3 (10)  $ rectangleUpperSolid 20 2 ,
                   Color black $ Translate 3 (0)  $ rectangleUpperSolid 20 2
                
                ]









                      
                 




























































                



{- 
colour palet
          | range 10 20 x = makeColorI 255 229 204 255
          | range 20 30 x = makeColorI 255 255 204 255
          | range 30 40 x = makeColorI 229 255 204 255
          | range 40 50 x = makeColorI 204 255 204 255
          | range 50 60 x = makeColorI 204 255 229 255
          | range 60 70 x = makeColorI 204 255 255 255
          | range 70 80 x  = makeColorI 204 229 204 255
          | range 80 90 x = makeColorI 102 178 255 255
          | range 90 100 x = makeColorI 255 204 255 255

zen :: Float -> Color  
zen time | range 0 25 x = makeColorI 204 255 255 255
          | range 25 50 x  = makeColorI 204 229 204 255
          | range 50 75 x = makeColorI 102 178 255 255
          | range 75 100 x = makeColorI 255 204 255 255
          | otherwise = makeColorI 255 204 204 255
             where x = round (100 * sin time)
                                                       
  -}













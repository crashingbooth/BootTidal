:set -XOverloadedStrings
:set prompt ""
:set prompt-cont ""

import Sound.Tidal.Context
import System.IO (hSetEncoding, stdout, utf8)
hSetEncoding stdout utf8

-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.3, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/30})
-- tidal <- startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})

:{
let p = streamReplace tidal
    hush = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal
    first = streamFirst tidal
    asap = once
    nudgeAll = streamNudgeAll tidal
    all = streamAll tidal
    resetCycles = streamResetCycles tidal
    setcps = asap . cps
    xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition tidal True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
    mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
    d1 = p 1 . (|< orbit 0)
    d2 = p 2 . (|< orbit 1)
    d3 = p 3 . (|< orbit 2)
    d4 = p 4 . (|< orbit 3)
    d5 = p 5 . (|< orbit 4)
    d6 = p 6 . (|< orbit 5)
    d7 = p 7 . (|< orbit 6)
    d8 = p 8 . (|< orbit 7)
    d9 = p 9 . (|< orbit 8)
    d10 = p 10 . (|< orbit 9)
    d11 = p 11 . (|< orbit 10)
    d12 = p 12 . (|< orbit 11)
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16
:}

:{
let wramp lower upper p = ((octfloor 12 lower).(octceil 12 upper)) <$> p where
      octceil by upper noteval = if noteval>upper then octceil by upper (noteval - by) else noteval
      octfloor by lower noteval = if noteval<lower then octfloor by lower (noteval + by) else noteval
    sine2 = slow 2 sine
    sine3 = slow 3 sine
    sine4 = slow 4 sine
    sine8 = slow 8 sine
    sine16 = slow 16 sine
    fsine2 = fast 2 sine
    fsine3 = fast 3 sine
    fsine4 = fast 4 sine
    saw2 = slow 2 saw
    saw3 = slow 3 saw
    saw4 = slow 4 saw
    saw8 = slow 8 saw
    saw16 = slow 16 saw
    fsaw2 = fast 2 saw
    fsaw3 = fast 3 saw
    fsaw4 = fast 4 saw
    s2 = slow 2
    s3 = slow 3
    s4 = slow 4
    s8 = slow 8
    f2 = fast 2
    f3 = fast 3
    f4 = fast 4
    f8 = fast 8
    t1 = trigger 1
    rsin mn mx period = range mn mx (slow period sine)
    mt = (# gain 0)
    churry = chunk 4 (hurry 2)
    cfurry = chunk 4 (fast 2)
    nr mx = n (run mx)
    vf = vowel "a e <i o>"
    rotate = (<~)
    trmelu = ((|+ n 12)  . slow 2 . rev )
    trmeld = ((|- n 12)  . slow 2 . rev )
    cbhm = s "superhammond" # squiz (rsin 1 2 8) # legato 0.3 # room 0.2 # gain 0.85
    cbvib = s "supervibe" # legato 2 # lpf 1200 # room 0.2 # size 0.5
    cbsaw = s  "supersaw" # resonance (range 0.5 0.8 perlin)
      # rate (range 0 3 (segment 32 rand)) # lfo (range 0 3 saw + perlin)
      # squiz (range 1.1 2  (segment 32 rand))
      # room 0.2 # size 0.5
    cbfk = s "superfork" # squiz 1.1 # gain 1.5 # room 0.2 # size 0.5
    cbpwm = s "superpwm"  # lfo 0.5 # resonance (rsin  0.2 0.8 3) # voice (rsin 0.2 0.7 4) # room 0.3 # size 0.1
    cbsqr = s "supersquare" # legato 0.3 # gain 0.9 # lpf 1000
    slurn n xs = slice (listToPat [n]) (churn n xs)
    splurn n xs = splice (listToPat [n]) (churn n xs)
    lock n offset = timeLoop n . ((offset |- (slow n $ run n)) <~)
    pfast = fast "1 1 2 1"
    pfast' = fast "<[1 1 2 1] [1 2 1 1]>"
    brand = rand * 3 - 1.5
    rspeed = (# speed (rand * 3 - 1.5)) . (# legato 1)
    revb = (# room 0.2) . (# size 0.1)
    isig mx f = round <$> (f * mx)
    isigscl scl mx f = scale scl (isig mx f)
    muted = (# legato 0.3) . (# lpf 800)
    slide x = accelerate (2 **| (x |/ 12) - 1)

    churn :: Int -> [Int] -> Pattern Int
    churn sz rots = listToPat (permute rots [0..(sz - 1)]) where
      permute [] lst = lst
      permute (n:ns) lst = permute ns (rotNth lst n)


    rotNth :: [Int] -> Int -> [Int]
    rotNth lst n = (getLast n lst) :  tail (rotNth' lst n (head lst) 0) where
      rotNth' [] n stored i = []
      rotNth' (x:xs) n stored i
        | i `mod` n == 0 = stored : (rotNth' xs n x (i + 1))
        | otherwise = x : (rotNth' xs n stored (i + 1))

    getLast :: Int -> [Int] -> Int
    getLast n lst = getLast' n lst 0 where
      getLast' n lst i
        |  n >= (length lst) = lst!!(mod n $ length lst)
        |  i + n >= (length lst) = lst!!i
        |  otherwise = getLast' n lst (i + n)
:}

:{
let setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetR tidal
    setB = streamSetB tidal
:}



:set prompt "tidal> "

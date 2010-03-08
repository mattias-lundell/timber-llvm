module LunarLander where

type Sink a = a -> Cmd _ _

altitudeSensor :: Port Bits8 -> Sink Int -> Time -> Class (Action, Action)
altitudeSensor port out period =
    class
        ref := 0
        tick = action
                    port.write sendPulseCmd
                    ref := baseline
                    after period tick
        echo = action
                    out ((baseline - ref) * metersPerSec / 2)
        result (tick, echo)


fuelSensor :: Port Bits8 -> Sink Int -> Time -> Class Action
fuelSensor port out period =
    class
        tick = action
                    v <- port.read
                    out (v * litersPerVolt)
                    start
        start = action
                    port.write startConvCmd
                    after period tick
        result start


thrustSensor :: Port Bits8 -> Sink [Bool] -> Class Action
thrustSensor port out =
    class
        thrustStatus val n  =  (1 `shiftLeft` n) `bitAnd` val /= 0
        statusChange = action
                    v <- port.read
                    out (map (thrustStatus v) [0..3])
        result statusChange


sensorSystem :: Port Bits8 -> Port Bits8 -> Port Bits8 -> 
                Sink Int -> Sink Int -> Sink [Bool] -> 
                Class (Action,Action,Action)
sensorSystem p1 p2 p3 d1 d2 d3 =
    class
        (altStart, echoHandler) = new altitudeSensor p1 d1 230
        fuelStart               = new fuelSensor     p2 d2 1000
        thrustHandler           = new thrustSensor   p3 d3
        start = action
                    altStart
                    fuelStart
        result (start, echoHandler, thrustHandler)


simulation :: GUI -> Action -> Action -> 
              Class (Action, Action, Port Bits8, Port Bits8, Port Bits8, Sink Int, Sink Int, Sink [Bool])
simulation gui echoHandler thrustHandler =
    class
        d1 = new intDisplayWidget gui
        d2 = new intDisplayWidget gui
        d3 = new flagDisplayWidget gui
        phys = new lunarLander gui echoHandler thrustHandler
        stick = new stickWidget phys.p0
        start = action
                    gui.open (vertical [phys.canvas,d1,d2,d3,stick])
                    phys.run
        result (start, stick, phys.p1, phys.p2, phys.p3, d1.set, d2.set, d3.set)


game :: GUI -> Class Action
game gui =
    class
        (simStart, stick, p1, p2, p3, d1, d2, d3) = new simulation gui echoHandler thrustHandler
        (sysStart, echoHandler, thrustHandler)    = new sensorSystem p1 p2 p3 d1 d2 d3
        result action
                    simStart
                    sysStart

control :: Port Bits8 -> Time -> Class (Action, Sink Int, Sink Int, Sink [Bool])
control p0 period =
    class
        altitude := 0
        fuel := 0
        thrust := [False,False,False,False]
        state := s0
        sink1 i = request
                    altitude := i
        sink2 i = request
                    fuel := i
        sink3 b = request
                    thrust := b
        tick = action
                    (thrusterCtrl,next) = f state altitude fuel thrust
                    state := next
                    p0.write thrustCtrl
                    after period tick
        result (tick, sink1, sink2, sink3)

autoGame gui =
    class
        phys = new lunarLander gui echoHandler thrustHandler
        (sysStart, echoHandler, thrustHandler) = new sensorSystem phys.p1 phys.p2 phys.p3 d1 d2 d3
        (ctrlStart, d1, d2, d3) = new control phys.p0 250
        return action
                    gui.open phys.canvas
                    sysStart
                    ctrlStart

autoReal p0 p1 p2 p3 =
    class
        (sysStart, echoHandler, thrustHandler) = new sensorSystem p1 p2 p3 d1 d2 d3
        (ctrlStart, d1, d2, d3) = new control p0 250
        result action
                    sysStart
                    ctrlStart

manual [p0,p1,p2,p3,p4,p5,p6] =
    class
        stickState := False
        thrustDisplay bs = p6.write (packBits bs)
        (sysStart, echoHandler, thrustHandler) = new sensorSystem p1 p2 p3 p4.write p5.write thrustDisplay
        stickHandler = action
                    stickState := not stickState
                    p0.write (packBits [stickState])
        result [sysStart, echoHandler, thrustHandler, stickHandler]

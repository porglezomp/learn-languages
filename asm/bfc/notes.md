# Some disassembly

    add r1, r2, r3  -> 0xe0821003
    add r1, r2, #15 -> 0xe282100f
    sub ...         -> ... 4 ...
    
    b (loc)         -> 0xea (number)
    cmp r4, #0      -> 0xe3540000
    beq (loc)       -> 0x0a (number)
    
    ldrb r4, [r3]   -> 0xe5d34000
    strb ...        -> ... c ...
    
    svc #0          -> 0xef000000

# General principles?

The leading bits are the condition code?
Register arithmetic is 0xe0 ... ?
Immediate arithmetic is 0xe2 ... ?
Register memory (?) is 0xe5 # ... ?

# Branch offsets

Branches are relative numbers, but are slightly weird because of the instruction pipeline.

    A: b A      -> b -2
       b B      -> b -1
    B: nop
       b C      -> b 0
       nop
    C: nop

etc.

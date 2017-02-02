#CNC ZEUS is the advanced low-cost machine controller (now Open Source!)

Utilizing commodity x86-based hardware CNC ZEUS creates a dedicated machine control with professional features such as:

- Cutter compensation
- Canned cycles (peck drill, tapping cycle, etc)
- G31 Allows for Digitizing Probe
- "S" commands for spindle RPM with support for up to 8 gear changes
- Sub-program calls up to 10 levels deep
- Four-axis selectable manual pulse generator with X1, X10, X100 steps per increment
- Assembly language optimized for step rates over 35,000 steps/sec

###Perfect for large professional machines.
Retro-fitted manual mills and rebuilt cnc mills run faster and smoother with dedicated control hardware. Professional operators will appreciate the similarities to a FANUC 6M control, making operation intuitive and easy to learn. Shop owners will enjoy the freedom from expensive control hardware and costly repairs.

###Perfect for small hobbyist machines
The advanced features and professional interface enhance the experience for enthusiasts. Why tie up your desktop computer with your CNC projects? The low cost of CNC ZEUS and with the low minimum hardware requirements make setting up a dedicated control more affordable than ever before.
 
#Features
- Cutter compensation
- Circular and helical interpolation
- Assembly language optimized for step rates over 35,000 steps/sec
- Canned cycles (peck drill, tapping cycle, etc)
- G31 Allows for Digitizing Probe
- Up to 100 tool offsets with easy entry
- US/Metric input conversion
- G54 through G59 work offsets
- Sub-program calls up to 10 levels deep
- Acceleration/Deceleration on all axis for stepper motors
- Backlash compensation
- Automatic homing with hardware limit switches (software limits also available)
- Expanded memory support for programs up to 1.6 million lines
- Support for four-axis selectable manual pulse generator with X1, X10, X100 steps per increment
- "S" commands for spindle RPM with support for up to 8 gear transmission (GS2 drive required)

###Supported G Codes
- G00 positioning (rapid traverse)
- G01 linear interpolation (feed)
- G02 circular interpolation CW
- G03 circular interpolation CCW
- G04 dwell
- G12 Hole Mill Clockwise
- G13 Hole Mill Counter Clockwise
- G17 XY plane selection
- G18 ZX plane selection
- G19 YZ plane selection
- G20 input in inch
- G21 input in mm
- G28 return to reference point
- G31 skip cutting
- G40 cutter compensation cancel
- G41 cutter compensation left
- G42 cutter compensation right
- G43 tool length compens'n + direction
- G44 tool length compens'n - direction
- G49 tool length compensation cancel
- G54 work conditions system 1 select
- G55 work coordinate system 2 select
- G56 work coordinate system 3 select
- G57 work coordinate system 4 select
- G58 work coordinate system 5 select
- G59 work coordinate system 6 select
- G73 peck drilling cycle
- G74 counter tapping cycle
- G80 canned cycle cancel
- G81 drilling cycle, spot boring
- G82 drilling cycle, counter boring (dwell)
- G83 peck drilling cycle
- G84 tapping cycle
- G85 boring cycle
- G89 boring cycle (dwell)
- G90 absolute programming
- G91 incremental programming
- G92 programming of absolute zero pt
- G98 return to initial point in canned style
- G99 return to R point in canned cycle


###Supported M Codes
- M00 program stop
- M01 optional stop
- M02 end of program (no rewind)
- M03 spindle CW
- M04 Spindle CCW
- M05 spindle stop
- M08 flood coolant ON
- M09 coolant OFF
- M30 end program (rewind stop)
- M98 call sub-program
- M99 end sub-program

# System Requirements

**CNC ZEUS version 1.x**

####Minimum Hardware
- Intel Pentium 150Mhz
- 4MB of RAM
- 4MB of hard disk space

####Preferred Hardware
- Intel Pentium 233Mhz with MMX or better
- 32MB of RAM or more
- 20MB of hard disk space or more

####Operating System (any of the following):
- Microsoft MS-DOS version 5.0 or later
- FreeDOS Release 2.03 or later
- Windows 95 or 98 running in MS-DOS mode

#Related Links

##Hardware
- [GS2 Drives (AutomationDirect)](http://www.automationdirect.com/)
- [Gecko Drives](http://www.geckodrive.com/)
- [Xylotex](http://www.xylotex.com/) 
- [Rutex](http://www.rutex.com/)
- [Digi-Key](http://www.digikey.com/) (multi-position rotary switches & more)

##Software
- [Windows Networking for DOS (download)](ftp://ftp.microsoft.com/bussys/clients/MSCLIENT/dsk3-1.exe) & [Installation Help](http://www.wown.com/j_helmig/Dosclnt3.htm)
- [FreeDOS](http://www.freedos.org/) (Free replacement for MS-DOS)

##Hobbyist Community
- [HobbyCNC](http://www.hobbycnc.com/)
- [CNCzone](http://www.cnczone.com/)
- [Inside-Woodworking](http://www.inside-woodworking.com/cnc/)

----

# Building from Source
CNC ZEUS was originally written in [PowerBASIC](http://www.powerbasic.com/) for DOS. There are no other dependencies outside of an appropriate compiler.






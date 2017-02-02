'---- Removed security for Open Source.
'---- Changed tool offset direction on Preset
'---- added GS2reset$ to send to sendGS2: for drive reset on startup!
'---- Checking to see if G96 constant SFM is possible, G97 normal RPM mode
'---- Fixed tool offset problem reading tools higher than T0909
'---- Fixed handle drop out problem on some chipsets
'---- Added GS2 register reading and setting from control
'---- Added key filter for num-lock and cap-lock keys
'---- Added Ftmp! for temporary feed rate control for G33 thread command
'---- Added variable feed_ipr% to set feed to IPR
'---- Applied axis scale to axis offsets also.
'---- Added X axis to tool offset table probably should add Y axis also.
'---- Added parameter to change jog keys to any key
'---- Added scaling to handle when axis scaling is on
'---- Re-worked scaling so an axis can be setup for LATHE diameter programming
'---- Added extra handle parameter to soften large handle steps
'---- Added extra parameter to get handle to work with odd pitched ball screws
'---- Changed (handadd:) accel/decel on handle input so it's smoother in large steps
'---- Start of --- LATHE ---- mods

'---- Fixed "A" axis feed rate multiplier prompt to display correct prompt.
'---- Added port read on E-stop "error_port%" and display the number read.
'---- Added save to tool and offset pages with "U" key
'---- Remarked out from upd_pos: routine the Lval% lines for digitizing in subs.
'---- Fixed the point save routine so points can be saved again.
'---- Attempted to fix E-stop so it don't false trigger and pause machine
'---- Fixed program hangup from operation without any feed rate set
'---- Increased the rotary port multiplier to hold larger number up to 999.
'---- Added custom port address settings
'---- Added password to enable access to the parameter setup screens
'---- Fixed the rtc interrupt routine by doing a second read to reset the chip interrupt
'---- Re-worked canned cycles, added delay to fix G31 (skip cut) and G92
'---- Fixed M00 & M01 to stop before reading next line
'---- Fixed corner dedeleration to work on all planes
'---- Re-fixed the E-stop problem
'---- finally fixed the RESET problem of next line running after reset
'---- still some nicks in the manual feed setting while changing speed
'---- limit switches can now be configured seperately for each axis
'---- increased axis step size limit of 32767 to 65535
'---- Reworked execution order, need to check ALL COMMANDS IN ALL ORDERS!
'---- Auto corner feed appears to be working! need to do full test!
'---- constant feed control working with one line look ahead
'---- mb
'---- added PWM option for spindle RPM control for the cheepie peepies
'---- added EMERGENCY STOP sub for E-stop shutdown control
'---- fixed the backlash problem again caused from the independant step fix
'---- added wfct! to re-scale A axis to acheive higher feed rate ratios
'---- added check to prevent MDI mode working when on axis limit
'---- added independant axis step counts per-unit
'---- corrected the step loss on tiny arcs through quadrants
'---- fixed G21 metric command to operate on the fly
'---- changed the rotary axis divider to same value as other axes to fix
'	rotation error problem on the A axis.
'----changed A and Z axes to follow feed rate even with diff steps per inch
'--- added a timer to the cmos clock interrupt to time RPM on the spindle
'--- fixed the problem with calling canned cycles in sub causing cycle run on call line
'--- feed would not inc, if keypad + key used and was fixed.
'--- fixed high speed wrap around on the output routine that caused position error
'--- fixed the wrong direction R calculation on G2/G3 commands in the G19/G18 planes,still marked "atest"just in case!
'--- fixed a problem with cutter comps on the G18 and G19 planes. marked lines with "atest" for searching
'--- fixed the string conversion problem that would cause a hang after zillion lines.
'--- fixed the "/" to skip the remaining line of G-gode
'--- fixed G21 & G20 problem of switching back and forth while running
'--- increased Nval& to long for larger line numbers
'--- adding M50 code to syncronize a program move to an external event
'--- added accel/decl to handle movements, cli and sti instructions around handx& inc and dec comands to stop handwheel count loss
'--- changed the Line repeat counter to display the previous subs L value if the current L is 0
'--- fixed the anoying flicker on the corner clock display
'--- increased the calculated feed rate maximum to be faster
'--- corrected the handle interrupt resetting to original address problem
'--- fixed the axis zero setting when axis mirror is on
'--- fixed a problem with G83 in metric mode(Q value was not converted by metric!)
'--- fixed the backlash problem effecting single step moves

'--- problem!!!! backlash is always on the g17 plane, may not be working
'--- correctly in G18 or G19, may be on wrong axis ??? don't know

'--- fixed totally a problem with jog and handle missing first pulse from no setup time
'--- removing light option from front control and replace with cool2
'--- rename the aux outputs on port2 to s1,s2,s3 and 4096hz status

'-----------------
$COM 512
sig$="2016-Open"
ver$="CNC ZEUS v1.90.2"
%USE_EMS=0	'1 = compile to use ems memory, 0 = no ems
%DEMO=0   	'1 = compile to demo
ON ERROR GOTO ETRAP

plug$="        www.cnczeus.com"

'------ 2010-JMS encrypted string ---------
'dcry$ = chr$(95,138,184,228,14,88,136,187)
'gosub decode
'sig$=nu1$
'-------- ZEUS LATHE v1.90.2  encrypted string -----------
'dcry$ = chr$(77,180,204,9,52,46,135,169,233,10,52,60,191,167,209,9,45,88,137)
'gosub decode
'ver$=nu1$

'cls
'dcry$=" ZEUS LATHE v1.90.2"
'gosub encode
'for i=1 to len(nu1$)
'print ascii(mid$(nu1$,i,1));
'next i
'end

'encode:
'nuky?=45
'nu1$=""
'for t%= 1 to len(dcry$)
'fst1?=ascii(mid$(dcry$,t%,1))
'fst2?=fst1? + t%*nuky?
'nu1$=nu1$ + chr$(fst2?)
'next t%
'return


flen1? = ascii(left$(sig$,1))

FLEXCHR$=chr$(0)
%line_length = 38
'--------------------------------------------------------

type movedat
	FLine as string * 124
        Dval as integer
        Fval as single
	Ftmp as single
        Hval as integer
        Nval as long
        Lval as integer
        Rval as long
	Sval as single
	Tval as integer
        Qval as long
        Canz as long
        Oval as integer
        Pval as integer
        mg0 as integer
        mg1 as integer
        mg2 as integer
        mg3 as integer
        mg4 as integer
        skp as integer
        g0 as integer
        g1 as integer
        g2 as integer
        g3 as integer
        g5 as integer
        g6 as integer
        g7 as integer
        g8 as integer
        g9 as integer
        g10 as integer
        g13 as integer
        g14 as integer
	x92 as long
	y92 as long
	z92 as long
	w92 as long
        xcmd as long
        ycmd as long
        zcmd as long
        wcmd as long
        xdist as long
        ydist as long
        zdist as long
        wdist as long
        xpos as long
        ypos as long
        zpos as long
        wpos as long
        Ival as long
        Jval as long
        Kval as long
        Xcp as long
        Ycp as long
        fvx1 as single
        fvy1 as single
        fvz1 as single
        fvw1 as single
        fvx2 as single
        fvy2 as single
        fvz2 as single
        fvw2 as single
        offx1 as long
        offy1 as long
        offx2 as long
        offy2 as long
        offI as long
        offJ as long
        Roff as long
        cdist as long
        hx as long
        hy as long
        hz as long
	lnum as long
        look as integer
        new as integer
	rpd as integer
	Sublevel as integer
        hold_next as integer
	candelay  as integer

end type


type disp_box
	x as byte
        y as byte
        h as byte
        v as byte
        cf as byte
        cb as byte
        hf as byte
        hb as byte
        bdr as byte

end type
type work_offset
    	x as long
        y as long
        z as long
        w as long
end type

'---------------------------- Program line buffer --------

$IF %DEMO
dim lbuf (flen1?*2) as string * %line_length
$ELSE

$IF  %USE_EMS
if fre(-11) > 0 then dim virtual lbuf (100) as string * %line_length else fail$="NEED EMS MEMORY TO OPERATE!": goto exitg
$ELSE
dim lbuf (100) as string * %line_length
$ENDIF
$ENDIF


gosub setpal

'--------------------------------------
dim hipseg as word
dim hipptr as word
dim hip1 as word
dim hip2 as word
dim p1 as byte ptr
dim p4 as word ptr
hipseg = codeseg(handsub)
hipptr = codeptr(handsub)

dim mdat(4) as movedat
dim dseg as word ptr
dim keyseg as word ptr
dim rtimer as word ptr
dim dosclk as dword ptr
dim jpdos as dword ptr
dim cseg1 as word
dim cseg2 as word
dim ipseg as word
dim ipptr as word
dim ip1 as word
dim ip2 as word
dim wrk(6) as work_offset
dim ktx?(128)
dim sublptr&(11)
dim hlp$(4)
'dim nnum$(20)
'dim blk$(20)

inrtc?=0	'in service rt_int interrupt counter DO NOT USE
lrtc2?=0	'current divide by 8 counter counts 0-8 use NO
kt1?? = varseg (ktx?(1))
kt2?? = varptr (ktx?(1))
tmr1??=0        'general purpose timer for delay times 1khz
tmr2??=0        'canned cycle timer for "P" delay time 1khz
tmr3??=0	'rpm counter do not use any tmr3 stuff for GP use 8192 HZ
tmr4??=0	'rpm averaging counter,do not use
tmr5??=0	'added for G04 delay timer
tmr6??=0        'spindle RPM update timer for G96 constant SFM
tmr3low??=0     'low if limitw pin is low
rpmcntr??=0	'last rpm count at 8192 HZ
revcnt??=0	'spindle rotation counter, increments each rotation
watchdog?=1	'enable watchdog timer output

pwm?=0		'0=disable PWM output for spindle drive. 1=enable pwm
pwmcount%=0     'incrementing up count for pwm
pwmpos%=0       'current pwm value (set from S command) 818= 50% RPM
pwmmax%=1637	'rollover count or full on value for pwm counter
pwmrpm%=1000    ' full motor RPM at max PWM
pwmphase?=0	'current output phase of 5hz pwm

comtmr1??=0     'com port timer
mult%=100
dseg = codeptr32(p)
@dseg= varseg(mult%)
keyseg = codeptr32(kak)
@keyseg = varseg(mult%)
rtimer = codeptr32(rtc)
@rtimer = varseg(mult%)
'------------------------pointers to handle isr -----
p1 = codeptr32(hy)
p4 = codeptr32(hp)
@p4 = varseg(multi%)
dim jp_loc_ptr as dword ptr

'--------------------------------------xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx------

pi1# = 1.57079632679	'--- pi/2
tg# = 57.2957795130823	'--- to convert radians to degrees
pi# = 3.141592653589795	'--- pi

f3?=200:f4?=200
os%=0
gb%(os%)=0

'---------------------- display box locations and sizes ----------------
'--------------- background colors ----- see setdefaultcolor

'--------------- title bar colors --- see setdefaultcolor

'----------- F key select menu colors ------- see setdefaultcolor

dim w1 as disp_box	'----- position window ------
w1.x=47
w1.y=2
w1.h=15
w1.v=5
w1.cf=12
w1.cb=2
w1.bdr=7

dim w2 as disp_box	'----- distance to go window -------
w2.x=64
w2.y=2
w2.h=15
w2.v=5
w2.cf=3
w2.cb=2
w2.bdr=7

dim w3 as disp_box	'---- run file window ------
w3.x=3
w3.y=3
w3.h=40
w3.v=15
w3.cf=0
w3.cb=2
w3.hf=15
w3.hb=7
w3.bdr=8

dim w4 as disp_box	'---- TOOL window ------
w4.x=47
w4.y=8
w4.h=16
w4.v=8
w4.cf=3
w4.cb=2
w4.bdr=7

dim w5 as disp_box	'---- G code window ------
w5.x=73 '75
w5.y=9 '2
w5.h=6
w5.v=12 '11
w5.cf=0
w5.cb=5
w5.bdr=8

dim w6 as disp_box	'---- MSG window ------
w6.x=2
w6.y=20
w6.h=43
w6.v=3
w6.cf=0
w6.cb=5
w6.bdr=8

dim w7 as disp_box	'---- directory window ------
w7.x=2
w7.y=3
w7.h=77
w7.v=17 '18
w7.cf=0
w7.cb=5
w7.hf=13
w7.hb=2
w7.bdr=8


dim w8 as disp_box	'---- tool D/H offset table window ------
w8.x=2
w8.y=2
w8.h=45
w8.v=21
w8.cf=0
w8.cb=11
w8.hf=7
w8.hb=2
w8.bdr=8

dim w9 as disp_box	'---- tool length input window ------
w9.x=47
w9.y=14
w9.h=25
w9.v=2
w9.cf=0
w9.cb=5
w9.bdr=0

dim w10 as disp_box	'---- PARAMETER window COLORS ------
w10.x=4
w10.y=6
w10.h=70
w10.v=16
w10.cf=0
w10.cb=11
w10.hf=7
w10.hb=2
w10.bdr=0

dim w11 as disp_box	'---- WORK OFFSET text input window ------
w11.x=62
w11.y=17
w11.h=16
w11.v=2
w11.cf=0
w11.cb=7
w11.bdr=7


dim w12 as disp_box	'---- WORK abs position window ------
w12.x=62
w12.y=3
w12.h=15
w12.v=5
w12.cf=12
w12.cb=2
w12.bdr=7

dim w13 as disp_box	'---- WORK machine position window ------
w13.x=62
w13.y=10
w13.h=15
w13.v=5
w13.cf=12
w13.cb=2
w13.bdr=7

dim w14 as disp_box	'---- WORK OFFSET table window ------
w14.x=2
w14.y=3
w14.h=70
w14.v=16
w14.cf=0
w14.cb=11
w14.hf=7
w14.hb=2
w14.bdr=0

dim w15 as disp_box	'---- SETUP window ------
w15.x=4
w15.y=2
w15.h=70
w15.v=10
w15.cf=0
w15.cb=5
w15.hf=7
w15.hb=2
w15.bdr=0

dim w16 as disp_box	'---- AXIS HOME window ------
w16.x=28
w16.y=11
w16.h=19
w16.v=7
w16.cf=12
w16.cb=2

dim w17 as disp_box	'---- M code window ------
w17.x=65
w17.y=9
w17.h=6
w17.v=7
w17.cf=0
w17.cb=5
w17.bdr=8

dim w18 as disp_box	'---- GS2 drive spindle Load window ------
w18.x=47
w18.y=18
w18.h=24
w18.v=3
w18.cf=0
w18.cb=5
w18.bdr=8

dim w19 as disp_box	'---- command window ------
w19.x=47
w19.y=18
w19.h=24
w19.v=5
w19.cf=0
w19.cb=5
w19.bdr=8

dim w20 as disp_box	'---- mach position display window ------
w20.x=64
w20.y=2
w20.h=15
w20.v=5
w20.cf=3
w20.cb=2
w20.bdr=7

dim w21 as disp_box	'---- tool abs position window ------
w21.x=47
w21.y=2
w21.h=15
w21.v=5
w21.cf=3
w21.cb=2
w21.bdr=7

dim w22 as disp_box	'---- tool mach position window ------
w22.x=64
w22.y=2
w22.h=15
w22.v=5
w22.cf=3
w22.cb=2
w22.bdr=7

dim w23 as disp_box	'---- tool command window ------
w23.x=47
w23.y=18
w23.h=24
w23.v=5
w23.cf=0
w23.cb=5
w23.bdr=8


dim lastmdi$(20)
txth%=40:txtv%=4
txth1%=30:txtv1%=6
'-----------------------------------------------------------------------
dataport% = &h378		'--- port address
statusport% = dataport%+1
controlport% = dataport%+2

xdirbit?=0:xstepbit?=1		'port step and dir bits
ydirbit?=2:ystepbit?=3
zdirbit?=4:zstepbit?=5
wdirbit?=6:wstepbit?=7

xlowhi%=0:ylowhi%=0:zlowhi%=0:wlowhi%=0

xlimpin?=15
ylimpin?=13
zlimpin?=12
wlimpin?=10
holdpin?=11

xlimbit? =3
ylimbit? =4
zlimbit? =5
wlimbit? =6
holdbit? =7

sfwdpin?=1
srevpin?=14
coolpin?=16
coolpin2?=17
'litpin?=17

sfwdbit? =0
srevbit? =1
coolbit? =2
coolbit2?=3
'litbit? =3

'----------------- set aux port pins

dataport2% = &h278		'--- port address
statusport2% = dataport2%+1
controlport2% = dataport2%+2

handirq% = 5
h1pulse% = 8 : h1step% = 1
h10pulse% = 4 : h10step% = 5
h100pulse% = 4 : h100step% = 50

fbits?=&h0f
auxio5?=&h10
auxio6?=&h20
hdlbits?=&hc0

auxin1? =&h08
handain?=&h10
handbin?=&h20
handint?=&h40
auxin2? =&h80

aux1pin?=1
aux2pin?=14
aux3pin?=16
aux4pin?=17

aux1bit?=0
aux2bit?=1
aux3bit?=2
aux4bit?=3


'------------------------ COMPORT SETTINGS FOR GS2 AC SPINDLE DRIVE
gsdrive%=0
spndlon$="0106091B0001D4"
spndloff$="0106091B0000D5"
spndlfwd$="0106091C0000D4"
spndlrev$="0106091C0001D3"
GS2reset$="0106091E0001"'--- no CRC on end like others bc it calls output!
comport1$="COM1"
baud1$="38400"
parity1$="N"
databits1$="7"
stopbit1$="2"
comdelay%=30
freqmin!=1.5
freqmax!=60
maxsovr%=120
minsovr%=50
rpminc%=10
maxs!=9999.9
spndelay?? = 500
spcnt% = 100
motor% = 1725
freqbase!=60
gearcount? = 8
pram_dec$="0000"
pram_dat$="0000"

dim ratio!(gearcount?)
ratio!(1)=.1133
ratio!(2)=.1615
ratio!(3)=.2408
ratio!(4)=.3456
ratio!(5)=.5552
ratio!(6)=.7875
ratio!(7)=1.2068
ratio!(8)=1.7054

softlim%=0
limxhi& = 1000:limxlo& = -10000
limyhi& = 1000:limylo& = -10000
limzhi& = 1000:limzlo& = -10000
limwhi& = 1000:limwlo& = -10000

extpercent?=0
lathe_bit%=0
'feedconst!=.0015   '-- max feed calculator constant, smaller allows faster clks
feedconst!=.0013   '-- max feed calculator constant, smaller allows faster clks
feedmult%=10	'---- 10=100% feedrate ------
feedinc!=1
feed1!=1
maxfeed!=20	'--- max feed rate, G0
rpd1!=1         '--- set inital rapid override at 100%
fpd1!=1         '--- set inital feed override at 100%
accl! = 2	'--- acceleration and deceleration in inch/per second
extfin?=1	'--- must be one if external feed control is not used
lastrin?=4	'--- last rapid multiplier read from external control
lastfin?=10	'--- last feed multiplier read from external control
acelg0%=1	'--- accel/decel on G0 moves only if "1"
corner_gain!=1	'--- multiplier for corner deceleration; 2 = 2x slower
makefeed%=0	'--- when set to one causes external feed override of zero to keep moving
revx%=0		'--- inverts the axis counting direction only +/-
revy%=1		'--- use for direction setup only not to mirror
revz%=0
revw%=0

'-------------------- MIRROR by data "multiply data by -1"
mirrx%=1	'--- mirror x axis when -1 , use this to mirror axis x
mirry%=1	'--- mirror y axis when -1 , use this to mirror axis y


mode$=" MODE "
auto%=10		'--- set auto run to off
gp3%=90		'--- default absolute mode = G90
gp2%=17		'--- default plane x/y = G17
gp7%=40		'--- default offset mode = G40
gp9%=80		'--- default canned cycle = 80
gp6%=20		'--- default inch input = 20
gp8%=49		'--- default tool length = 49 cancel
gp10%=98	'--- default return point = 98 inital start point
gp5%=94		'--- set default feed to IPM=94 not IPR=95
gp13%=97
gp14%=54	'--- default work cooridinate = 54
lastplane%=gp2%

'-------------------------------------------------

bcklshx%=1      '--- axis backlash compensation
bcklshy%=1
bcklshz%=1
bcklshw%=1

xyscale# = 1.
xscale# = 1.
yscale# = 1.
zscale# = 1.
wscale# = 1.
iscale# = 1.
jscale# = 1.
kscale# = 1.
iscale! = 0
jscale! = 0
kscale! = 0
metric! = 1.
jx1?=75:jx2?=77
jy1?=80:jy2?=72
jz1?=81:jz2?=73
jw1?=83:jw2?=82

handcount&=0
handadder%=100
handcarry%=100
handsoft%=1
rotarya%=0	'---- 1 = rotary axis for A

	 	'---- steps per inch -----------------
spix?? = 2000
spiy?? = 2000
spiz?? = 2000
spiw?? = 2000
spi?? = max (spix??,spiy??,spiz??,spiw??)

dvspi&=spi??*100
dvspix&=spix??*100
dvspiy&=spiy??*100
dvspiz&=spiz??*100
dvspiw&=spiw??*100

dvdrx&=0
dvdry&=0
dvdrz&=0
dvdrw&=0
wfct!=1.0000 '--- sets default W axis factor to 1 (same as other axes)

errtol% = 200	'--- error tolerance for tool path calculations = 200 --
                '--- this is in expanded units and is about = to dv! --

expand& = 100000  '-- multiplier to improve acuracy -----?
dv! = expand& / spi??

expandw& = expand&
dvw!=dv!

rolla%=360
'Wmax&=rolla% * expandw&
'rolldspa&= rolla% * spi??/wfct!

zerox%=0
zeroy%=0
zeroz%=0
zerow%=0

limclear% = 200
homefeed1! = 30
retratio% = 30
dirhomex%=1
dirhomey%=1
dirhomez%=1

extfeed%=0	'--- 1 enables external feed control
exthand%=0	'--- 1 enables manual pulse generator

srevtime?? = 1000
pram28% = 20 '-- sets distance to move from limits on zero return, in steps
pram83% = 20 '-- sets rapid in hole stop short distance! in steps
menu%=1
doit%=0
filename$="NEW.NC"
dirname$=curdir$

if right$(dirname$,1) <>"\" then
dirname$=dirname$+"\"
end if
password$="zeus"
passkey$=password$
datafile$="NEWDATA.NC"

savename$="LTOOL.CFG"
dfext$ =".NC"
maxtools%=100
toolpage%=0
toolptr%=0
helpbit%=1
dim mcode%(6)

dim doffset#(maxtools%)
dim z_offset&(maxtools%)
dim x_offset&(maxtools%)
dim y_offset&(maxtools%)

if pbvCpu <3 then fail$="CPU must be at least 486 or better": goto exitg


gosub setdefaultcolor

'----------- READ THE LAST CONFIG FILE !!!!!!!!!!!!!!!!!!!!!!!!!
50 f$=dir$ (savename$)
if f$ = savename$ then gosub getall '--- read config file if exists ---!


'-------- Things that need to be set after reading config file ----!!!!

'------ CALCULATE COMPUTER SPEED USING LOOP AND MTIMER --------
mtimer
for t&=0 to 10000
tmp%=inp(dataport%)
for n&=0 to 2
incr sptest&
decr sptest&
next n&
next t&
ppcnt& = mtimer
if ppcnt& < 9333 then ppcnt&=9333 '--- set minimum for ppcnt&


'--- set palette colors and draw background --
palette using pal%(0)
gosub bckdraw

'if maxtools%=0 then maxtools%=100:dim doffset#(maxtools%):dim z_offset&(maxtools%)


'-------------calc bit mask settings -------------------
gosub maskcalc

gosub port_clear

'------- set interrupt vector for timing control!------
gosub setirq

'------- set interrupt vector for rtc interrupt for additional timer ---
gosub settime_int

gosub sethirq	'----- setup port interrupt handler

'-------------- DON'T do this , it messes it up! -------
'for t%=0 to 4      '--- preload the move data structs to default data
'mdat(t%).Fline="XYZA"
'call line_read# (bycopy t% ,mdat(),wrk())
'next t%

'-------------------------START HERE -----------------------

sout!=1		'startup spindle RPM
'if lathe_bit% then feed1!=.001 else feed1!=1 ' set startup feed rate

'------ turn on caps lock ------
def seg=&h40
d?=peek (&h0017)
poke &h0017,(d? or &h40)

gosub str_key_filter
gosub opnfile
gosub sreset
gosub vline
gosub displaytool
'gosub dist_to_go
'gosub page1
gosub newfeed	'initilize feed so it don't hang if ran with no feedrate

if gsdrive% then
comtmr1??=0
while comtmr1?? < 100 '--- delay for 100ms so watchdog can activate!
wend
newdat$ = GS2reset$ :gosub sendGS2 '--- reset the GS2 drive
end if

sng_block%=1
ky$=chr$(0,65) :goto rescan	'---- start in single run mode!


kwait:
while not instat	'---- wait for keypress
if pg%=0 then gosub clkdsp:gosub page1
'gosub displaytool
'if m_ready% then gosub M_exc
wend

'------------------read key, x= quit! "ESC"= reset ----

ky$=inkey$

rescan:
if menu%=1 then
if ky$=chr$(0,59) then gosub setfile:goto rescan
'if ky$=chr$(0,60) then ky$="" 'gosub setedit:goto rescan
if ky$=chr$(0,61) then gosub settool:goto rescan
if ky$=chr$(0,62) then gosub setwkoff:goto rescan
'if ky$=chr$(0,63) then gosub setup:goto rescan
else
gosub mankeys
end if


menu1:

kmnu:
if ky$=chr$(0,63) then gosub setup:goto rescan
if ky$=chr$(0,64) then gosub setmdi:goto rescan

if ky$=chr$(0,65) then
auto%=0
gosub filerun
goto rescan
end if

if Ky$=chr$(0,66) then auto%=1:gosub filerun:goto rescan
if ky$=chr$(0,67) then gosub setjog :goto rescan
if ky$=chr$(0,68) then gosub sethandle :goto rescan
if ky$=chr$(0,133) or ky$=chr$(0,134) or ky$=chr$(0,84) or ky$=chr$(0,85) then gosub setmenu


goto kwait
'---------------------- key scan 1 ------------------

'------------- un-encrypt string drcy$ with key nuky? returns nu1$ -------
decode:
nuky?=45
nu1$=""
for t%= 1 to len(dcry$)
fst1?=ascii(mid$(dcry$,t%,1))
fst2?=fst1? - t%*nuky?
nu1$=nu1$ + chr$(fst2?)
next t%
return

'----------------------------------------------------------------
setpal:
dim pal%(16)
pal%(0)=0
pal%(1)=56
pal%(2)=24
pal%(3)=3
pal%(4)=20
pal%(5)=14
pal%(6)=42
pal%(7)=7
pal%(8)=8
pal%(9)=16
pal%(10)=4
pal%(11)=28
pal%(12)=22
pal%(13)=23
pal%(14)=26
pal%(15)=63
return


filerun:

hlp$(0)="RUN    =  Enter           RESET =  Esc"
hlp$(1)="HOLD   =  Space           MENU  =  F12"
hlp$(2)="EDIT   =  F2              RPM   =  < >"
hlp$(3)="                          FEED  =  +/-"
hlp$(4)="CURSOR =  "+chr$(24)+chr$(25)+"   Home/End   Page  = up/dn"
if helpbit% then gosub help_dsp

pg%=0
if auto%=0 then mode$=" RUN SINGLE":gosub selects
if auto%=1 then mode$=" RUN AUTO  ":gosub selects
if fload%=0 then gosub vline
'if fload%=0 or fload%=1 then gosub vline

'--------------- moves the line cursor and pages thru file ------
rkey:
while not instat
if in_op%=0 and cancyc%=0 and ll% then gosub vline:ll%=0
gosub clkdsp
gosub page1
gosub autoread
if do_reset% and mov%=0 then gosub sreset:lptr&=0:cusr%=0:gosub vline
if cancyc% and arun% and mov%=0 and in_op%=0 then gosub cancycle
if m_ready% then gosub M_exc
wend

ky$=inkey$
'-----------------------------

if ky$=chr$(27) then gosub sreset

if mov% then goto skpky

if ky$=chr$(0,72) then
	if fload%=1 then gosub upd_pos
rechk1:
        if lptr&+cusr% > 1 then
	if right$(lbuf(lptr&+cusr%-2),1) = chr$(0) then
	if cusr% > 0 then decr cusr% else if lptr& > 0 then decr lptr&
        else
        if cusr% > 0 then decr cusr% else if lptr& > 0 then decr lptr&
        goto rechk1
	end if
        else
	if cusr% > 0 then decr cusr% else if lptr& > 0 then decr lptr&
        end if
gosub vline
end if

if ky$=chr$(0,80) then
	if fload%=1 then gosub upd_pos
	if right$(lbuf(lptr&+cusr%),1) = chr$(0) then
	if cusr% < w3.v-1 then incr cusr% else if lptr&+cusr% < numl&+addlines% then incr lptr&
	else
rechk2:
	if cusr% < w3.v-1 then incr cusr% else if lptr&+cusr% < numl&+addlines% then incr lptr&
	if right$(lbuf(lptr&+cusr%-1),1) <> chr$(0) then goto rechk2

        end if
gosub vline
end if

if fload% then goto skpky

if ky$=chr$(0,73) then
	decr lptr&,w3.v:if lptr& < 0 then lptr&=0
rechk3:
        if lptr&+cusr% > 1 then
	if right$(lbuf(lptr&+cusr%-1),1) <> chr$(0) then
	if cusr% > 0 then decr cusr% else if lptr& > 0 then decr lptr&
        goto rechk3
        end if
        end if
end if

if ky$=chr$(0,81) then
	incr lptr&,w3.v:if lptr& > numl&-w3.v then lptr&=numl&-w3.v+1
rechk4:
        if lptr&+cusr% < numl& then
	if right$(lbuf(lptr&+cusr%-1),1) <> chr$(0) then
	if cusr% < w3.v-1 then incr cusr% else if lptr& < numl& then incr lptr&
        goto rechk4
        end if
        end if
	if lptr&<0 then lptr&=0
end if


if ky$=chr$(0,71) then lptr&=0:cusr%=0
if ky$=chr$(0,79) then lptr&=numl&+addlines%-w3.v+1 :if lptr&<0 then lptr&=0
'if ky$=chr$(0,71) then cusrx%=0
if ky$=chr$(0,79) then cusrx%=len(lbuf(lptr&+cusr%))
if ky$ = chr$(0,63) then goto back2
if ky$ = chr$(0,64) then goto back2
if ky$ = chr$(0,67) then goto back2
if ky$ = chr$(0,68) then goto back2

skpky:

if menu% then
if ky$ = chr$(0,59) then goto back2
if ky$ = chr$(0,61) then goto back2
if ky$ = chr$(0,62) then goto back2
if ky$ = chr$(0,60) then gosub setedit:goto filerun
else
gosub mankeys
end if

if ky$=chr$(44) then if spcnt% > minsovr% then decr spcnt% ,rpminc%:gosub newrpm
if ky$=chr$(46) then if spcnt% < maxsovr% then incr spcnt% ,rpminc%:gosub newrpm

'if ky$ = "=" then
if ky$ = chr$(61) or ky$ = chr$(43) then

if extfeed% then
if mdat(1).Fval < maxfeed! then incr mdat(1).Fval,feedinc!
if mdat(2).Fval < maxfeed! then incr mdat(2).Fval,feedinc!
if mdat(3).Fval < maxfeed! then incr mdat(3).Fval,feedinc!
if Fval! < maxfeed! then incr Fval!,feedinc!
if feed1! < maxfeed! then incr feed1!,feedinc!
if mov_F! < maxfeed! then incr mov_F!,feedinc!
'gosub newfeed
updfeed%=1
else
if lastfin? < 20 then
incr lastfin?
'gosub newfeed
updfeed%=1
else
if mdat(1).Fval < maxfeed! then incr mdat(1).Fval,feedinc!
if mdat(2).Fval < maxfeed! then incr mdat(2).Fval,feedinc!
if mdat(3).Fval < maxfeed! then incr mdat(3).Fval,feedinc!
if Fval! < maxfeed! then incr Fval!,feedinc!
if feed1! < maxfeed! then incr feed1!,feedinc!
if mov_F! < maxfeed! then incr mov_F!,feedinc!
'gosub newfeed
updfeed%=1
end if
end if
end if

'if ky$ = "-" then
if ky$ = chr$(45) then
if extfeed% then
if mdat(1).Fval > feedinc! then decr mdat(1).Fval,feedinc!
if mdat(2).Fval > feedinc! then decr mdat(2).Fval,feedinc!
if mdat(3).Fval > feedinc! then decr mdat(3).Fval,feedinc!
if Fval! > feedinc! then decr Fval!,feedinc!
if feed1! > feedinc! then decr feed1!,feedinc!
if mov_F! > feedinc! then decr mov_F!,feedinc!
'gosub newfeed
updfeed%=1
else
if lastfin? > 1 then
decr lastfin?
'gosub newfeed
updfeed%=1
else
if mdat(1).Fval > feedinc! then decr mdat(1).Fval,feedinc!
if mdat(2).Fval > feedinc! then decr mdat(2).Fval,feedinc!
if mdat(3).Fval > feedinc! then decr mdat(3).Fval,feedinc!
if Fval! > feedinc! then decr Fval!,feedinc!
if feed1! > feedinc! then decr feed1!,feedinc!
if mov_F! > feedinc! then decr mov_F!,feedinc!
'gosub newfeed
updfeed%=1
end if
end if
end if

if ky$=chr$(27) then gosub sreset

if ky$ = chr$(0,65) then '--- F7 switch to single block mode auto%=0 ---
auto%=0 : sng_block%=1
'if cancyc%=0 then arun%=0   ' this must be not set or may single block in TAP CYCLE!
if mov% then	'--- set current move to full deceleration
decl&=1
last_decl&=1   '--- set last deceleration to full so next acceleration is full also
last_declmod!=0
end if

goto filerun
end if
                    '--- F8 auto mode ----
if ky$ = chr$(0,66) then
auto%=1
'if cancyc%=0 then arun%=0
goto filerun
end if

if ky$=chr$(0,133) or ky$=chr$(0,134) or ky$=chr$(0,84) or ky$=chr$(0,85) then gosub setmenu

if ky$=chr$(13) and estop%=0 and mov%=0 then
ky$=""
if fload%=0 then gosub reloadfile
if auto%=1 then sng_block%=0
arun%=1
gosub loadf
end if

if ky$=" " and mov%=1 and estop%=0 and ehold%=0 then ky$="":gosub fhold
if ky$=chr$(13) and estop%=1 then
ky$="":estop%=0
if auto%=0 then mode$=" RUN SINGLE" else mode$=" RUN AUTO  "
end if
'if mov(os%).exc=0 and estop%=0 and cancyc%=0 then gosub vline:gosub displaytool
if in_op%=0 and cancyc%=0 then gosub vline:gosub displaytool

goto rkey

back2:
return

'------------------------- File Edit -------------
setedit:

hlp$(0)="FILE TOP = Ctrl+Home     FIND   = Ctrl+F"
hlp$(1)="FILE END = Ctrl+End      EXIT   = Esc"
hlp$(2)="SAVE CURRENT FILE WITH NEW NAME = Crtl+S"
hlp$(3)=""
hlp$(4)="CURSOR =  "+chr$(24)+chr$(25)+"   Home/End   Page  = up/dn"
if helpbit% then gosub help_dsp


edit%=1:cusrx%=0
gosub selects
gosub chop
gosub vline
k2:
while not instat
gosub clkdsp
loop
ky$= inkey$


if ky$=chr$(19) then
prompt$="SAVE ACTIVE FILE WITH NEW NAME"
tmp$=filename$
filename$ = ucase$( gettext$ (filename$,prompt$,34,20,10,bycopy w7.cf,bycopy w7.cb))
if tmp$ <> filename$ then
if instr(filename$,".") = 0 then filename$=filename$+dfext$
fchanged%=1:gosub savcurfile
end if
gosub bckdraw
if helpbit% then gosub help_dsp
gosub page1
gosub vline
gosub displaytool
end if

if ky$=chr$(6) then
prompt$="FIND ? "
find$ = ucase$(gettext$ (find$,prompt$,24,30,14,bycopy w7.cf,bycopy w7.cb))
if find$ <>"" then
t&=lptr&+cusr%+1:found%=0
while t& < numl&+addlines% and found%=0
incr t&
found% = instr(lbuf(t&),find$)
if found% then lptr&=t&-cusr%:cusrx%=found%-1
wend
end if
gosub bckdraw
if helpbit% then gosub help_dsp
gosub page1
gosub vline
gosub displaytool
end if



if ky$ = chr$(27) then edit%=0:gosub savcurfile:goto xit
if ky$=chr$(0,73) then decr lptr&,w3.v:if lptr& < 0 then lptr&=0

if ky$=chr$(0,81) then
incr lptr&,w3.v:if lptr& > numl&-w3.v then lptr&=numl& - w3.v+1
if lptr& < 0 then lptr&=0
end if

if ky$=chr$(0,119) then lptr&=0:cusr%=0
if ky$=chr$(0,117) then lptr&=numl&+addlines%-w3.v+1:if lptr&<0 then lptr&=0

if ky$=chr$(0,72) then
        if lptr&+cusr% > 0 then
	if cusr% > 0 then decr cusr% else if lptr& > 0 then decr lptr&
        t%= instr(lbuf(lptr&+cusr%),any chr$(0)+chr$(255))-1
        if t%<0 then t%=0
        if t% < cusrx% then cusrx% = t%
        end if
end if

if ky$=chr$(0,80) then
	if lptr&+cusr% < numl&+addlines% then
	if cusr% < w3.v-1 then incr cusr% else incr lptr&
        t%= instr(lbuf(lptr&+cusr%),any chr$(0)+chr$(255))-1
        if t%<0 then t%=0
        if t% < cusrx% then cusrx% = t%
        end if
end if

if ky$=chr$(0,71) then cusrx%=0
if ky$=chr$(0,79) then cusrx%=instr(lbuf(lptr&+cusr%),any chr$(0)+chr$(255))-1

if ky$=chr$(0,77) then
 	if right$(lbuf(lptr&+cusr%),1) <> chr$(0) then
        if cusrx% < %line_length-1 then
        incr cusrx%
        else
	if cusr% < w3.v-2 then incr cusr% else incr lptr&
        cusrx%=0
        end if
        else
        if edchr$ <> chr$(0) then incr cusrx%
        end if

end if

if ky$=chr$(0,75)  then
        if cusrx% > 0 then
        decr cusrx%
        else
        if lptr&+cusr% > 0 and right$(lbuf(lptr&+cusr%-1),1) <> chr$(0) then
        if cusr% < 1 then decr lptr& else decr cusr%
        cusrx% = %line_length-1
        end if
	end if
end if

if ky$=chr$(0,83) then
	ky$=""
        if edchr$ = chr$(0) or edchr$ = chr$(255) then
           if  lptr&+cusr% < numl&+addlines% then
	        if cusrx%=0 then
	        	gosub line_del
                        fchanged%=1
		        else
        		tmp$=edstr1$ + left$(lbuf(lptr&+cusr%+1),instr(lbuf(lptr&+cusr%+1),any chr$(255)+chr$(0))-1)
		        gosub breakline
			incr cusr%
			gosub line_del
			decr cusr%
			fchanged%=1
                       end if
	        end if
        else
		if cusrx% < %line_length-1 then
			lbuf(lptr&+cusr%) = edstr1$+edstr2$
                        fchanged%=1
		end if
        end if
end if


if ky$=chr$(8) then
	ky$=""
        if cusrx% > 0 then
	        if len(edstr1$) > 0 then edstr1$ = left$ (edstr1$,len(edstr1$)-1)
                lbuf(lptr&+cusr%) = edstr1$+edchr$+edstr2$:decr cusrx%
		fchanged%=1
        else
        decr cusr%
      	cusrx%=instr(lbuf(lptr&+cusr%),any chr$(255)+chr$(0))-1
	tmp$ = left$(lbuf(lptr&+cusr%),cusrx%) + left$(lbuf(lptr&+cusr%+1),instr(lbuf(lptr&+cusr%+1),any chr$(255)+chr$(0))-1)
        gosub breakline
        incr cusr%
	gosub line_del
        decr cusr%
	fchanged%=1
	end if
end if

if ky$ > chr$(31) and ky$ < chr$(123) then
        if instr(lbuf(lptr&+cusr%),any chr$(255)+chr$(0)) < %line_length then
	lbuf(lptr&+cusr%) = edstr1$+ucase$(ky$)+edchr$+edstr2$
        incr cusrx%:fchanged%=1
        end if
end if
'--------------------- inserts a line --------
if ky$=chr$(13) then
gosub line_add
fchanged%=1
end if
'---------------------
gosub chop
gosub vline
goto k2

breakline:
if len(tmp$) > %line_length - 1 then
lbuf(lptr&+cusr%)=left$(tmp$,%line_length -1) + chr$(255)
lbuf(lptr&+cusr%+1)=mid$(tmp$,%line_length )
else
lbuf(lptr&+cusr%)=tmp$
end if
return


chop:
if cusrx%=0 then edstr1$="" else edstr1$=mid$(lbuf(lptr&+cusr%),1,cusrx%)
edchr$=mid$(lbuf(lptr&+cusr%),cusrx%+1,1)
edstr2$=mid$(lbuf(lptr&+cusr%),cusrx%+2)
return

line_add:
incr addlines%
t&=numl&+addlines%
while t& > lptr&+cusr%
lbuf(t&) = lbuf(t&-1)
decr t&
wend
lbuf(t&)=edstr1$
lbuf(t&+1)=edchr$+edstr2$
if cusr% < w3.v-1 then incr cusr% else incr lptr&
cusrx%=0
return

line_del:
decr addlines%
t&=lptr&+cusr%
while t& < numl&+addlines%
lbuf(t&) = lbuf(t&+1)
incr t&
wend
lbuf(t&)=""

return

savcurfile:
if fchanged%=0 then return

msg$="SAVING FILE"
gosub message

t&=0:fchanged%=0
650 if instr(filename$,ANY ":\")=0 then
open dirname$+filename$ for output as 1
else
open filename$ for output as 1
end if
while t& < numl& + addlines%
l$=""
nubuf:
if right$(lbuf(t&),1) = chr$(0) then
l$ =l$ + left$(lbuf(t&),instr(lbuf(t&),chr$(0))-1)
else
l$=l$+ left$(lbuf(t&),instr(lbuf(t&),any chr$(255)+chr$(0))-1)
incr t&:goto nubuf
end if

print# 1,l$
incr t&
wend
close #1
tl&=lptr&:tl%=cusr%
gosub opnfile
lptr&=tl&:cusr%=tl%

652 return
'-------------------- Print misc, values -----------
ds:
'view text (w6.x,w6.y+1)-(w6.x+w6.h,w6.y+w6.v)
'print f$
'print dirname$+filename$
return

getblock:	'--- find the current block number
if fload% then
        oout%=mdat(1).Oval
        nout&=mdat(1).Nval
	if nextline% then
        pblk&=mdat(2).lnum+1
        else
        pblk&=mdat(1).lnum+1
        end if
        else
	pblk&=lptr&+cusr%+1
	nout&=Nval&
        oout%=Oval%
end if

!cli
blk$=str$(pblk&)
nnum$=str$(nout&)
onum$=str$(oout%)
!sti

blk$="BLOCK"+blk$+" "+string$(8,chr$(205))
nnum$="N"+nnum$+" "+string$(8,chr$(205))
onum$="O"+onum$+" "+string$(5,chr$(205))
return

'------------------------------ View Lines of File -----------------
vline:
gosub getblock


view text (w3.x+2,w3.y-1)-(w3.x+w3.h-1,w3.y-1)
color w3.bdr,w3.cb


print using" \         \";nnum$;
print using" \      \";onum$;
print using" \             \";blk$;


view text (w3.x,w3.y)-(w3.x+w3.h,w3.y+w3.v)
color w3.cf,w3.cb
cls


if auto%=0 then
if in_op% then dl&=cur_lnum& -2 else dl&=mdat(2).lnum-2
end if
if auto% then
if in_op% then dl&=cur_lnum& -2 else dl&=cur_lnum&-1
end if



for t%=0 to w3.v - 1

if fload%=0 then
        if edit%=0 then
	if cusr% = t% then print "> " lbuf(t%+lptr&) else print "  " lbuf(t%+lptr&)
	else
	if cusr% = t% then
        	print "> " edstr1$;
                color w3.cf,w3.hb:print edchr$;
                color w3.cf,w3.cb:print edstr2$
        	else
        	print "  " lbuf(t%+lptr&)
        	end if
                end if
end if

if fload%=1 then

	if dl&+t% < 0 then
	print""
	else
	if t%=2 and in_op% then
        	color w3.hf,w3.cb:print"> " lbuf(dl&+t%)
         	else
                if t%=2 and in_op%=0 then
                color w3.hf,w3.cb :print "> ";
                color w3.cf,w3.cb :print lbuf(dl&+t%)
                else
                color w3.cf,w3.cb:print "  " lbuf(dl&+t%)
                end if
	end if


end if
end if

next t%
return


autoread:


if in_op%=0 and auto% and fload% and arun% and mov%=0 and cancyc%=0 and estop%=0 then gosub loadf
'---- mdat().look is set to 1 to indicate next move cannot be read ahead

if do_reset% then return


'---- setting buffer% =0  will stop read ahead!!!!
'buffer1%=1
if buffer1% then
if next_op% = 0 and auto% and arun% and fload% and cancyc%=0 and estop%=0 then gosub loadf
'if next_op% = 0 and mdat(2).look =0 and auto% and arun% and fload% and cancyc%=0 and estop%=0 then gosub loadf
end if



if lptr& > numl&+4 then
lptr&=numl&+2
gosub sreset ':if auto% < 2 then gosub vline
end if

'if lptr& > numl&+2 then arun%=0:fload%=0 'gosub sreset:if auto% < 2 then gosub vline
return


'-------------------------------
reloadfile:
fload%=1
os2%=1
lptr&=lptr&+cusr% : cusr%=0
for t%=0 to 1
gosub lineread
if Lval%(sublevel%) = 0 then incr lptr&
next t%
if requpd%=0 then gosub vline
dspf%=1

return

loadf:
if requpd% then gosub upd_pos
if cancyc% then return
gosub lineread
if auto%=0 then arun%=0
os2%=1

'--------------------- set dir bits if plane changes -------
newplane% = mdat(1).g2
if lastplane% <> newplane% then gosub setdirbits	'check for plane change
if mdat(1).g9 = 80 then gosub offset:gosub movecalc
if mdat(1).g9 <> 80 then gosub cancycle

if abrt%=1 then gosub sreset
'end if

'if auto%=0 and cancyc% then arun%=0 '--- changes back to single block after
'--------
if Lval%(sublevel%) = 0 then incr lptr&
'--------
'nextline%=1
return

'------------  lines from lind buffer "lbuf" starting at line 5 ----
'------------ and save in struct 0-3


lineread:
		'------- reads new line into struct 3 --
        	'-------- others are shifted ahead ----
for nxtl% = 0 to 2
mdat(nxtl%) = mdat(nxtl%+1)
next nxtl%
incr nxtl%

if multimove% then decr lptr& '----- set the line pointer back to rerun last line--

if lptr& < numl& then
        tmp$=lbuf(lptr&)
adline:
        if right$(tmp$,1) <> chr$(0) then
        incr lptr&
	tmp$=left$(tmp$,instr(tmp$,chr$(255)) - 1) + lbuf(lptr&)
        goto adline
        else
	mdat(3).Fline = tmp$
	end if
else
	mdat(3).Fline = "%"
end if

nextline%=0
call line_read# (3,mdat(),wrk())	'------ call line  read to get values!
return


'-------------------- open nc file for input -------------------
opnfile:
msg$="READING FILE"
gosub message
addlines%=0
maxems&=fre(-11):maxmem&= fre(-1)

if instr(filename$,ANY ":\") = 0 then
nuf$ = dirname$+filename$
else
nuf$ = filename$
end if
f$ = dir$ (nuf$)
if f$="" or instr(nuf$,f$) =0 then gosub nofile:return

$IF %DEMO
$ELSE
erase lbuf
$ENDIF

numl&=0:lptr&=0:cusr%=0

open nuf$ for input as 1
while not eof(1)
line input #1,l$:incr numl&
llgth%=len(l$):incr numl&,fix( llgth% / %line_length)
wend
close #1

$IF %DEMO
'-------------- clear buffer , demo only
for dem1%=0 to 100
lbuf(dem1%)=""
next dem1%
'-------------- check file size , demo only
if numl& > flen1? then
lbuf(0)= "Demo limit is 50 program lines."
lbuf(1)=filename$ +" is"+str$(numl&) +" Lines."
lbuf(3)=plug$
numl&=6
msg$=""
gosub message
return
end if

$ELSE

$IF %USE_EMS
if maxems& < ((numl&+100) * %line_length) then
fail$=" NOT ENOUGH FREE EXPANDED MEMORY"
dim lbuf (100) as string * %line_length
goto nofile
else
dim virtual lbuf (numl&+100) as string * %line_length
end if

$ELSE

if maxmem& < ((numl&+100) * %line_length) then
fail$=" NOT ENOUGH FREE MEMORY"
dim lbuf (100) as string * %line_length
goto nofile
else
dim lbuf (numl&+100) as string * %line_length
end if

$ENDIF
$ENDIF

open nuf$ for input as 1
t&=0
while not eof(1)
line input #1,linein$
nxtscn:
if len(linein$) > %line_length - 1 then
lbuf(t&)=left$(linein$,%line_length -1) + chr$(255)
linein$=mid$(linein$,%line_length )
incr t&
goto nxtscn
else
lbuf(t&)=linein$
incr t&
end if
wend
close #1
lptr&=0
msg$=""
gosub message

return
'----------------------- end file load ---------

nofile:
numl&=6
lbuf(0)="File not found!"
msg$=""
gosub message
return


'--------------------- SET HANDLE MODE ------------------!!!!!!!!!!!!!!
sethandle:

hlp$(0)="RESET =  Esc"
hlp$(1)="MENU  = F12"
hlp$(2)="RPM   = < >"
hlp$(3)="SAVE POINT TO FILE = P"
hlp$(4)=""

if helpbit% then gosub help_dsp

auto%=3
mode$=" HANDLE    "
if mov%=1 then gosub fhold
pg%=0
gosub selects
gosub vline
hdat%=0


dat1!=0		'clear counter
handx&=0:handy&=0:handz&=0:handw&=0::handcount&=0:lastdat2?=5

if exthand% = 1 then
'------------------- enable pic interrupt
!cli
out &h21,(inp(&h21) and not hpic%)
!sti
handbit%=1	'----turn on hand mode ----
end if

gosub newfeed
hnewkey:

while not instat
gosub clkdsp
gosub page1
stat2? = (inp(statusport2%) xor &h80) and &h88
dat2? = (inp(dataport2%) and &hc0)
rotate left dat2?,2

if dat2? <> lastdat2? then
	lastdat2?=dat2?:hdat%=0:handcount&=0

select case dat2?	'Scale the adder count per axis scale
	case=0:handadder1%=handadder%*xscale#
        case=1:handadder1%=handadder%*yscale#
        case=2:handadder1%=handadder%*zscale#
        case=3:handadder1%=handadder%*wscale#
end select
end if

if stat2? <> laststat2? then
	laststat2?=stat2?:hdat%=0:handcount&=0
	if stat2? = 8 then hinc%=h1pulse%     : hstep%=h1step%
	if stat2? = 128 then hinc%=h10pulse%  : hstep%=h10step%
	if stat2? = 136 then hinc%=h100pulse% : hstep%=h100step%
end if


if hdat% >= hinc% then
decr hdat%,hinc%: incr handcount& ,(handadder1% * hstep%)
end if

if handcount& >= handcarry% then
handcount1%= handcount&/handcarry%
decr handcount&,(handcarry% * handcount1%)

!cli
select case dat2?
	case=0:incr handx&,handcount1%
        case=1:incr handy&,handcount1%
        case=2:incr handz&,handcount1%
        case=3:incr handw&,handcount1%
end select
!sti
end if

if hdat% <= -hinc% then
incr hdat%,hinc%: decr handcount& , (handadder1% * hstep%)
end if

if handcount& <= -handcarry% then
handcount1% = handcount&/handcarry%
decr handcount&,(handcarry% * handcount1%)
!cli
select case dat2?
	case=0:incr handx&,handcount1%
        case=1:incr handy&,handcount1%
        case=2:incr handz&,handcount1%
        case=3:incr handw&,handcount1%
end select
!sti
end if
wend
ky$=inkey$

if ucase$(ky$)=chr$(80) and savedat% then
gosub logpoint ':msg$="Point "+str$(datpoint%)
gosub message
goto hnewkey
end if


if ky$=chr$(44) then
if spcnt% > minsovr% then decr spcnt% ,rpminc%:gosub newrpm
goto hnewkey
end if

if ky$=chr$(46) then
if spcnt% < maxsovr% then incr spcnt% ,rpminc%:gosub newrpm
goto hnewkey
end if

if ky$=chr$(0,133) or ky$=chr$(0,134) or ky$=chr$(0,84) or ky$=chr$(0,85) then gosub setmenu:goto hnewkey

if menu%=1 then goto hmenu
if ky$=chr$(0,59) then gosub setspndl:goto hnewkey
if ky$=chr$(0,60) then gosub setsdir:goto hnewkey
if ky$=chr$(0,61) then gosub setcool1:goto hnewkey
if ky$=chr$(0,62) then gosub setcool2:goto hnewkey
'if ky$=chr$(0,63) then gosub setlight:goto hnewkey
hmenu:
if ky$=chr$(0,60) then ky$="":goto hnewkey
if ky$=chr$(27) then gosub sreset:goto hnewkey

if limithit% then dx&=0:dy&=0:dz&=0:dw&=0:estop%=0:es3&=0:limithit%=0

if exthand% = 1 then
 handbit%=0
'---- stop hand mode ---
'-------- clear pic interrupt
!cli
out &h21,(inp(&h21) or hpic%)
!sti
end if

gosub upd_pos
msg$="":gosub message
return

'--------------------------- reset handle irq ------------
resethirq:
			'disable the pic irq bit mask
handon?=&h20
!cli
out &h21,(inp(&h21) or hpic%)
!sti
			'reset old interrupt vector to "whatever"
reg 1 ,(hirq% or &h2500)
reg 4 ,hip1
reg 8 ,hip2
call interrupt (&h21)

                       'set the printer control port to irq disabled

tmpin?=inp(controlport2%) ':bit toggle tmpin?,2
tmpin?=tmpin? and &h0f
tmpin?=tmpin? or &h20
out controlport2%,tmpin? '(&h20)

return

'-------------------------- set handle irq -----------
sethirq:
                       'set the printer control port to irq enable
		'select port irq
if handirq% = 5 then hirq% = &h0d : hpic%=&h20
if handirq% = 7 then hirq% = &h0f : hpic%=&h80
handon?=&h30


tmpin?=inp(controlport2%) ':bit toggle tmpin?,2
tmpin?=tmpin? and &h0f
tmpin?=tmpin? or &h30
out controlport2%,tmpin?


       			'get current interrupt vector and save it !
reg 1 ,(hirq% or &h3500)
call interrupt (&h21)
hip1 = reg(2)
hip2 = reg(9)
			'set new interrupt vector to "handsub"
reg 1 ,(hirq% or &h2500)
reg 4 ,hipptr
reg 8 ,hipseg
call interrupt (&h21)
			'enable the pic irq bit mask
'!cli
'out &h21,(inp(&h21) and not hpic%)
'!sti
return

'------------------------- control port output data -------
control_out:
bit toggle tmpin?,2
out controlport%,tmpin?
return

spdriveout:
while comtmr1?? < comdelay%
wend
print #4,":"+spctl$+chr$(13)+chr$(10)
comtmr1??=0
gosub selects
return

'--------- set gs2 drive spindle rpm ---------
newrpm:

if feed_ipr% then updfeed%=1	'update feed if RPM changed when in IPR

if no_feed_overide% then
sout!=lastsval!
else
sout!=lastsval! * spcnt% / 100
end if

gear?=1
if pwm? then
while  ratio!(gear?) * pwmrpm% < (lastsval! * maxsovr% *.01) and gear? < gearcount?
incr gear?
wend
end if

if gsdrive% then
while  ratio!(gear?)* motor% < (lastsval! * maxsovr% *.01)  and gear? < gearcount?
incr gear?
wend
end if

setgear? = gear? - 1
tmpin?=inp(controlport2%):bit toggle tmpin?,2
tmpin?=tmpin? and &hf0
tmpin?=tmpin? or not setgear?
tmpin?=tmpin? or handon?
bit toggle tmpin?,2
out controlport2%,tmpin?

if pwm? then
maxs!= pwmrpm% * ratio!(gearcount?)
mins!=1
'mins!= pwmrpm% * ratio!(1)

if sout! > maxs! then sout! = maxs!
if sout! < mins! then sout! = mins!
pwmpos% =  (pwmmax% -1) * sout!/(pwmrpm% * ratio!(gear?))
end if


if gsdrive% then

'---- use high gear ratio and max freq to find max spindle rpm = maxs! ---
maxs!=freqmax!/freqbase! * motor% * ratio!(gearcount?)
mins!=freqmin!/freqbase! * motor% * ratio!(1)

if sout! > maxs! then sout! = maxs!
if sout! < mins! then sout! = mins!

freqout! = sout! * freqbase! / (motor% * ratio!(gear?))
if freqout! < freqmin! then freqout! = freqmin!
if freqout! > freqmax! then freqout! = freqmax!
newdat$="0106091A"+ right$ ("000"+hex$(freqout!*10),4)

gosub sendGS2
end if

return

sendGS2:
'----- calc crc for GS2 drive newdat$ and send -----
while comtmr1?? < comdelay%
wend
crc?=0
sl%=len(newdat$)
scn%=1
while scn% < sl%
incr crc?,val("&h"+mid$(newdat$,scn%,2))
incr scn%,2
wend
crc?=not crc?
incr crc?
crc$ = right$("0"+hex$(crc?),2)
trans$=":"+newdat$ +crc$+chr$(13)+chr$(10)
print #4,trans$
comtmr1?? = 0
return


'------------------------- F1 key -------------
setspndl:

if gsdrive% then
tmpin?=inp(controlport%):bit toggle tmpin?,2
if spndl%=1 then
spndl%=0:spctl$=spndloff$
bit set tmpin?,sfwdbit?
gosub control_out
gosub spdriveout
else
if spndldir%=1 then spctl$=spndlrev$  else spctl$=spndlfwd$
gosub spdriveout
'while comtmr1?? < comdelay%
'wend
spndl%=1:spctl$=spndlon$
bit reset tmpin?,sfwdbit?
gosub control_out
gosub spdriveout
end if
return
end if

if spndl%=1 then spndl%=0 else spndl%=1
tmpin?=inp(controlport%):bit toggle tmpin?,2
if spndl%=1 then if spndldir%=0 then bit reset tmpin?,sfwdbit? _
:bit set tmpin?,srevbit?: else bit set tmpin?,sfwdbit? _
:bit reset tmpin?,srevbit?
if spndl%=0 then bit set tmpin?,sfwdbit?:bit set tmpin?,srevbit?
gosub control_out
ky$=""
gosub selects

'--- New Delay on spindle change
'tmr1??=0
'while tmr1?? < spndelay??
'wend
return


'------------------------- F2 key -------------
setsdir:

if gsdrive% then
if spndldir%=1 then spndldir%=0:spctl$=spndlfwd$  else spndldir%=1:spctl$=spndlrev$
gosub spdriveout
return
end if

tmr1??=0
if spndl%=1 then gosub setspndl:while tmr1?? < spndelay?? :wend
if spndldir%=1 then spndldir%=0:gosub setspndl else spndldir%=1:gosub setspndl
if spndl%=0 then if spndldir%=1 then spndldir%=0 else spndldir%=1

ky$=""
gosub selects
return

'------------------------- F3 key -------------
setcool1:
tmpin?=inp(controlport%):bit toggle tmpin?,2
if cool1%=1 then cool1%=0:bit set tmpin?,coolbit? else cool1%=1:bit reset tmpin?,coolbit?
gosub control_out
ky$=""
gosub selects
return

'------------------------- F4 key -------------
setcool2:
if pwm? then return
tmpin?=inp(controlport%):bit toggle tmpin?,2
if cool2%=1 then cool2%=0:bit set tmpin?,coolbit2? else cool2%=1:bit reset tmpin?,coolbit2?
gosub control_out
ky$=""
gosub selects
return

'------------------------- F5 key -------------
setlight:
tmpin?=inp(controlport%):bit toggle tmpin?,2
if light%=1 then light%=0:bit set tmpin?,litbit? else light%=1:bit reset tmpin?,litbit?
gosub control_out
ky$=""
gosub selects
return

'------------------------- F11 key -------------
setmenu:
if menu%=1 then menu%=0 else menu%=1
ky$=""
gosub selects
return

'------------------------ SETUP F5 ------
setup:
auto%=8
mode$=" SETUP     "
screen 0,,2,2

maxcon&=fre(-1)
maxems&=fre(-11)
if %USE_EMS then maxlines&= maxems& / %line_length else maxlines&= maxcon& / %line_length

rst1:
view text (1,2)-(80,23)
color w15.hf,bgsetup%
cls
gosub bkgsetup
gosub bkclk
gosub clkdsp
gosub selects

kyin:
'view text (24,3)-(75,4)
'color w10.cf,w10.cb
'if mov%=1 then print" PARAMETERS CANNOT BE CHANGED WHILE IN OPERATION!" else _
' print"  "

view text (6,3)-(22,23)
color w15.bdr,w15.cb:print chr$(218) repeat$ (14,chr$(196));:color 7,w15.cb:print chr$(191)
color w15.bdr,w15.cb:print chr$(179);
color w15.cf
if set_op2%=0 then color w15.hf,w15.hb :if ky$=chr$(13) then :goto exitf
print " EXIT CONTROL ";:color 7,w15.cb:print chr$(179)
color w15.bdr,w15.cb:print chr$(192);:color 7,w15.cb:print repeat$(14,chr$(196)) chr$(217)

color w15.bdr,w15.cb:print chr$(218) repeat$ (14,chr$(196));:color 7,w15.cb:print chr$(191)
color w15.bdr,w15.cb:print chr$(179);
color w15.cf
if set_op2%=1 then color w15.hf,w15.hb :if ky$=chr$(13) and mov%=0 and fload%=0 then ky$="":gosub sreset:gosub pramkyin:goto rst1
print " PARAMETERS   ";:color 7,w15.cb:print chr$(179)
color w15.bdr,w15.cb:print chr$(192);:color 7,w15.cb:print repeat$(14,chr$(196)) chr$(217)

color w15.bdr,w15.cb:print chr$(218) repeat$ (14,chr$(196));:color 7,w15.cb:print chr$(191)
color w15.bdr,w15.cb:print chr$(179);
color w15.cf
if set_op2%=2 then color w15.hf,w15.hb:if ky$=chr$(13) and mov%=0 and fload%=0then ky$="":gosub sreset:gosub portkyin:goto rst1
print " PORT SETUP   ";:color 7,w15.cb:print chr$(179)
color w15.bdr,w15.cb:print chr$(192);:color 7,w15.cb:print repeat$(14,chr$(196)) chr$(217)

view text (24,3)-(40,18)
color w15.bdr,w15.cb:print chr$(218) repeat$ (14,chr$(196));:color 7,w15.cb:print chr$(191)
color w15.bdr,w15.cb:print chr$(179);
color w15.cf
if set_op2%=3 then color w15.hf,w15.hb :if ky$=chr$(13) then gosub setcolor:goto rst1
print " SCREEN COLOR ";:color 7,w15.cb:print chr$(179)
color w15.bdr,w15.cb:print chr$(192);:color 7,w15.cb:print repeat$(14,chr$(196)) chr$(217)

color w15.bdr,w15.cb:print chr$(218) repeat$ (14,chr$(196));:color 7,w15.cb:print chr$(191)
color w15.bdr,w15.cb:print chr$(179);
color w15.cf
if set_op2%=4 then color w15.hf,w15.hb :if ky$=chr$(13) and mov%=0 and fload%=0 then ky$="":gosub sreset:gosub setkyin:goto rst1
print "   SETTINGS   ";:color 7,w15.cb:print chr$(179)
color w15.bdr,w15.cb:print chr$(192);:color 7,w15.cb:print repeat$(14,chr$(196)) chr$(217)

color w15.bdr,w15.cb:print chr$(218) repeat$ (14,chr$(196));:color 7,w15.cb:print chr$(191)
color w15.bdr,w15.cb:print chr$(179);
color w15.cf
if set_op2%=5 then color w15.hf,w15.hb :if ky$=chr$(13) and mov%=0 and fload%=0 then ky$="":gosub sreset:gosub setdrive:goto rst1
print "PWM/GS2 DRIVE ";:color 7,w15.cb:print chr$(179)
color w15.bdr,w15.cb:print chr$(192);:color 7,w15.cb:print repeat$(14,chr$(196)) chr$(217)




view text (42,3)-(72,18)
color w15.bdr,w15.cb
print " REMAINING FREE MEMORY "
color w15.bdr,w15.cb:print chr$(218) repeat$ (28,chr$(196));:color 7,w15.cb:print chr$(191)
color w15.bdr,w15.cb:print chr$(179);
color w15.cf
print using " MEMORY           ######### "; maxcon&;:color 7,w15.cb:print chr$(179)
color w15.bdr,w15.cb:print chr$(179);
color w15.cf
print using " EXPANDED MEM    ########## "; maxems&;:color 7,w15.cb:print chr$(179)
color w15.bdr,w15.cb:print chr$(179);
color w15.cf
print using " PROGRAM LINES      ####### "; maxlines&;:color 7,w15.cb:print chr$(179)
color w15.bdr,w15.cb:print chr$(192);:color 7,w15.cb:print repeat$(28,chr$(196)) chr$(217)

kcheck:
while not instat
gosub rpmdsp
gosub clkdsp
'if in_op%=0 and cancyc%=0 and estop%=0 and ll% then gosub vline:ll%=0
gosub autoread
if cancyc% and arun% and mov%=0 and in_op%=0 then gosub cancycle
loop
ky$=inkey$

'----------- added setup password check ---------
if ky$=chr$(13) and set_op2% > 0 then if password$ <> passkey$ then ky$=" "

if ky$=chr$(0,25) then
prompt$="ACCESS PASSWORD"
if password$=passkey$ then fill$=passkey$ else fill$=repeat$(len(password$),"*")
passkey$= gettext$ (fill$,prompt$,26,12,16,bycopy w10.cf,bycopy w10.cb)

goto rst1
end if
'------------------------

if ucase$(ky$)="X" then goto exitf
if ky$=chr$(0,72) and set_op2% > 0 then decr set_op2%
if ky$=chr$(0,80) and set_op2% < 5 then incr set_op2%
if ky$=chr$(0,75) and set_op2% > 2 then decr set_op2%,3
if ky$=chr$(0,77) and set_op2% < 3 then incr set_op2%,3
if ky$=chr$(0,133) or ky$=chr$(0,134) or ky$=chr$(0,84) or ky$=chr$(0,85) then gosub setmenu
if ky$=chr$(27) then gosub sreset
if ky$ > chr$(0,63) and ky$ < chr$(0,69) then screen 0,,0,0:return

if menu% then
if ky$ = chr$(0,59) then screen 0,,0,0:return
if ky$ > chr$(0,60) and ky$ < chr$(0,63) then screen 0,,0,0:return
else
gosub mankeys
end if

goto kyin

mankeys:
if ky$=chr$(0,59) then gosub setspndl
if ky$=chr$(0,60) then gosub setsdir
if ky$=chr$(0,61) then gosub setcool1
if ky$=chr$(0,62) then gosub setcool2
return

rpmdsp:
view text (44,9)-(62,12)
color w15.bdr,w15.cb
copyrpm??=rpmcntr??

'---- RPM calculated with interrupt to tmr3 (8192 cps) by each revolution.
if copyrpm?? > 49 then actrpm! = 491520 / copyrpm??
if tmr3?? = 65535 then actrpm!=0
'if actrpm! <> previousrpm! then
'actrpm!=(previousrpm! + actrpm!)/2
'previousrpm!=actrpm!
'end if

print using "RPM = ####.#"; actrpm!
print "GEAR =  "gear?
print "COUNT = "revcnt??

'print tmr3??
return

function gettext$ (old$,msg$,lgth%,locx%,locy%,fg,bg)
shared ts%,doit%
newin$ = old$
noold:
view text (locx%,locy%)-(locx%+lgth%+2,locy%+3)
color fg,bg
print chr$(218) repeat$ (lgth%,chr$(196)) chr$(191)
print chr$(179) repeat$(lgth%,chr$(32)) chr$(179)
print chr$(192) repeat$(lgth%,chr$(196)) chr$(217);
doit%=0
gnkey:
while not instat
gosub clkdsp
if ts% <> ud% then gosub dspudtd
wend
ky$=inkey$
'ky$=ucase$(inkey$)

if ky$=chr$(0,71) then newin$="":decr ud%:goto noold
if ky$=chr$(13) then gettext$=newin$:doit% = 1:goto back
if ky$=chr$(27) then gettext$=old$:goto back
if ky$=chr$(8) then ky$="":if len(newin$) > 0 then newin$ = left$ (newin$,len(newin$)-1)
if ky$ > chr$(31) and ky$ < chr$(123) then if len(newin$) < lgth%-1 then newin$ = newin$+ky$
gosub dspudtd
goto gnkey

back:
view text (locx%,locy%)-(locx%+lgth%+2,locy%+3)
color fg,bg
cls
exit function

dspudtd:
ud% = ts%
if cn%=1 then cn%=0:cur$=" " else cn%=1:cur$="_"
view text (locx%+1,locy%)-(locx%+lgth%,locy%+2)
color fg,bg
'locate 1,1
print "  "msg$
print newin$+cur$"  ";

return

end function

'----------------- GS2 ac drive settings --------------
setdrive:

view text (1,2)-(80,23)
color w10.cf,w10.cb
cls
print
print " GS2 SERIES AC SPINDLE DRIVE CONTROLLER SETUP "
print " DEFAULT COM SETTINGS = ASCII 9600,7,N,2"
view text (48,3)-(80,6)
color w14.cf,w10.cb
print "        ESC TO EXIT"
print "   " chr$(24) chr$(25)" TO  SELECT OPTION"
print "   ENTER  TO  CHANGE VALUE"

gs2pram:

view text (5,6)-(27,10)

color w14.cf,w10.cb
color 0,w10.cb:print chr$(218) repeat$(1,chr$(196)) " GS2 COM SETTINGS "  repeat$ (1,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " ENABLE DRIVE  ";
if set_op5%=0 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
if gsdrive%=1 then
gsdrive%=0
gosub driveoff
else
gsdrive%=1
pwm?=0
comtmr1??=0
gosub driveon
end if
gosub bckdraw
gosub displaytool
screen 0,,2,2
goto gs2pram
end if
end if
if gsdrive%=0 then print " OFF "; else print " ON  ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " COM PORT     ";
if set_op5%=1 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if comport1$="COM1" then comport1$="COM2" else comport1$="COM1":goto gs2pram
print " "comport1$ " ";
color 7,w10.cb:print chr$(179)
'color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(20,chr$(196)) chr$(217);

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " BAUD RATE    ";
if set_op5%=2 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
select case baud1$
case="4800" :baud1$="9600":goto gs2pram
case="9600" :baud1$="19200":goto gs2pram
case="19200":baud1$="38400":goto gs2pram
case="38400":baud1$="4800":goto gs2pram
end select
end if
end if
print using" \   \"; baud1$;
color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(20,chr$(196)) chr$(217);


view text (5,11)-(33,19)
color w14.cf,w10.cb
color 0,w10.cb:print chr$(218) repeat$(4,chr$(196))" GS2 DRIVE LIMITS "repeat$(4,chr$(196));:color 7,w10.cb:print chr$(191)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " BASE FREQUENCY  ";
if set_op5%=3 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="BASE FREQUENCY OF MOTOR "
freqbase!=val(gettext$ (str$(freqbase!),prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb))
goto setdrive
end if
end if
print using"###.##hz ";freqbase!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " MOTOR RPM AT BASE  ";
if set_op5%=4 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="RPM OF MOTOR AT BASE FREQ."
motor%=val(gettext$ (str$(motor%),prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb))
goto setdrive
end if
end if
print using"##### ";motor%;:color 7,w10.cb:print chr$(179)


color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " MAX FREQUENCY   ";
if set_op5%=5 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="Max frequency set on GS2 drive"
freqmax!=val(gettext$ (str$(freqmax!),prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb))
goto setdrive
end if
end if
print using"###.##hz ";freqmax!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " MIN FREQUENCY   ";
if set_op5%=6 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="Min frequency set on GS2 drive"
freqmin!=val(gettext$ (str$(freqmin!),prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb))
goto setdrive
end if
end if
print using"###.##hz ";freqmin!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " MAX SPINDLE OVR %  ";
if set_op5%=7 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="MaxPercent of programmed S"
maxsovr%=val(gettext$ (str$(maxsovr%),prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb))
goto setdrive
end if
end if
print using" ###% ";maxsovr%;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " MIN SPINDLE OVR %  ";
if set_op5%=8 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="MinPercent of programmed S"
minsovr%=val(gettext$ (str$(minsovr%),prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb))
goto setdrive
end if
end if
print using" ###% ";minsovr%;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " SPINDLE INC/DEC %  ";
if set_op5%=9 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="Inc/Dec Percent of programmed S"
rpminc%=val(gettext$ (str$(rpminc%),prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb))
goto setdrive
end if
end if
print using" ###% ";rpminc%;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(26,chr$(196)) chr$(217);


'-------------------------- PWM SETTING -------------------------------------------
view text (5,20)-(34,23)
color w14.cf,w10.cb
color 0,w10.cb:print chr$(218) repeat$(6,chr$(196))" PWM SETTINGS "repeat$(6,chr$(196));:color 7,w10.cb:print chr$(191)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " ENABLE PWM OUTPUT   ";
if set_op5%=10 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
if pwm?=1 then
pwm?=0
else
pwm?=1
gsdrive%=0
gosub driveoff
end if
goto gs2pram
end if
end if
if pwm?=0 then print " OFF "; else print " ON  ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " RPM AT FULL PWM  ";
if set_op5%=11 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$=" MOTOR RPM AT FULL PWM "
pwmrpm%=val(gettext$ (str$(pwmrpm%),prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb))
goto gs2pram
end if
end if
print using"  ##### ";pwmrpm%;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(26,chr$(196)) chr$(217);

'--- GS2 SET ADDRESS -----
view text (40,6)-(64,10)
color w14.cf,w10.cb
color 0,w10.cb:print chr$(218) repeat$(2,chr$(196))" SEND GS2 COMMAND "repeat$(2,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " ADDRESS (dec)  ";
if set_op5%=12 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$=" ADDRESS (dec) "
pram_dec$ = gettext$ (pram_dec$,prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb)
pram_dec$ = right$ ("0000"+ pram_dec$,4)

goto gs2pram
end if
end if
print pram_dec$;:color w10.cf,w10.cb:print "  ";
color 7,w10.cb:print chr$(179)

'--- GS2 SET DATA ------------
color 0,w10.cb :print chr$(179);
color w10.cf,w10.cb:print " DATA (decimal) ";
if set_op5%=13 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$=" DATA (dec) "
pram_dat$ = gettext$ (pram_dat$,prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb)
pram_dat$ = right$ ("0000" + pram_dat$,4)
goto gs2pram
end if
end if
print pram_dat$;:color w10.cf,w10.cb:print "  ";
color 7,w10.cb:print chr$(179)

'---- GS2 WRITE DATA ------
color 0,w10.cb:print chr$(179);
if set_op5%=14 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""

if gsdrive%=1 then

pram_low$= right$("00"+ hex$(val(right$(pram_dec$,2))),2)
pram_high$= right$("00"+ hex$(val(left$(pram_dec$,2))),2)
pram_address$ = pram_high$ + pram_low$

newdat$ = "0106"+ pram_address$ + right$("0000" + hex$(val(pram_dat$)),4)
gosub sendGS2
end if
goto gs2pram
end if
end if
print " WRITE ";:color w10.cf,w10.cb:print "    ";

'---- READ GS2 DATA register ----

color 0,w10.cb:print chr$(179);
if set_op5%=15 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
if gsdrive%=1 then

pram_low$= right$("00"+ hex$(val(right$(pram_dec$,2))),2)
pram_high$= right$("00"+ hex$(val(left$(pram_dec$,2))),2)
pram_address$ = pram_high$ + pram_low$

newdat$ = "0103"+ pram_address$ + "0001"

temp_dat$=input$(loc(4),#4) '--- make sure the buffer is empty
gosub sendGS2
while comtmr1?? < comdelay% '--- wait for reply
wend

temp_dat$=input$(loc(4),#4) '--- read the reply from com port
pram_dat$= right$("0000"+ mid$(str$(val("&h" + mid$(temp_dat$,8,4))),2),4)

end if
goto gs2pram
end if
end if
print " READ ";:color w10.cf,w10.cb:print "    ";
color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(22,chr$(196)) chr$(217);


'----GEAR CHANGE SETUP ----------------------------------------------------

view text (40,11)-(68,23)
color w14.cf,w10.cb
color 0,w10.cb:print chr$(218) repeat$(6,chr$(196))" GEAR CHANGES "repeat$(6,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " NUMBER OF GEAR CHANGES ";
if set_op5%=16 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="Number of gear changes "
gearcount? = val(gettext$ (str$(gearcount?),prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb))
if gearcount? >8 then gearcount?=8
if gearcount? <1 then gearcount?=1
goto setdrive
end if
end if
print using"# ";gearcount?;:color 7,w10.cb:print chr$(179)

t?=0
while t? < gearcount?
incr t?
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " GEAR RATIO #" t?"= ";
if set_op5%=16+t? then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="RATIO OF SPINDLE/MOTOR FOR #"+str$(t?)+" "
ratio!(t?)=val(gettext$ (str$(ratio!(t?)),prompt$,34,txth1%,txtv1%,bycopy w10.cf,bycopy w10.cb))
goto setdrive
end if
end if
print using"##.#### ";ratio!(t?);:color 7,w10.cb:print chr$(179)
wend

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " MAX SPINDLE RPM =";
if pwm? then
maxs!=pwmrpm% * ratio!(gearcount?)
else
maxs!=freqmax!/freqbase! * motor% * ratio!(gearcount?)
end if
print using" ####.# ";maxs!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " MIN SPINDLE RPM =";
if pwm? then
mins!=1
else
mins!=freqmin!/freqbase! * motor% * ratio!(1)
end if
print using" ####.# ";mins!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(26,chr$(196)) chr$(217);




while not instat
gosub clkdsp
loop
ky$=inkey$

if ky$=chr$(0,72) and set_op5% > 0 then decr set_op5%
if ky$=chr$(0,80) and set_op5% < 16 + gearcount? then incr set_op5%
if ky$=chr$(27) then gosub sreset:gosub saveall: return
goto gs2pram

return



'----------------------------------SCREEN COLOR --------------------
setcolor:
view text (1,2)-(80,23)
color 7,bgcolor%
cls

newc:
view text (2,2)-(79,4)
color 7,bgcolor%
if cselect%=0 then
if ky$=chr$(61) or ky$=chr$(43) or ky$=chr$(78) then
if pal%(cptr%) < 63 then incr pal%(cptr%):palette cptr%,pal%(cptr%)
end if
if ky$=chr$(45) or ky$=chr$(95) or ky$=chr$(74) then
if pal%(cptr%) > 0 then decr pal%(cptr%):palette cptr%,pal%(cptr%)
end if
print"         ";
for t=0 to 15
if cptr% = t then print "  "chr$(25)" "; else print "    ";
next t
nuc%=pal%(cptr%)
print
print chr$(26)"PALETTE ";
else
print
print " PALETTE ";
end if

for t=0 to 15
color t,bgcolor%
print repeat$(4,chr$(219));
next t
print
color 7,bgcolor%
print "         ";
for t=0 to 15
print using" ## "; t;
next t

lhl%=0
select case cpage%
case = 0:gosub colors1
case = 1:gosub colors2
case = 2:gosub colors3
end select

if cchange%=1 then cchange%=0:goto newc
view text (2,2)-(9,2)
color 7,bgcolor%
print "COLOR ";
print using"##";nuc%;

kyw1:
while not instat
gosub clkdsp
gosub selects
wend
ky$=inkey$

ex1:
if ky$=chr$(27) then
gosub bckdraw
gosub page1
gosub displaytool
screen 0,,2,2
return
end if

if ky$=chr$(0,77) then
if cselect%=0 and cptr% < 15 then incr cptr%
if cselect% > 0 then
incr cselect%,19
'if cselect% + 18 < maxsel% then incr cselect%,19
end if
end if

if ky$=chr$(0,75) then
if cselect%=0 and cptr% > 0 then decr cptr%
if cselect% > 0 then decr cselect%,19:if cselect% <1 then cselect%=1
'if cselect% > 19 then decr cselect%,19
end if

if ky$=chr$(0,80) then
if cpage%=0 then
if cselect% < maxsel% then incr cselect%
else
if cselect% < maxsel% then incr cselect%
end if
end if

if ky$=chr$(0,72) and cselect% > 0 then decr cselect%
if ky$=chr$(0,81) and cpage% < 2 then incr cpage%:goto setcolor
if ky$=chr$(0,73) and cpage% > 0 then decr cpage%:goto setcolor
if ucase$(ky$)="D" then gosub setdefaultcolor
goto newc

'---------------- reset default colors --------
setdefaultcolor:
gosub setpal
palette using pal%(0)

fgmain%=0
bgmain%=2
fgsetup%=0
bgsetup%=2
fgwork%=0
bgwork%=2
fgtool%=0
bgtool%=2
fgfile%=0
bgfile%=2
fgcolor%=0
bgcolor%=1

modecf%=13:modecb%=2
filecf%=12:filecb%=2
vercf%=15:vercb%=2
clkcf%=13:clkcb%=2
fksel%=3
cfsel%=7:cbsel%=1
cfhsel%=0:cbhsel%=5
cfhpic%=7:cbhpic%=4
w1.cf=12
w1.cb=2
w1.bdr=7
w2.cf=3
w2.cb=2
w2.bdr=7
w3.cf=0
w3.cb=3
w3.hf=15
w3.hb=7
w3.bdr=8
w4.cf=3
w4.cb=2
w4.bdr=7
w5.cf=0
w5.cb=5
w5.bdr=8
w6.cf=0
w6.cb=5
w6.bdr=8
w7.cf=0
w7.cb=5
w7.hf=13
w7.hb=2
w7.bdr=8
w8.cf=0
w8.cb=11
w8.hf=7
w8.hb=2
w8.bdr=8
w9.cf=0
w9.cb=5
w9.bdr=0
w10.cf=0
w10.cb=11
w10.hf=7
w10.hb=2
w10.bdr=0
w11.cf=0
w11.cb=1
w11.bdr=7
w12.cf=12
w12.cb=2
w12.hf=7
w12.hb=2
w12.bdr=7
w13.cf=3
w13.cb=2
w13.bdr=7
w14.cf=0
w14.cb=11
w14.hf=7
w14.hb=2
w14.bdr=0
w15.cf=0
w15.cb=5
w15.hf=7
w15.hb=2
w15.bdr=0
w16.cf=12
w16.cb=2
w17.cb=5
w17.bdr=8
w18.cf=0
w18.cb=5
w18.bdr=8
w19.cf=0
w19.cb=5
w19.bdr=8
w20.cf=3
w20.cb=2
w20.bdr=7
w21.cf=12
w21.cb=2
w21.bdr=7
w22.cf=3
w22.cb=2
w22.bdr=7
w23.cf=0
w23.cb=5
w23.bdr=8

return


'------------------------------------------

colors1:
maxsel%=30
if cselect% > maxsel% then cselect%=maxsel%
'---------------------- main screen colors --------
view text (2,5)-(39,23)
color 0,bgcolor%
print "   MAIN RUN SCREEN COLORS "
print "   " chr$(24) chr$(25)" OR "chr$(27) chr$(26) " TO  SELECT"
print "   +/-  TO  CHANGE VALUE"
print

'------------------ MAIN  BACKGROUND
color fgmain%,bgmain%
incr lhl%
if cselect%=lhl% then
nuc%=bgmain%
bgmain% = nucolor% (nuc%,7)
color fgmain%,bgmain%
print chr$(26);
else
print" ";
end if
print"MAIN SCREEN BACKGROUND "


color fgmain%,bgmain%
incr lhl%
if cselect%=lhl% then
nuc%=fgmain%
fgmain% = nucolor% (nuc%,15)
color fgmain%,bgmain%
print chr$(26);
else
print" ";
end if
print"MAIN SCREEN FOREGROUND "

'------------------- position window colors -----
color w1.cf,w1.cb
incr lhl%
if cselect%=lhl% then
nuc%=w1.cb
w1.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"POSITION WINDOW BACKGROUND "

color w1.cf,w1.cb
incr lhl%
if cselect%=lhl% then
nuc%=w1.cf
w1.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"POSITION WINDOW FOREGROUND "

color w1.cf,w1.cb
incr lhl%
if cselect%=lhl% then
nuc%=w1.bdr
w1.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"POSITION WINDOW BORDER ";
color w1.bdr,w1.cb:print repeat$(4,chr$(205))

'-------------- dist window colors ---
color w2.cf,w2.cb
incr lhl%
if cselect%=lhl% then
nuc%=w2.cb
w2.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"DIST WINDOW BACKGROUND "

color w2.cf,w2.cb
incr lhl%
if cselect%=lhl% then
nuc%=w2.cf
w2.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"DIST WINDOW FOREGROUND "

color w2.cf,w2.cb
incr lhl%
if cselect%=lhl% then
nuc%=w2.bdr
w2.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"DIST WINDOW BORDER ";
color w2.bdr,w2.cb:print repeat$(4,chr$(205))

'-------------- tool window colors ---
color w4.cf,w4.cb
incr lhl%
if cselect%=lhl% then
nuc%=w4.cb
w4.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"TOOL WINDOW BACKGROUND "

color w4.cf,w4.cb
incr lhl%
if cselect%=lhl% then
nuc%=w4.cf
w4.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL WINDOW FOREGROUND "

color w4.cf,w4.cb
incr lhl%
if cselect%=lhl% then
nuc%=w4.bdr
w4.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL WINDOW BORDER ";
color w4.bdr,w4.cb:print repeat$(4,chr$(205))


'-------------- file run window colors ---
color w3.cf,w3.cb
incr lhl%
if cselect%=lhl% then
nuc%=w3.cb
w3.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"RUN FILE WINDOW BACKGROUND "

color w3.cf,w3.cb
incr lhl%
if cselect%=lhl% then
nuc%=w3.cf
w3.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"RUN FILE WINDOW FOREGROUND "

color w3.cf,w3.cb
incr lhl%
if cselect%=lhl% then
nuc%=w3.bdr
w3.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"RUN FILE WINDOW BORDER ";
color w3.bdr,w3.cb:print repeat$(4,chr$(205))

color w3.hf,w3.cb
incr lhl%
if cselect%=lhl% then
nuc%=w3.hf
w3.hf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"RUN FILE HILITE FOREGROUND ";


'---------------------------------------------------------------------------
view text (40,5)-(79,23)

color w3.cf,w3.hb
incr lhl%
if cselect%=lhl% then
nuc%=w3.hb
w3.hb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"FILE EDIT CURSOR BACKGROUND "

'-------------- command/GS drive window colors ---
color w18.cf,w18.cb
incr lhl%
if cselect%=lhl% then
nuc%=w18.cb
w18.cb = nucolor% (nuc%,7)
w19.cb = w18.cb
print chr$(26);
else
print" ";
end if
print"CMD/GS2 WINDOW BACKGROUND "

color w18.cf,w18.cb
incr lhl%
if cselect%=lhl% then
nuc%=w18.cf
w18.cf = nucolor% (nuc%,15)
w19.cf = w18.cf
print chr$(26);
else
print" ";
end if
print"CMD/GS2 WINDOW FOREGROUND "

color w18.cf,w18.cb
incr lhl%
if cselect%=lhl% then
nuc%=w18.bdr
w18.bdr = nucolor% (nuc%,15)
w19.bdr = w18.bdr
print chr$(26);
else
print" ";
end if
print"CMD/GS2 WINDOW BORDER ";
color w18.bdr,w18.cb:print repeat$(4,chr$(205))

'-------------- MSG window colors ---
color w6.cf,w6.cb
incr lhl%
if cselect%=lhl% then
nuc%=w6.cb
w6.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"MSG WINDOW BACKGROUND "

color w6.cf,w6.cb
incr lhl%
if cselect%=lhl% then
nuc%=w6.cf
w6.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"MSG WINDOW FOREGROUND "

color w6.cf,w6.cb
incr lhl%
if cselect%=lhl% then
nuc%=w6.bdr
w6.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"MSG WINDOW BORDER ";
color w6.bdr,w6.cb:print repeat$(4,chr$(205))

'-------------- AXIS HOME window colors ---
color w16.cf,w16.cb
incr lhl%
if cselect%=lhl% then
nuc%=w16.cb
w16.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"HOME WINDOW BACKGROUND "

color w16.cf,w16.cb
incr lhl%
if cselect%=lhl% then
nuc%=w16.cf
w16.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"HOME WINDOW FOREGROUND "

'-------------- G code window colors ---
color w5.cf,w5.cb
incr lhl%
if cselect%=lhl% then
nuc%=w5.cb
w5.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"G CODE WINDOW BACKGROUND "

color w5.cf,w5.cb
incr lhl%
if cselect%=lhl% then
nuc%=w5.cf
w5.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"G CODE WINDOW FOREGROUND "

color w5.cf,w5.cb
incr lhl%
if cselect%=lhl% then
nuc%=w5.bdr
w5.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"G CODE WINDOW BORDER ";
color w5.bdr,w5.cb:print repeat$(4,chr$(205))


'-------------- M code window colors ---
color w17.cf,w17.cb
incr lhl%
if cselect%=lhl% then
nuc%=w17.cb
w17.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"M CODE WINDOW BACKGROUND "

color w17.cf,w17.cb
incr lhl%
if cselect%=lhl% then
nuc%=w17.cf
w17.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"M CODE WINDOW FOREGROUND "

color w17.cf,w17.cb
incr lhl%
if cselect%=lhl% then
nuc%=w17.bdr
w17.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"M CODE WINDOW BORDER ";
color w17.bdr,w17.cb:print repeat$(4,chr$(205))

color 0,bgcolor%
print
print "   PAGE DOWN FOR MORE "
return


'-----------------------------------COLOR page 2 ------------------------
colors2:
'-----------------------TOOL OFFSET BACKGROUND
maxsel%=35
if cselect% > maxsel% then cselect%=maxsel%
view text (2,5)-(39,23)
color 0,bgcolor%
print "    TOOL,WORK & FILE SCREEN COLORS"
print "   " chr$(24) chr$(25)" OR "chr$(27) chr$(26) " TO  SELECT"
print "   +/-  TO  CHANGE VALUE"
'print


'--------------------------------
color fgtool%,bgtool%
incr lhl%
if cselect%=lhl% then
nuc%=bgtool%
bgtool% = nucolor% (nuc%,7)
color fgtool%,bgtool%
print chr$(26);
else
print" ";
end if
print"TOOL OFFSET BACKGROUND "


color fgtool%,bgtool%
incr lhl%
if cselect%=lhl% then
nuc%=fgtool%
fgtool% = nucolor% (nuc%,15)
color fgtool%,bgtool%
print chr$(26);
else
print" ";
end if
print"TOOL OFFSET FOREGROUND "

'-------------- D/H offset window colors ---
color w8.cf,w8.cb
incr lhl%
if cselect%=lhl% then
nuc%=w8.cb
w8.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print "TOOL OFFSET WINDOW BACKGROUND "

color w8.cf,w8.cb
incr lhl%
if cselect%=lhl% then
nuc%=w8.cf
w8.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL OFFSET WINDOW FOREGROUND "

color w8.cf,w8.cb
incr lhl%
if cselect%=lhl% then
nuc%=w8.bdr
w8.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL OFFSET WINDOW BORDER ";
color w8.bdr,w8.cb:print repeat$(4,chr$(205))

color w8.hf,w8.hb
incr lhl%
if cselect%=lhl% then
nuc%=w8.hb
w8.hb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"TOOL OFFSET HILITE BACKGROUND "

color w8.hf,w8.hb
incr lhl%
if cselect%=lhl% then
nuc%=w8.hf
w8.hf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL OFFSET HILITE FOREGROUND "

'-------------- TOOL OFFSET INPUT window colors ---
color w9.cf,w9.cb
incr lhl%
if cselect%=lhl% then
nuc%=w9.cb
w9.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print "TOOL INPUT WINDOW BACKGROUND "

color w9.cf,w9.cb
incr lhl%
if cselect%=lhl% then
nuc%=w9.cf
w9.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL INPUT WINDOW FOREGROUND "

color w9.cf,w9.cb
incr lhl%
if cselect%=lhl% then
nuc%=w9.bdr
w9.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL INPUT WINDOW BORDER ";
color w9.bdr,w9.cb:print repeat$(4,chr$(205))

'-------------- TOOL abs position display window colors ---
color w21.cf,w21.cb
incr lhl%
if cselect%=lhl% then
nuc%=w21.cb
w21.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print "TOOL ABS DISPLAY BACKGROUND "

color w21.cf,w21.cb
incr lhl%
if cselect%=lhl% then
nuc%=w21.cf
w21.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL ABS DISPLAY FOREGROUND "

color w21.cf,w21.cb
incr lhl%
if cselect%=lhl% then
nuc%=w21.bdr
w21.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL ABS DISPLAY BORDER ";
color w21.bdr,w21.cb:print repeat$(4,chr$(205))


'-------------- TOOL abs position display window colors ---
color w22.cf,w22.cb
incr lhl%
if cselect%=lhl% then
nuc%=w22.cb
w22.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print "TOOL MACH DISPLAY BACKGROUND "

color w22.cf,w22.cb
incr lhl%
if cselect%=lhl% then
nuc%=w22.cf
w22.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL MACH DISPLAY FOREGROUND "

color w22.cf,w22.cb
incr lhl%
if cselect%=lhl% then
nuc%=w22.bdr
w22.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL MACH DISPLAY BORDER ";
color w22.bdr,w22.cb:print repeat$(4,chr$(205));


'-----------------------------
view text (40,5)-(79,23)

'-------------- TOOL command window colors ---
color w23.cf,w23.cb
incr lhl%
if cselect%=lhl% then
nuc%=w23.cb
w23.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print "TOOL COMMAND WINDOW BACKGROUND "

color w23.cf,w23.cb
incr lhl%
if cselect%=lhl% then
nuc%=w23.cf
w23.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL COMMAND WINDOW FOREGROUND "

color w23.cf,w23.cb
incr lhl%
if cselect%=lhl% then
nuc%=w23.bdr
w23.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TOOL COMMAND WINDOW BORDER ";
color w23.bdr,w23.cb:print repeat$(4,chr$(205))



'----------------- work offset background --- ---- --- --- --

color fgwork%,bgwork%
incr lhl%
if cselect%=lhl% then
nuc%=bgwork%
bgwork% = nucolor% (nuc%,7)
color fgwork%,bgwork%
print chr$(26);
else
print" ";
end if
print"WORK OFFSET BACKGROUND "

color fgwork%,bgwork%
incr lhl%
if cselect%=lhl% then
nuc%=fgwork%
fgwork% = nucolor% (nuc%,15)
color fgwork%,bgwork%
print chr$(26);
else
print" ";
end if
print"WORK OFFSET FOREGROUND "


'-------------- WORK OFFSET window colors ---
color w14.cf,w14.cb
incr lhl%
if cselect%=lhl% then
nuc%=w14.cb
w14.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print "WORK OFFSET WINDOW BACKGROUND "

color w14.cf,w14.cb
incr lhl%
if cselect%=lhl% then
nuc%=w14.cf
w14.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"WORK OFFSET WINDOW FOREGROUND "


color w14.cf,w14.cb
incr lhl%
if cselect%=lhl% then
nuc%=w14.bdr
w14.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"WORK OFFSET WINDOW BORDER ";
color w14.bdr,w14.cb:print repeat$(4,chr$(205))

color w14.hf,w14.hb
incr lhl%
if cselect%=lhl% then
nuc%=w14.hb
w14.hb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"WORK OFFSET HILITE BACKGROUND "

color w14.hf,w14.hb
incr lhl%
if cselect%=lhl% then
nuc%=w14.hf
w14.hf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"WORK OFFSET HILITE FOREGROUND "

'-------------- WORK OFFSET INPUT window colors ---
color w11.cf,w11.cb
incr lhl%
if cselect%=lhl% then
nuc%=w11.cb
w11.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print "WRK OFFSET INPUT WINDOW BACKGROUND "

color w11.cf,w11.cb
incr lhl%
if cselect%=lhl% then
nuc%=w11.cf
w11.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"WRK OFFSET INPUT WINDOW FOREGROUND "

color w11.cf,w11.cb
incr lhl%
if cselect%=lhl% then
nuc%=w11.bdr
w11.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"WRK OFFSET INPUT WINDOW DORDER ";
color w11.bdr,w11.cb:print repeat$(4,chr$(205))


'-------------- WORK ABS DISPLAY window colors ---
color w12.cf,w12.cb
incr lhl%
if cselect%=lhl% then
nuc%=w12.cb
w12.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print "WORK ABS POSITION WINDOW BACKGROUND "

color w12.cf,w12.cb
incr lhl%
if cselect%=lhl% then
nuc%=w12.cf
w12.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"WORK ABS POSITION WINDOW FOREGROUND "

color w12.cf,w12.cb
incr lhl%
if cselect%=lhl% then
nuc%=w12.bdr
w12.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"WORK ABS POSITION WINDOW BORDER ";
color w12.bdr,w12.cb:print repeat$(4,chr$(205))



'-------------- WORK MACHINE POSITION DISPLAY window colors ---
color w13.cf,w13.cb
incr lhl%
if cselect%=lhl% then
nuc%=w13.cb
w13.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print "WORK MACH POSITION WINDOW BACKGROUND "

color w13.cf,w13.cb
incr lhl%
if cselect%=lhl% then
nuc%=w13.cf
w13.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"WORK MACH POSITION WINDOW FOREGROUND "

color w13.cf,w13.cb
incr lhl%
if cselect%=lhl% then
nuc%=w13.bdr
w13.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"WORK MACH POSITION WINDOW BORDER ";
color w13.bdr,w13.cb:print repeat$(4,chr$(205));

'color 0,bgcolor%
'print
'print "   PAGE UP/DOWN FOR MORE ";
return

'--------------------------------- COLOR page 3 -------------------------
colors3:
maxsel%=33
if cselect% > maxsel% then cselect%=maxsel%
view text (2,5)-(39,23)
color 0,bgcolor%
print "    PRAM,MENU & TITLE BAR COLORS"
print "   " chr$(24) chr$(25)" OR "chr$(27) chr$(26) " TO  SELECT"
print "   +/-  TO  CHANGE VALUE"
'print


'-------------- file window colors ---

color fgfile%,bgfile%
incr lhl%
if cselect%=lhl% then
nuc%=bgfile%
bgfile% = nucolor% (nuc%,7)
color fgfile%,bgfile%
print chr$(26);
else
print" ";
end if
print"FILE BACKGROUND "

color fgfile%,bgfile%
incr lhl%
if cselect%=lhl% then
nuc%=fgfile%
fgfile% = nucolor% (nuc%,15)
color fgfile%,bgfile%
print chr$(26);
else
print" ";
end if
print"FILE FOREGROUND "


color w7.cf,w7.cb
incr lhl%
if cselect%=lhl% then
nuc%=w7.cb
w7.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"FILE WINDOW BACKGROUND "

color w7.cf,w7.cb
incr lhl%
if cselect%=lhl% then
nuc%=w7.cf
w7.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"FILE WINDOW FOREGROUND "

color w7.cf,w7.cb
incr lhl%
if cselect%=lhl% then
nuc%=w7.bdr
w7.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"FILE WINDOW BORDER ";
color w7.bdr,w7.cb:print repeat$(4,chr$(205))

color w7.hf,w7.hb
incr lhl%
if cselect%=lhl% then
nuc%=w7.hb
w7.hb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"FILE HILITE BACKGROUND "

color w7.hf,w7.hb
incr lhl%
if cselect%=lhl% then
nuc%=w7.hf
w7.hf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"FILE HILITE FOREGROUND "

'-------------------- menu bar colors -------

color fksel%,cbsel%
incr lhl%
if cselect%=lhl% then
nuc%=fksel%
fksel% = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"F-KEY NUMBER COLOR "

color cfsel%,cbsel%
incr lhl%
if cselect%=lhl% then
nuc%=cbsel%
cbsel% = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"MENU BACKGROUND "

color cfsel%,cbsel%
incr lhl%
if cselect%=lhl% then
nuc%=cfsel%
cfsel% = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"MENU FOREGROUND "

color cfhsel%,cbhsel%
incr lhl%
if cselect%=lhl% then
nuc%=cfhsel%
cfhsel% = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"MENU HILITE FOREGROUND "

color cfhsel%,cbhsel%
incr lhl%
if cselect%=lhl% then
nuc%=cbhsel%
cbhsel% = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"MENU HILITE BACKGROUND "

color cfhpic%,cbhpic%
incr lhl%
if cselect%=lhl% then
nuc%=cfhpic%
cfhpic% = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"MENU MODE HILITE FOREGROUND "

color cfhpic%,cbhpic%
incr lhl%
if cselect%=lhl% then
nuc%=cbhpic%
cbhpic% = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"MENU MODE HILITE BACKGROUND "

color modecf%,modecb%
incr lhl%
if cselect%=lhl% then
nuc%=modecb%
modecb% = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"TITLE MODE BACKGROUND "

color modecf%,modecb%
incr lhl%
if cselect%=lhl% then
nuc%=modecf%
modecf% = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TITLE MODE FOREGROUND ";


'-------------------------------------
view text (40,5)-(79,23)

'-------------- PARAMETER windows colors ---


color filecf%,filecb%
incr lhl%
if cselect%=lhl% then
nuc%=filecb%
filecb% = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"TITLE FILENAME BACKGROUND "

color filecf%,filecb%
incr lhl%
if cselect%=lhl% then
nuc%=filecf%
filecf% = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TITLE FILENAME FOREGROUND "


color vercf%,vercb%
incr lhl%
if cselect%=lhl% then
nuc%=vercb%
vercb% = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"TITLE VERSION BACKGROUND "

color vercf%,vercb%
incr lhl%
if cselect%=lhl% then
nuc%=vercf%
vercf% = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TITLE VERSION FOREGROUND "


color clkcf%,clkcb%
incr lhl%
if cselect%=lhl% then
nuc%=clkcb%
clkcb% = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"TITLE CLOCK BACKGROUND "

color clkcf%,clkcb%
incr lhl%
if cselect%=lhl% then
nuc%=clkcf%
clkcf% = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"TITLE CLOCK FOREGROUND "


color w10.cf,w10.cb
incr lhl%
if cselect%=lhl% then
nuc%=w10.cb
w10.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print "PARAMETER WINDOW BACKGROUND "

color w10.cf,w10.cb
incr lhl%
if cselect%=lhl% then
nuc%=w10.cf
w10.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"PARAMETER WINDOW FOREGROUND "

color w10.cf,w10.cb
incr lhl%
if cselect%=lhl% then
nuc%=w10.bdr
w10.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"PARAMETER WINDOW BORDER ";
color w10.bdr,w10.cb:print repeat$(4,chr$(205))

color w10.hf,w10.hb
incr lhl%
if cselect%=lhl% then
nuc%=w10.hb
w10.hb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"PARAMETER HILITE BACKGROUND "

color w10.hf,w10.hb
incr lhl%
if cselect%=lhl% then
nuc%=w10.hf
w10.hf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"PARAMETER HILITE FOREGROUND "


'------------------------ setup colors
color 7,bgsetup%
incr lhl%
if cselect%=lhl% then
nuc%=bgsetup%
bgsetup% = nucolor% (nuc%,7)
color 7,bgsetup%
print chr$(26);
else
print" ";
end if
print"SETUP BACKGROUND "

'-------------- setup window colors ---
color w15.cf,w15.cb
incr lhl%
if cselect%=lhl% then
nuc%=w15.cb
w15.cb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"SETUP WINDOW BACKGROUND "

color w15.cf,w15.cb
incr lhl%
if cselect%=lhl% then
nuc%=w15.cf
w15.cf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"SETUP WINDOW FOREGROUND "

color w15.cf,w15.cb
incr lhl%
if cselect%=lhl% then
nuc%=w15.bdr
w15.bdr = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"SETUP WINDOW BORDER ";
color w15.bdr,w15.cb:print repeat$(4,chr$(205))

color w15.hf,w15.hb
incr lhl%
if cselect%=lhl% then
nuc%=w15.hb
w15.hb = nucolor% (nuc%,7)
print chr$(26);
else
print" ";
end if
print"SETUP HILITE BACKGROUND "

color w15.hf,w15.hb
incr lhl%
if cselect%=lhl% then
nuc%=w15.hf
w15.hf = nucolor% (nuc%,15)
print chr$(26);
else
print" ";
end if
print"SETUP HILITE FOREGROUND "

color 0,bgcolor%
print
print "   PAGE UP FOR MORE ";


return
'-------------- end color setup -------------


'----------- + & - KEY CHECK FUNCTION ---- - - - - - - - - - - - - - - - - -
function nucolor% (oldcolor%,places%)
shared ky$
shared cchange%
if ky$=chr$(61) or ky$=chr$(43) or ky$=chr$(78) then
if oldcolor% < places% then incr oldcolor%
cchange%=1
end if
if ky$=chr$(45) or ky$=chr$(95) or ky$=chr$(74) then
if oldcolor% > 0 then decr oldcolor%
cchange%=1
end if
ky$=""
nucolor% = oldcolor%
exit function
end function

$segment

'---------------------------------- SETTINGS --------------------
setkyin:
view text (1,2)-(80,23)
color w10.cf,w10.cb
cls
view text (40,3)-(80,6)
color w14.cf,w10.cb
print "        ESC TO EXIT"
print "   " chr$(24) chr$(25)" TO  SELECT OPTION"
print "   ENTER  TO  CHANGE VALUE"
'txth%=24:txtv%=4
setkyin2:


view text (2,2)-(37,5)
color 0,w10.cb:print chr$(218) repeat$ (28,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " SAVE POINT DATA: ";
if set_op4%=0 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
if savedat%=1 then savedat%=0 else savedat%=1
gosub opndatsav:goto setkyin2
end if
end if
if savedat%=1 then print " ENABLED  "; else print " DISABLED ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " File";
if set_op4%=1 then
color w10.hf,w10.hb
if ky$=chr$(13) then
if savedat%=1 then savedat%=0:gosub opndatsav
ky$="":prompt$="Enter filename for G31 data"
datafile$ = gettext$ (datafile$,prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)
goto setkyin
end if
end if
if len(datafile$) < 20 then spa$=space$(10-len(datafile$)/2) else spa$=""
print using" \                    \";spa$+datafile$;
color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(28,chr$(196)) chr$(217);



view text (2,6)-(29,15)
'view text (2,6)-(29,14)

color w14.cf,w10.cb
'print "     AXIS SETTINGS "
'color 0,w10.cb:print chr$(218) repeat$ (25,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);

color w10.cf,w10.cb:print " DEFAULT UNITS     ";
if set_op4%=2 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
if defunits% then
defunits%=0
nunits%=0
else
defunits%=1
nunits%=1
end if
gosub setsys
goto setkyin2
end if
end if
if defunits%=1 then print "   MM "; else print " INCH ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " X AXIS MIRROR      ";:if set_op4%=3 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if mirrx%=1 then mirrx%=-1 else mirrx%=1:goto setkyin2
if mirrx%=1 then print " OFF "; else print " ON  ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Y AXIS MIRROR      ";:if set_op4%=4 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if mirry%=1 then mirry%=-1 else mirry%=1:goto setkyin2
if mirry%=1 then print " OFF "; else print " ON  ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " 4th AXIS IS ROTARY ";:if set_op4%=5 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if rotarya%=1 then rotarya%=0 else rotarya%=1:goto setkyin2
if rotarya%=1 then print " ON  "; else print " OFF ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " ROTARY FEED MUL";
if set_op4%=6 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Feed rate multiplier for A"
wfct!=val(gettext$ (str$(wfct!),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
if wfct!> max_wfct! then wfct!=max_wfct!
if wfct! < 1 then wfct!=1
goto setkyin
end if
end if
print using"###.#### ";wfct!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " SCALE X ";
if set_op4%=7 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Scale for X machine axis"
xscale#=val(gettext$ (str$(xscale#),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
if iscale! then iscale#=xscale# else iscale#=1.
goto setkyin
end if
end if
print using"##.##### ";xscale#;

color w10.cf,w10.cb:print "  I ";
if set_op4%=8 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":if iscale!=0 then iscale!=1 else iscale!= 0
if iscale! then iscale#=xscale# else iscale#=1.
goto setkyin
end if
end if
if iscale!=1 then print " on"; else print "off";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " SCALE Y ";
if set_op4%=9 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Scale for Y machine axis"
yscale#=val(gettext$ (str$(yscale#),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
if jscale! then jscale#=yscale# else jscale#=1.
goto setkyin
end if
end if
print using"##.##### ";yscale#;

color w10.cf,w10.cb:print "  J ";
if set_op4%=10 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":if jscale!=0 then jscale!=1 else jscale!= 0
if jscale! then jscale#=yscale# else jscale#=1.
goto setkyin
end if
end if
if jscale!=1 then print " on"; else print "off";
color 7,w10.cb:print chr$(179)


color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " SCALE Z ";
if set_op4%=11 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Scale for Z machine axis"
zscale#=val(gettext$ (str$(zscale#),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)):goto setkyin
if kscale! then kscale#=zscale# else kscale#=1.
goto setkyin
end if
end if
print using"##.##### ";zscale#;

color w10.cf,w10.cb:print "  K ";
if set_op4%=12 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":if kscale!=0 then kscale!=1 else kscale!= 0
if kscale! then kscale#=zscale# else kscale#=1.
goto setkyin
end if
end if
if kscale!=1 then print " on"; else print "off";
color 7,w10.cb:print chr$(179)



color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " SCALE A AXIS   ";
if set_op4%=13 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Scale for A machine axis"
wscale#=val(gettext$ (str$(wscale#),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using"##.##### ";wscale#;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(25,chr$(196)) chr$(217);

'------ Soft limit settings -----------------------------------

view text (2,16)-(29,23)

color w14.cf,w10.cb
'print "     SOFT LIMITS "
color 0,w10.cb:print chr$(218) repeat$ (25,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);


color w10.cf,w10.cb:print " SOFT LIMITS   ";:if set_op4%=14 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if softlim%=1 then softlim%=0 else softlim%=1:goto setkyin2
if softlim%=1 then print "  ENABLED "; else print " DISABLED ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " AXIS"chr$(179)"  UPPER  "chr$(179)"  LOWER  ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print "  X  "chr$(179);:if set_op4%=15 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="X Axis HI soft limit"_
:limxhi& = spi?? * val(gettext$ (str$(limxhi&/spi??*metric!,6),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!:goto setkyin
print using limout$;limxhi&/spi??*metric!;
'color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
if set_op4%=16 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="X Axis LO soft limit"_
:limxlo& = spi?? * val(gettext$ (str$(limxlo&/spi??*metric!,6),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!:goto setkyin
print using limout$;limxlo&/spi??*metric!;:color 7,w10.cb:print chr$(179)


color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print "  Y  "chr$(179);:if set_op4%=17 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="Y Axis HI soft limit"_
:limyhi& = spi?? * val(gettext$ (str$(limyhi&/spi??*metric!,6),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!:goto setkyin
print using limout$;limyhi&/spi??*metric!;
'color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
if set_op4%=18 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="Y Axis LO soft limit"_
:limylo& = spi?? * val(gettext$ (str$(limylo&/spi??*metric!,6),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!:goto setkyin
print using limout$;limylo&/spi??*metric!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print "  Z  "chr$(179);:if set_op4%=19 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="Z Axis HI soft limit"_
:limzhi& = spi?? * val(gettext$ (str$(limzhi&/spi??*metric!,6),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!:goto setkyin
print using limout$;limzhi&/spi??*metric!;
'color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
if set_op4%=20 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="Z Axis LO soft limit"_
:limzlo& = spi?? * val(gettext$ (str$(limzlo&/spi??*metric!,6),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!:goto setkyin
print using limout$;limzlo&/spi??*metric!;:color 7,w10.cb:print chr$(179)


color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print "  A  "chr$(179);
if set_op4%=21 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="A Axis HI soft limit"
if rotarya% then
limwhi& = spi?? * val(gettext$ (str$(limwhi&/spi??*wfct!,6),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/wfct!
else
limwhi& = spi?? * val(gettext$ (str$(limwhi&/spi??*metric!,6),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!
end if
goto setkyin
end if
end if
if rotarya% then
print using limout$;limwhi&/spi?? * wfct!;
else
print using limout$;limwhi&/spi??*metric!;
end if
'color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
if set_op4%=22 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
prompt$="A Axis LO soft limit"
if rotarya% then
limwlo& = spi?? * val(gettext$ (str$(limwlo&/spi??*wfct!,6),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/wfct!
else
limwlo& = spi?? * val(gettext$ (str$(limwlo&/spi??*metric!,6),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!
end if
goto setkyin
end if
end if
if rotarya% then
print using limout$;limwlo&/spi?? * wfct!;
else
print using limout$;limwlo&/spi??*metric!;
end if
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(25,chr$(196)) chr$(217);

'-----------------------------------------

view text (29,6)-(57,13)

color w14.cf,w10.cb
'print "  MACHINE HOME  "
color 0,w10.cb:print chr$(218) repeat$ (26,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " HOME FEEDRATE    ";
if set_op4%=23 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Home feedrate in units per min"
homefeed1!=val(gettext$ (str$((homefeed1!*metric!)),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!
if homefeed1! > maxfeed! then homefeed1!=maxfeed!
goto setkyin
end if
end if
print using"#####.# ";homefeed1!*metric!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " LIMIT RETURN RATIO  1/";
if set_op4%=24 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Limit return feed ratio 1/x"
retratio%=val(gettext$ (str$(retratio%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
if retratio% > 99 then retratio%=99
goto setkyin
end if
end if
print using"## ";retratio%;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " LIMIT CLEAR DIST,  ";
if set_op4%=25 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Clear limits in encoder steps"
limclear%=val(gettext$ (str$(limclear%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using" #### ";limclear%;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " X HOME DIRECTION   ";:if set_op4%=26 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if dirhomex%=1 then dirhomex%=0 else dirhomex%=1:goto setkyin2
if dirhomex%=1 then print "  +   "; else print "  -   ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Y HOME DIRECTION   ";:if set_op4%=27 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if dirhomey%=1 then dirhomey%=0 else dirhomey%=1:goto setkyin2
if dirhomey%=1 then print "  +   "; else print "  -   ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Z HOME DIRECTION   ";:if set_op4%=28 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if dirhomez%=1 then dirhomez%=0 else dirhomez%=1:goto setkyin2
if dirhomez%=1 then print "  +   "; else print "  -   ";
color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(26,chr$(196)) chr$(217);

'----set as lathe or mill --------------------------
view text (29,14)-(57,16)

color 0,w10.cb:print chr$(218) repeat$ (26,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " CONTROL SETUP FOR ";
if set_op4%=29 then
color w10.hf,w10.hb

if ky$=chr$(13) then
ky$=""
if lathe_bit%=1 then lathe_bit%=0 else lathe_bit%=1
gosub bckdraw
gosub displaytool
screen 0,,2,2
goto setkyin
end if
end if
if lathe_bit%=1 then print " LATHE "; else print " MILL  ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(26,chr$(196)) chr$(217);

'---------MPG setings-----------------------------

view text (29,17)-(57,23)

color 0,w10.cb:print chr$(218,196,196);
color w14.cf,w10.cb:print " MPG STEP CONVERSION ";
color 0,w10.cb:print chr$(196,196,196);
color 7,w10.cb:print chr$(191)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " X1  ";
if set_op4%=30 then color w10.hf,w10.hb:if ky$=chr$(13) then _
ky$="":prompt$="Encoder steps per machine step"_
:h1pulse%=val(gettext$ (str$(h1pulse%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)) _
:goto setkyin
print using"###";h1pulse%;
color w10.cf,w10.cb:print " HAND = ";
if set_op4%=31 then color w10.hf,w10.hb:if ky$=chr$(13) then _
ky$="":prompt$="Machine steps per encoder step"_
:h1step%=val(gettext$ (str$(h1step%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)) _
:goto setkyin
print using"###";h1step%;:color w10.cf,w10.cb:print " STEPS ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " X10 ";
if set_op4%=32 then color w10.hf,w10.hb:if ky$=chr$(13) then _
ky$="":prompt$="Encoder steps per machine step"_
:h10pulse%=val(gettext$ (str$(h10pulse%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)) _
:goto setkyin
print using"###";h10pulse%;
color w10.cf,w10.cb:print " HAND = ";
if set_op4%=33 then color w10.hf,w10.hb:if ky$=chr$(13) then _
ky$="":prompt$="Machine steps per encoder step"_
:h10step%=val(gettext$ (str$(h10step%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)) _
:goto setkyin
print using"###";h10step%;:color w10.cf,w10.cb:print " STEPS ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " X100";
if set_op4%=34 then color w10.hf,w10.hb:if ky$=chr$(13) then _
ky$="":prompt$="Encoder steps per machine step"_
:h100pulse%=val(gettext$ (str$(h100pulse%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)) _
:goto setkyin
print using"###";h100pulse%;
color w10.cf,w10.cb:print " HAND = ";
if set_op4%=35 then color w10.hf,w10.hb:if ky$=chr$(13) then _
ky$="":prompt$="Machine steps per encoder step"_
:h100step%=val(gettext$ (str$(h100step%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)) _
:goto setkyin
print using"###";h100step%;:color w10.cf,w10.cb:print " STEPS ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " ADDER ";
if set_op4%=36 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Adder for each encoder step"
handadder%=val(gettext$ (str$(handadder%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
if handadder% > handcarry% then handadder%= handcarry%
:goto setkyin
end if
end if
print using"#####";handadder%;
color w10.cf,w10.cb:print "  CARRY ";
if set_op4%=37 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Carry point when step is made"
:handcarry%=val(gettext$ (str$(handcarry%),prompt$,35,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
if handadder% > handcarry% then handcarry%= handadder%
:goto setkyin
end if
end if
print using"##### ";handcarry%;
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " MPG STEP SOFTNESS  ";
if set_op4%=38 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Handle softness adjustment 1-4 "
:handsoft%=val(gettext$ (str$(handsoft%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
if handsoft%>4 then handsoft%=4
if handsoft% < 1 then handsoft%=1
:goto setkyin
end if
end if
print using"##### ";handsoft%;
color 7,w10.cb:print chr$(179)


color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(26,chr$(196)) chr$(217);


'------------------------------
view text (57,6)-(80,9)

color 0,w10.cb:print chr$(218) repeat$ (21,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " ON-SCREEN HELP ";
if set_op4%=39 then
color w10.hf,w10.hb

if ky$=chr$(13) then
ky$=""
if helpbit%=1 then helpbit%=0 else helpbit%=1
gosub bckdraw
gosub displaytool
screen 0,,2,2
goto setkyin
end if
end if
if helpbit%=1 then print " ON  "; else print " OFF ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " SETUP PASSWORD";
if set_op4%=40 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$=" Enter NEW SETUP PASSWORD "
password$ =gettext$ (password$,prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)
goto setkyin
end if
end if
print "******";:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(21,chr$(196)) chr$(217);

'-----------------------------------------

view text (57,10)-(80,16)

color w14.cf,w10.cb
print "  CANNED CYCLE "
color 0,w10.cb:print chr$(218) repeat$ (21,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " G83/73 R-stop  ";
if set_op4%=41 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="stop rapid short in steps"
pram83%=val(gettext$ (str$(pram83%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using"#### ";pram83%;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " SPDL REV DELAY ";
if set_op4%=42 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="Spindle relay delay MS"_
:spndelay??=val(gettext$ (str$(spndelay??),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)):goto setkyin
print using"#### ";spndelay??;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " G84/74 REV DLY ";
if set_op4%=43 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="Spindle reverse delay MS"_
:srevtime??=val(gettext$ (str$(srevtime??),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)):goto setkyin
print using"#### ";srevtime??;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(21,chr$(196)) chr$(217);

'--------------------------------------

view text (57,16)-(80,23)

color w14.cf,w10.cb
print " JOG KEY SCAN CODES "
color 0,w10.cb:print chr$(218) repeat$ (21,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);

color w10.cf,w10.cb:print " +X =";
if set_op4%=44 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Default key is 75"
jx1?=val(gettext$ (str$(jx1?),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using"### ";jx1?;

color w10.cf,w10.cb:print "    -X =";
if set_op4%=45 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Default key is 77"
jx2?=val(gettext$ (str$(jx2?),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using"### ";jx2?;
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " +Y =";
if set_op4%=46 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="default key is 80"
jy1?=val(gettext$ (str$(jy1?),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using"### ";jy1?;

color w10.cf,w10.cb:print "    -Y =";
if set_op4%=47 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="default key is 72"
jy2?=val(gettext$ (str$(jy2?),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using"### ";jy2?;
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " +Z =";
if set_op4%=48 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Default key is 81"
jz1?=val(gettext$ (str$(jz1?),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using"### ";jz1?;

color w10.cf,w10.cb:print "    -Z =";
if set_op4%=49 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Default key is 73"
jz2?=val(gettext$ (str$(jz2?),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using"### ";jz2?;
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " +A =";
if set_op4%=50 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="default key is 83"
jw1?=val(gettext$ (str$(jw1?),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using"### ";jw1?;
color w10.cf,w10.cb:print "    -A =";
if set_op4%=51 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="default key is 82"
jw2?=val(gettext$ (str$(jw2?),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto setkyin
end if
end if
print using"### ";jw2?;
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(21,chr$(196)) chr$(217);

'-------------------------------------

while not instat
gosub clkdsp
loop
ky$=inkey$

if ky$=chr$(0,72) and set_op4% > 0 then decr set_op4%
if ky$=chr$(0,80) and set_op4% < 51 then incr set_op4%
if ky$=chr$(27) then gosub sreset:gosub saveall: return
goto setkyin2

return

'------------------------- PARAMETER INPUT SCREEN -----------
pramkyin:
view text (1,2)-(80,23)
color w10.cf,w10.cb
cls
pramkyin2:
view text (40,3)-(80,6)
color w14.cf,w10.cb
print "        ESC TO EXIT"
print "   " chr$(24) chr$(25)" TO  SELECT OPTION"
print "   ENTER  TO  CHANGE VALUE"


view text (4,2)-(34,6)
color 0,w10.cb:print chr$(218) repeat$ (28,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print "     FILE DIRECTORY PATH    ";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(179);
if set_op3%=0 then color w10.hf,w10.hb:if ky$=chr$(13) then ky$="":prompt$="Enter new directory path"_
:dirname$ = gettext$ (dirname$,prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb) _
:if right$(dirname$,1)<> "\" then dirname$=dirname$+"\":goto pramkyin else goto  pramkyin
if len(dirname$) < 24 then spa$=space$(12-len(dirname$)/2) else spa$=""
print using" \                         \";spa$+dirname$;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " DEFAULT FILE EXT.=   ";
if set_op3%=1 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Default file extension  .xxx"
tmp$ = ucase$( gettext$ (dfext$,prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
if left$(tmp$,1) = "." then dfext$=left$(tmp$,4) else dfext$=""
goto pramkyin
end if
end if
print using"\    \"; dfext$;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(28,chr$(196)) chr$(217);


view text (4,7)-(32,10)

color 0,w10.cb:print chr$(218) repeat$ (26,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " EXT-FEED CONTROLS   ";
if set_op3%=2 then
	color w10.hf,w10.hb
	if ky$=chr$(13) then
		ky$=""
		if extfeed%=1 then
		extfeed%=0
		extfin?=1
		lastrin?=4
		lastfin?=10
		else
		extfeed%=1
		end if
	goto pramkyin2
	end if
end if
if extfeed%=1 then print " ON  "; else print " OFF ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(26,chr$(196)) chr$(217);


view text (4,10)-(32,17)

color 0,w10.cb:print chr$(218) repeat$ (26,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb
if defunits% then
print " MAX FEED MM/Min  ";
prompt$="Max feed in MM/Min    "
else
print " MAX FEED Inch/Min";
prompt$="Max feed in Inch/Min"
end if
if set_op3%=3 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
maxfeed!=val(gettext$ (str$((maxfeed!*metric!)),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!
goto pramkyin
end if
end if
print using"#####.# "; maxfeed!*metric!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb
if defunits% then
print " ACCEL. MM/Sec     ";
prompt$="Acceleration in MM per sec"
else
print " ACCEL. Inch/Sec   ";
prompt$="Acceleration in inches per sec"
end if
if set_op3%=4 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
setaccl:
accl!=val(gettext$ (str$((accl!*metric!)),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!
if accl! <= 0 then prompt$="Must be greater than 0 ":accl!=1:goto setaccl
goto pramkyin
end if
end if
print using"####.# "; accl!*metric!;:color 7,w10.cb:print chr$(179)

'color 0,w10.cb:print chr$(218) repeat$ (26,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " CORNER DECELERATION ";:if set_op3%=5 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if acelg0%=1 then acelg0%=0 else acelg0%=1:goto pramkyin2
if acelg0%=1 then print "AUTO "; else print "FULL ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " CORNER DECL- GAIN  ";
if set_op3%=6 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Corner slowdown gain multiplier"
corner_gain!=val(gettext$ (str$((corner_gain!)),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
goto pramkyin
end if
end if
print using" ##.# ";corner_gain!;:color 7,w10.cb:print chr$(179)


color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " FEED +/- INCREMENT ";
if set_op3%=7 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Jog feed increment/decrement"
feedinc!=val(gettext$ (str$((feedinc!*metric!)),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))/metric!
goto pramkyin
end if
end if
print using"###.# ";feedinc!*metric!;:color 7,w10.cb:print chr$(179)


color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb
if defunits% then
print " Stop dist  MM   ";
else
print " Stop dist Inch  ";
end if
if accl! <= 0 then goto setaccl
stp1! =  (maxfeed! / 60)*(maxfeed! / 60)*.5 / accl!
print using" ###.### ";stp1!*metric!;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(26,chr$(196)) chr$(217);


view text (4,18)-(32,23)

color 0,w10.cb:print chr$(218) repeat$ (26,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " X STEPS PER UNIT   ";
if set_op3%=8 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Steps per unit for X axis"
spix??=val(gettext$ (str$((CSNG(spix??/metric!))),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))*metric!
goto pramkyin
end if
end if
print using"##### ";spix??/metric!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Y STEPS PER UNIT   ";
if set_op3%=9 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Steps per unit for Y axis"
spiy??=val(gettext$ (str$((CSNG(spiy??/metric!))),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))*metric!
goto pramkyin
end if
end if
print using"##### ";spiy??/metric!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Z STEPS PER UNIT   ";
if set_op3%=10 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Steps per unit for Z axis"
spiz??=val(gettext$ (str$((CSNG(spiz??/metric!))),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))*metric!
goto pramkyin
end if
end if
print using"##### ";spiz??/metric!;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " A STEPS PER UNIT   ";
if set_op3%=11 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$="":prompt$="Steps per unit for A axis"
if rotarya% then
spiw??=val(gettext$ (str$((CSNG(spiw??))),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))
else
spiw??=val(gettext$ (str$((CWRD(spiw??/metric!))),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb))*metric!
end if
goto pramkyin
end if
end if
if rotarya% then
print using"##### ";spiw??;:color 7,w10.cb:print chr$(179)
else
print using"##### ";spiw??/metric!;:color 7,w10.cb:print chr$(179)
end if
color 0,w10.cb:print chr$(192);
color 7,w10.cb:print repeat$(26,chr$(196)) chr$(217);


view text (33,8)-(57,11)

color 0,w10.cb:print chr$(218) repeat$ (22,chr$(196));:color 7,w10.cb:print chr$(191)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " PULSE GENERATOR ";:if set_op3%=12 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if exthand%=1 then exthand%=0 else exthand%=1:goto pramkyin2
if exthand%=1 then print " ON  "; else print " OFF ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " PULSE GEN IRQ     ";:if set_op3%=13 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if handirq% = 5 then handirq% = 7 else handirq% = 5:goto pramkyin2
print handirq%;
color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(22,chr$(196)) chr$(217);



view text (33,12)-(57,17)

color 0,w10.cb:print chr$(218) repeat$ (22,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " X AXIS BACKLASH ";:if set_op3%=14 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="X axis Backlash in steps"_
:bcklshx%=val(gettext$ (str$(bcklshx%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)):goto pramkyin
print using"#### ";bcklshx%;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Y AXIS BACKLASH ";:if set_op3%=15 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="Y axis Backlash in steps"_
:bcklshy%=val(gettext$ (str$(bcklshy%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)):goto pramkyin
print using"#### ";bcklshy%;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Z AXIS BACKLASH ";:if set_op3%=16 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="Z axis Backlash in steps"_
:bcklshz%=val(gettext$ (str$(bcklshz%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)):goto pramkyin
print using"#### ";bcklshz%;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " A AXIS BACKLASH ";:if set_op3%=17 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":prompt$="A axis Backlash in steps"_
:bcklshw%=val(gettext$ (str$(bcklshw%),prompt$,34,txth%,txtv%,bycopy w10.cf,bycopy w10.cb)):goto pramkyin
print using"#### ";bcklshw%;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(22,chr$(196)) chr$(217);

view text (33,18)-(57,23)

color 0,w10.cb:print chr$(218) repeat$ (22,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " X AXIS DIRECTION  ";:if set_op3%=18 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if revx%=1 then revx%=0 else revx%=1:goto pramkyin2
print revx%;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Y AXIS DIRECTION  ";:if set_op3%=19 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if revy%=1 then revy%=0 else revy%=1:goto pramkyin2
print revy%;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Z AXIS DIRECTION  ";:if set_op3%=20 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if revz%=1 then revz%=0 else revz%=1:goto pramkyin2
print revz%;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " A AXIS DIRECTION  ";:if set_op3%=21 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if revw%=1 then revw%=0 else revw%=1:goto pramkyin2
print revw%;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(22,chr$(196)) chr$(217);

view text (58,7)-(79,13)

color 0,w10.cb:print chr$(218) repeat$ (19,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " X AXIS LIMIT ";:if set_op3%=22 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if xlim%=1 then xlim%=0 else xlim%=1:goto pramkyin2
if xlim%=1 then print " ON  "; else print " OFF ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Y AXIS LIMIT ";:if set_op3%=23 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if ylim%=1 then ylim%=0 else ylim%=1:goto pramkyin2
if ylim%=1 then print " ON  "; else print " OFF ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " Z AXIS LIMIT ";:if set_op3%=24 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if zlim%=1 then zlim%=0 else zlim%=1:goto pramkyin2
if zlim%=1 then print " ON  "; else print " OFF ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " A AXIS LIMIT ";:if set_op3%=25 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if wlim%=1 then wlim%=0 else wlim%=1:goto pramkyin2
if wlim%=1 then print " ON  "; else print " OFF ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print " EMERGENCY STP";
if set_op3%=26 then
color w10.hf,w10.hb
if ky$=chr$(13) then
ky$=""
if emstop%=1 then
emstop%=0
extstop%=0
else
if not statin1? and servready? then emstop%=1
goto pramkyin2
end if
end if
end if
if emstop%=1 then print " ON  "; else print " OFF ";
color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(19,chr$(196)) chr$(217);

view text (58,14)-(79,23)

color 0,w10.cb:print chr$(218) repeat$ (19,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);:print "  STEP TRANSITION  ";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(179);

color w10.cf,w10.cb:print "  X AXIS    ";:if set_op3%=27 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if xlowhi%=1 then xlowhi%=0 else xlowhi%=1:goto pramkyin2
if xlowhi%=1 then print chr$(196) chr$(196)chr$(191) chr$(25) chr$(218) chr$(196) " ";_
 else print chr$(196) chr$(196) chr$(217) chr$(24) chr$(192) chr$(196)" ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179)"                   ";:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print "  Y AXIS    ";:if set_op3%=28 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if ylowhi%=1 then ylowhi%=0 else ylowhi%=1:goto pramkyin2
if ylowhi%=1 then print chr$(196) chr$(196)chr$(191) chr$(25) chr$(218) chr$(196) " ";_
 else print chr$(196) chr$(196) chr$(217) chr$(24) chr$(192) chr$(196)" ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179)"                   ";:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print "  Z AXIS    ";:if set_op3%=29 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if zlowhi%=1 then zlowhi%=0 else zlowhi%=1:goto pramkyin2
if zlowhi%=1 then print chr$(196) chr$(196)chr$(191) chr$(25) chr$(218) chr$(196) " ";_
 else print chr$(196) chr$(196) chr$(217) chr$(24) chr$(192) chr$(196)" ";
color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179)"                   ";:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:print "  A AXIS    ";:if set_op3%=30 then color w10.hf,w10.hb _
:if ky$=chr$(13) then ky$="":if wlowhi%=1 then wlowhi%=0 else wlowhi%=1:goto pramkyin2
if wlowhi%=1 then print chr$(196) chr$(196)chr$(191) chr$(25) chr$(218) chr$(196) " ";_
 else print chr$(196) chr$(196) chr$(217) chr$(24) chr$(192) chr$(196)" ";
color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(19,chr$(196)) chr$(217);



while not instat
gosub clkdsp
loop
ky$=inkey$

if ky$=chr$(0,72) and set_op3% > 0 then decr set_op3%
if ky$=chr$(0,80) and set_op3% < 30 then incr set_op3%
if ky$=chr$(27) then gosub sreset:gosub saveall :return
goto pramkyin2
return

vwind1:
view text (43,2)-(79,2)
color w10.cf,w10.cb
ky$=""
return

'----------------------- port pin select functions --------------
function chgport% (portval%,ud%)
select case portval%
case = &h278:if ud% then chgport% = &h378 else chgport% = &h3bc
case = &h378:if ud% then chgport% = &h3bc else chgport% = &h278
case = &h3bc:if ud% then chgport% = &h278 else chgport% = &h378
end select
end function

function statbit? (portpin?,ud%)
shared newpin?
if portpin?=0 then portpin?=10
select case portpin?
case = 10:if ud% then newpin?=11:statbit?=7 else newpin?=15:statbit?=3
case = 11:if ud% then newpin?=12:statbit?=5 else newpin?=10:statbit?=6
case = 12:if ud% then newpin?=13:statbit?=4 else newpin?=11:statbit?=7
case = 13:if ud% then newpin?=15:statbit?=3 else newpin?=12:statbit?=5
case = 15:if ud% then newpin?=10:statbit?=6 else newpin?=13:statbit?=4
end select
end function

function cntlbit? (portpin?,ud%)
shared newpin1?
if portpin?=0 then portpin?=1
select case portpin?
'case = 1 :if ud% then newpin1?=14:cntlbit?=1 else newpin1?=17:cntlbit?=3
'case = 14:if ud% then newpin1?=16:cntlbit?=2 else newpin1?=1 :cntlbit?=0
'case = 16:if ud% then newpin1?=17:cntlbit?=3 else newpin1?=14:cntlbit?=1
'case = 17:if ud% then newpin1?=1 :cntlbit?=0 else newpin1?=16:cntlbit?=2
case = 1 :if ud% then newpin1?=14:cntlbit?=1 else newpin1?=16:cntlbit?=2
case = 14:if ud% then newpin1?=16:cntlbit?=2 else newpin1?=1 :cntlbit?=0
case = 16:if ud% then newpin1?=1:cntlbit?=0 else newpin1?=14:cntlbit?=1
end select
end function

'---------------------- PORT SETUP / INPUT SCREEN -----------------
portkyin:
view text (1,2)-(80,23)
color w10.cf,w10.cb
cls
portkyin2:
view text (4,3)-(22,23)
color w10.cf,w10.cb
print "AXIS CONTROL PORT"

color 0,w10.cb:print chr$(218) repeat$ (11,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);

if set_op%=0 then
color w10.hf,w10.hb:pbin?=0

if ky$=chr$(13) then
prompt$="Port &h378,&h278,&h3bc cust"
dataport% = val(gettext$ ("&H"+ hex$(dataport%),prompt$,30,21,14,bycopy w10.cf,bycopy w10.cb))
ky$="":goto portkyin
else
if ky$=chr$(0,75) then dataport% = chgport% (dataport%,1)
if ky$=chr$(0,77) then dataport% = chgport% (dataport%,0)
end if
end if

print " Port "hex$(dataport%)"h ";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(11,chr$(196)) chr$(217)

color 0,w10.cb:print chr$(218) repeat$ (14,chr$(196));:color 7,w10.cb:print chr$(191)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=1 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,xstepbit? _
:if ky$=chr$(0,75) and xstepbit? < 7 then incr xstepbit?:pbin?=0:bit set pbin?,xstepbit? else _
if ky$=chr$(0,77) and xstepbit? > 0 then decr xstepbit?:pbin?=0:bit set pbin?,xstepbit?
print " X STEP Pin";xstepbit?+2;:color 7,w10.cb:print chr$(179)



color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=2 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,xdirbit? _
:if ky$=chr$(0,75) and xdirbit? < 7 then incr xdirbit?:pbin?=0:bit set pbin?,xdirbit? else _
if ky$=chr$(0,77) and xdirbit? > 0 then decr xdirbit?:pbin?=0:bit set pbin?,xdirbit?
print " X DIR  Pin"; xdirbit?+2;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=3 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,ystepbit? _
:if ky$=chr$(0,75) and ystepbit? < 7 then incr ystepbit?:pbin?=0:bit set pbin?,ystepbit? else _
if ky$=chr$(0,77) and ystepbit? > 0 then decr ystepbit?:pbin?=0:bit set pbin?,ystepbit?
print " Y STEP Pin";ystepbit?+2;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=4 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,ydirbit? _
:if ky$=chr$(0,75) and ydirbit? < 7 then incr ydirbit?:pbin?=0:bit set pbin?,ydirbit? else _
if ky$=chr$(0,77) and ydirbit? > 0 then decr ydirbit?:pbin?=0:bit set pbin?,ydirbit?
print " Y DIR  Pin"; ydirbit?+2;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=5 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,zstepbit? _
:if ky$=chr$(0,75) and zstepbit? < 7 then incr zstepbit?:pbin?=0:bit set pbin?,zstepbit? else _
if ky$=chr$(0,77) and zstepbit? > 0 then decr zstepbit?:pbin?=0:bit set pbin?,zstepbit?
print " Z STEP Pin";zstepbit?+2;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=6 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,zdirbit? _
:if ky$=chr$(0,75) and zdirbit? < 7 then incr zdirbit?:pbin?=0:bit set pbin?,zdirbit? else _
if ky$=chr$(0,77) and zdirbit? > 0 then decr zdirbit?:pbin?=0:bit set pbin?,zdirbit?
print " Z DIR  Pin"; zdirbit?+2;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=7 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,wstepbit? _
:if ky$=chr$(0,75) and wstepbit? < 7 then incr wstepbit?:pbin?=0:bit set pbin?,wstepbit? else _
if ky$=chr$(0,77) and wstepbit? > 0 then decr wstepbit?:pbin?=0:bit set pbin?,wstepbit?
print " A STEP Pin";wstepbit?+2;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=8 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,wdirbit? _
:if ky$=chr$(0,75) and wdirbit? < 7 then incr wdirbit?:pbin?=0:bit set pbin?,wdirbit? else _
if ky$=chr$(0,77) and wdirbit? > 0 then decr wdirbit?:pbin?=0:bit set pbin?,wdirbit?
print " A DIR  Pin"; wdirbit?+2;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$ (14,chr$(196)); chr$(217)

'------------------------ status port -----
color 0,w10.cb:print chr$(218) repeat$ (16,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=9 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,xlimbit? _
:if ky$=chr$(0,75) then xlimbit? = statbit?(xlimpin?,1) :xlimpin?=newpin?:pbin?=0:bit set pbin?,xlimbit? else _
 if ky$=chr$(0,77) then xlimbit? = statbit?(xlimpin?,0) :xlimpin?=newpin?:pbin?=0:bit set pbin?,xlimbit?
print " X LIMIT Pin" xlimpin?;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=10 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,ylimbit? _
:if ky$=chr$(0,75) then ylimbit? = statbit?(ylimpin?,1) :ylimpin?=newpin?:pbin?=0:bit set pbin?,ylimbit? else _
 if ky$=chr$(0,77) then ylimbit? = statbit?(ylimpin?,0) :ylimpin?=newpin?:pbin?=0:bit set pbin?,ylimbit?
print " Y LIMIT Pin" ylimpin?;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=11 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,zlimbit? _
:if ky$=chr$(0,75) then zlimbit? = statbit?(zlimpin?,1) :zlimpin?=newpin?:pbin?=0:bit set pbin?,zlimbit? else _
 if ky$=chr$(0,77) then zlimbit? = statbit?(zlimpin?,0) :zlimpin?=newpin?:pbin?=0:bit set pbin?,zlimbit?
print " Z LIMIT Pin" zlimpin?;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=12 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,wlimbit? _
:if ky$=chr$(0,75) then wlimbit? = statbit?(wlimpin?,1) :wlimpin?=newpin?:pbin?=0:bit set pbin?,wlimbit? else _
 if ky$=chr$(0,77) then wlimbit? = statbit?(wlimpin?,0) :wlimpin?=newpin?:pbin?=0:bit set pbin?,wlimbit?
print " A LIMIT Pin" wlimpin?;:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=13 then color w10.hf,w10.hb:pbin?=holdbit?:pbin?=0:bit set pbin?,holdbit? _
:if ky$=chr$(0,75) then holdbit? = statbit?(holdpin?,1) :holdpin?=newpin?:pbin?=0:bit set pbin?,holdbit? else _
 if ky$=chr$(0,77) then holdbit? = statbit?(holdpin?,0) :holdpin?=newpin?:pbin?=0:bit set pbin?,holdbit?
print " EMG STP Pin" holdpin?;:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$ (16,chr$(196)); chr$(217);



'------------------------ error input port setting ---------
view text (22,14)-(50,16)
color w10.cf,w10.cb
color 0,w10.cb:print chr$(218) repeat$ (25,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb
print chr$(179);" ON E-STOP READ PORT ";

if set_op%=14 then
color w10.hf,w10.hb

if ky$=chr$(13) then
prompt$="Enter Port to read on E-stop"
error_port% = val(gettext$ ("&H"+ hex$(error_port%),prompt$,30,21,14,bycopy w10.cf,bycopy w10.cb))
ky$="":goto portkyin
end if
end if

print using "\ \&";hex$(error_port%)"h";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$ (25,chr$(196)); chr$(217);



'------------------------------ control port -------
view text (24,17)-(48,22)
color w10.cf,w10.cb
color 0,w10.cb:print chr$(218) repeat$ (20,chr$(196));:color 7,w10.cb:print chr$(191)


color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=15 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,sfwdbit? _
:if ky$=chr$(0,75) then sfwdbit? = cntlbit?(sfwdpin?,1) :sfwdpin?=newpin1?:pbin?=0:bit set pbin?,sfwdbit? else _
 if ky$=chr$(0,77) then sfwdbit? = cntlbit?(sfwdpin?,0) :sfwdpin?=newpin1?:pbin?=0:bit set pbin?,sfwdbit?
print using$(" SPINDLE FWD Pin### ", sfwdpin?);:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=16 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,srevbit? _
:if ky$=chr$(0,75) then srevbit? = cntlbit?(srevpin?,1) :srevpin?=newpin1?:pbin?=0:bit set pbin?,srevbit? else _
 if ky$=chr$(0,77) then srevbit? = cntlbit?(srevpin?,0) :srevpin?=newpin1?:pbin?=0:bit set pbin?,srevbit?
print using$(" SPINDLE REV Pin### ", srevpin?);:color 7,w10.cb:print chr$(179)


color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=17 then color w10.hf,w10.hb:pbin?=0:bit set pbin?,coolbit? _
:if ky$=chr$(0,75) then coolbit? = cntlbit?(coolpin?,1) :coolpin?=newpin1?:pbin?=0:bit set pbin?,coolbit? else _
 if ky$=chr$(0,77) then coolbit? = cntlbit?(coolpin?,0) :coolpin?=newpin1?:pbin?=0:bit set pbin?,coolbit?
print using$(" COOLANT1    Pin### ", coolpin?);:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:
if set_op%=18 then
color w10.hf,w10.hb:pbin?=0
end if
print using$(" COOL2 / PWM Pin### ", coolpin2?);:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$ (20,chr$(196)); chr$(217);

'------------------------- port #2 --------------
view text (58,3)-(80,8)
color w10.cf,w10.cb


print "HANDLE/FEED PORT"
color 0,w10.cb:print chr$(218) repeat$ (11,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);

if set_op%=19 then
color w10.hf,w10.hb:pbin?=0

if ky$=chr$(13) then
prompt$="Port &h378,&h278,&h3bc cust"
dataport2% = val(gettext$ ("&H"+ hex$(dataport2%),prompt$,30,21,14,bycopy w10.cf,bycopy w10.cb))
ky$="":goto portkyin
else
if ky$=chr$(0,75) then dataport2% = chgport% (dataport2%,1) else
if ky$=chr$(0,77) then dataport2% = chgport% (dataport2%,0)
end if
end if

print " Port "hex$(dataport2%)"h ";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(11,chr$(196)) chr$(217)


view text (54,7)-(80,12)
color 0,w10.cb:print chr$(218) repeat$ (22,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=20 then color w10.hf,w10.hb:pbin?=15
print " FEED O/RIDE Pins 2-5 ";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=21 then color w10.hf,w10.hb:pbin?=&H30
print " RPD  O/RIDE Pins 6,7 ";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=22 then color w10.hf,w10.hb:pbin?=&Hc0
print " AXIS SLCT   Pins 8,9 ";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(22,chr$(196)) chr$(217)

view text (54,12)-(80,20)
color 0,w10.cb:print"  PULSE GENERATOR"
color 0,w10.cb:print chr$(218) repeat$ (22,chr$(196));:color 7,w10.cb:print chr$(191)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=23 then color w10.hf,w10.hb:pbin?=&h88
print " INCR SLCT Pins 15,11 ";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=24 then color w10.hf,w10.hb:pbin?=&h30
print " PULSE A/B Pins 12,13 ";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=25 then color w10.hf,w10.hb:pbin?=&h40
print " PULSE IRQ     Pin 10 ";:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$(22,chr$(196)) chr$(217)

'--------------------- Control port 2 ----------
view text (56,18)-(80,23)
color w10.cf,w10.cb

color 0,w10.cb:print chr$(218) repeat$ (18,chr$(196));:color 7,w10.cb:print chr$(191)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=26 then color w10.hf,w10.hb
print using$(" S1 OUT    Pin### ", 1);:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=27 then color w10.hf,w10.hb
print using$(" S2 OUT    Pin### ", 14);:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=28 then color w10.hf,w10.hb
print using$(" S3 OUT    Pin### ", 16);:color 7,w10.cb:print chr$(179)

color 0,w10.cb:print chr$(179);
color w10.cf,w10.cb:if set_op%=29 then color w10.hf,w10.hb
print using$(" 4khz W/D  Pin### ", 17);:color 7,w10.cb:print chr$(179)
color 0,w10.cb:print chr$(192);:color 7,w10.cb:print repeat$ (18,chr$(196)); chr$(217);


'-------------------------------
view text (23,3)-(54,18)
color w10.cf,w10.cb
'cls
if set_op% > 0 and set_op% < 9 or set_op% > 19 and set_op% < 23 then cdat1=12 else cdat1=0
if set_op% > 8 and set_op% < 14 or set_op% > 22 and set_op% < 26 then cstat1 =12 else cstat1 =0
if set_op% > 14 and set_op% < 19 or set_op% > 25 and set_op% < 30 then ccon1 =12 else ccon1 =0

print " _13________________________1_"
print "\  ";:color cstat1,w10.cb:print"o o o o";:color cdat1,w10.cb:print" o o o o o o o o";:color ccon1,w10.cb:print" o";:color 0,w10.cb:print"  /"
print " \  o o o o o o o o";:color ccon1,w10.cb:print" o o";:color cstat1,w10.cb:print" o";:color ccon1,w10.cb:print" o";:color 0,w10.cb:print" /"
print "   25~~~~~~~~~~~~~~~~~~~~~14"
print "  View from back of computer"
print "  Actual port bits "right$("00000000"+bin$(pbin?),8)
print
color w14.cf,w10.cb
print "        ESC TO EXIT"
print "   " chr$(24) chr$(25)" TO  SELECT PORT/PIN"
print "   " chr$(27) chr$(26)" TO  CHANGE VALUE"


while not instat
gosub clkdsp
loop
ky$=inkey$

if ky$=chr$(0,72) and set_op% > 0 then decr set_op%
if ky$=chr$(0,80) and set_op% < 29 then incr set_op%
if ky$=chr$(27) then gosub sreset:gosub saveall:return
goto portkyin2

$segment

'------------------------- WORK OFFSET ----------------
setwkoff:
auto%=7
mode$="WORK OFFSET"
screen 0,,3,3
color fgwork%,bgwork%
cls
view text (8,21)-(80,23)

print "ZERO AXIS    = any XYZA + Enter"
print "PRESET AXIS  = Input + Enter"
print "SAVE TO FILE = U";

gosub bkclk
gosub clkdsp

swap w12,w1
swap w13,w20
gosub back_pos
gosub back_mach
gosub selects

view text (w11.x,w11.y)-(w11.x+w11.h,w11.y+w11.v)
color w11.bdr,w11.cb
print  chr$(213) repeat$(w11.h-2,chr$(205))chr$(184)
print  chr$(179) repeat$(w11.h-2,chr$(32)) chr$(179)
print  chr$(212) repeat$(w11.h-2,chr$(205))chr$(190);
locate 1,3:print"INPUT";

repg3:
view text (w11.x+1,w11.y+1)-(w11.x+w11.h-2,w11.y+1)
color w11.cf,w11.cb
cls

repg4:
view text (w14.x,w14.y)-(w14.x+w14.h,w14.y+w14.v)
color w14.bdr,w14.cb
'cls
print chr$(213) repeat$(8,chr$(205))chr$(209)repeat$(11,chr$(205)) chr$(209)repeat$(11,chr$(205)) chr$(209)repeat$(11,chr$(205)) chr$(209)repeat$(11,chr$(205)) chr$(184)
print chr$(179)" OFFSET "chr$(179)"   X AXIS  "chr$(179)"   Y AXIS  "chr$(179)"   Z AXIS  "chr$(179)"   A AXIS  " chr$(179)

for t%=0 to 6
print chr$(198) repeat$(8,chr$(205))chr$(216) repeat$(11,chr$(205))chr$(216) repeat$(11,chr$(205))chr$(216) repeat$(11,chr$(205))chr$(216) repeat$(11,chr$(205))chr$(181)
if t%=0 then print chr$(179)" G 92   "chr$(179); else print chr$(179)" G"t%+53 "  "chr$(179);
color w14.cf,w14.cb
if t% = wovert% and wdh%=0 then color w14.hf,w14.hb
print using psout$; wrk(t%).x*metric! / spi??;
color w14.bdr,w14.cb
print chr$(179);
color w14.cf,w14.cb
if t% = wovert% and wdh%=1 then color w14.hf,w14.hb
print using psout$; wrk(t%).y*metric! / spi??;
color w14.bdr,w14.cb
print chr$(179);
color w14.cf,w14.cb
if t% = wovert% and wdh%=2 then color w14.hf,w14.hb
print using psout$; wrk(t%).z * metric! / spi??;
color w14.bdr,w14.cb
print chr$(179);
color w14.cf,w14.cb
if t% = wovert% and wdh%=3 then color w14.hf,w14.hb
if rotarya% then
print using psout$; wrk(t%).w / spi??*wfct!;
else
print using psout$; wrk(t%).w*metric! / spi??;
end if
color w14.bdr,w14.cb
print chr$(179)
next t%
print chr$(212) repeat$(8,chr$(205))chr$(207) repeat$(11,chr$(205))chr$(207) repeat$(11,chr$(205))chr$(207)repeat$(11,chr$(205))chr$(207) repeat$(11,chr$(205))chr$(190);

while not instat
gosub pos_dsp
gosub mach_pos
gosub clkdsp
if cancyc% and arun% and mov%=0 and in_op%=0 then gosub cancycle
gosub autoread
if ts% <> ud% then gosub dspud3
loop
ky$=inkey$

if fload% then goto wskpky
if ky$ = chr$(0,59) or ky$ = chr$(0,61) then:goto xitwork
if ky$ > chr$(0,62) and ky$ < chr$(0,69) then:goto xitwork

wskpky:
if ky$=chr$(0,59) or ky$=chr$(0,61) or ky$=chr$(0,65) or ky$=chr$(0,66) then goto xitwork
if ucase$(ky$)="U" then gosub saveall
if ucase$(ky$)="X" then
wdh%=0
if wovert%=0 then
offmdi$ = str$(((machx& - wrk(gp14%-53).x)*metric! / spi??),6)
else
offmdi$ = str$(((machx& - wrk(0).x)* metric! / spi??),6)
end if
end if
'offmdi$ = str$((machx& - wrk(0).x)*metric! / spi?? * mirrx%,6)

if ucase$(ky$)="Y" then
wdh%=1
if wovert%=0 then
offmdi$ = str$(((machy& - wrk(gp14%-53).y)*metric! / spi??),6)
else
offmdi$ = str$(((machy& - wrk(0).y)*metric! / spi??) ,6)
end if
end if
'offmdi$ = str$((machy& - wrk(0).y)*metric! / spi?? * mirry%,6)

if ucase$(ky$)="Z" then wdh%=2:if wovert%=0 then offmdi$ = str$((machz& - wrk(gp14%-53).z)*metric! / spi?? ,6) else _
offmdi$ = str$((machz& - wrk(0).z)*metric! / spi?? ,6)

if ucase$(ky$)="A" then
wdh%=3
if rotarya% then
if wovert%=0 then offmdi$ = str$((machw& - wrk(gp14%-53).w)*wfct! / spi?? ,6) else _
offmdi$ = str$((machw& - wrk(0).w)*wfct! / spi?? ,6)
else
if wovert%=0 then offmdi$ = str$((machw& - wrk(gp14%-53).w)*metric! / spi?? ,6) else _
offmdi$ = str$((machw& - wrk(0).w)*metric! / spi?? ,6)
end if
end if

if ky$=chr$(47) then gosub newoff:ky$=""
if ky$=chr$(0,72) then decr wovert%:if wovert% < 0 then wovert%=0
if ky$=chr$(0,80) then incr wovert%:if wovert% > 6 then wovert%=6
if ky$=chr$(0,77) then incr wdh%:if wdh% > 3 then wdh%=3
if ky$=chr$(0,75) then decr wdh%:if wdh% < 0 then wdh%=0
if ky$=chr$(13) then gosub entline1:goto repg3
if ky$=chr$(27) then gosub sreset:offmdi$="":goto repg3
if ky$=chr$(8) then ky$="":if len(offmdi$) > 0 then offmdi$ = left$ (offmdi$,len(offmdi$)-1)
if ky$ > chr$(44) and ky$ < chr$(58) then if len(offmdi$) < 9 then offmdi$ = offmdi$+ky$
gosub dspud3
goto repg4

dspud3:
ud% = ts%
if cn%=1 then cn%=0:cur$=" " else cn%=1:cur$="_"
view text (w11.x+1,w11.y+1)-(w11.x+w11.h-2,w11.y+1)
color w11.cf,w11.cb
'locate 1,1
print "> "offmdi$;cur$;"  ";
return


entline1:
select case wdh%
case = 0:wrk(wovert%).x = val(offmdi$)/metric! * spi??
case = 1:wrk(wovert%).y = val(offmdi$)/metric! * spi??
case = 2:wrk(wovert%).z = val(offmdi$)/metric! * spi??
case = 3:if rotarya% then
wrk(wovert%).w = val(offmdi$)/wfct! * spi??
else
wrk(wovert%).w = val(offmdi$)/metric! * spi??
end if

end select

offmdi$=""
return

newoff:
select case wdh%
case = 0:offval! = ((wrk(wovert%).x * metric! / spi??) + val(offmdi$)) / 2
case = 1:offval! = ((wrk(wovert%).y * metric! / spi??) + val(offmdi$)) / 2
case = 2:offval! = ((wrk(wovert%).z * metric! / spi??) + val(offmdi$)) / 2
case = 3:offval! = ((wrk(wovert%).w * metric! / spi??) + val(offmdi$)) / 2
end select
offmdi$=str$(offval!,6)
return

offmdi$= str$((val(offmdi$)+val(offmdi$))/2):goto repg3

xitwork:
swap w12,w1
swap w13,w20
screen 0,,0,0
return




'--------------------- SET TOOL LENGTH & DIA ----- F3 --------
settool:
auto%=6
mode$="TOOL OFFSET"
screen 0,,3,3
color fgtool%,bgtool%
cls

view text (50,8)-(80,13)
print "X/Z - insert current X/Z pos."
print "P -reset from current X/Z"
print "A -dd to current value"
print "S -ubtract from current value"
print "U -date settings to file"

swap w1,w21
swap w20,w22
swap w19,w23
gosub back_pos
gosub back_cmd
gosub back_mach

view text (w9.x,w9.y)-(w9.x+w9.h,w9.y+w9.v)
color w9.bdr,w9.cb
print  chr$(213) repeat$(w9.h-2,chr$(205))chr$(184)
print  chr$(179) repeat$(w9.h-2,chr$(32)) chr$(179)
print  chr$(212) repeat$(w9.h-2,chr$(205))chr$(190);
locate 1,5:print"INPUT";


gosub bkclk
gosub clkdsp
gosub selects
if toolptr% < 1 then toolptr%=1

repg2:
view text (w9.x+1,w9.y+1)-(w9.x+w9.h-2,w9.y+1)
color w9.cf,w9.cb
cls
repg1:
view text (w8.x,w8.y)-(w8.x+w8.h,w8.y+w8.v)
color w8.bdr,w8.cb
'cls
print " " chr$(213)"TOOL"chr$(213)chr$(205)" X OFFSET"chr$(205)chr$(209)chr$(205)" Z OFFSET"chr$(205)chr$(209)chr$(205)" RADIUS"repeat$(3,chr$(205))chr$(184)" "
for t%=toolpage%+1 to toolpage%+10
print " " chr$(198) repeat$(4,chr$(205))chr$(216) repeat$(11,chr$(205))chr$(216) repeat$(11,chr$(205))chr$(216) repeat$(11,chr$(205))chr$(181)" "
print " " chr$(179);
color w8.cf,w8.cb
print using "### ";t%;
color w8.bdr,w8.cb
print chr$(179);

color w8.cf,w8.cb
if t% = toolptr% and dh%=0 then color w8.hf,w8.hb
print using hdoff$; x_offset&(t%)*metric!/spi??,
color w8.bdr,w8.cb
print chr$(179);

'color w8.cf,w8.cb
'if t% = toolptr% and dh%=0 then color w8.hf,w8.hb
'print using hdoff$; y_offset&(t%)*metric!/spi??,
'color w8.bdr,w8.cb
'print chr$(179);

color w8.cf,w8.cb
if t% = toolptr% and dh%=1 then color w8.hf,w8.hb
print using hdoff$; z_offset&(t%)*metric!/spi??,
color w8.bdr,w8.cb
print chr$(179);

color w8.cf,w8.cb
if t% = toolptr% and dh%=2 then color w8.hf,w8.hb
print using hdoff$; doffset#(t%) * metric!,
color w8.bdr,w8.cb
print chr$(179)" "
next t%
print " " chr$(212) repeat$(4,chr$(205))chr$(207) repeat$(11,chr$(205))chr$(207)repeat$(11,chr$(205))chr$(207) repeat$(11,chr$(205))chr$(190)" ";

while not instat
gosub cmd_pos
gosub mach_pos
gosub pos_dsp
gosub clkdsp
if cancyc% and arun% and mov%=0 and in_op%=0 then gosub cancycle
gosub autoread
if ts% <> ud% then gosub dspud2
loop
ky$=inkey$

if ucase$(ky$)="U" then gosub saveall

if fload% then
if ky$=chr$(0,59) or ky$=chr$(0,62) or ky$=chr$(0,65) or ky$=chr$(0,66) then goto xittool
else
if ky$ = chr$(0,59) or ky$ = chr$(0,61) then goto xittool
if ky$ > chr$(0,61) and ky$ < chr$(0,69) then goto xittool
end if

if ucase$(ky$)="X" then dh%=0:offmdi$ = str$((machx&/xscale# - wrk(0).x - wrk(gp14%-53).x)*metric! / spi?? ,6)
if ucase$(ky$)="Z" then dh%=1:offmdi$ = str$((machz&/zscale# - wrk(0).z - wrk(gp14%-53).z)*metric! / spi?? ,6)

if ucase$(ky$)="P" then
if dh%=0 then
x_offset&(toolptr%) = (machx&/xscale# - wrk(0).x - wrk(gp14%-53).x) - round(val(offmdi$),4)/metric! * spi??
end if
if dh%=1 then
z_offset&(toolptr%) = (machz&/zscale# - wrk(0).z - wrk(gp14%-53).z) - round(val(offmdi$),4)/metric! * spi??
end if
end if

if ky$=chr$(0,73) and toolpage% > 9 then decr toolpage%,10:decr toolptr%,10
if ky$=chr$(0,81) and toolpage% < maxtools%-10 then incr toolpage%,10:incr toolptr%,10
if ky$=chr$(0,72) and toolptr% > 1 then decr toolptr%:if toolptr% < toolpage%+1 then decr toolpage%,10
if ky$=chr$(0,80) and toolptr% < maxtools% then incr toolptr%:if toolptr% > toolpage%+10 then incr toolpage%,10
if ky$=chr$(0,71) then toolptr%=1:toolpage%=0
if ky$=chr$(0,79) then toolptr%=maxtools%:toolpage%=toolptr%-10
if ky$=chr$(0,77) then
if dh%<2 then incr dh%
end if
if ky$=chr$(0,75) then
if dh%>0 then decr dh%
end if
if ky$=chr$(13) then
if dh%=0 then x_offset&(toolptr%)=round(val(offmdi$),4)/metric! * spi??
if dh%=1 then z_offset&(toolptr%)=round(val(offmdi$),4)/metric! * spi??
if dh%=2 then doffset#(toolptr%)=round(val(offmdi$),4)/metric!
offmdi$=""
goto repg2
end if

if ucase$(ky$) = "S" then 'chr$(0,130) or ky$=chr$(0,45) then
if dh%=0 then decr x_offset&(toolptr%),round(val(offmdi$),4)/metric! * spi??
if dh%=1 then decr z_offset&(toolptr%),round(val(offmdi$),4)/metric! * spi??
if dh%=2 then doffset#(toolptr%)=round(doffset#(toolptr%)-val(offmdi$)/metric!,4)
goto repg1
end if

if ucase$(ky$) = "A" then 'chr$(0,131) or ky$=chr$(0,43) then
if dh%=0 then incr x_offset&(toolptr%),val(offmdi$)/metric! * spi??
if dh%=1 then incr z_offset&(toolptr%),val(offmdi$)/metric! * spi??
if dh%=2 then doffset#(toolptr%)=round(doffset#(toolptr%)+val(offmdi$)/metric!,4)
goto repg1
end if

if ky$=chr$(27) then gosub sreset:offmdi$="":goto repg2


if ky$=chr$(8) then ky$="":if len(offmdi$) > 0 then offmdi$ = left$ (offmdi$,len(offmdi$)-1)
if ky$ > chr$(44) and ky$ < chr$(58) then if len(offmdi$) < 9 then offmdi$ = offmdi$+ky$
gosub dspud2
goto repg1
'-------------------------

dspud2:
ud% = ts%
if cn%=1 then cn%=0:cur$=" " else cn%=1:cur$="_"
view text (w9.x+1,w9.y+1)-(w9.x+w9.h-2,w9.y+1)
color w9.cf,w9.cb
locate 1,2
print "> "offmdi$;cur$;"  ";
return

xittool:
swap w1,w21
swap w20,w22
swap w19,w23
screen 0,,0,0
return


'--------------------------------- Initilize port1! --------
port_clear:

out controlport%,&h0b	'set control1 to all off
out controlport2%,&h3b	'set control2 to all off and irq enabled
			'and read mode on
return

'-----------------------sets bit masks ------------------oooooooooooooooo
opndatsav:
if savedat%=1 then
open dirname$+datafile$ for output as #3
else
print #3,"(File closed "realtime$" on "DATE$")"
close #3
end if
return

maskcalc:
'----------------- direction and step masks ------

'dataport% = &h378		'--- port1 address
statusport% = dataport%+1
controlport% = dataport%+2

'dataport2% = &h278		'--- port2 address
statusport2% = dataport2%+1
controlport2% = dataport2%+2

'---------------------- sets port pins to axis step --------

mxstep?=0:bit set mxstep?,xstepbit?
mystep?=0:bit set mystep?,ystepbit?
mzstep?=0:bit set mzstep?,zstepbit?
mwstep?=0:bit set mwstep?,wstepbit?
stepmask?=0
stepmask? = mxstep? or mystep? or mzstep? or mwstep?

steplevmask? = 0
if xlowhi% then bit set steplevmask?,xstepbit?
if ylowhi% then bit set steplevmask?,ystepbit?
if zlowhi% then bit set steplevmask?,zstepbit?
if wlowhi% then bit set steplevmask?,wstepbit?

'------------------ sets port pins to axis direction -------------

mxdir?=0:bit set mxdir?,xdirbit?
mydir?=0:bit set mydir?,ydirbit?
mzdir?=0:bit set mzdir?,zdirbit?
mwdir?=0:bit set mwdir?,wdirbit?

clrdir?=0
clrdir?=mxdir? or mydir? or mzdir? or mwdir?

xdir?=mxdir?:ydir?=mydir?:zdir?=mzdir?:wdir?=mwdir?
xstep?=mxstep?:ystep?=mystep?:zstep?=mzstep?:wstep?=mwstep?

'----------- inverts the count direction +/- of axis  if revx% ---------
if revx%=0 then xdup%=-1 else xdup%=1
if revy%=0 then ydup%=-1 else ydup%=1
if revz%=0 then zdup%=-1 else zdup%=1
if revw%=0 then wdup%=-1 else wdup%=1

chdirmask?=0
if revx%=1 then bit set chdirmask?,xdirbit?
if revy%=1 then bit set chdirmask?,ydirbit?
if revz%=1 then bit set chdirmask?,zdirbit?
if revw%=1 then bit set chdirmask?,wdirbit?

limitx?=0:bit set limitx?,xlimbit?
limity?=0:bit set limity?,ylimbit?
limitz?=0:bit set limitz?,zlimbit?
limitw?=0:bit set limitw?,wlimbit?
servready?=0:bit set servready?,holdbit?

limchk?=0:softlimchk?=0
if xlim% then limchk? = limitx? else stopx%=0
if ylim% then limchk? = limchk? or limity? else stopy%=0
if zlim% then limchk? = limchk? or limitz? else stopz%=0
if wlim% then limchk? = limchk? or limitw? else stopw%=0
if softlim% then softlimchk? = limitx? or limity? or limitz? or limitw? else stopx%=0:stopy%=0:stopz%=0:stopw%=0



spinfwd?=0:bit set spinfwd?,sfwdbit?
spinrev?=0:bit set spinrev?,srevbit?
light?=0:bit set light?,litbit?
cool1?=0:bit set cool1?,coolbit?
aux1?=0:bit set aux1?,aux1bit?
aux2?=0:bit set aux2?,aux2bit?
aux3?=0:bit set aux3?,aux3bit?
aux4?=0:bit set aux4?,aux4bit?
nunits%=defunits%
'if defunits% then metric!=25.4 else metric!=1.000

'------------------------------------------------------------
gosub setsys	'---- set print for inch or metric

spi??= max (spix??,spiy??,spiz??,spiw??)
dvspi&=spi??*100
dvspix&=spix??*100
dvspiy&=spiy??*100
dvspiz&=spiz??*100
dvspiw&=spiw??*100

max_wfct!=spi??/spiw??
if wfct! > max_wfct! then wfct!=max_wfct!

clklimit& = ppcnt& * feedconst! '-- calculates the maximun clock speed
maxf!=(1193180/clklimit&/spi??)* 60
if maxfeed! > maxf! then maxfeed!=maxf!
'print ppcnt&, clklimit&,maxf!: end '-- prints max feed values and ends ---

if rotarya% then
dvspiw&=spiw??*100*wfct!
end if

expandw& = expand&/wfct!
dv! = expand& / spi??
dvw!=dv!

Wmax&=rolla% * expandw&
rolldspa&= rolla% * spi??/wfct!

lastplane% = 17

driveon:
if gsdrive% then
600 if gsdon%=0 then
opn$=comport1$+":"+baud1$+","+parity1$+","+databits1$+","+stopbit1$+",DS0,CS0"
open opn$ as #4 len=256
'open "com1:9600,N,7,2,RS,DS" as #4
gsdon%=1
602 end if
end if
if gsdon%=0 then gosub driveoff
return

driveoff:
605 close #4
607 gsdon%=0
gsdrive%=0
return

setsys:
if nunits%=0 then

'------------------------- String Print formats  inch --------------

metric! = 1.00000
xlout$=" X####.####  "
ylout$=" Y####.####  "
zlout$=" Z####.####  "
wlout$=" A####.####  "
ilout$=" I####.#### "
jlout$=" J####.#### "
klout$=" K####.#### "
hdoff$=" ####.#### "
psout$=" ####.#### "
cdout$="####.####"
limout$="####.### "
else
'------------------------- String Print formats metric --------------

metric! = 25.4000
xlout$=" X#####.###  "
ylout$=" Y#####.###  "
zlout$=" Z#####.###  "
wlout$=" A#####.###  "
ilout$=" I#####.### "
jlout$=" J#####.### "
klout$=" K#####.### "
hdoff$=" ####.### "
psout$=" #####.### "
cdout$="#####.###"
limout$="#####.## "
end if

if rotarya% then wlout$=" A#####.###"

return


'---------------------- Save DATA, saves all data before exit! ---
saveall:
open savename$ for output as #2
write #2,ver$
write #2,dataport%,statusport%,controlport%
write #2,dataport2%,statusport2%,controlport2%
write #2,xdirbit?,ydirbit?,zdirbit?,wdirbit?
write #2,xstepbit?,ystepbit?,zstepbit?,wstepbit?
write #2,xlimpin?,ylimpin?,zlimpin?,wlimpin?,holdpin?
write #2,xlimbit?,ylimbit?,zlimbit?,wlimbit?,holdbit?
write #2,xyzlim%,wlim%,xlowhi%,ylowhi%,zlowhi%,wlowhi%
write #2,limclear%,homefeed1!,retratio%,dirhomex%,dirhomey%,dirhomez%
write #2,emstop%,spndelay??,pram83%,srevtime??
write #2,sfwdbit?,srevbit?,litbit?,coolbit?
write #2,sfwdpin?,srevpin?,litpin?,coolpin?
write #2,aux1pin?,aux2pin?,aux3pin?,aux4pin?
write #2,aux1bit?,aux2bit?,aux3bit?,aux4bit?
write #2,handirq%,h1pulse%,h1step%,h10pulse%,h10step%,h100pulse%,h100step%
write #2,gsdrive%,comport1$,baud1$,parity1$,stopbit1$
write #2,freqbase!,freqmin!,freqmax!,maxsovr%,minsovr%,rpminc%,motor%,gearcount?
write #2,cpage%,cselect%,cptr%,helpbit%

for t?=1 to 8
write #2,ratio!(t?)
next t?


write #2,"colors"
'--- save palette --
for t=0 to 15
write #2,pal%(t)
next t

'--------------- background colors -----
write #2,bgmain%,bgsetup%,bgwork%,bgtool%,bgfile%,bgcolor%
write #2,fgmain%,fgsetup%,fgwork%,fgtool%,fgfile%,fgcolor%
write #2,modecf%,modecb%,filecf%,filecb%,vercf%,vercb%,clkcf%,clkcb%
write #2,fksel%,cfsel%,cbsel%,cfhsel%,cbhsel%,cfhpic%,cbhpic%
write #2,w1.cf,w1.cb,w1.bdr,w2.cf,w2.cb,w2.bdr
write #2,w3.cf,w3.cb,w3.hf,w3.hb,w3.bdr
write #2,w4.cf,w4.cb,w4.bdr,w5.cf,w5.cb,w5.bdr
write #2,w6.cf,w6.cb,w6.bdr,w7.cf,w7.cb,w7.hf,w7.hb,w7.bdr
write #2,w8.cf,w8.cb,w8.hf,w8.hb,w8.bdr
write #2,w9.cf,w9.cb,w9.bdr,w10.cf,w10.cb,w10.hf,w10.hb,w10.bdr
write #2,w11.cf,w11.cb,w11.bdr,w12.cf,w12.cb,w12.hf,w12.hb,w12.bdr
write #2,w13.cf,w13.cb,w13.bdr,w14.cf,w14.cb,w14.hf,w14.hb,w14.bdr
write #2,w15.cf,w15.cb,w15.hf,w15.hb,w15.bdr,w16.cf,w16.cb,w17.cb,w17.bdr
write #2,w18.cb,w18.bdr,w19.cb,w19.bdr,w20.cf,w20.cb,w20.bdr
write #2,w21.cf,w21.cb,w21.bdr,w22.cf,w22.cb,w22.bdr,w23.cf,w23.cb,w23.bdr

write #2,feedinc!,maxfeed!,accl!,extfeed%,exthand%,acelg0%
write #2,mirrx%,mirry%,feedmult%,xyscale#,zscale#,wscale#
write #2,revx%,revy%,revz%,revw%
write #2,bcklshx%,bcklshy%,bcklshz%,bcklshw%
write #2,scale#,spi??,spiz??,spiw??
write #2,set_op%,set_op2%,set_op3%,set_op4%,set_op5%

write #2,maxtools%,toolpage%,toolptr%,wovert%,wdh%,dh%
write #2,dirname$,filename$,dfext$,flptr%,ndex%
for t%=1 to maxtools%
write #2,x_offset&(t%),y_offset&(t%),z_offset&(t%),doffset#(t%)
next t%
for t%=0 to 6
write #2,wrk(t%).x,wrk(t%).y,wrk(t%).z,wrk(t%).w
next t%
write #2, machx&,machy&,machz&,machw&

'-------last line added
write #2, rotarya%,jogrpd%,datafile$,defunits% 'metric!
write #2, softlim%,limxhi&,limxlo&,limyhi&,limylo&,limzhi&,limzlo&,limwhi&,limwlo&
'write #2, coolbit2?,coolpin2? '--- CHANGED PERMANATELY TO BIT 3 PIN 17 ---
write #2, 3,17
write #2,scale#,spix??,spiy??,spiz??,spiw??,wfct!
write #2,pwm?,pwmrpm%
write #2, xlim%,ylim%,zlim%,wlim%
write #2, corner_gain!,password$,passkey$
write #2, error_port%
write #2, handadder%,handcarry%,handsoft%
write #2, xscale#,yscale#,zscale#,wscale#
write #2, iscale#,jscale#,kscale#,iscale!,jscale!,kscale!
write #2, jx1?,jx2?,jy1?,jy2?,jz1?,jz2?,jw1?,jw2?
write #2, lathe_bit%,feed_ipr%
close #2
return

'-------------------- Get DATA, gets all saved data from file! ---
getall:
860 open savename$ for input as #2
input #2,cver$
'if left$(cver$,13) <> left$(ver$,13) then fail$="INVALID CFG FILE! ":goto invalcfg

865 input #2,dataport%,statusport%,controlport%
input #2,dataport2%,statusport2%,controlport2%
input #2,xdirbit?,ydirbit?,zdirbit?,wdirbit?
input #2,xstepbit?,ystepbit?,zstepbit?,wstepbit?
input #2,xlimpin?,ylimpin?,zlimpin?,wlimpin?,holdpin?
input #2,xlimbit?,ylimbit?,zlimbit?,wlimbit?,holdbit?
input #2,xyzlim%,wlim%,xlowhi%,ylowhi%,zlowhi%,wlowhi%
input #2,limclear%,homefeed1!,retratio%,dirhomex%,dirhomey%,dirhomez%
input #2,emstop%,spndelay??,pram83%,srevtime??
input #2,sfwdbit?,srevbit?,litbit?,coolbit?
input #2,sfwdpin?,srevpin?,litpin?,coolpin?
input #2,aux1pin?,aux2pin?,aux3pin?,aux4pin?
input #2,aux1bit?,aux2bit?,aux3bit?,aux4bit?
input #2,handirq%,h1pulse%,h1step%,h10pulse%,h10step%,h100pulse%,h100step%
input #2,gsdrive%,comport1$,baud1$,parity1$,stopbit1$
input #2,freqbase!,freqmin!,freqmax!,maxsovr%,minsovr%,rpminc%,motor%,gearcount?
input #2,cpage%,cselect%,cptr%,helpbit%

for t?=1 to 8
input #2,ratio!(t?)
next t?

'-----
input #2,tmp$
'--- save palette --
for t=0 to 15
input #2,pal%(t)
next t

'--------------- background colors -----
input #2,bgmain%,bgsetup%,bgwork%,bgtool%,bgfile%,bgcolor%
input #2,fgmain%,fgsetup%,fgwork%,fgtool%,fgfile%,fgcolor%
input #2,modecf%,modecb%,filecf%,filecb%,vercf%,vercb%,clkcf%,clkcb%
input #2,fksel%,cfsel%,cbsel%,cfhsel%,cbhsel%,cfhpic%,cbhpic%
input #2,w1.cf,w1.cb,w1.bdr,w2.cf,w2.cb,w2.bdr
input #2,w3.cf,w3.cb,w3.hf,w3.hb,w3.bdr
input #2,w4.cf,w4.cb,w4.bdr,w5.cf,w5.cb,w5.bdr
input #2,w6.cf,w6.cb,w6.bdr,w7.cf,w7.cb,w7.hf,w7.hb,w7.bdr
input #2,w8.cf,w8.cb,w8.hf,w8.hb,w8.bdr
input #2,w9.cf,w9.cb,w9.bdr,w10.cf,w10.cb,w10.hf,w10.hb,w10.bdr
input #2,w11.cf,w11.cb,w11.bdr,w12.cf,w12.cb,w12.hf,w12.hb,w12.bdr
input #2,w13.cf,w13.cb,w13.bdr,w14.cf,w14.cb,w14.hf,w14.hb,w14.bdr
input #2,w15.cf,w15.cb,w15.hf,w15.hb,w15.bdr,w16.cf,w16.cb,w17.cb,w17.bdr
input #2,w18.cb,w18.bdr,w19.cb,w19.bdr,w20.cf,w20.cb,w20.bdr
input #2,w21.cf,w21.cb,w21.bdr,w22.cf,w22.cb,w22.bdr,w23.cf,w23.cb,w23.bdr

input #2,feedinc!,maxfeed!,accl!,extfeed%,exthand%,acelg0%
input #2,mirrx%,mirry%,feedmult%,xyscale#,zscale#,wscale#
input #2,revx%,revy%,revz%,revw%
input #2,bcklshx%,bcklshy%,bcklshz%,bcklshw%
input #2,scale#,spi??,spiz??,spiw??
input #2,set_op%,set_op2%,set_op3%,set_op4%,set_op5%

input #2,maxtools1%,toolpage%,toolptr%,wovert%,wdh%,dh%
input #2,dirname$,filename$,dfext$,flptr%,ndex%

for t%=1 to maxtools%
input #2,x_offset&(t%),y_offset&(t%),z_offset&(t%),doffset#(t%)
next t%
for t%=0 to 6
input #2,wrk(t%).x,wrk(t%).y,wrk(t%).z,wrk(t%).w
next t%
input #2, machx&,machy&,machz&,machw&

'-------last line added
input #2, rotarya%,jogrpd%,datafile$,defunits% 'metric!
input #2, softlim%,limxhi&,limxlo&,limyhi&,limylo&,limzhi&,limzlo&,limwhi&,limwlo&
input #2, voidcoolbit2?,voidcoolpin2?
input #2,scale#,spix??,spiy??,spiz??,spiw??,wfct!
input #2,pwm?,pwmrpm%
input #2, xlim%,ylim%,zlim%,wlim%
input #2, corner_gain!,password$,passkey$
input #2, error_port%
input #2, handadder%,handcarry%,handsoft%
input #2, xscale#,yscale#,zscale#,wscale#
input #2, iscale#,jscale#,kscale#,iscale!,jscale!,kscale!
input #2, jx1?,jx2?,jy1?,jy2?,jz1?,jz2?,jw1?,jw2?
input #2, lathe_bit%,feed_ipr%
invalcfg:
870 close #2

return


'-------------------------- file select function -------------
setfile:
auto%=5
mode$=" FILE      "
screen 0,,1,1

flt$=dir$(dirname$)
flgth%=0
while flt$ <>""
incr flgth%
flt$=dir$
wend
dim flist$(flgth%)
flist$(0)=ucase$(dir$(dirname$))
for I%=1 to flgth%
flist$(I%)=dir$
next I%
array sort flist$()


rdfile:
view text (1,2)-(80,23)
color fgfile%,bgfile%
cls

view text (6,21)-(80,23)
print "Crtl+C  =  CHANGE DIRECTORY                    F1  =  LOAD FILE"
print "Crtl+S  =  SAVE ACTIVE FILE TO NEW NAME"
print "Crtl+D  =  DELETE SELECTED FILE";



view text (w7.x,w7.y)-(w7.x+w7.h-1,w7.y+w7.v)
color w7.bdr,w7.cb
ldir$=ucase$(dirname$) '+" "+string$(20,chr$(205))
print chr$(201)repeat$(w7.h-2,chr$(205)) chr$(187);
for t%=0 to w7.v-2
print chr$(186)repeat$(w7.h-2,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w7.h-2,chr$(205))chr$(188);
locate 1,5
print " " ldir$ " "

scrlgth% = w7.v-1
fpage% = 5 * scrlgth%

gosub bkclk
gosub clkdsp
gosub selects
view text (w7.x+1,w7.y+1)-(w7.x+w7.h-2,w7.y+w7.v-1)
color w7.cf,w7.cb
cls

nk1:
if flptr% < 1 then flptr% = 1
if flptr% > flgth% then flptr% = flgth%
view text (w7.x+1,w7.y+1)-(w7.x+w7.h-2,w7.y+w7.v-1)
color w7.cf,w7.cb
if redsc% then cls : redsc%=0
for i%=1 to fpage%
if i%+ndex% <= flgth% then
if i%+ndex%=flptr% then
color w7.hf,w7.hb
print using" \           \";flist$(I%+ndex%);
else
color w7.cf,w7.cb
print using" \           \";flist$(I%+ndex%);
end if
end if
next i%


while not instat
gosub clkdsp
if cancyc% and arun% and mov%=0 and in_op%=0 then gosub cancycle
if m_ready% then gosub M_exc
gosub autoread
wend
ky$=inkey$

if ky$=chr$(27) then gosub sreset ':goto rdfile


if fload% then goto fskpky
if ky$ > chr$(0,60) and ky$ < chr$(0,69) then goto xit
if ky$=chr$(0,59) then
	filename$ = flist$(flptr%)
	filexch%=1
	screen 0,,0,0:gosub opnfile
	ky$=chr$(0,65)
	goto xit
end if

fskpky:

if ky$=chr$(0,61) or ky$=chr$(0,62) or ky$=chr$(0,65) or ky$=chr$(0,66) then goto xit


if ky$=chr$(0,72) then
if flptr% > ndex% + 5 then
decr flptr% ,5
else
if ndex% >= 5 then decr ndex%,5:decr flptr%,5:redsc%=1
end if
end if

if ky$=chr$(0,80) then
if flptr% <= flgth%-5 and flptr% < ndex% + fpage% - 5 then
incr flptr%,5
else
if ndex% + fpage% < flgth% then incr ndex%,5:incr flptr%,5:redsc%=1
end if
end if

if ky$=chr$(0,77) then if flptr% < flgth% and flptr% < ndex%+fpage% then incr flptr%
if ky$=chr$(0,75) then if flptr% > ndex% then decr flptr%

if ky$=chr$(0,81) then
if ndex% + fpage% < flgth% then incr ndex%,fpage%:incr flptr%,fpage%:redsc%=1
end if

if ky$=chr$(0,73) then
if ndex% > 0 then decr ndex%,fpage%:decr flptr%,fpage%:redsc%=1
if ndex% < 0 then ndex%=0
end if


if ky$=chr$(4) then
view text (20,w7.v+3)-(37,w7.v+6)
color w10.cf,w10.cb
print chr$(218) chr$(196)" DELETE Y/N "chr$(196)chr$(196) chr$(191)
print chr$(179);
print using" \            \";flist$(flptr%);
print chr$(179)
print chr$(192) repeat$(15,chr$(196)) chr$(217);
while not instat
wend
ky$=ucase$(inkey$)
if ky$="Y" then
kill dirname$+flist$(flptr%)
erase flist$:goto setfile
end if
goto rdfile
end if


if ky$=chr$(19) then
prompt$="SAVE ACTIVE FILE WITH NEW NAME"
tmp$=filename$
filename$ = ucase$( gettext$ (filename$,prompt$,34,20,w7.v+3,bycopy w10.cf,bycopy w10.cb))
if doit% then
'if tmp$ <> filename$ then
if instr(filename$,".") = 0 then filename$=filename$+dfext$
fchanged%=1:gosub savcurfile
erase flist$:goto setfile
end if
goto rdfile
end if


if ky$=chr$(3) then
prompt$="CHANGE DIRECTORY"
tmp$=dirname$
dirname$ = ucase$( gettext$ (dirname$,prompt$,34,20,w7.v+3,bycopy w10.cf,bycopy w10.cb))
if tmp$ <> dirname$ then
if right$(dirname$,1) <>"\" then
dirname$=dirname$+"\"
end if
erase flist$:goto setfile
end if
goto rdfile
end if


goto nk1


'--------------------------------
xit:
erase flist$
screen 0,,0,0
return

setoffset:
auto%=6
mode$=" OFFSET    "
gosub selects
return

setsetup:
auto%=7
mode$=" SETUP     "
gosub selects
return


'-------------- feed hold and emergency stop ----------
fhold:
estop%=1
mode$=" FEED HOLD "
!cli
while estop% and mov% and ehold% = 0
!sti
'gosub clkdsp     'disabled so emgstop don't loop
gosub page1
wend
return
'-------------------- cancel feed , no resume -----------
canchold:
dx&=0:dy&=0:dz&=0:dw&=0
if limithit% then estop%=0 : limithit%=0 :es3& = 0
return

'--------------------------------- CANNED CYCLE ----------------------------


cancycle:
'if mdat(os2%).new = 0 then return

if sng_block% then arun%=0
if cancyc% then goto incycle

if mdat(os2%).new then
cancyc%=1:Qdepth&=0
if mdat(os2%).Canz < mdat(os2%).Rval then
peckz& = - mdat(os2%).Qval/metric!
else
peckz&=mdat(os2%).Qval/metric!
end if

xpout&=mdat(os2%).xpos
ypout&=mdat(os2%).ypos
'zpout&=mdat(os2%).zpos
wpout&=mdat(os2%).wpos

xdout&=xpout&-pxp&
ydout&=ypout&-pyp&
'zdout&=zpout&-pzp&
wdout&=wpout&-pwp&

if wdout&=0 and xdout&=0 and ydout&=0 then goto incycle
end if

gosub movecalc

return

incycle:
select case cancyc%
case = 1
	select case mdat(os2%).g9
	case = 73 : gosub R73
	case = 74 : gosub R81
	case = 81 : gosub R81
	case = 82 : gosub R81
	case = 83 : gosub R83
	case = 84 : gosub R81
	case = 85 : gosub R81
	case = 89 : gosub R81
        case <> 80 : fail$="ERROR Invalid canned cycle"
	end select

case = 2
	select case mdat(os2%).g9
	case = 73 : gosub F73
	case = 74 : gosub FD84
	case = 81 : gosub F81
	case = 82 : gosub F81
	case = 83 : gosub F83
	case = 84 : gosub FD84
	case = 85 : gosub F81
	case = 89 : gosub F81
	end select


case = 3
	select case mdat(os2%).g9
	case = 73 : gosub PA83
	case = 74 : gosub RV84
 	case = 81 : gosub E81
	case = 82 : gosub PA82
	case = 83 : gosub PA83
	case = 84 : gosub RV84
	case = 85 : gosub FU85
	case = 89 : gosub PA82

	end select

case = 4
	select case mdat(os2%).g9
	case = 73 : gosub E73
	case = 74 : gosub PA84
	case = 82 : gosub E81
	case = 83 : gosub E83
	case = 84 : gosub PA84
	case = 85 : gosub E81
	case = 89 : gosub FU85
	end select
case = 5
	select case mdat(os2%).g9
	case = 74 : gosub FU84
	case = 84 : gosub FU84
	case = 89 : gosub E81
	end select
case = 6
	select case mdat(os2%).g9
        case = 84 : gosub E84
        case = 74 : gosub E84
	end select

end select
incr cancyc%
if getnew% then getnew%=0:goto incycle '-- if getnew then get another move --
return

'------------------------ Move canned cycle -------------
zrun:
zdout& = zpout& - pzp&
gosub movecalc
return
'---------------------------------------------------------------------

'---------------------------- Rapid plane --------------
R81:
'----- return if no rapid move and get next cancyc% move ---
if mdat(os2%).Rval = mdat(os2%).zpos then getnew%=1:return
mdat(os2%).rpd=1
zpout& = mdat(os2%).Rval
gosub zrun
return

'----------------------- Feed to depth -------------------------------
F81:
zpout& = mdat(os2%).Canz
mdat(os2%).rpd=0
gosub zrun
arun%=1
return

'------------------------- Return plane cycle and end ----------------
E81:
mdat(os2%).rpd=1
if mdat(os2%).g10=98 then zpout& = mdat(os2%).zpos
if mdat(os2%).g10=99 then zpout& = mdat(os2%).Rval
gosub zrun
cancyc%=-1
return

'----------------------- Rapid plane for G83 ---------------------
R83:
mdat(os2%).rpd=1
zpout& = mdat(os2%).Rval + Qdepth& + pram83% * dv!
gosub zrun
if sng_block% then arun%=0
return

F83:
incr Qdepth&,peckz&
if sgn(peckz&)=-1 and Qdepth& + mdat(os2%).Rval < mdat(os2%).Canz then Qdepth& = mdat(os2%).Canz-mdat(os2%).Rval
if sgn(peckz&)=1 and Qdepth& + mdat(os2%).Rval > mdat(os2%).Canz then Qdepth& = mdat(os2%).Canz-mdat(os2%).Rval
zpout& = Qdepth& + mdat(os2%).Rval
mdat(os2%).rpd=0
gosub zrun
arun%=1


if Qdepth& <> mdat(os2%).Canz - mdat(os2%).Rval then incr cancyc%

return

PA83:
if tmr2?? < mdat(os2%).candelay then decr cancyc%
arun%=1
return


E83:
mdat(os2%).rpd=1
zpout& = mdat(os2%).Rval
if Qdepth& = mdat(os2%).Canz - mdat(os2%).Rval then
 if mdat(os2%).g10=98 then zpout& = mdat(os2%).zpos
end if
gosub zrun
cancyc%=0
if Qdepth& = mdat(os2%).Canz - mdat(os2%).Rval then cancyc%=-1

return


'----------------------- Rapid plane for G73 ---------------------
R73:
mdat(os2%).rpd=1
zpout& = mdat(os2%).Rval+ Qdepth& + pram83% * dv!
gosub zrun
return

F73:
incr Qdepth&,peckz&
if sgn(peckz&)=-1 and Qdepth& + mdat(os2%).Rval < mdat(os2%).Canz then Qdepth& = mdat(os2%).Canz-mdat(os2%).Rval
if sgn(peckz&)=1 and Qdepth& + mdat(os2%).Rval > mdat(os2%).Canz then Qdepth& = mdat(os2%).Canz-mdat(os2%).Rval
zpout& = Qdepth& + mdat(os2%).Rval
mdat(os2%).rpd=0
gosub zrun
if Qdepth& <> mdat(os2%).Canz - mdat(os2%).Rval then incr cancyc%
arun%=1
return

E73:
mdat(os2%).rpd=1
zpout& = Qdepth& + mdat(os2%).Rval + pram83% * dv!
if Qdepth& = mdat(os2%).Canz - mdat(os2%).Rval then _
 if mdat(os2%).g10=98 then zpout& = mdat(os2%).zpos else zpout& = mdat(os2%).Rval
gosub zrun
cancyc%=1
if Qdepth& = mdat(os2%).Canz - mdat(os2%).Rval then cancyc%=-1
'if sng_block% then arun%=0

return


'---------------------------- G84 Tap Cycle Rapid plane --------------
R84:
mdat(os2%).rpd=1
zpout& = mdat(os2%).Rval
gosub zrun
return

'----------------------- G84 Feed to depth -------------------------------
FD84:
no_feed_overide%=1:mdat(os2%).rpd=0
if gsdrive% then gosub newrpm
zpout& = mdat(os2%).Canz
gosub zrun
arun%=1
return
'---------------------- reverse spindle ------
RV84:
gosub setsdir		'reverse spindle dir
arun%=1
return

'----------------- pause while spindle reverses
PA84:
if tmr2?? < srevtime?? then decr cancyc%
arun%=1
return

'----------------------- G84 Feed back up  -------------------------------
FU84:
no_feed_overide%=1:mdat(os2%).rpd=0
if gsdrive% then gosub newrpm
zpout& = mdat(os2%).Rval
gosub zrun
arun%=1
return



'------------------------- G84 Return plane cycle and end ----------------
E84:
mdat(os2%).rpd=1:skpblk%=0:no_feed_overide%=0
if gsdrive% then gosub newrpm
if mdat(os2%).g10=98 then zpout& = mdat(os2%).zpos
if mdat(os2%).g10=99 then zpout& = mdat(os2%).Rval
gosub zrun
gosub setsdir		'reverse spindle dir
cancyc%=-1

return

PA82:
if tmr2?? < mdat(os2%).candelay then decr cancyc%
arun%=1
return


FU85:
mdat(os2%).rpd=0
zpout& = mdat(os2%).Rval
gosub zrun
arun%=1
return


return
'-------- end of canned cycles ---------------

'---------------------- EXEC M CODE ----------
M_exc:
!cli
m_ready%=0
!sti

'-------- G20, G21,  set inch or metric mode --------
if g6_out% <> lastgp6% then
lastgp6% = g6_out%
if g6_out% = 20 then nunits%=0
if g6_out% = 21 then nunits%=1
gosub setsys
end if

'------------- this only displays M values in window array ----
if m0_out% > -1 then mcode%(6) = m0_out%:gosub Mshift
if m1_out% > 0 then mcode%(6) = m1_out%:gosub Mshift
if m2_out% > 0 then mcode%(6) = m2_out%:gosub Mshift
if m3_out% > 0 then mcode%(6) = m3_out%:gosub Mshift

'------ sets new rpm --------------------
if g13_out%=97 then
if lastsval! <> s_out! then lastsval!= s_out! :gosub newrpm
end if

if m0_out% = 30 then do_reset%=1 ':return
if m0_out% = 2 then do_reset%=1 ':return
if m0_out% = 0 then arun%=0:spndl%=1:gosub setspndl
if m0_out% = 1 then arun%=0:spndl%=1:gosub setspndl
'if m0_out% = 50 then syncdelay%=1:synclow%=0


if m1_out% = 3 then if spndldir% and spndl% then gosub setsdir _
 else spndldir%=0:spndl%=0:gosub setspndl
if m1_out% = 4 then if spndldir%=0 and spndl% then gosub setsdir _
 else spndldir%=1:spndl%=0:gosub setspndl
if m1_out% = 5 then spndl%=1:gosub setspndl

if m2_out% = 6 then
toolins% = t_out%
end if

if m3_out% = 7 then cool2% = 0 :gosub setcool2
if m3_out% = 8 then cool1% = 0 :gosub setcool1

if m3_out% = 9 then
cool1% = 1:gosub setcool1
cool2% = 1:gosub setcool2
end if

if m3_out% = 13 then
cool1%=0:gosub setcool1
if spndldir% and spndl% then
gosub setsdir
else
spndldir%=0:spndl%=0
gosub setspndl
end if
end if

if m3_out% = 14 then
cool1%=0:gosub setcool1
if spndldir%=0 and spndl% then
gosub setsdir
else
spndldir%=1:spndl%=0
gosub setspndl
end if
end if

if m0_out% = 40 then mirrx%=1:mirry%=1
if m0_out% = 41 then mirrx%=-1
if m0_out% = 42 then mirry%=-1

'if g0_out% = 4 then    'G4 delay routine
'tmr1??=0:tmr5??=0
'gosub pos_dsp	 'reset timers and update position dispaly
'while not instat and tmr5?? < p_out%
'if tmr1?? > 20 then tmr1??=0:gosub clkdsp
'loop
'end if

'--------------------- if M33 then save position to the data to file ---
if m0_out% = 33 then gosub logpoint

m_ready% = 0  'reset the M-code ready signal
m0_out%=-1
m1_out%=0
m2_out%=0
m3_out%=0

if auto% < 2 then gosub vline:ll%=1:gosub displaytool
if auto%=4 then gosub displaytool 'in MDI update the M and T displays
return

'--------------------------------------------------------

'--------------------------------------------------------
Mshift:
for t% = 1 to 6
mcode%(t%-1) = mcode%(t%)
next t%
'if auto% < 2 or auto% = 4 then gosub displaytool
return

'--------------------------------------------------------

logpoint:
if savedat% then
if first31% then
print #3,"(Data start "realtime$" on "DATE$")"
end if
gosub poscalc
'gosub dist_to_go

incr datpoint%:print #3,"N"+mid$(str$(datpoint%),2);
msg$="Point "+str$(datpoint%)
if defunits% then mplc%=3 else mplc%=4	'--- set places for metric ----
if dspx! <> lastxsave! or first31% then
lastxsave!=dspx!
if dspx! < 0 then
print #3, "X"+ str$(round(dspx!,mplc%));
else
print #3, "X"+ mid$(str$(round(dspx!,mplc%)),2);
end if
end if

if dspy! <> lastysave! or first31% then
lastysave!=dspy!
if dspy! < 0 then
print #3, "Y"+ str$(round(dspy!,mplc%));
else
print #3, "Y"+ mid$(str$(round(dspy!,mplc%)),2);
end if
end if

if dspz! <> lastzsave! or first31%  then
lastzsave!=dspz!
if dspz! < 0 then
print #3, "Z"+ str$(round(dspz!,mplc%));
else
print #3, "Z"+ mid$(str$(round(dspz!,mplc%)),2);
end if
end if


if rotarya% then mplc%=3
if dspw! <> lastwsave! or first31%  then
lastwsave!=dspw!
if dspw! < 0 then
print #3, "A"+ str$(round(dspw!,mplc%));
else
print #3, "A"+ mid$(str$(round(dspw!,mplc%)),2);
end if
end if
print #3,
first31%=0
end if
return

'------------------- AXIS HOME ---------------!!!!!!!!!!!!!!
home:

gosub sreset
view text (w16.x,w16.y)-(w16.x+w16.h,w16.y+w16.v)
cls
color 0,w16.cb:print chr$(218) repeat$ (18,chr$(196));:color 7,w16.cb:print chr$(191);
color 0,w16.cb:print chr$(179);:color w16.cf,w16.cb
'if xyzlim%=0 then
'print " MANUAL AXIS HOME ";:color 7,w16.cb:print chr$(179);
'else
print "  AUTO AXIS HOME  ";:color 7,w16.cb:print chr$(179);
'end if
color 0,w16.cb:print chr$(179);:color w16.cf,w16.cb
print "  (enter for XYZ) ";:color 7,w16.cb:print chr$(179);
color 0,w16.cb:print chr$(179);:color w16.cf,w16.cb
print "   X  HOME        ";:color 7,w16.cb:print chr$(179);
color 0,w16.cb:print chr$(179);:color w16.cf,w16.cb
print "   Y  HOME        ";:color 7,w16.cb:print chr$(179);
color 0,w16.cb:print chr$(179);:color w16.cf,w16.cb
print "   Z  HOME        ";:color 7,w16.cb:print chr$(179);
color 0,w16.cb:print chr$(179);:color w16.cf,w16.cb
print "   A  HOME        ";:color 7,w16.cb:print chr$(179);
color 0,w16.cb:print chr$(192);:color 7,w16.cb:print repeat$ (18,chr$(196)); chr$(217);

while not instat
gosub clkdsp
wend
ky$=ucase$(inkey$)
if ky$=chr$(27) then goto nohome
dohome%=1

if ky$="X" then
	if xlim%=0 then machx&=0:zerox%=1 else gosub homex
end if

if ky$="Y" then
	if ylim%=0 then machy&=0:zeroy%=1 else gosub homey
end if

if ky$="Z" then
	if zlim%=0 then machz&=0:zeroz%=1 else gosub homez
end if

if ky$="A" then
	if wlim%=0 then machw&=0:zerow%=1 else gosub homew
end if

if ky$=chr$(13) then
	if zlim% then gosub homez else machz&=0:zeroz%=1
        if xlim% and ylim% then
        gosub homexy
        else
        if xlim% then gosub homex else machx&=0:zerox%=1
	if ylim% then gosub homey else machy&=0:zeroy%=1
        end if
	'if xyzlim%=0 then
        'machx&=0:machy&=0:machz&=0
        'zerox%=1:zeroy%=1:zeroz%=1
        'else
        'gosub homez
        'if zeroz% then gosub homexy
        'end if
end if
if limithit% then dx&=0:dy&=0:dz&=0:dw&=0:estop%=0:es3&=0:limithit%=0
dohome%=0
homebit%=0
x2home%=0:xfromlim%=0
y2home%=0:yfromlim%=0
z2home%=0:zfromlim%=0
w2home%=0:wfromlim%=0

nohome:
gosub bckdraw
if helpbit% then gosub help_dsp
gosub page1
gosub displaytool
gosub sreset
ky$=""
'limithit%=0

return

'------------------------- X/Y HOME
homexy:
zerox%=0:zeroy%=0
feed1!=homefeed1!:rfeed%=0
gosub newfeed
x2home%=1
y2home%=1
homebit%=1
while (not instat) and dohome% and (zerox%=0 or zeroy%=0)
gosub clkdsp
gosub page1
wend
if (zerox%=0 or zeroy%=0) then fail$=" AXIS HOME FAILED"

return
'-------------------------- X HOME
homex:
zerox%=0
feed1!=homefeed1!:rfeed%=0
gosub newfeed
x2home%=1
homebit%=1
while (not instat) and dohome% and zerox%=0
gosub clkdsp
gosub page1
wend
if zerox%=0 then fail$=" ERROR ON X HOME"
return

'---------------------------- Y HOME
homey:
zeroy%=0
feed1!=homefeed1!:rfeed%=0
gosub newfeed
y2home%=1
homebit%=1
while (not instat) and dohome% and zeroy%=0
gosub clkdsp
gosub page1
wend
if zeroy%=0 then fail$=" ERROR ON Y HOME"
return

'------------------------- Z HOME
homez:
zeroz%=0
feed1!=homefeed1!:rfeed%=0
gosub newfeed
z2home%=1
homebit%=1
while (not instat) and dohome% and zeroz%=0
gosub clkdsp
gosub page1
wend
if zeroz%=0 then fail$=" ERROR ON Z HOME"
return


'------------------------- W HOME
homew:
zerow%=0
feed1!=homefeed1!:rfeed%=0
gosub newfeed
w2home%=1
homebit%=1
while (not instat) and dohome% and zerow%=0
gosub clkdsp
gosub page1
wend
if zerow%=0 then fail$=" ERROR ON A HOME"
return


'----------------------- MDI input ----------------
setmdi:

hlp$(0)="EXECUTE LINE  = Enter           "
hlp$(1)="MACHINE HOME  = Home   (Quit any key)"
hlp$(2)="HOLD  =  Space"
hlp$(3)="MENU  =  F12"
hlp$(4)="RESET =  Esc"
if helpbit% then gosub help_dsp

auto%=4:pg%=0
mode$=" MDI       "
gosub selects
switchmdi%=0
if mov%=1 then gosub fhold
gosub displaytool

rn2ky:
gosub dspman
rn1ky:
while not instat
if requpd% then requpd%=0:gosub upd_pos
'------ if arun% is in this line then MDI will single block cancycles !!!
'if cancyc% > 0 and arun% and mov%=0 and in_op%=0 then  gosub cancycle
if cancyc% > 0 and mov%=0 and in_op%=0 then  gosub cancycle


'---------- added multimove line for G12 and G13 circles ----------
if multimove% and mov%=0 and in_op%=0 then gosub runline
gosub clkdsp
gosub page1


if Lval%(sublevel%) > 0 and cancyc%=0 and mov%=0 and in_op%=0 then gosub runline
if ts% <> ud% then gosub dspud
if m_ready% then gosub M_exc
if do_reset% and mov%=0 then gosub sreset:gosub dspman
wend
ky$=inkey$
if ky$=chr$(0,133) or ky$=chr$(0,134) or ky$=chr$(0,84) or ky$=chr$(0,85) then gosub setmenu:goto rn1ky

if ky$= chr$(0,72) then
mdi$=lastmdi$(w3.v-2)
tmp$=lastmdi$(20)
for t%=20 to 1 step -1
lastmdi$(t%)=lastmdi$(t%-1)
next t%
lastmdi$(0)=tmp$
gosub dspman
gosub dspud
goto rn1ky
end if

if ky$= chr$(0,80) then
if mdi$="" then
mdi$=lastmdi$(w3.v-1)
else
mdi$=lastmdi$(w3.v)
tmp$=lastmdi$(0)
for t%=0 to 19
lastmdi$(t%)=lastmdi$(t%+1)
next t%
lastmdi$(20)=tmp$
end if
gosub dspman
gosub dspud
goto rn1ky
end if

ky$=ucase$(ky$)

if ky$= chr$(0,71) then gosub home

if ky$=" " and mov% and estop%=0 then ky$="":gosub fhold
if ky$=" " and estop%=1 then ky$=""
if ky$=chr$(13) and mov% = 0 and mdi$<>"" and onlimit%=0 then ky$="" :gosub runline:goto rn1ky
if ky$=chr$(13) and estop%=1 then estop%=0:ky$="":mode$=" MDI       "
if ky$=chr$(0,64) then ky$=""


if ky$ > chr$(0,62) and ky$ < chr$(0,69) then return' gosub upd_pos:return
if ky$=chr$(27) then mdi$="":gosub sreset:mode$=" MDI       ":goto rn2ky

if ky$=chr$(8) then ky$="":if len(mdi$) > 0 then mdi$ = left$ (mdi$,len(mdi$)-1)
if ky$ > chr$(31) and ky$ < chr$(123) then mdi$ = mdi$+ky$

if menu% then
if ky$=chr$(0,59) then gosub upd_pos:return
if ky$ > chr$(0,60) and ky$ < chr$(0,64) then return' gosub upd_pos:return

else
gosub mankeys
end if

gosub dspud
goto rn1ky

dspud:
ud% = ts%
if cn%=1 then cn%=0:cur$=" " else cn%=1:cur$="_"
view text (w3.x,w3.y)-(w3.x+w3.h,w3.y+w3.v)
color w3.cf,w3.cb
locate w3.v,1
print "> " mdi$;cur$;"  "
return

'-------- update manual display lines -----
dspman:
view text (w3.x,w3.y)-(w3.x+w3.h,w3.y+w3.v)
color w3.cf,w3.cb
cls
for t%=0 to w3.v-2
print "  "lastmdi$(t%)
next t%
return

runline:
os2%=4
mdat(4).Fline=mdi$
call line_read# (4,mdat(),wrk())
mdat(4).g7=40
gosub run_mdi
gosub displaytool
if Lval%(sublevel%) > 0 then return
if multimove% then return
lastmdi$(w3.v-1)=mdi$
tmp$=lastmdi$(0)
for t%=0 to 19
lastmdi$(t%)=lastmdi$(t%+1)
next t%
lastmdi$(20)=tmp$
mdi$=""
gosub dspman

return



'----------------------------------- RUN MDI DATA ___--------------
run_mdi:
'---------- set feed rate and acceleration -------
feed1! = mdat(4).Fval
if mdat(4).g1 = 0 then mdat(4).rpd=1 else mdat(4).rpd=0
if feed1!=0 then feed1!=.1
gosub newfeed

'--------------------- set dir bits if plane changes -------
newplane% = mdat(4).g2
if lastplane% <> newplane% then gosub setdirbits	'check for plane change

if mdat(4).g9 <> 80 then arun%=1:goto cancycle

Idout&=mdat(4).Ival:Jdout&=mdat(4).Jval
xpout&=mdat(4).xpos:ypout&=mdat(4).ypos
zpout&=mdat(4).zpos:wpout&=mdat(4).wpos
g1%=mdat(4).g1:xcp&=mdat(4).Xcp:ycp&=mdat(4).Ycp

zpout&=mdat(4).zpos:wpout&=mdat(4).wpos
xdout&=xpout&-pxp&:ydout&=ypout&-pyp&
zdout&=zpout&-pzp&:wdout&=wpout&-pwp&

if fail$ <> "" then return
if cancyc%=0 then gosub movecalc
'if m_ready% then gosub M_exc
return



'--------------------- SET JOG MODE -------------
setjog:

hlp$(0)="Jog X =   "+chr$(27)+chr$(26)+ "              RESET =  Esc"
hlp$(1)="Jog Y =   "+chr$(24)+chr$(25)+"              MENU  =  F12"
hlp$(2)="Jog Z =  Page up/dn       RPM   =  < >"
hlp$(3)="Jog A =  Ins/Del          INC/DEC = +/-"
hlp$(4)="F9 = RAPID/FEED           L = LOCK FEED"
if helpbit% then gosub help_dsp

auto%=2
mode$=" JOG       "
if mov% = 1 then gosub fhold
pg%=0		'--- switches display page ---
gosub selects
gosub vline
public kt0%,kt1??,kt2??

'----------------- set key interrupts ---------
'----------------- get old interrupt vector and save it! ----
reg 1, &h3509
call interrupt (&h21)
sav1=reg(9) : sav2=reg(2)
cstr=codeseg (keysub)
pttr=codeptr (keysub)

'---------------------- set new keyboard interrupt vector ---
reg 1,&h2509
reg 8,cstr:reg 4,pttr
call interrupt (&h21)
jogbit%=1

incfeed:
feed1! = Fval!
if jogrpd% then rfeed%=1 else rfeed%=0
gosub newfeed
nkey:
kt0%=0

while kt0% = 0
gosub clkdsp
gosub page1
wend

if joglock% then		'Lock the jog keys-----------------
if kt0%=57 then gosub clearkeys
select case kt0%
case 75,77,72,80,73,81,82,83
gosub lockkeys
end select
end if

if kt0%=38 then
gosub clearkeys:jogrpd%=0
if joglock% then joglock%=0 else joglock%=1
gosub selects:goto incfeed
end if

if kt0%=67 then
gosub clearkeys
if jogrpd% then jogrpd% = 0 else jogrpd%=1
joglock%=0:gosub selects:goto incfeed
end if


if kt0%=25 and savedat% then
gosub logpoint ':msg$="Point "+str$(datpoint%)
gosub message
end if


if kt0%=51 then if spcnt% > minsovr% then decr spcnt% ,rpminc%:gosub newrpm
if kt0%=52 then if spcnt% < maxsovr% then incr spcnt% ,rpminc%:gosub newrpm

if extfeed% then
if kt0%=12 or kt0%=74 then decr Fval!,feedinc!:if Fval! < 0 then Fval!=0:goto incfeed else goto incfeed
if kt0%=13 or kt0%=78 then incr Fval!,feedinc!:if Fval! > maxfeed! then Fval!=maxfeed!:goto incfeed else goto incfeed
else

if kt0%=12 or kt0%=74 then if lastfin? > 0 then decr lastfin?:goto incfeed

if kt0%=13 or kt0%=78 then
	if lastfin? < 20 then
	incr lastfin?
	goto incfeed
	else
	incr Fval!,feedinc!
	if Fval! > maxfeed! then Fval!=maxfeed!
	goto incfeed
	end if
end if
end if

if kt0%=1 then gosub clearkeys:gosub sreset:jogbit%=1 ':gosub displaytool

if kt0% > 62 and kt0% < 67 then ky$ =chr$(0)+chr$(kt0%):goto res_old
if kt0% = 68 then ky$ = chr$(0) + chr$(kt0%):goto res_old

if ktx?(42) then
if kt0% = 59 or kt0% = 60 then gosub setmenu
end if
if kt0% = 87 or kt0% = 88 then gosub setmenu


if menu%=0 and ktx?(42)=0 then
if kt0% = 59 then gosub setspndl
if kt0% = 60 then gosub setsdir
if kt0% = 61 then gosub setcool1
if kt0% = 62 then gosub setcool2
'if kt0% = 63 then gosub setlight
end if

if menu% and ktx?(42)=0 then
if kt0% = 59  then ky$ =chr$(0)+chr$(kt0%):goto res_old
if kt0% > 60 and kt0% < 67 then ky$ =chr$(0)+chr$(kt0%):goto res_old
end if
goto nkey

'--------------- restore old keyboard interrupt vector ----------
res_old:
msg$="":gosub message
if limithit% then dx&=0:dy&=0:dz&=0:dw&=0:estop%=0:es3&=0:limithit%=0
jogbit%=0:mov%=0
reg 1,&h2509
reg 8,sav1:reg 4,sav2
call interrupt (&h21)
for t% = 0 to 128
ktx?(t%) = 0
next t%
gosub upd_pos
while instat: k$=inkey$: wend
return

clearkeys: 'clear all jog keys
ktx?(jx1?)=0
ktx?(jx2?)=0
ktx?(jy1?)=0
ktx?(jy2?)=0
ktx?(jz1?)=0
ktx?(jz2?)=0
ktx?(jw1?)=0
ktx?(jw2?)=0
return

lockkeys:
if ktx?(kt0%) then
gosub clearkeys
else
gosub clearkeys
ktx?(kt0%)=1
end if
return
'---------------------------------------------------------------------


'---------------------- get old int 70 timer interrupt and save it! --
settime_int:

jpdos = codeptr32(rtc2)
dosclk = &h1c0
oldcvec??? = @dosclk
@jpdos = oldcvec???
'------------------------ get vector to new isr --------
newcvec??? = codeptr32(rt_int)
'------ set new int 70h  interrupt vector, inc's tmr1??,tmr2?? after set ---
@dosclk = newcvec???

!cli
'----------- set interrupt controller first to acknowledge int's ---
tint?= inp (&ha1)
tint1? = tint? and &hfe
out &ha1,tint1?

out &h70,&ha      'set address to status register A
out &h71,&h23     'set to 8192 (23h) interrupts per second from 1024 (26h)
out &h70,&hb      'set address of clock to status register B
out &h71,&h42     'send 42h setting perodic interrupts on and bcd units
out &h70,&hc      'set address to status register C
t? = inp (&h71)   'read status reg C in assembler this requires 2 reads
!sti

return

'---------------- reset int 70h back before quitting --------
rsettime_int:
!cli
out &ha1,tint?
@dosclk = oldcvec???
out &h70,&hb      'set address to status register B
out &h71,&h02     'send 02h setting perodic interrupts off and bcd units
out &h70,&ha      'set address to status register A
out &h71,&h26     'set to 1024 (26h) interrupts per second from 8192 (23h)
!sti

return

'----------------------------------------------------------------------------
clearmsg:
fail$=""
gosub bkgmsg
return

ETRAP:
if err = 57 and erl=500 then resume 501
if erl=600 then resume 602 '----- error opening com port
if erl=605 then resume 607
if erl=650 then resume 652
if erl=860 then fail$="Configuration file not found":resume 870
if erl=865 then fail$="Check configuration parameters!": resume 870
if erl=720 then resume 721	'resumes divide by zero error on print ??
'if erl=720 then incr zdivide% : resume 721	'resumes divide by zero error on print ??
fail$="exiting from error "+ str$(err)+" at"+str$(eradr)
resume exitf

'------------------ exit PROGRAM !!!!!!!
exitf:
close
'fchanged%=1:gosub savcurfile
gosub sreset
if fail$="" then gosub saveall
gosub rsettime_int
gosub resethirq
gosub resetirq
gosub fixtime
gosub filter_int_reset
exitg:
palette
screen 0,0,0,0
cls
locate 6,1
if fail$<>"" then print fail$:while not instat:wend
'print "divide error count was" zdivide%
end

'--------------- EMERGENCY STOP ------------------------------
emgstop:

if limithit% then dx&=0:dy&=0:dz&=0:dw&=0:estop%=0:es3&=0:limithit%=0

'dohome%=0
'homebit%=0


gosub sreset
tmr1??=0
while tmr1?? < 100	'delay 1/10 of a second for the servos to stop
wend
tmr1??=400
'watchdog?=0	'turn off watchdog timer!

tmr2??=0
while tmr2?? < 400
if extstop% then tmr2??=0

'---read a port for error number
current_error? = inp(error_port%)

if tmr1?? > 400 then
tmr1??=0
if t%=0 then t%=1:colr1%=modecb% else t%=0:colr1%=11
view text (67,1) - (80,1)
color modecf%,colr1%
print using "& ###"; " EMG-STOP" current_error?;
end if
if auto% < 5 then gosub page1
wend

'-- Clear the home setting flags on the display
hdat%=0	 'clear any handle movement
'watchdog?=1	'turn watchdog output back on

while instat 'clear any keys pressed while in e-stop
k$=inkey$
wend

if auto%=4 then mode$=" MDI       "
'zerox%=0:zeroy%=0:zeroz%=0:zerow%=0	'clear the home display flag

return

'---------------------- SYSTEM RESET  ------------------------------
sreset:

!cli
Gdelay%=0 '--- removes the move loop from delay if set with G4
next_op%=0  '--- make sure next move in not executed
if mov% then
if extfeed% and extfin?=0 then makefeed%=1
if auto% <> 2 and auto% <> 3 then gosub fhold '-- do not call fhold in jog(2) or handle(3) modes
fin?=1
end if

'------------------------clear move buffer ----------

xdout&=0:ydout&=0:zdout&=0:wdout&=0  '--- clear the next calculated move

mov_xdi&=0       '--- clear the next ready move
mov_ydi&=0
mov_wdi&=0
mov_zdi&=0
mov_cir&=0
mov_cd&=0
s3&=0
'dcircum&=0
'dc1&=0

dx&=0:dy&=0:dz&=0:dw&=0  '-- clear the current move

decl&=1
se1&=1
last_decl&=1   '--- set last deceleration to full so next acceleration is full also
last_declmod!=0


'mdat_g9%=80	'--- to make sure current canned cycle is canceled --

'------------------- reset all positions to actual ---------
nxp&=machx&:nyp&=machy&:nzp&=machz&:nwp&=machw&
xpos&=machx&*dv!:ypos&=machy&*dv!:zpos&=machz&*dv!:wpos&=machw&*dvw!
pxp&=xpos&:pyp&=ypos&:pzp&=zpos&:pwp&=wpos&
pxpos&=pxp&:pypos&=pyp&:pzpos&=pzp&:pwpos&=pwp&
xpout&=xpos&:ypout&=ypos&:zpout&=zpos&:wpout&=wpos&
!sti
makefeed%=0
gosub maskcalc

'------------- turn off everything on control port ------

'tmpin?=&hf0:bit toggle tmpin?,2
tmpin?=inp(controlport%):bit toggle tmpin?,2
tmpin1?=&hf0 ':bit set tmpin1?,litbit?
tmpin?= tmpin? or not tmpin1?
gosub control_out

if gsdrive% then
spndl%=1:gosub setspndl
end if


hxoffset&=0
hyoffset&=0
hzoffset&=0
Dval%=0
Hval%=0
if fload% then lptr&=lptr&-4:cusr%=2:if lptr& <0 then lptr&=0
fload%=0
cancyc%=0
arun%=0
spndl%=0
cool1%=0
cool2%=0
dx&=0:dy&=0:dz&=0:dw&=0
estop%=0
ehold%=0
cstart%=0
no_feed_overide%=0
dspf%=1
acl%=0
calcfin%=0
abrt%=0
mov%=0
xtralines%=0
sublevel%=0
rmkskip%=0
if auto%=0 then mode$=" RUN SINGLE"
if auto%=1 then mode$=" RUN AUTO  "

if auto%=4 then
for t%=0 to w3.v-2:lastmdi$(t%)=""
next t%

end if

arun%=0
first31% = 1
requpd% = 0
if ky$=chr$(27) and fail$ <>"" then :gosub clearmsg
msg$=""
datpoint%=0
multimove%=0
h_out%=0
d_out%=0
l_out%=0
sub_out%=0
xtoo%=0
ytoo%=0
ztoo%=0
wtoo%=0
syncdelay%=0
synclow%=0
dohome%=0

for t%=0 to 10		'-- clear all sub levels of Lval%() ----
Lval%(t%)=0
sublptr&(t%)=0
next t%
'----------------- clear all M codes
for t% = 0 to 6
mcode%(t%) = -1
next t%

mg0%=-1         '--- set mg0% to -1 so M00 will display ok
mg1%=0
mg2%=0
mg3%=0
mov_mg0%=-1
mov_mg1%=0
mov_mg2%=0
mov_mg3%=0

m_ready%=0
m0_out%=-1
m1_out%=0
m2_out%=0
m3_out%=0

gp0%=0
gp1%=0 		'--- G0 set
gp3%=90		'--- default absolute mode = G90
gp2%=17		'--- default plane x/y = G17
gp7%=40		'--- default offset mode = G40
gp9%=80		'--- default canned cycle = 80
nunits%=defunits%
if defunits%=0 then gp6%=20:rdmetric!=1.00000 '--- default = inch input = 20
if defunits%=1 then gp6%=21:rdmetric!=25.4000 '--- default = MM input = 20
lastgp6%=gp6%
gp8%=49		'--- default tool length = 49 cancel
gp10%=98	'--- default return point = 98 inital start point
gp5%=94 	'--- Feed in IPM
gp13%=97	'--- SFM is off
gp14%=54	'--- default work cooridinate = 54


g0_out%=gp0%
g1_out%=gp1%
g2_out%=gp2%
g7_out%=gp7%
g3_out%=gp3%
g9_out%=gp9%
g6_out%=gp6%
g8_out%=gp8%
g10_out%=gp10%
g5_out%=gp5%
g13_out%=gp13%
g14_out%=gp14%

gosub setsys
gosub selects

if auto% < 2 or auto% = 4 then
gosub displaytool
end if

fin?=0
if do_reset% then lptr&=0
do_reset%=0
return



'----------------- resets previous machine environment -----------
upd_pos:
if fload%=1 then
fload%=0:cusr%=2
 gp0% = 0 'g0_out%
 gp1% = g1_out%
 gp2% = g2_out%
 gp3% = g3_out%
 gp6% = g6_out%
 gp7% = g7_out%
 gp8% = g8_out%
 gp9% = g9_out%
 gp10% = g10_out%
 gp5% = g5_out%
 gp14% = g14_out%
 gp13% = g13_out%
 mg0%  = m0_out%
 mg1%  = m1_out%
 mg2%  = m2_out%
 mg3%  = m3_out%
 Fval! = feed1!
 Sval! = s_out!
 Dval! = d_out%
 Hval! = h_out%
 Qval! = q_out%
 Tval%=t_out%
 sublevel% = sub_out%
' Lval%(sub_out%) = l_out%
 hxoffset& = hx_out&
 hyoffset& = hy_out&
 hzoffset& = hz_out&
 lptr& = cur_lnum&-1
 candelay% = cdly_out%
' sublptr&(sublevel%)=lptr&
 multimove% = 0

end if


'if mov%=1 then gosub fhold		'stops any movement


'-------- set everything to actual machine position ----

if lastplane%=17 then
nxp&=machx&:nyp&=machy&:nzp&=machz&:nwp&=machw&
xpos&=machx&*dv!:ypos&=machy&*dv!:zpos&=machz&*dv!:wpos&=machw&*dvw!
pxpos&=xpos&:pypos&=ypos&:pzpos&=zpos&:pwpos&=wpos&
pxp&=xpos&:pyp&=ypos&:pzp&=zpos&:pwp&=wpos&
xpout&=xpos&:ypout&=ypos&:zpout&=zpos&:wpout&=wpos&
end if

if lastplane%=18 then
nxp&=machx&:nzp&=machy&:nyp&=machz&:nwp&=machw&
xpos&=machx&*dv!:ypos&=machy&*dv!:zpos&=machz&*dv!:wpos&=machw&*dvw!
pxpos&=xpos&:pypos&=ypos&:pzpos&=zpos&:pwpos&=wpos&
pxp&=xpos&:pzp&=ypos&:pyp&=zpos&:pwp&=wpos&
xpout&=xpos&:ypout&=ypos&:zpout&=zpos&:wpout&=wpos&
end if

if lastplane%=19 then
nzp&=machx&:nyp&=machy&:nxp&=machz&:nwp&=machw&
xpos&=machx&*dv!:ypos&=machy&*dv!:zpos&=machz&*dv!:wpos&=machw&*dvw!
pxpos&=xpos&:pypos&=ypos&:pzpos&=zpos&:pwpos&=wpos&
pzp&=xpos&:pyp&=ypos&:pxp&=zpos&:pwp&=wpos&
xpout&=xpos&:ypout&=ypos&:zpout&=zpos&:wpout&=wpos&
end if

if requpd% then
gosub reloadfile
requpd%=0
fload%=1
end if
return


clkdsp:
'---------------- show clock on screen-------

view text (1,1) - (80,1)
color modecf%,modecb%
print using" \           \"; mode$;

color filecf%,filecb%
print" FILE ";
print using " \             \ ";filename$;
print using "\          \";sig$;
color vercf%,vercb%
print using"\                 \";ver$;


color clkcf%,clkcb%
print using" \        \";realtime$;



'------ select one or the other sends to gs2 drive every 125 ms

if tmr6?? > 125 then
tmr6??=0
if toggel1% then
toggel1%=0

'---- check gs2 drive for new data ------
500 if gsdrive% and comtmr1?? > comdelay% then
	if loc(4) > 1 then
	comread$=input$(loc(4),#4)
		if len(comread$)=31 then
		gs2error%=val("&h"+mid$(comread$,8,4))
		gs2freq!=val("&h"+mid$(comread$,20,4))/10
		gs2amp!=val("&h"+mid$(comread$,24,4))/10
		gs2load%=gs2amp!/17*100
		end if
	end if
501 end if


'---- SFM calc, try to install constant SFM to monitor X axis and adjust RPM

	if g13_out%=96 then
        !sti
        xloc_new! = dspx!/xscale#
        !cli
	cir1!= pi# * xloc_new!
	if cir1! > 0 then nextsval! = (sf_out! * 12) / cir1!
	if nextsval! > maxs! then nextsval! = maxs!
		if nextsval! <> lastsval! then
		lastsval!=nextsval!
		gosub newrpm
		end if
	end if
else
toggel1%=1

'---- request new gs2 update
	if gsdrive% then
        while comtmr1?? < comdelay%
        wend
	comread$=input$(loc(4),#4) '--- empty the read buffer
	newdat$="010321000005"
        gosub sendGS2
	end if

end if
end if
'-----  end



'---- request new load info from GS2 drive ----
'comread$=input$(loc(4),#4) '--- clear the read input buffer
'if comtmr1?? > 300 then newdat$="010321000005":gosub sendGS2
'end if







if extstop% then gosub emgstop

return


'------------------- setup position numbers for display ----
poscalc:
if fload% then
!cli
dspx!=(machx& - x92_out& - wrk(g14_out%-53).x - hx_out&)/ spi?? * metric! * mirrx%
dspy!=(machy& - y92_out& - wrk(g14_out%-53).y - hy_out&)/ spi?? * metric! * mirry%
dspz!=(machz& - z92_out& - wrk(g14_out%-53).z - hz_out&)/ spi?? * metric!
if rotarya% then
dspw!=(machw& - w92_out& - wrk(g14_out%-53).w)/ spi??* wfct!
else
dspw!=(machw& - w92_out& - wrk(g14_out%-53).w)/ spi??* metric!
end if
else
dspx!=(machx& - wrk(0).x - wrk(gp14%-53).x - hxoffset&)/ spi?? * metric! * mirrx%
dspy!=(machy& - wrk(0).y - wrk(gp14%-53).y - hyoffset&)/ spi?? * metric! * mirry%
dspz!=(machz& - wrk(0).z - wrk(gp14%-53).z - hzoffset&)/ spi?? * metric!
if rotarya% then
dspw!=(machw& - wrk(0).w - wrk(gp14%-53).w)/ spi??* wfct!
else
dspw!=(machw& - wrk(0).w - wrk(gp14%-53).w)/ spi?? * metric!
end if
end if
if rotarya% then
if dspw! < 0 then incr dspw!,360
if dspw! >= 360 then decr dspw!,360
end if
!sti
return

'------------------ DISPLAY PAGE 1 PRINT SETTINGS ---------
page1:
if auto% < 5 then  '-- only print in AUTO, SINGLE, or MDI mode
gosub pos_dsp
gosub tool_dsp
gosub dist_to_go
if gsdrive% then gosub gs2_dsp
end if

'----------------------- MSG WINDOW ---------------------
failstop:
view text (w6.x+1,w6.y+1) - (w6.x+w6.h-1,w6.y+w6.v-1)
color w6.cf,w6.cb

if fail$ <>"" then
arun%=0
print fail$
print " ESC to clear";
'gosub sreset
end if

'--------------------- LIMIT HIT WINDOWS -----
if onlimit% then
view text (w6.x+1,w6.y+1) - (w6.x+w6.h-1,w6.y+w6.v-1)
color w6.cf,w6.cb

if stopx% then print"LIMIT X! ";
if stopy% then print"LIMIT Y! ";
if stopz% then print"LIMIT Z! ";
if stopw% then print"LIMIT A! ";
limdsp%=1
else
if limdsp% then
view text (w6.x+1,w6.y+1) - (w6.x+w6.h-1,w6.y+w6.v-1)
color w6.cf,w6.cb
cls
limdsp%=0
end if
end if
return

'------------- abs machine pos display ----
pos_dsp:
gosub poscalc:
view text (w1.x+1,w1.y+1) - (w1.x+w1.h-1,w1.y+w1.v-1)
'view text (w1.x+2,w1.y+1) - (w1.x+w1.h-1,w1.y+w1.v)
color w1.cf,w1.cb
if zerox% then print" "; else print chr$(7);
print using xlout$;dspx!/xscale#;
if zeroy% then print" "; else print chr$(7);
print using ylout$;dspy!/yscale#;
if zeroz% then print" "; else print chr$(7);
print using zlout$;dspz!/zscale#;
if zerow% then print" "; else print chr$(7);
print using wlout$;dspw!/wscale#;
return


'----------- print S and F at the same update as position display ---
tool_dsp:
view text (w4.x+1,w4.y+1)-(w4.x+w4.h-1,w4.y+3)
color w4.cf,w4.cb
print using" S###%"; spcnt%;
print using" #####.#"; sout!

print using " F###%";fpd1!*100;
if feed_ipr% then
print using " ##.####";CSNG(ovr_feed! * metric!)
else
print using " #####.#";CSNG(ovr_feed! * metric!)
end if

print using" R###%";rpd1!*100;
print using " #####.#";CSNG(maxfeed!*rpd1!*metric!);

'view text (w6.x+1,w6.y+1) - (w6.x+w6.h-1,w6.y+w6.v-1)
'color w6.cf,w6.cb
return


'---------------------DISTANCE TO GO WINDOW ----------
dist_to_go:
view text (w2.x+2,w2.y+1) - (w2.x+w2.h-1,w2.y+4)
color w2.cf,w2.cb

if g2_out% = 17 then
print using xlout$;dx&/spi?? * metric! /xscale#;
print using ylout$;dy&/spi?? * metric! /yscale#;
print using zlout$;dz&/spi?? * metric! /zscale#;
print using wlout$;dw&/spi??;
end if

if g2_out% = 18 then
print using xlout$;dx&/spi?? * metric! /xscale#;
print using ylout$;dz&/spi?? * metric! /yscale#;
print using zlout$;dy&/spi?? * metric! /zscale#;
print using wlout$;dw&/spi??;
end if

if g2_out% = 19 then
print using xlout$;dz&/spi?? * metric! /xscale#;
print using ylout$;dy&/spi?? * metric! /yscale#;
print using zlout$;dx&/spi?? * metric! /zscale#;
print using wlout$;dw&/spi??;
end if

return

'---------- MACHINE POS WINDOW -------
mach_pos:
color w20.cf,w20.cb
view text (w20.x+2,w20.y+1) - (w20.x+w20.h,w20.y+w20.v)
print using xlout$;machx&/spi??* metric!
print using ylout$;machy&/spi??* metric!
print using zlout$;machz&/spi??* metric!
if rotarya% then
print using wlout$;machw&/spi??*wfct!;
else
print using wlout$;machw&/spi?? * metric!;
end if
return

'----------- gs2load window ------
gs2_dsp:
gs2message:
view text (w18.x+1,w18.y+1) - (w18.x+w18.h-1,w18.y+w18.v-1)
color w18.cf,w18.cb
print using" LOAD ###%";gs2load%;
print using"  AMPS ###.#";gs2amp!
print using" GEAR   #";gear?;
print using"   FREQ####.#";gs2freq!;
return



'----------------------- tool window display ---------
displaytool:
view text (w4.x+1,w4.y+4)-(w4.x+w4.h-1,w4.y+w4.v-1)
color w4.cf,w4.cb

print using" H## "; h_out%;
print using cdout$;CSNG(z_offset&(h_out%) * metric!/spi??);
print using" D## ";d_out%;
print using cdout$;CSNG(doffset#(d_out%)*metric!);
print using" L ####";l_out%;
print using" Sub ##";sub_out%
print using" Ts ###";toolins%;
print using" Tn ###";t_out%;


'----------------------- command window display ---------
displaycmd:
if gsdrive%=0 then gosub cmd_pos
gosub message

'----------------- G-code window display ----------
view text (w5.x+1,w5.y+1)-(w5.x+4,w5.y+w5.v-1)
color w5.cf,w5.cb
720
print using " 0#";g1_out%;
if g0_out% > 0 then print g0_out%;
if g2_out% > 0 then print g2_out%;
if g6_out% > 0 then print g6_out%;
if g7_out% > 0 then print g7_out%;
if g8_out% > 0 then print g8_out%;
if g14_out% > 0 then print g14_out%;
if g9_out% > 0 then print g9_out%;
if g3_out% > 0 then print g3_out%;
if g5_out% > 0 then print g5_out%;
if g13_out% > 0 then print g13_out%;
if g10_out% > 0 then print g10_out%;
print
721


'---------- M code display --------
view text (w17.x+1,w17.y+1)-(w17.x+4,w17.y+w17.v-1)
color w17.cf,w17.cb

for t% = 5 to 0 step -1
if mcode%(t%) > -1 then
print mcode%(t%);
else
print "   ";
end if
next t%

return

'----------- command position window display
cmd_pos:
view text (w19.x+1,w19.y+1)-(w19.x+w19.h-1,w19.y+w19.v-1)
color w19.cf,w19.cb

if fload% then
cmdx&=mdat(1).xcmd
cmdy&=mdat(1).ycmd
cmdz&=mdat(1).zcmd
cmdw&=mdat(1).wcmd
cmdi&=mdat(1).Ival
cmdj&=mdat(1).Jval
cmdk&=mdat(1).Kval
else

cmdx&=Xval&
cmdy&=Yval&
cmdz&=Zval&
cmdw&=Wval&
cmdi&=Ival&
cmdj&=Jval&
cmdk&=Kval&

end if

print using " X"+cdout$;cmdx&/expand&/xscale#;
print using " I"+cdout$;cmdi&/expand&/iscale#

print using " Y"+cdout$;cmdy&/expand&/yscale#;
print using " J"+cdout$;cmdj&/expand&/jscale#

print using " Z"+cdout$;cmdz&/expand&/zscale#;
print using " K"+cdout$;cmdk&/expand&/kscale#

print using wlout$;cmdw&/expandw&/wscale#;
return


'----------- print a message to msg window ------
message:
view text (w6.x+1,w6.y+1) - (w6.x+w6.h-1,w6.y+w6.v-1)
color w6.cf,w6.cb
if msg$ ="" then
cls
else
print msg$;
end if

return
'------------------------ help dsplay window
help_dsp:

color fgmain%,bgmain%
view text (4,15)-(46,19)
cls
for t2%=0 to 3
print hlp$(t2%)
next t2%
print hlp$(4);
return


selects:
'--------------- selection display --------

view text (1,24)-(80,25)
color fksel%,cbsel%
print"  F1      F2      F3      F4      F5      F6      F7      F8      F9      F10   ";
if jogrpd% then jogfast$=" Rpd" else jogfast$="  Fd"
if joglock% then jogfast$=" Loc"
if menu%=0 then
if spndl%=1 then color cfhsel%,cbhsel%:print" SPINDLE "; else color cfsel%,cbsel%:print" SPINDLE ";
if spndldir%=1 then :print" REV ";:color cfsel%,cbsel%:print" "; else print" FWD ";:color cfsel%,cbsel%:print" ";
if cool1%=1 then color cfhsel%,cbhsel%:print" COOL-1 ";:color cfsel%,cbsel% else print" COOL-1 ";
if pwm? then
print"  PWM   ";
else
if cool2%=1 then color cfhsel%,cbhsel%:print" COOL-2 ";:color cfsel%,cbsel% else print" COOL-2 ";
end if
'if light%=1 then print" ";:color cfhsel%,cbhsel%:print" LIGHT ";:color cfsel%,cbsel%:print" ";:color cfsel%,cbsel% else print"  LIGHT  ";
else
if auto%=5 then color cfhsel%,cbhsel%:print" LOAD  "; else color cfsel%,cbsel%:print" FILE  ";
if edit% then color cfhsel%,cbhsel%:print"  EDIT  "; else color cfsel%,cbsel%:print"  EDIT  ";
if auto%=6 then color cfhsel%,cbhsel%:print"  TOOL "; else color cfsel%,cbsel%:print"  TOOL ";
if auto%=7 then color cfhsel%,cbhsel%:print" POSITION"; else color cfsel%,cbsel%:print" POSITION";
end if

mnu3:
if auto%=8 then print" ";:color cfhsel%,cbhsel%:print" SETUP ";:color cfsel%,cbsel%:print" "; else color cfsel%,cbsel%: print"  SETUP  ";
if auto%=4 then color cfhpic%,cbhpic%:print"  MDI  ";:color cfsel%,cbsel% else print"  MDI  ";
if auto%=0 then color cfhpic%,cbhpic%:print" SINGLE ";:color cfsel%,cbsel% else print" SINGLE ";
if auto%=1 then color cfhpic%,cbhpic%:print"  AUTO  ";:color cfsel%,cbsel% else print"  AUTO  ";
if auto%=2 then
color cfhpic%,cbhpic%:print jogfast$ " JOG ";:color cfsel%,cbsel%
else
print jogfast$ " JOG ";
end if
if auto%=3 then color cfhpic%,cbhpic%:print" HANDLE ";:color cfsel%,cbsel% else print" HANDLE ";

return


'------------- background layout redraw ---------------
bckdraw:
screen 0,,0,0
color fgmain%,bgmain%
cls

if helpbit% then
w3.v=10
else
w3.v=15
end if


gosub back_pos
gosub back_dist
gosub back_tool

if gsdrive% then
gosub back_gs2
else
gosub back_cmd
end if

gosub back_file
gosub back_gm
gosub bkgmsg
gosub selects
return

'-------------- position display -----------
back_pos:
view text (w1.x,w1.y) - (w1.x+w1.h,w1.y+w1.v)
color w1.bdr,w1.cb
cls
print chr$(201)repeat$(w1.h-1,chr$(205))chr$(187);
for t%=0 to w1.v-2
print chr$(186)repeat$(w1.h-1,chr$(32)) chr$(186);
next t%
print chr$(200)repeat$(w1.h-1,chr$(205))chr$(188);
locate 1,3
print"PGM POSITION"
return

'--------------- distance to go display ---
back_dist:
view text (w2.x,w2.y) - (w2.x+w2.h,w2.y+w2.v)
color w2.bdr,w2.cb
cls
print chr$(201)repeat$(w2.h-1,chr$(205))chr$(187);
for t%=2 to w2.v
print chr$(186)repeat$(w2.h-1,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w2.h-1,chr$(205))chr$(188);
locate 1,3
print" DIST TO GO "
return



'--------------- distance to go display ---
back_mach:
view text (w20.x,w20.y) - (w20.x+w20.h,w20.y+w20.v)
color w20.bdr,w20.cb
cls
print chr$(201)repeat$(w20.h-1,chr$(205))chr$(187);
for t%=2 to w2.v
print chr$(186)repeat$(w20.h-1,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w20.h-1,chr$(205))chr$(188);
locate 1,3
print" MACHINE POS "
return



'-------------- Tool info display ----------
back_tool:
view text (w4.x,w4.y)-(w4.x+w4.h,w4.y+w4.v)
color w4.bdr,w4.cb
cls
print chr$(201)repeat$(w4.h-1,chr$(205))chr$(187);
for t%=2 to w4.v
print chr$(186)repeat$(w4.h-1,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w4.h-1,chr$(205))chr$(188);
locate 1,6
print" TOOL "
return

'-------------- GS2 drive info display ----------
back_gs2:
view text (w18.x,w18.y)-(w18.x+w18.h,w18.y+w18.v)
color w18.bdr,w18.cb
cls
print chr$(201)repeat$(w18.h-1,chr$(205))chr$(187);
for t%=2 to w18.v
print chr$(186)repeat$(w18.h-1,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w18.h-1,chr$(205))chr$(188);
locate 1,4
print" GS2 DRIVE "
return

'-------------- command window background ----------
back_cmd:
view text (w19.x,w19.y)-(w19.x+w19.h,w19.y+w19.v)
color w19.bdr,w19.cb
cls
print chr$(201)repeat$(w19.h-1,chr$(205))chr$(187);
for t%=2 to w19.v
print chr$(186)repeat$(w19.h-1,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w19.h-1,chr$(205))chr$(188);
locate 1,4
print" COMMAND"
return

'------------------file display -----------
back_file:
view text (w3.x-1,w3.y-1)-(w3.x+w3.h+2,w3.y+w3.v+1)
color w3.bdr,w3.cb
cls
print chr$(201)repeat$(w3.h+2,chr$(205))chr$(187);
for t%=0 to w3.v
print chr$(186)repeat$(w3.h+2,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w3.h+2,chr$(205))chr$(188);
return


'------------------ G code -------------
back_gm:
view text (w5.x,w5.y)-(w5.x+w5.h,w5.y+w5.v)
color w5.bdr,w5.cb
cls
print chr$(201)chr$(205)" G "chr$(205)chr$(187);
for t%=2 to w5.v
print chr$(186)repeat$(w5.h-1,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w5.h-1,chr$(205))chr$(188);

'------------------ M code -------------
view text (w17.x,w17.y)-(w17.x+w17.h,w17.y+w17.v)
color w17.bdr,w17.cb
cls
print chr$(201)chr$(205)" M "chr$(205)chr$(187);
for t%=2 to w17.v
print chr$(186)repeat$(w17.h-1,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w17.h-1,chr$(205))chr$(188);
return

'------------------ MSG window ------------
bkgmsg:

view text (w6.x,w6.y) - (w6.x+w6.h,w6.y+w6.v)
color w6.bdr,w6.cb
cls
print chr$(201)repeat$(w6.h-1,chr$(205))chr$(187);
for t%=2 to w6.v
print chr$(186)repeat$(w6.h-1,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w6.h-1,chr$(205))chr$(188);
locate 1,4
print" MSG "
return

'------------------ SETUP window ------------
bkgsetup:

view text (w15.x,w15.y) - (w15.x+w15.h,w15.y+w15.v)
color w15.bdr,w15.cb
cls
print chr$(201)repeat$(31,chr$(205))" SETUP "repeat$(31,chr$(205))chr$(187);
for t%=2 to w15.v
print chr$(186)repeat$(w15.h-1,chr$(32))chr$(186);
next t%
print chr$(200)repeat$(w15.h-1,chr$(205))chr$(188);
return

'----------------- Clock back ground redraw --------
bkclk:
view text (1,1) - (80,1)
color 3,1
cls
return

$segment

'----------------------- direction check function --------------
'------------- uses tisecx&,tisecy&,isecx&,isecy& --------------
'------- returns 1 if direction of offset is same as original ---
function dir_chk% (d%,mdat() as movedat) shared
local nw1#,nw2#,nw3#
!cli
nw1# = fnatn#(mdat(d%).offx2 - mdat(d%).offx1,mdat(d%).offy2-mdat(d%).offy1)
nw2# = fnatn#(isecx&-tisecx&,isecy&-tisecy&)
if nw1# < pi1# then incr nw1#,pi1#:incr nw2#,pi1#
if nw1# > pi1#*3 then decr nw1#,pi1#:decr nw2#,pi1#
if nw2# > pi#*2 then decr nw2#,pi#*2
if nw2# < 0 then incr nw2#,pi#*2
nw3#=nw2#-nw1#
if abs(nw3#) < 1.57 then dir_chk%=1 else dir_chk%=0
!sti
end function

'vector_calc:

'return


'--------------------- OFFSET calc ------------------NNNNNNNNNNNNNNNNNNNNN
offset:

'os2%=1
'---------- set feed rate and acceleration -------
'feed1! = mdat(1).Fval
'if mdat(1).g1 = 0 then mdat(1).rpd=1 else mdat(1).rpd=0


'--------------------- set dir bits if plane changes -------
'newplane% = mdat(1).g2
'if lastplane% <> newplane% then gosub setdirbits	'check for plane change

'if mdat(1).g9 <> 80 then goto cancycle

if auto%=4 then cstart%=0:goto no_off


if mdat(1).g7 < 41 and cstart%=0 then goto no_off
if mdat(1).g7 = 40 then goto no_off




if mdat(1).g1 > 1 and mdat(2).g1 > 1 then goto c2c	'-- circle to circle int.
if mdat(1).g1 > 1 and mdat(2).g1 < 2 then goto c2l	'-- circle to line int.
if mdat(1).g1 < 2 and mdat(2).g1 > 1 then goto l2c	'-- line to circle int.
if mdat(1).g1 < 2 and mdat(2).g1 < 2 then goto l2l	'-- line to line intersec.

'------------------ line to line offset calc ------
l2l:
if mdat(1).skp=1 then xpout&=mdat(1).offx2:ypout&=mdat(1).offy2:goto olend
if mdat(2).g7 = 40 then xpout&= mdat(1).offx2:ypout&=mdat(1).offy2:cstart%=0:goto olend
if cstart%=0 then xpout&=mdat(2).offx1:ypout&=mdat(2).offy1:cstart%=1:goto olend

'--------- if next line is continuation of first then do this --------
if mdat(1).offx2=mdat(2).offx1 and mdat(1).offy2=mdat(2).offy1 then _
 	xpout&=mdat(2).offx1:ypout&=mdat(2).offy1:nodecl%=1:goto olend

call line2line (1,2,mdat()):tisecx&=isecx&:tisecy&=isecy&:tseg1%=seg1%:tseg2%=seg2%

'------- if move 3 is line then do this -----------------
if mdat(3).g7 = 40 then xpout&=tisecx&:ypout&=tisecy&:goto olend

if mdat(3).g1 < 2 then
call line2line (2,3,mdat())
	if dir_chk% (2,mdat()) then
        xpout&=tisecx&:ypout&=tisecy&:goto olend
	else
        call line2line (1,3,mdat())
	mdat(2).skp=1:mdat(2).offx2=isecx&:mdat(2).offy2=isecy&
	xpout&=isecx&:ypout&=isecy&:goto olend
        end if
end if

'------ if move 3 is an arc then do this -----------------
if mdat(3).g1 >1 then
call line2arc (2,3,mdat())
	if dir_chk% (2,mdat()) then
        xpout&=tisecx&:ypout&=tisecy&:goto olend
	else
        call line2line (1,3,mdat())
	mdat(2).skp=1:mdat(2).offx2=isecx&:mdat(2).offy2=isecy&
	xpout&=isecx&:ypout&=isecy&
	mdat(3).offx1=isecx&:mdat(3).offI=mdat(3).Xcp-isecx&
	mdat(3).offy1=isecy&:mdat(3).offJ=mdat(3).Ycp-isecy&: goto olend
        end if
end if

'---------- if that did'nt fix it then what ? ------


goto olend


'-------------------- line to arc comp -----------------
l2c:
if mdat(1).skp=1 then xpout&=mdat(1).offx2:ypout&=mdat(1).offy2:goto olend
if mdat(2).g7 =40 then xpout&=mdat(1).offx2:ypout&=mdat(1).offy2:cstart%=0:goto olend
if cstart%=0 then xpout&=mdat(2).offx1:ypout&=mdat(2).offy1:cstart%=1:goto olend

if mdat(2).skp=1 then
	if mdat(3).g1 < 2 then
	call line2line(1,3,mdat())
	mdat(2).offx2=isecx&:mdat(2).offy2=isecy&:xpout&=isecx&:ypout&=isecy&
	mdat(2).g1=1:goto olend
	else
	call line2arc(1,3,mdat()):mdat(2).offx2=isecx&
	mdat(2).offy2=isecy&:xpout&=isecx&:ypout&=isecy&:mdat(2).g1=1
	mdat(3).offx1=isecx&:mdat(3).offI=mdat(3).Xcp-isecx&
	mdat(3).offy1=isecy&:mdat(3).offJ=mdat(3).Ycp-isecy&: goto olend
	end if
end if


'-------- if next move is tangent to first then do this -------------
if abs(mdat(1).offx2-mdat(2).offx1) < errtol% and abs(mdat(1).offy2-mdat(2).offy1)< errtol%  then _
xpout&=mdat(1).offx2:ypout&=mdat(1).offy2:nodecl%=1:goto olend

call line2arc (1,2,mdat())
tisecx&=isecx&:tisecy&=isecy&:tseg1%=seg1%:tseg2%=seg2%

'------- if move 3 is line then do this -----------------
if mdat(3).g7 = 40 then
	xpout&=tisecx&:ypout&=tisecy&
	mdat(2).offI=mdat(2).Xcp-tisecx&
	mdat(2).offJ=mdat(2).Ycp-tisecy&
        goto olend
end if

if mdat(3).g1 < 2 then
call arc2line (2,3,mdat())
	if dir_chk% (2,mdat()) then
	xpout&=tisecx&:ypout&=tisecy&
	mdat(2).offx1=tisecx&:mdat(2).offI=mdat(2).Xcp-tisecx&
	mdat(2).offy1=tisecy&:mdat(2).offJ=mdat(2).Ycp-tisecy&
	goto olend
	else
	call line2line (1,3,mdat())
	mdat(2).skp=1:mdat(2).offx2=isecx&:mdat(2).offy2=isecy&
	mdat(2).g1=1:xpout&=isecx&:ypout&=isecy&:goto olend
	end if
end if

'------ if move 3 is an arc then do this -----------------
if mdat(3).g1 >1 then
call arc2arc (2,3,mdat())
	if dir_chk% (2,mdat()) then
	xpout&=tisecx&:ypout&=tisecy&
	mdat(2).offx1=tisecx&:mdat(2).offI=mdat(2).Xcp-tisecx&
	mdat(2).offy1=tisecy&:mdat(2).offJ=mdat(2).Ycp-tisecy&:goto olend
	else
	call line2arc (1,3,mdat())
	mdat(2).skp=1:mdat(2).offx2=isecx&:mdat(2).offy2=isecy&
	mdat(2).g1=1:xpout&=isecx&:ypout&=isecy&
	mdat(3).offx1=isecx&:mdat(3).offI=mdat(3).Xcp-isecx&
	mdat(3).offy1=isecy&:mdat(3).offJ=mdat(3).Ycp-isecy&: goto olend
	end if
end if

'---------------------- arc to line comp ---------------------
c2l:
if mdat(1).skp=1 then xpout&=mdat(1).offx2:ypout&=mdat(1).offy2:goto olend
'if mdat(1).Roff <=0 then print "Over Compensation ERROR "
if mdat(2).g7 =40 then xpout&=mdat(1).offx2:ypout&=mdat(1).offy2:cstart%=0:goto olend
if cstart%=0 then xpout&=mdat(2).offx1:ypout&=mdat(2).offy1:cstart%=1:goto olend

'------if next move is tangent then do this -----------------
if abs(mdat(1).offx2 - mdat(2).offx1) < errtol% and abs(mdat(1).offy2 - mdat(2).offy1) < errtol%  then _
xpout&=mdat(2).offx1:ypout&=mdat(2).offy1:nodecl%=1:goto olend


call arc2line (1,2,mdat())
tisecx&=isecx&:tisecy&=isecy&:tseg1%=seg1%:tseg2%=seg2%

'------- if comp ends on line 3 then do this -----------------
if mdat(3).g7 = 40 then xpout&=tisecx&:ypout&=tisecy&:goto olend

'------- if move 3 is line then do this -----------------
if mdat(3).g1 < 2 then
call line2line (2,3,mdat())
	if dir_chk% (2,mdat()) then
        xpout&=tisecx&:ypout&=tisecy&:goto olend
	else
        call arc2line (1,3,mdat())
	mdat(2).skp=1:mdat(2).offx2=isecx&:mdat(2).offy2=isecy&
	mdat(2).g1=1:xpout&=isecx&:ypout&=isecy&:goto olend
	end if
end if

'------ if move 3 is an arc then do this -----------------
if mdat(3).g1 >1 then
call line2arc (2,3,mdat())
	if dir_chk% (2,mdat()) then
        xpout&=tisecx&:ypout&=tisecy&:goto olend
	else
        call arc2arc (1,3,mdat())
	mdat(2).skp=1:mdat(2).offx2=isecx&:mdat(2).offy2=isecy&
	xpout&=isecx&:ypout&=isecy&
	mdat(3).offx1=isecx&:mdat(3).offI=mdat(3).Xcp-isecx&
	mdat(3).offy1=isecy&:mdat(3).offJ=mdat(3).Ycp-isecy&: goto olend
        end if
end if

'call arc2line (1,2,mdat()):xpout&=isecx&:ypout&=isecy&
'xpout&=mdat(2).offx1:ypout&=mdat(1).offy2
'goto olend

'----------------- arc to arc comp -----------------------
c2c:
if mdat(1).skp=1 then xpout&=mdat(1).offx2:ypout&=mdat(1).offy2:goto olend
if mdat(2).g7 =40 then xpout&=mdat(1).offx2:ypout&=mdat(1).offy2:cstart%=0:goto olend
if cstart%=0 then xpout&=mdat(2).offx1:ypout&=mdat(2).offy1:cstart%=1:goto olend

if mdat(2).skp=1 and mdat(3).g1 < 2 then
call arc2line(1,3,mdat())
mdat(2).offx2=isecx&:mdat(2).offy2=isecy&:xpout&=isecx&:ypout&=isecy&
mdat(2).g1=1:goto olend
end if

if mdat(2).skp=1 and mdat(3).g1 > 1 then
call arc2arc(1,3,mdat())
	if fail%=1 then
        mdat(2).g1=1:mdat(2).skp=0
	xpout&=mdat(1).offx2:ypout&=mdat(1).offy2:mdat(2).offx1=mdat(1).offx2
	mdat(2).offy1=mdat(1).offy2:mdat(2).offx2=mdat(3).offx1
	mdat(2).offy2=mdat(3).offy1:goto olend
        else
        mdat(2).offx2=isecx&:mdat(2).offy2=isecy&:xpout&=isecx&:ypout&=isecy&:mdat(2).g1=1
	mdat(3).offx1=isecx&:mdat(3).offI=mdat(3).Xcp-isecx&
	mdat(3).offy1=isecy&:mdat(3).offJ=mdat(3).Ycp-isecy&: goto olend
        end if
end if

if abs(mdat(1).offx2-mdat(2).offx1) < errtol% and abs(mdat(1).offy2-mdat(2).offy1)< errtol% then _
 	xpout&=mdat(2).offx1:ypout&=mdat(2).offy1:nodecl%=1:goto olend

call arc2arc (1,2,mdat()):tisecx&=isecx&:tisecy&=isecy&:tseg1%=seg1%:tseg2%=seg2%
if fail%=1 then fail$="":gosub l_end:goto fit_rad

'------- if move 3 is line then do this -----------------
if mdat(3).g1 < 2 then
call arc2line (2,3,mdat())
	if dir_chk% (2,mdat()) then
        xpout&=tisecx&:ypout&=tisecy&
	mdat(2).offx1=tisecx&:mdat(2).offI=mdat(2).Xcp-tisecx&
	mdat(2).offy1=tisecy&:mdat(2).offJ=mdat(2).Ycp-tisecy&:goto olend
	else
        call arc2line (1,3,mdat())
	mdat(2).skp=1:mdat(2).offx2=isecx&:mdat(2).offy2=isecy&
	mdat(2).g1=1:xpout&=isecx&:ypout&=isecy&:goto olend
        end if
end if

'------ if move 3 is an arc then do this -----------------
if mdat(3).g1 >1 then
call arc2arc (2,3,mdat())
	if dir_chk% (2,mdat()) then
        xpout&=tisecx&:ypout&=tisecy&
	mdat(2).offx1=tisecx&:mdat(2).offI=mdat(2).Xcp-tisecx&
	mdat(2).offy1=tisecy&:mdat(2).offJ=mdat(2).Ycp-tisecy&:goto olend
	else
        call arc2arc (1,3,mdat())
	mdat(2).skp=1:mdat(2).offx2=isecx&:mdat(2).offy2=isecy&
	mdat(2).g1=1:xpout&=isecx&:ypout&=isecy&
	mdat(3).offx1=isecx&:mdat(3).offI=mdat(3).Xcp-isecx&
	mdat(3).offy1=isecy&:mdat(3).offJ=mdat(3).Ycp-isecy&: goto olend
        end if
end if

'---------------------- end comp routines -----------------

fit_rad:
doffval& = doffset#(mdat(1).Dval) * expand& * mirrx% * mirry%
if mdat(1).g7=42 then doffval&=-doffval&

Idout&=mdat(1).xpos-mdat(1).offx2:Jdout&=mdat(1).ypos-mdat(1).offy2
xpout&=mdat(2).offx1:ypout&=mdat(2).offy1
if doffval& < 0 then g1%=3 else g1%=2
zpout&=mdat(1).zpos:wpout&=mdat(1).wpos
xdout&=xpout&-pxp&:ydout&=ypout&-pyp&
zdout&=zpout&-pzp&:wdout&=wpout&-pwp&
xcp&=mdat(1).xpos:ycp&=mdat(1).ypos
goto b2

olend:
g1%=mdat(1).g1:xcp&=mdat(1).Xcp:ycp&=mdat(1).Ycp
Idout&=mdat(1).offI:Jdout&=mdat(1).offJ
goto olend2

no_off:
Idout&=mdat(1).Ival:Jdout&=mdat(1).Jval
xpout&=mdat(1).xpos:ypout&=mdat(1).ypos
zpout&=mdat(1).zpos:wpout&=mdat(1).wpos
g1%=mdat(1).g1:xcp&=mdat(1).Xcp:ycp&=mdat(1).Ycp

olend2:
zpout&=mdat(1).zpos:wpout&=mdat(1).wpos
xdout&=xpout&-pxp&:ydout&=ypout&-pyp&
zdout&=zpout&-pzp&:wdout&=wpout&-pwp&


b1:
'--------------- final direction error checking ------------
'ckd1# = fnatn#(mdat(1).xpos - mdat(0).xpos,mdat(1).ypos-mdat(0).ypos)

ckd1# = fnatn#(mdat(1).offx2 - mdat(1).offx1,mdat(1).offy2-mdat(1).offy1)
ckd2# = fnatn#(xdout&,ydout&)

if ckd1# < pi1# then incr ckd1#,pi1#:incr ckd2#,pi1#
if ckd1# > pi1#*3 then decr ckd1#,pi1#:decr ckd2#,pi1#
if ckd2# > pi#*2 then decr ckd2#,pi#*2
if ckd2# < 0 then incr ckd2#,pi#*2
ckd3#=ckd2#-ckd1#

if mdat(1).skp=0 and mdat(1).g7 > 40 and abs(ckd3#) > 1.57 then
 fail$=" Cutter over compensation ERROR on"+ str$(lptr&-1)
end if

b2:
os2%=1
return
'-----------------------end cutter calc ----------------

l_end:

g1%=mdat(1).g1
Idout&=mdat(1).offI:Jdout&=mdat(1).offJ
xpout&=mdat(1).offx2:ypout&=mdat(1).offy2

zpout&=mdat(1).zpos:wpout&=mdat(1).wpos
xdout&=xpout&-pxp&:ydout&=ypout&-pyp&
zdout&=zpout&-pzp&:wdout&=wpout&-pwp&
xcp&=mdat(1).Xcp:ycp&=mdat(1).Ycp
gosub movecalc
return



'-----------------calculate line offset points ---------

function line2off (d%,mdat() as movedat) shared

local r1&,offx&,offy&,offs&,oxd&,oyd&,ozd&,owd&,cut_vec&
!cli
offs& = doffset#(mdat(d%).Dval) * expand& * mirrx% * mirry%
if mdat(d%).g7=40 then offs&=0
if mdat(d%).g7=42 then offs&=-offs&

if mdat(d%).g2=18 or mdat(d%).g2=19 then offs&=-offs& 'atest


oxd&=mdat(d%).xpos - pxpos&
oyd&=mdat(d%).ypos - pypos&
ozd&=mdat(d%).zpos - pzpos&
owd&=mdat(d%).wpos - pwpos&

'-------- calculate feed vectors for start and end directions
'cut_vec& = sqr(oxd&*oxd&+oyd&*oyd&+ozd&*ozd&+owd&*owd&)
'if cut_vec& >0 then
'mdat(d%).cdist = cut_vec&
'mdat(d%).fvx1=oxd&/cut_vec&
'mdat(d%).fvy1=oyd&/cut_vec&
'mdat(d%).fvz1=ozd&/cut_vec&
'mdat(d%).fvw1=owd&/cut_vec&
'end if

'mdat(d%).fvx2=mdat(d%).fvx1
'mdat(d%).fvy2=mdat(d%).fvy1
'mdat(d%).fvz2=mdat(d%).fvz1
'mdat(d%).fvw2=mdat(d%).fvw1
'----------------------------------
'mdat(d%).xdist=oxd&
'mdat(d%).ydist=oyd&
'mdat(d%).zdist=ozd&
'mdat(d%).wdist=owd&

r1&=sqr(oxd& * oxd& + oyd& * oyd&)
if r1&=0 then r1&=1
offx& = offs& * oxd& / r1&:offy& = offs& * oyd& / r1&
mdat(d%).offx1= pxpos& - offy&:mdat(d%).offy1 = pypos& + offx&
mdat(d%).offx2= mdat(d%).xpos - offy&:mdat(d%).offy2 = mdat(d%).ypos + offx&

!sti
end function

'-------------- calculate arc offset points -------------

function arc2off% (d%,mdat() as movedat) shared
local ip3&,jp3&,ie&,je&,rd1&,dx1&,dy1&,dx2&,dy2&,oset&,oxd&,oyd&,ozd&,owd&
!cli
oset& = doffset#(mdat(d%).Dval) * expand& * mirrx% * mirry%
if mdat(d%).g7=40 then oset&=0
if mdat(d%).g7=42 then oset&=-oset&
if mdat(d%).g1 =3 then oset&=-oset&

ip3&=mdat(d%).Ival:jp3&=mdat(d%).Jval
ie&=mdat(d%).xpos-mdat(d%).Xcp
je&=mdat(d%).ypos-mdat(d%).Ycp
rd1& = sqr(ip3& * ip3& + jp3& * jp3&)
if rd1&=0 then fail$="Invalid arc center on BLOCK "+ str$(lptr&+1):goto skpfnc1
rd2&=rd1& + oset&
dx1&=ip3&/rd1&*rd2&:dy1&=jp3&/rd1&*rd2&
dx2&=ie&/rd1&*rd2&:dy2&=je&/rd1&*rd2&
mdat(d%).offx1=mdat(d%).Xcp-dx1&
mdat(d%).offy1=mdat(d%).Ycp-dy1&
mdat(d%).offx2=mdat(d%).Xcp+dx2&
mdat(d%).offy2=mdat(d%).Ycp+dy2&
mdat(d%).offI=dx1&:mdat(d%).offJ=dy1&
mdat(d%).Roff=rd2&:mdat(d%).Rval=rd1&
if rd2& <=0 then mdat(d%).skp=1

skpfnc1:
!sti
end function



'----------------------- CALCULATE MOVES -----------------------------
'------------ needs xdist&(0),ydist&(0),zdist&(0),wdist&(0) ----------
'------------ g1% as G0-G3 and Ival#(0) as I and Jval#(0) as J ---
'------------ also needs previous xy point for finding arc centers ---
movecalc:
if m_ready% then gosub M_exc
if do_reset% then return '--- go back to do system reset!

		'-- calculate moves for buffer set number "x" and = 0 for now
!cli
i1&=Idout&
j1&=Jdout&

cdist&=sqr(xdout&*xdout& + ydout&*ydout& + zdout&*zdout& + wdout&*wdout&)

rad&=sqr(i1&*i1&+j1&*j1&)
nomove%=0
if cdist& =0 and rad& =0 then nomove%=1:goto skipcir
!sti

if xdout& < 0 then cdirx%=1 else cdirx%=-1
if ydout& < 0 then cdiry%=1 else cdiry%=-1
if zdout& < 0 then cdirz%=1 else cdirz%=-1
if wdout& < 0 then cdirw%=1 else cdirw%=-1

if g1% < 2 goto skipcir
!cli
'sang#=fnatn#(i1&* -1,j1&* -1)
sang#=fnatn#(-i1&,-j1&)
!sti
i2&=xpout&-xcp&
j2&=ypout&-ycp&
!cli
rad1&=sqr(i2&*i2&+j2&*j2&)
!sti
if rad&=0 or rad1&=0 or abs(rad1&-rad&) > errtol% then fail$="Invalid ARC END POINT on"+ str$(lptr&-1):goto skipcir
eang#=fnatn#(i2&,j2&)

if lastplane%=18 or lastplane%=19 then if g1%=3 then g1%=2 else g1%=3 'atest


if g1%=3 and eang# =< sang# then incr eang#,6.2831853
if g1%=2 and sang# =< eang# then incr sang#,6.2831853

!cli
rad&=sqr(i1&*i1&+j1&*j1&)/dv!
!sti
aseg#=eang#-sang#
circum&=(aseg#*rad&)
'-------------- Trying to correct for rotary axis  feedrate ----!!
!sti
gdir?=0 	'---- change direction of circle 0=cw 1=ccw
if g1% = 3 then gdir? = 1
cdirx%=1
cdiry%=1
cdir%=-1
i1&=i1&/dv!
j1&=j1&/dv!
i2&=i2&/dv!
j2&=j2&/dv!
if i1& <0 and j1& <0 then cdiry%=1:cdirx%=-1:cdir%=-1
if i1& <0 and j1& >0 then cdiry%=1:cdirx%=1:cdir%=1
if i1& >0 and j1& <0 then cdiry%=-1:cdirx%=-1:cdir%=1
if i1& >0 and j1& >0 then cdiry%=-1:cdirx%=1:cdir%=-1
if gdir?=1 then  cdirx%=-cdirx%:cdiry%=-cdiry%:cdir%=-cdir%
if i1&=0 and j1& >0 then cdir%=-1:cdirx%=1:cdiry%=-1:if gdir?=1 then cdirx%=-1
if i1&=0 and j1& <0 then cdir%=-1:cdiry%=1:cdirx%=-1:if gdir?=1 then cdirx%=1
if j1&=0 and i1& >0 then cdir%=1:cdirx%=-1:cdiry%=-1:if gdir?=1 then cdiry%=1
if j1&=0 and i1& <0 then cdir%=1:cdiry%=1:cdirx%=1:if gdir?=1 then cdiry%=-1
skipcir:

if fail$ <>"" then return

'----------------- set move direction and values --------------------
'---------- automatically writes to next move line after current ----

'------- write to next open move buffer line position, then inc ----

'--------- added to correct for rounding error -----------------
xdout&=xpout&/dv!-nxp&:incr nxp&,xdout&
ydout&=ypout&/dv!-nyp&:incr nyp&,ydout&
zdout&=zpout&/dv!-nzp&:incr nzp&,zdout&
wdout&=wpout&/dvw!-nwp&:incr nwp&,wdout&

'------------------------- test ---------
'cdist&=abs(xdout&)
'if abs(ydout&) > cdist& then cdist&=abs(ydout&)
'if abs(zdout&) > cdist& then cdist&=abs(zdout&)
'if abs(wdout&) > cdist& then cdist&=abs(wdout&)
'cdist&= circum& + sqr((xdout&*xdout&) + (ydout&*ydout&) + (zdout&*zdout&) + (wdout&*wdout&))
!cli
cdist&=sqr((circum&*circum&) + (xdout&*xdout&) + (ydout&*ydout&) + (zdout&*zdout&) + (wdout&*wdout&))
if cdist& > abs(circum&) then
circum& = cdist&
if abs(aseg#) > 0 then rad&=cdist&/abs(aseg#)
end if
!sti

'------Stop autorun if this move is M00 or M01  -------------------
if mdat(1).mg0 = 0 or mdat(1).mg0 = 1 then arun%=0

'----- ATTEMP auto feed control -----------------------

'if auto%=1 or auto%=0 then '--- sets values in single block for testing feed predicition
if auto%=1 then

'--------- if either current or next move are rapid then skip
'-------  vector calculations and use full deceleration values
if mdat(1).g1 > 0 and mdat(2).g1 > 0 then

'------ calculate linear ending vectors for current move ----
if cdist& > 0 then
end_fvecx!=xdout&/cdist&
end_fvecy!=ydout&/cdist&
end_fvecz!=zdout&/cdist&
end_fvecw!=wdout&/cdist&
'-----------------

'---- if this move is circular then get the ending vector calc based on
'---- I and J and modify the previously calculated linear xy vectors
!cli
if mdat(1).g1 > 1 then
hyp&=sqr(j1&*j1& + i1&*i1&)
!sti
end_fvecx!= j2&/hyp&
end_fvecy!= i2&/hyp&

if sgn(end_fvecx!) = sgn(end_fvecy!) then

if mdat(1).g1 =2 then
end_fvecy!= -end_fvecy!
else
end_fvecx!= -end_fvecx!
end if

else

if mdat(1).g1 = 3 then
end_fvecx!= -end_fvecx!
else
end_fvecy!= -end_fvecy!
end if
end if

if mdat(1).g2 =19 or mdat(1).g2 =18 then end_fvecx!=-end_fvecx!:end_fvecy!=-end_fvecy!
end if
end if

'--------- calculate vectors for start of NEXT move
'--- the next move vectors .fvx1 were already calculated for
'--- linear moves in line2off  function

oxdn&=mdat(2).offx2 - mdat(2).offx1
oydn&=mdat(2).offy2 - mdat(2).offy1
ozdn&=mdat(2).zpos - mdat(1).zpos
owdn&=mdat(2).wpos - mdat(1).wpos
!cli
next_cut_dist& = sqr(oxdn&*oxdn&+oydn&*oydn&+ozdn&*ozdn&+owdn&*owdn&)
!sti
mdat(2).cdist = next_cut_dist&

if next_cut_dist& > 0 then
next_gvecx!= oxdn& / next_cut_dist&
next_gvecy!= oydn& / next_cut_dist&
next_fvecz!= ozdn& / next_cut_dist&
next_fvecw!= owdn& / next_cut_dist&
next_fvecx!= next_gvecx!
next_fvecY!= next_gvecY!
end if

'------------------- if next move is circular then get the start vectors
if mdat(2).g1 >1 then
!cli
next_hyp&= sqr((mdat(2).offJ * mdat(2).offJ) + (mdat(2).offI * mdat(2).offI))
!sti
next_fvecx!= -mdat(2).offJ / next_hyp& 'mdat(2).Roff
next_fvecy!= -mdat(2).offI / next_hyp& 'mdat(2).Roff

if sgn(next_fvecx!) = sgn(next_fvecy!) then

if mdat(2).g1 =2 then
next_fvecy!= -next_fvecy!
else
next_fvecx!= -next_fvecx!
end if

else

if mdat(2).g1 = 3 then
next_fvecx!= -next_fvecx!
else
next_fvecy!= -next_fvecy!
end if
end if



if mdat(2).g2 =19 or mdat(2).g2 =18 then next_fvecx!=-next_fvecx!:next_fvecy!=-next_fvecy!

if abs(next_fvecx!-next_gvecx!) > .5 or abs(next_fvecy!-next_gvecy!) >.5 then
mdat(2).cdist& = mdat(2).Roff
end if
end if
'--- need to calc length of next circular move also, it could be short!



'------------ calc vector difference from next move
difx! = abs(end_fvecx! - next_fvecx!)
dify! = abs(end_fvecy! - next_fvecy!)
difz! = abs(end_fvecz! - next_fvecz!)
difw! = abs(end_fvecw! - next_fvecw!)

'new_f_mod!=difx!+dify!+difz!+difw!

new_f_mod!= difx!
if dify! > new_f_mod! then new_f_mod! = dify!
if difz! > new_f_mod! then new_f_mod! = difz!
if difw! > new_f_mod! then new_f_mod! = difw!

decl_mod! = 1 - abs(new_f_mod!) * corner_gain! '--- modifier bigger = slower corners
if decl_mod! < 0 then decl_mod! = 0


end if
'-------------------

if mdat(1).g1 =0 then
decl_mod!=0
end if

if mdat(2).skp = 1 then '-- if the next move is skiped then use full decl --
decl_mod!=0
end if

if mdat(2).g1 =0 then
decl_mod!=0
end if

if nomove% then
decl_mod!=0
end if

if mdat(2).look then
decl_mod!=0
end if

else

decl_mod!=0
end if

'----------------- print some results --------

'view text (47,17) - (80,17)
'color w6.cf,w6.cb

'print using "##.####"; new_f_mod!,
'print using "##.####"; decl_mod!,
'print using "##"; mdat(3).look,
'print using "##.####"; end_fvecy!;next_fvecy!,
'print using "#######"; next_cut_dist&,
'print using "#######"; mdat(2).Roff,

'------------------------------------------------------------------------
'os2%=1
'----------------------------------------------------------
!cli
mov_D% = mdat(os2%).Dval
mov_Ftmp! = mdat(os2%).Ftmp
mov_F! = mdat(os2%).Fval
mov_H% = mdat(os2%).Hval
mov_R& = mdat(os2%).Rval
mov_Q& = mdat(os2%).Qval
mov_P% = mdat(os2%).Pval
mov_S! = mdat(os2%).Sval
mov_T% = mdat(os2%).Tval
mov_L% = mdat(os2%).Lval
mov_sub%= mdat(os2%).Sublevel
mov_cdly% = mdat(os2%).candelay
mov_mg0% = mdat(os2%).mg0
mov_mg1% = mdat(os2%).mg1
mov_mg2% = mdat(os2%).mg2
mov_mg3% = mdat(os2%).mg3
mov_mg4% = mdat(os2%).mg4
mov_g0% = mdat(os2%).g0
mov_g1% = mdat(os2%).g1
mov_g2% = mdat(os2%).g2
mov_g3% = mdat(os2%).g3
mov_g5% = mdat(os2%).g5
mov_g6% = mdat(os2%).g6
mov_g7% = mdat(os2%).g7
mov_g8% = mdat(os2%).g8
mov_g9% = mdat(os2%).g9
mov_g10% = mdat(os2%).g10
mov_g13% = mdat(os2%).g13
mov_g14% = mdat(os2%).g14
mov_x92& = mdat(os2%).x92
mov_y92& = mdat(os2%).y92
mov_z92& = mdat(os2%).z92
mov_w92& = mdat(os2%).w92
mov_hx& = mdat(os2%).hx
mov_hy& = mdat(os2%).hy
mov_hz& = mdat(os2%).hz
mov_xp& = mdat(os2%).xpos
mov_yp& = mdat(os2%).ypos
mov_zp& = mdat(os2%).zpos
mov_wp& = mdat(os2%).wpos

mov_I& = mdat(os2%).Ival
mov_J& = mdat(os2%).Jval
mov_K& = mdat(os2%).Kval
mov_lnum& = mdat(os2%).lnum
next_dist& =mdat(2).cdist/dv!	'get the rought length of next move
mov_xdi&=xdout&
mov_ydi&=ydout&
mov_wdi&=wdout&
mov_zdi&=zdout&

mov_idi&=abs(i1&)
mov_jdi&=abs(j1&)
mov_rdi&=rad&
mov_cir&=abs(circum&)
mov_cd&=cdist&
mov_vec& = fcdist&
mov_xdr?=xdir?:mov_ydr?=ydir?
mov_zdr?=zdir?:mov_wdr?=wdir?
mov_xst?=xstep?:mov_yst?=ystep?
mov_zst?=zstep?:mov_wst?=wstep?
mov_gtp%=g1%:mov_cdr%=cdir%
mov_cdx%=cdirx%:mov_cdy%=cdiry%
mov_cdz%=cdirz%:mov_cdw%=cdirw%
'mov_acel!=acel_mod!
mov_decl!=decl_mod!
'mov_feed!=feed!
mov_rpd%=mdat(os2%).rpd
mov_nomove%=nomove%
'if mov_g0% = 31 then skipcut%=1 else skipcut%=0

'--- stop next move read ahead on some moves like M30 M00 G31
if mdat(os2%).hold_next = 1 then buffer1% = 0 else buffer1% = 1

pxp&=xpout&:pyp&=ypout&:pzp&=zpout&:pwp&=wpout&
xdout&=0:ydout&=0:zdout&=0:wdout&=0:circum&=0

'---- starts move engine running --- that's why it's last!

in_op%=1:next_op%=1
!sti

return

'----------------------------------------------------

'-------------------- calculates I and J from R --------
function rad2ij (stc3%, mdat() as movedat) shared
local xdst&,ydst&,d1#,d2#,rd11&,a1#,a2#,dx4&,dy4&,newI&,newJ&,pia#
shared pi1#,dv!
!cli
'---------------calculates I,J from Start,Fin, and Radius

rd11&=mdat(stc3%).Rval
radsign% = sgn(rd11&)
xdst&= (mdat(stc3%).xpos - pxpos&)
ydst&= (mdat(stc3%).ypos - pypos&)

if mdat(stc3%).g1 = 2 then pia#=-pi1# else pia# = pi1#
if mdat(stc3%).g2 = 19 then pia# = -pia# :rd11&=-rd11&
if mdat(stc3%).g2 = 18 then pia# = -pia# 'atest

'if mdat(st%).g2 = 18 or mdat(st%).g2 = 19 then pia# = -pia#

pia# = pia# * radsign%

d1#=sqr(xdst&*xdst&+ydst&*ydst&)/2

if abs(rd11&) < d1#  then
if abs(rd11&) + errtol% < d1# then
fail$="Radius to small to reach end point on"+ str$(lptr&-1):goto lst
else
rd11&= abs(d1#) * radsign%
end if
end if

d2#=sqr(abs((rd11&*rd11&)-(d1#*d1#)))
a1# = fnatn#(xdst&,ydst&)

a2#=a1#-pia#   '--------pia# + or - changes which side I,J, are on!
dy4&=sin(a2#)*d2#
dx4&=cos(a2#)*d2#
newI#=round((xdst&/2-dx4&)/expand&,4)
newJ#=round((ydst&/2-dy4&)/expand&,4)
mdat(stc3%).Ival= newI#*expand&
mdat(stc3%).Jval= newJ#*expand&
lst:
!sti
end function
'------------------------ end ----------- function ---------------

'----------------FIND INTERSECTION POINT OF TWO LINES-----------
'------line xi1&,yi1& to xi2&,yi2&   intersecs line xi3&,yi3& to xi4&,yi4&------'

function line2line (d%,e%,mdat() as movedat) shared

local xo&,yo&,dx1&,dx2&,dy1&,dy2&,vecx1#,vecx2#,vecy1#,vecy2#
local xi1&,yi1&,xi2&,yi2&,xi3&,yi3&,xi4&,yi4&,vx%,vy%

isecx&=0:isecy&=0:vx%=0:vy%=0
xo&=0:yo&=0:seg1%=0:seg2%=0:fail%=0
xi2&=mdat(d%).offx2 :xi1&=mdat(d%).offx1 :xi4&=mdat(e%).offx2 :xi3&=mdat(e%).offx1
yi2&=mdat(d%).offy2 :yi1&=mdat(d%).offy1 :yi4&=mdat(e%).offy2 :yi3&=mdat(e%).offy1

    dx1&=xi2&-xi1&:dx2&=xi4&-xi3&:dy1&=yi2&-yi1&:dy2&=yi4&-yi3&
    if dx1&=0 then vecx1#=0:xo&=xi1&:vx%=1 else vecx1#= (dy1&)/(dx1&)
    if dx2&=0 then vecx2#=0:xo&=xi3&:vx%=1 else vecx2#= (dy2&)/(dx2&)
    if vecx1#=0 and vecx2#=0 or vecx1#-vecx2#=0 then fail%=1 _
      else isecx&=((((xi1&-xi3&) * vecx2#)+(yi3&-yi1&))/(vecx1#-vecx2#))+xi1&

    if dy1&=0 then vecy1#=0:yo&=yi1&:vy%=1 else vecy1#= (dx1&)/(dy1&)
    if dy2&=0 then vecy2#=0:yo&=yi3&:vy%=1 else vecy2#= (dx2&)/(dy2&)
    if vecy1#=0 and vecy2#=0 or vecy1#-vecy2#=0 then fail%=1 _
     else isecy&=((((yi1&-yi3&) * vecy2#)+(xi3&-xi1&))/(vecy1#-vecy2#))+yi1&
    if vx%=1 then isecx&=xo&
    if vy%=1 then isecy&=yo&
    if xi1& > xi2& then swap xi1&,xi2&
    if yi1& > yi2& then swap yi1&,yi2&
    if xi3& > xi4& then swap xi3&,xi4&
    if yi3& > yi4& then swap yi3&,yi4&
    if isecx& >= xi1& and isecx& <= xi2& and isecy& >= yi1& and isecy& <= yi2& then seg1%=1
    if isecx& >= xi3& and isecx& <= xi4& and isecy& >= yi3& and isecy& <= yi4& then seg2%=1

end function


'------------------- CIRCLE TO CIRCLE INTERSECTION -----
'--- xpc&,ypc& =cent of current circle with rd16& as radius -----
'--- xpc1&,ypc1& = cent of second circle with rad1& as radius --
'----- to the first clockwise intersection, -rd16& = first ccw intersection
'--- d1# will always be -1 to 1, less or more = NO intersection!
'--- rounding d1# when close to -1 or 1 = tangent helps accuracy at tangents

function arc2arc (d%,e%,mdat() as movedat) shared

local hyp&,ang1#,ang2#,ang3#,d1#,dx&,dy&,rt&
local xdist1&,ydist1&,xpc&,ypc&,rd16&,xpc1&,ypc1&,rad1&

xpc&=mdat(d%).Xcp:ypc&=mdat(d%).Ycp:rd16&=mdat(d%).Roff
xpc1&=mdat(e%).Xcp:ypc1&=mdat(e%).Ycp:rad1&=mdat(e%).Roff

'---- if both centers are the same then mark failed ! ----------

if abs(mdat(d%).offx2-mdat(e%).offx1) < errtol% and abs(mdat(d%).offy2-mdat(e%).offy1)< errtol% then _
 	isecx&=mdat(e%).offx1:isecy&=mdat(e%).offy1:goto ierr1

if xpc&=xpc1& and ypc&=ypc1& then fail%=1:goto ierr1

!cli
chk1#=fnatn#(mdat(d%).offx1-mdat(e%).Xcp,mdat(d%).offy1-mdat(e%).Ycp)
chk2#=fnatn#(mdat(d%).offx2-mdat(e%).Xcp,mdat(d%).offy2-mdat(e%).Ycp)
if chk1# > chk2# then swap chk1#,chk2#


xdist1&=xpc1&-xpc&	'x distance between centers
ydist1&=ypc1&-ypc&	'y distance between centers
hyp&=sqr(xdist1&*xdist1&+ydist1&*ydist1&)
ang1#=fnatn#(xdist1&,ydist1&)
d1#=(hyp&*hyp&+rd16&*rd16&-rad1&*rad1&)/(2*hyp&*rd16&)

if 1-abs(d1#) < .000001 then d1#=round(d1#,5)
if d1# > 1 or d1# < -1 then fail%=1:fail$="No arc to arc intersection found! on"+ str$(lptr&-1):goto ierr1

'---------- find intersection points of both intersections! ----
ang2# = fnacos#(d1#)
ang3#=ang1#+ang2#
if d1#=-1 then ang3#=ang1#+pi#
nuxpoint1&=xpc&+cos(ang3#)* rd16&
nuypoint1&=ypc&+sin(ang3#)* rd16&
'---------------------- intersection 2 ----------------------
ang2# = fnacos#(-d1#)
ang3#=ang1#+ang2#
if d1#=-1 then ang3#=ang1#+pi#
nuxpoint2&=xpc&+cos(ang3#)* -rd16&
nuypoint2&=ypc&+sin(ang3#)* -rd16&
!sti
dx1&=mdat(d%).xpos-nuxpoint1&
dy1&=mdat(d%).ypos-nuypoint1&
dx2&=mdat(d%).xpos-nuxpoint2&
dy2&=mdat(d%).ypos-nuypoint2&
!cli
tdist1&= sqr(dx1&*dx1& + dy1&*dy1&)
tdist2&= sqr(dx2&*dx2& + dy2&*dy2&)
!sti
if tdist1& < tdist2& then isecx&=nuxpoint1&:isecy&=nuypoint1& else _
isecx&=nuxpoint2&:isecy&=nuypoint2&

ierr1:

end function



'--------------- LINE TO ARC INTERSECTION ---------------------
'------need line x,y - x,y point to circle cent x,y with rad R
function line2arc (d%,e%,mdat() as movedat) shared

xp1&=mdat(d%).offx1:yp1&=mdat(d%).offy1:xp2&=mdat(d%).offx2:yp2&=mdat(d%).offy2
xpc&=mdat(e%).Xcp:ypc&=mdat(e%).Ycp:rd12&=mdat(e%).Roff


local xdist1&,ydist1&,xdist2&,ydist2&
local d1#,d2#,d3#,ang1#,ang2#,ang3#,hyp#
seg1%=0:seg2%=0

'------ check if move is tangent and skip calculating if is -------
if abs(mdat(d%).offx2-mdat(e%).offx1) < errtol% and abs(mdat(d%).offy2-mdat(e%).offy1)< errtol%  then
isecx&=mdat(e%).offx1
isecy&=mdat(e%).offy1
goto ierr
end if


xdist1&=xp2&-xp1&
ydist1&=yp2&-yp1&
xdist2&=xpc&-xp1&
ydist2&=ypc&-yp1&
!cli
ang1#=fnatn#(xdist1&,ydist1&)
ang2#=fnatn#(xdist2&,ydist2&)
ang3#=ang1#-ang2#
hyp#=sqr(xdist2&*xdist2&+ydist2&*ydist2&)

d1#=sin(ang3#)*hyp#
d2#=cos(ang3#)*hyp#

if abs(d1#) > errtol%+abs(rd12&) then fail$="ERROR line to arc endpoint! on"+ str$(lptr&-1):goto ierr
if  abs(rd12&)-abs(d1#) < .000001 then d3#= abs(rd12&) else d3#=sqr(rd12&*rd12&-d1#*d1#)

midxp#=xpc&-sin(ang1#)*d1#
midyp#=ypc&+cos(ang1#)*d1#

ndx1#=cos(ang1#)*d3#
ndy1#=sin(ang1#)*d3#
!sti
nuxpoint1&=midxp#-ndx1#
nuypoint1&=midyp#-ndy1#
nuxpoint2&=midxp#+ndx1#
nuypoint2&=midyp#+ndy1#

dx1&=mdat(d%).xpos-nuxpoint1&
dy1&=mdat(d%).ypos-nuypoint1&
dx2&=mdat(d%).xpos-nuxpoint2&
dy2&=mdat(d%).ypos-nuypoint2&
!cli
tdist1&= sqr(dx1&*dx1& + dy1&*dy1&)
tdist2&= sqr(dx2&*dx2& + dy2&*dy2&)
!sti
if tdist2& < tdist1& then
isecx&=nuxpoint2&
isecy&=nuypoint2&
else
isecx&=nuxpoint1&
isecy&=nuypoint1&
end if

ierr:

end function


'--------------- ARC TO LINE INTERSECTION ---------------------
'------need line x,y - x,y point to circle cent x,y with rad R
function arc2line (e%,d%,mdat() as movedat) shared


xp1&=mdat(d%).offx1:yp1&=mdat(d%).offy1:xp2&=mdat(d%).offx2:yp2&=mdat(d%).offy2
xpc&=mdat(e%).Xcp:ypc&=mdat(e%).Ycp:rd17&=mdat(e%).Roff


local xdist1&,ydist1&,xdist2&,ydist2&
local d1#,d2#,d3#,ang1#,ang2#,ang3#,hyp#
seg1%=0:seg2%=0


'------ check if move is tangent and skip calculating if is -------
if abs(mdat(d%).offx2-mdat(e%).offx1) < errtol% and abs(mdat(d%).offy2-mdat(e%).offy1)< errtol%  then
isecx&=mdat(d%).offx2
isecy&=mdat(d%).offy2
goto ierr
end if



xdist1&=xp2&-xp1&
ydist1&=yp2&-yp1&
xdist2&=xpc&-xp1&
ydist2&=ypc&-yp1&
!cli
ang1#=fnatn#(xdist1&,ydist1&)
ang2#=fnatn#(xdist2&,ydist2&)
ang3#=ang1#-ang2#
hyp#=sqr(xdist2&*xdist2&+ydist2&*ydist2&)

d1#=sin(ang3#)*hyp#
d2#=cos(ang3#)*hyp#

if abs(d1#) > errtol%+abs(rd17&) then fail$="ERROR arc to line endpoint! on"+ str$(lptr&-1):goto jerr
if  abs(rd17&)-abs(d1#) < .000001 then d3#= abs(rd17&) else d3#=sqr(rd17&*rd17&-d1#*d1#)

midxp#=xpc&-sin(ang1#)*d1#
midyp#=ypc&+cos(ang1#)*d1#

ndx1#=cos(ang1#)*d3#
ndy1#=sin(ang1#)*d3#
!sti
nuxpoint1&=midxp#-ndx1#
nuypoint1&=midyp#-ndy1#
nuxpoint2&=midxp#+ndx1#
nuypoint2&=midyp#+ndy1#

dx1&=mdat(e%).xpos-nuxpoint1&
dy1&=mdat(e%).ypos-nuypoint1&
dx2&=mdat(e%).xpos-nuxpoint2&
dy2&=mdat(e%).ypos-nuypoint2&
!cli
tdist1&= sqr(dx1&*dx1& + dy1&*dy1&)
tdist2&= sqr(dx2&*dx2& + dy2&*dy2&)
!sti
if tdist1& < tdist2& then
isecx&=nuxpoint1&
isecy&=nuypoint1&
else
isecx&=nuxpoint2&
isecy&=nuypoint2&
end if

jerr:

end function


'------- returns the arc-cosine (angle in radians) of cosine tn# --
def fnacos# (tn#)

if tn#=1 then fnacos#=0:goto ed1
if tn#=-1 then fnacos#=3.141592653589795:goto ed1
!cli
fnacos#=-atn(tn#/sqr(1-tn#*tn#))+1.57079632#
!sti
ed1:
end def

def fnatn# (x#,y#)
'---------- unlike atn this returns the atn of full circle! ------
'---------- needs x distance and y distance of angle line --------
local pi#,t#
!cli
pi#=3.141592653589795
if x#=0 then if y# < 0 then t#=pi#*1.5 else t#=pi#/2
if x#<>0 then if x# < 0 then t#= atn(y#/x#)+pi# else _
  if y#<0 then t#=atn(y#/x#)+pi#*2 else t#=atn(y#/x#)
fnatn#=t#
!sti

end def


'------------ Read l$ and set values of x,y,z,w,i,j,g - ect -----------
'---- saves values in Xval#,Yval# ect, and Gval% to group gp0%-gp14% --
'-------- reads the string from struct mdat(x).fline then sets values --

function line_read#  (lct%,mdat() as movedat, wrk() as work_offset)  shared
public lbuf()

local n%,t%,t1%,l1$,l2$,l$

pxpos&=xpos&:pypos&=ypos&:pzpos&=zpos&:pwpos&=wpos&
l$ = mdat(lct%).fline
n%=1:l$=ucase$(l$)
while n% < len(l$)
t%=instr(n%,l$, ANY "/()ADFGHIJKLMNOPQRSTWXYZ")
incr n%
if t%= 0 goto lend
t1%=instr(n%,l$, ANY "/()ADFGHIJKLMNOPQRSTWXYZ")
if t1%=0 then t1%=len(l$)+1
l1$ = mid$(l$,t%,t1%-t%)
l2$ = left$(l1$,1)
if l2$ =")" then rmkskip%=0:lookahead%=1
if l2$ ="(" then rmkskip%=1:lookahead%=1
if t1% > n% then n%=t1%
if rmkskip% then goto remskip
if l2$ ="/" then goto lend
select case l2$
case "D":Dval%=val(mid$(l1$,2))
case "F":Fval!=val(mid$(l1$,2))/rdmetric!:if Fval! > maxfeed! then Fval! = maxfeed!
case "G":Gval%=val(mid$(l1$,2)):gosub setgval
case "H":Hval%=val(mid$(l1$,2)):gosub setlength
case "I":Ival&=val(mid$(l1$,2))/rdmetric!*expand&*mirrx%*iscale#:newmove%=1
case "J":Jval&=val(mid$(l1$,2))/rdmetric!*expand&*mirry%*jscale#:newmove%=1
case "K":if gp0%=33 then
	Ftmp!=val(mid$(l1$,2))
        else
	Kval&=val(mid$(l1$,2))/rdmetric!*expand&*kscale#:newmove%=1
        end if
case "L":if Lval%(sublevel%) > 0 then
	decr Lval%(sublevel%)
        else
        Lval%(sublevel%)=val(mid$(l1$,2))-1
        setl%=1
        if Lval%(sublevel%)<0 then Lval%(sublevel%)=0
        end if
case "M":Mval%=val(mid$(l1$,2)):lookahead% = 1:gosub setmval
case "N":Nval&=val(mid$(l1$,2))
case "O":Oval%(sublevel%)=val(mid$(l1$,2)):lookahead%=1
case "P":Pval%=val(mid$(l1$,2)):if Pval% < 0 then Pval%=0 else if Pval% >32000 then Pval%=32000
case "Q":Qval&=val(mid$(l1$,2))*expand&: mdat(lct%).Qval = Qval&
case "R":Rval&=val(mid$(l1$,2))*expand&/rdmetric!:gosub setr
case "S":Sval!=val(mid$(l1$,2)):if gsdrive% then
	if Sval! > maxs! then Sval! = maxs! else if Sval! < mins! then Sval!=mins!
        end if
case "T":Tval%=val(mid$(l1$,2))
        if lathe_bit% then
        Tval$=right$("0"+ str$(Tval%),4) ':tlen%=len$(Tval$)
        Tval%= val(mid$(Tval$,1,2))
	Hval%= val(mid$(Tval$,3,2)) :gosub setlength
        Dval%= Hval%
	'Dval%= val(mid$(Tval$,1,2))
        else
	if Tval% > maxtools% then
	Tval% = Maxtools%
	fail$=" TOOL NOT FOUND on "+ str$(lptr&+1)
	end if
        end if

case "X":Xval&=val(mid$(l1$,2))*expand&*mirrx%*xscale#:gosub setx:newmove%=1
case "Y":Yval&=val(mid$(l1$,2))*expand&*mirry%*yscale#:gosub sety:newmove%=1
case "Z":Zval&=val(mid$(l1$,2))*expand&*zscale#:gosub setz:newmove%=1
case "A":Wval&=val(mid$(l1$,2))*expandw&*wscale#:gosub setw:newmove%=1
end select
remskip:
wend
lend:


mdat(lct%).lnum = lptr& '-- need to set line number before setsub

if newmove%=0 then lookahead%=1
if newcancyc% then gosub fixcanz
if Mval%=98 then gosub setsub
if Mval%=99 then gosub setret
mdat(lct%).candelay = candelay%
mdat(lct%).xcmd=Xval&
mdat(lct%).ycmd=Yval&
mdat(lct%).zcmd=Zval&
mdat(lct%).wcmd=Wval&
mdat(lct%).hx = hxoffset&
mdat(lct%).hy = hyoffset&
mdat(lct%).hz = hzoffset&
mdat(lct%).mg0 = mg0%
mdat(lct%).mg1 = mg1%
mdat(lct%).mg2 = mg2%
mdat(lct%).mg3 = mg3%
mdat(lct%).look = lookahead%
mdat(lct%).Pval = Pval%
mdat(lct%).Canz=Canz&
mdat(lct%).Dval=Dval%
mdat(lct%).Fval=Fval!
mdat(lct%).Ftmp=Ftmp!
mdat(lct%).Hval=Hval%
mdat(lct%).Nval=Nval&
mdat(lct%).Oval=Oval%(sublevel%)
mdat(lct%).Rval = rpos&
mdat(lct%).Sval=Sval!
mdat(lct%).Tval=Tval%
if gp0% = 12 or gp0% = 13 then incr multimove% : gosub multicirc
mdat(lct%).g0=gp0%
mdat(lct%).g1=gp1%
mdat(lct%).g3=gp3%
mdat(lct%).g2=gp2%
mdat(lct%).g5=gp5%
mdat(lct%).g6=gp6%
mdat(lct%).g7=gp7%
mdat(lct%).g8=gp8%
mdat(lct%).g9=gp9%
mdat(lct%).g10=gp10%
mdat(lct%).g13=gp13%
mdat(lct%).g14=gp14%
mdat(lct%).x92=wrk(0).x
mdat(lct%).y92=wrk(0).y
mdat(lct%).z92=wrk(0).z
mdat(lct%).w92=wrk(0).w
if mdat(lct%).g1 = 0 then mdat(lct%).rpd=1 else mdat(lct%).rpd=0

if gp2%=17 then mdat(lct%).xpos=xpos&:mdat(lct%).ypos=ypos&:mdat(lct%).zpos=zpos&:mdat(lct%).wpos=wpos&
if gp2%=18 then mdat(lct%).xpos=xpos&:mdat(lct%).ypos=zpos&:mdat(lct%).zpos=ypos&:mdat(lct%).wpos=wpos&:swap pypos&,pzpos&
if gp2%=19 then mdat(lct%).xpos=zpos&:mdat(lct%).ypos=ypos&:mdat(lct%).zpos=xpos&:mdat(lct%).wpos=wpos&:swap pxpos&,pzpos&
if gp2%=18 then Jval& = Kval&
if gp2%=19 then Ival& = Kval&
mdat(lct%).Ival = Ival&
mdat(lct%).Jval = Jval&
mdat(lct%).Kval = Kval&
mdat(lct%).new = newmove%

'remember--------------------
if sublevel% and Lval%(sublevel%)=0 then
mdat(lct%).Lval = Lval%(sublevel%-1)
mdat(lct%).Sublevel = sublevel%-1
else
mdat(lct%).Lval = Lval%(sublevel%)
end if
mdat(lct%).Sublevel = sublevel%


if gp1% > 1 and Rval& <> 0 then call rad2ij (lct%, mdat())
mdat(lct%).Xcp= pxpos& + mdat(lct%).Ival
mdat(lct%).Ycp= pypos& + mdat(lct%).Jval
mdat(lct%).skp=0
if lct%=3 then if gp1% > 1 then call arc2off (lct%,mdat()) else call line2off (lct%,mdat())
Ival&=0:Jval&=0:Kval&=0:Rval&=0:Mval%=0
Pval%=0  '------must not be cleared or the cancycle delay don't work

newmove%=0:planechg%=0:newcancyc%=0

mg0%=-1:mg1%=0:mg2%=0:mg3%=0:mg4%=0
lookahead%=0
setl%=0
gp0%=0

exit function

'------------- G12 and G13 multi move circle ----
multicirc:
if gp2%=17 then
select case multimove%
case = 1
        gp1%=1
        circrep%=Pval%
	incr xpos&,Ival&
	incr ypos&,Jval&
case = 2
        if gp0%=12 then gp1%=2 else gp1%=3
        if circrep% > 1 then decr circrep% : decr multimove%
        Ival& = -Ival&
        Jval& = -Jval&
	if ztoo% then gosub setz1
        if wtoo% then gosub setw1
	xtoo%=0:ytoo%=0:ztoo%=0:wtoo%=0
case = 3
        gp1%=0 : gp7%=40 : multimove%=0
        decr xpos&,Ival&
        decr ypos&,Jval&
end select
end if

if gp2%=18 then
select case multimove%
case = 1
        gp1%=1
        circrep%=Pval%
	incr xpos&,Ival&
	incr zpos&,Kval&
case = 2
        if gp0%=12 then gp1%=2 else gp1%=3
        if circrep% > 1 then decr circrep% : decr multimove%
        Ival& = -Ival&
        Kval& = -Kval&
	if ytoo% then gosub sety1
        if wtoo% then gosub setw1
	xtoo%=0:ytoo%=0:ztoo%=0:wtoo%=0
case = 3
        gp1%=0 : gp7%=40 : multimove%=0
        decr xpos&,Ival&
        decr zpos&,Kval&
end select
end if

if gp2%=19 then
select case multimove%
case = 1
        gp1%=1
        circrep%=Pval%
	incr ypos&,Jval&
	incr zpos&,Kval&
case = 2
        if gp0%=12 then gp1%=2 else gp1%=3
        if circrep% > 1 then decr circrep% : decr multimove%
        Jval& = -Jval&
        Kval& = -Kval&
	if xtoo% then gosub setx1
        if wtoo% then gosub setw1
	xtoo%=0:ytoo%=0:ztoo%=0:wtoo%=0
case = 3
        gp1%=0 : gp7%=40 : multimove%=0
        decr ypos&,Jval&
        decr zpos&,Kval&
end select
end if

return


'-------------- set call to sub -----
setsub:
sublptr&(sublevel%)=lptr&
incr sublevel%
if sublevel% > 10 then fail$="ERROR exceeded 10 sub levels!"
srcho$="O"+mid$(str$(Pval%),2)
t&=1
while t& < numl&
i% = instr (lbuf(t&) , srcho$)
if i% > 0 then	sublptr&(sublevel%) = t& :lptr&=t&-1: return
incr t&
wend
fail$ = "ERROR SUB "+srcho$+" not found on"+ str$(lptr&+1)
arun%=0
return

'------------- set return from sub ----
setret:
if setl% then
fail$="ERROR,SUB RETURN WITH REPEAT on block"+str$(lptr&+1)
arun%=0
return
end if
nuptr&=lptr&
decr sublevel%

if sublevel% < 0 then
sublevel% = 0
lptr&=0
else
lptr&=sublptr&(sublevel%)
end if

if Pval% > 0 then
srchn$="N"+mid$(str$(Pval%),2)
t&=lptr&
while t& < numl&
i% = instr (lbuf(t&) , srchn$)
if i% > 0 then	sublptr&(sublevel%) = t& :lptr&=t&-1: return
incr t&
wend
t&=0
while t& < lptr&
if i% > 0 then	sublptr&(sublevel%) = t& :lptr&=t&-1: return
incr t&
wend
fail$ = "ERROR "+srchn$+" not found on"+ str$(nuptr&+1):lptr&=n&
arun%=0
end if
return

'---------------- M set-------
setmval:
if Mval% < 3 or Mval% = 30 then mg0% = Mval% : mdat(lct%).hold_next = 1 'stops next line read ahead
if Mval% > 2 and Mval% < 6 then mg1% = Mval%
if Mval% = 6 then mg2% = Mval% ': mdat(lct%).hold_next = 1
if Mval% > 6 and Mval% < 15 then mg3% = Mval%
if Mval% > 30 and Mval% < 100 then mg0% = Mval%
return

setr:
rpos& = Rval& '* xyscale#  (removed xyscale for lathe operation)
if gp9% <> 80 then
if gp2%=17 then
if gp3%=90 then newcancyc%=1:rpos& = Rval& * zscale# + (hzoffset& + wrk(0).z + wrk(gp14%-53).z) * dv!
if gp3%=91 then newcancyc%=1:rpos& = zpos& + Rval& * zscale#
end if

if gp2%=18 then
if gp3%=90 then newcancyc%=1:rpos& = Rval& * xscale# + (hyoffset& + wrk(0).y + wrk(gp14%-53).y) * dv!
if gp3%=91 then newcancyc%=1:rpos& = ypos& + Rval& * xyscale#
end if

if gp2%=19 then
if gp3%=90 then newcancyc%=1:rpos& = Rval& * yscale# + (hxoffset& + wrk(0).x + wrk(gp14%-53).x) * dv!
if gp3%=91 then newcancyc%=1:rpos& = xpos& + Rval& * xyscale#
end if
end if
return

setcycdefault:
canzinc%=0
if gp2%=17 then rpos& = zpos&:cycz& = zpos&
if gp2%=18 then rpos& = ypos&:cycz& = ypos&
if gp2%=19 then rpos& = xpos&:cycz& = xpos&
newcancyc%=1
if planechg%=1 then fail$="PLANE CHANGE WHILE IN CANNED CYCLE on"+str$(lptr&+1)
return

fixcanz:
candelay%=Pval%
if canzinc% then
Canz& = rpos& + cycz&
else
Canz& = cycz&
end if
return

setx:
if gp0%=28 then
if zerox%=1 then xpos&=0 else xpos&=pxpos&
return
end if
if gp0%=12 or gp0%=13 then xtoo%=1:return 'do not set x value on G12 or G13
if gp0%=92 then wrk(0).x = (xpos& - Xval&/rdmetric!)/dv! - wrk(gp14%-53).x : return

if gp2%=19 and gp9% <> 80 then
if gp3%=90 then canzinc%=0:cycz& = Xval&/rdmetric! + (hxoffset& + wrk(0).x + wrk(gp14%-53).x) * dv!
if gp3%=91 then canzinc%=1:cycz& = Xval&/rdmetric!
newcancyc%=1
return
end if

setx1:
if gp3%=91 then incr xpos&,Xval&/rdmetric!
if gp3%=90 then xpos& = Xval&/rdmetric! + (hxoffset& + wrk(0).x + wrk(gp14%-53).x) * dv!

return

sety:
if gp0%=28 then
if zeroy%=1 then ypos&=0 else ypos&=pypos&
return
end if
if gp0%=12 or gp0%=13 then ytoo%=1:return 'do not set x value on G12 or G13
if gp0%=92 then wrk(0).y = (ypos& - Yval&/rdmetric!)/dv! - wrk(gp14%-53).y : return

if gp2%=18 and gp9% <> 80 then
if gp3%=90 then canzinc%=0:cycz& = Yval&/rdmetric! + ((hyoffset& + wrk(0).y + wrk(gp14%-53).y)* yscale#) * dv!
if gp3%=91 then canzinc%=1:cycz& = Yval&/rdmetric!
newcancyc%=1
return
end if

sety1:
if gp3%=91 then incr ypos&,Yval&/rdmetric!
if gp3%=90 then ypos& = Yval&/rdmetric! + ((hyoffset& + wrk(0).y + wrk(gp14%-53).y)* yscale#) * dv!

return

setz:
if gp0%=28 then
if zeroz%=1 then zpos&=0 else zpos&=pzpos&
return
end if
if gp0%=12 or gp0%=13 then ztoo%=1:return 'do not set x value on G12 or G13
if gp0%=92 then wrk(0).z = (zpos& - Zval&/rdmetric!)/dv! - wrk(gp14%-53).z : return

if gp2%=17 and gp9% <> 80 then
if gp3%=90 then canzinc%=0:cycz& = Zval&/rdmetric! + ((hzoffset& + wrk(0).z + wrk(gp14%-53).z)* zscale#) * dv!
if gp3%=91 then canzinc%=1:cycz& = Zval&/rdmetric!
newcancyc%=1
return
end if

setz1:
if gp3%=91 then incr zpos&,Zval&/rdmetric!
if gp3%=90 then zpos& = Zval&/rdmetric! + ((hzoffset& + wrk(0).z + wrk(gp14%-53).z)* zscale#) * dv!

return

setw:
if gp0%=28 then
if zerow%=1 then wpos&=0 else wpos& = pwpos&
return
end if
if gp0%=12 or gp0%=13 then wtoo%=1:return 'do not set x value on G12 or G13
setw1:

if rotarya% then
if gp0%=92 then
wrk(0).w = (wpos& - Wval&)/dvw! - wrk(gp14%-53).w
return
end if

if gp3%=90 then
wpos& = Wval& + ((wrk(0).w + wrk(gp14%-53).w)* Wscale#) * dvw!
while wpos& > Wmax&:decr wpos&,Wmax&:wend
while wpos& < -Wmax&:incr wpos&,Wmax&:wend
while pwpos& - wpos& < -180*expandw& :decr wpos&,(360*expandw&):wend
while pwpos& - wpos& > 180*expandw& :incr wpos&,(360*expandw&):wend
end if

if gp3%=91 then
incr wpos&,Wval&
end if

else
if gp0%=92 then wrk(0).w = (wpos& - Wval&/rdmetric!)/dvw! - wrk(gp14%-53).w : return
if gp3%=91 then incr wpos&,Wval&/rdmetric!
if gp3%=90 then wpos& = Wval&/rdmetric! + ((wrk(0).w + wrk(gp14%-53).w)* wscale#) * dvw!
end if
return

setgval:
 '---- stop line read ahead on all but G1-G3 moves
'if Gval%=0 or Gval% > 3 then lookahead% = 1

if Gval% < 4 then
gp1%= Gval%
if gp9% <> 80 then gp9%=80 ':gosub setcycdefault
if Gval% > 1 and (mirrx%=-1 or mirry%=-1) then gosub mirrorg
end if


if Gval% > 39 and Gval% < 43 then gp7% = Gval%
if Gval% > 3 and Gval% < 14 then gp0% = Gval% ': mdat(lct%).g0=gp0%

if Gval% > 16 and Gval% < 20 then
if gp2% <> Gval% then
planechg%=1
if newcancyc% or gp9% <> 80 then fail$="PLANE CHANGED WHILE IN CANNED CYCLE on"+str$(lptr&+1)
gp2%=Gval%
end if
end if

if Gval% =20 then gp6%=Gval% : rdmetric!=1
if Gval% =21 then gp6%=Gval% : rdmetric!=25.4

if Gval% > 26 and Gval% < 34 then
gp0% = Gval%
mdat(lct%).g0=gp0%
if gp0%=28 then gp9%=80:gp1%=0 : mdat(lct%).hold_next = 1
if gp0%=31 then gp9%=80:gp1%=1 : mdat(lct%).hold_next = 1
end if

if Gval% = 43 or Gval%=44 or Gval%=49 then gp8% = Gval%:gosub setlength
if Gval% > 52 and Gval% < 60 then gp14% = Gval%
if Gval% = 60 then gp0% = Gval% : mdat(lct%).g0=gp0%
if Gval% = 73 or Gval% = 74 then gp9% = Gval% : gp1%=0:gosub setcycdefault
if Gval% > 79 and Gval% < 90 then gp9% = Gval% : gp1%=0:gosub setcycdefault
if Gval% = 90 or Gval% = 91 then gp3% = Gval%
if Gval% = 92 then gp0% = Gval% : mdat(lct%).g0=gp0% : mdat(lct%).hold_next = 1

if Gval% = 94 then gp5%=Gval%	'set feed to IPM
if Gval% = 95 then gp5%=Gval%	'set feed to IPR
if Gval% > 95 and Gval% < 98 then gp13% = Gval%  'G96 RPM is SFM. G97 RPM=constant
if Gval% > 97 and Gval% < 100 then gp10% = Gval% 'G98 G99 Gauge height return
gend:
return

setlength:
if gp8%=49 then hxoffset&=0:hyoffset&=0:hzoffset&=0

if lathe_bit% then
if gp8%=43 then hzoffset&=z_offset&(Hval%)*zscale#:hxoffset&=x_offset&(Hval%)*xscale#
if gp8%=44 then hzoffset&=-z_offset&(Hval%)*zscale#:hxoffset&=-x_offset&(Hval%)*xscale#
else
if gp2%=17 and gp8%=43 then hzoffset&=z_offset&(Hval%)
if gp2%=17 and gp8%=44 then hzoffset&=-z_offset&(Hval%)
if gp2%=18 and gp8%=43 then hyoffset&=z_offset&(Hval%)
if gp2%=18 and gp8%=44 then hyoffset&=-z_offset&(Hval%)
if gp2%=19 and gp8%=43 then hxoffset&=z_offset&(Hval%)
if gp2%=19 and gp8%=44 then hxoffset&=-z_offset&(Hval%)
end if
return

'setlength:
'if gp8%=49 then hxoffset&=0:hyoffset&=0:hzoffset&=0
'if gp2%=17 and gp8%=43 then hzoffset&=z_offset&(Hval%)
'if gp2%=17 and gp8%=44 then hzoffset&=-z_offset&(Hval%)
'if gp2%=18 and gp8%=43 then hyoffset&=z_offset&(Hval%)
'if gp2%=18 and gp8%=44 then hyoffset&=-z_offset&(Hval%)
'if gp2%=19 and gp8%=43 then hxoffset&=z_offset&(Hval%)
'if gp2%=19 and gp8%=44 then hxoffset&=-z_offset&(Hval%)
'return



mirrorg:
if mirrx%=-1 then if gp1% = 2 then gp1% = 3 else gp1% = 2
if mirry%=-1 then if gp1% = 2 then gp1% = 3 else gp1% = 2
return
end function


'-------------------------- end read line function --------------

setdirbits: 	'---- set plane direction bits xy yz xy

if lastplane%=17 and newplane%=18 then swap ydir?,zdir?:swap ystep?,zstep?:swap mdat(0).ypos,mdat(0).zpos:swap nyp&,nzp&:swap pyp&,pzp&
if lastplane%=18 and newplane%=17 then swap ydir?,zdir?:swap ystep?,zstep?:swap mdat(0).ypos,mdat(0).zpos:swap nyp&,nzp&:swap pyp&,pzp&
if lastplane%=17 and newplane%=19 then swap xdir?,zdir?:swap xstep?,zstep?:swap mdat(0).xpos,mdat(0).zpos:swap nxp&,nzp&:swap pxp&,pzp&
if lastplane%=19 and newplane%=17 then swap xdir?,zdir?:swap xstep?,zstep?:swap mdat(0).xpos,mdat(0).zpos:swap nxp&,nzp&:swap pxp&,pzp&

if lastplane%=18 and newplane%=19 then swap ydir?,zdir?:swap ystep?,zstep?:swap mdat(0).ypos,mdat(0).zpos _
:swap nyp&,nzp&:swap pyp&,pzp&:swap xdir?,zdir?:swap xstep?,zstep?:swap mdat(0).xpos,mdat(0).zpos:swap nxp&,nzp&:swap pxp&,pzp&

if lastplane%=19 and newplane%=18 then swap xdir?,zdir?:swap xstep?,zstep?:swap mdat(0).xpos,mdat(0).zpos _
:swap nxp&,nzp&:swap pxp&,pzp&:swap ydir?,zdir?:swap ystep?,zstep?:swap mdat(0).ypos,mdat(0).zpos:swap nyp&,nzp&:swap pyp&,pzp&
lastplane% = newplane%
return

'----------- start keyboard filter ISR! ------------------
str_key_filter:
'---------- get keyboard int address and fix jump location ---------
jp_loc_ptr = codeptr32 (jmpfix) +1

reg 1, &h3509
call interrupt (&h21)
s1??=reg (9):s2??=reg (2)
oldint??? = s1??*65536 + s2??
@jp_loc_ptr = oldint???

'------------- get address for new int location and set interrupt-------
cs??=codeseg (nonsub)
pt??=codeptr (nonsub)
'print "new interrupt " cs??,pt??
reg 1,&h2509
reg 8,cs??:reg 4,pt??
call interrupt (&h21)
return

'-------------- stop keyboard filter ISR!--------------
filter_int_reset:
'------------- reset the int address back to original location ----------
reg 1,&h2509
reg 8,s1??:reg 4,s2??
call interrupt (&h21)
return

'--sub---------- read real time clock, just for fun ----
function realtime$
shared ts%,tm%,th24%
static th$,tm$,ts$,lastts?
!cli
out &h70,&hA
rdclk?=inp(&h71)	'read status and see if time update is in progress
!sti
if (rdclk? and &h80) then goto skiprtf  'skip read if updating time

!cli
out &h70,0
ts?=inp(&h71) 	'get seconds
!sti
if ts? = lastts? then goto skiprtf
lastts?=ts?

!cli
out &h70,2
tm?=inp(&h71) 'get minutes
out &h70,4
th?=inp(&h71) 'get hours
!sti
ts1?=ts? and &hf0
ts2?=ts? and &h0f
rotate right ts1?,4
ts%=ts1?*10+ts2?
ts$=str$(ts%)
ts$=right$("0"+right$(ts$,len(ts$)-1),2)
'!cli
'out &h70,2
'tm?=inp(&h71)
'!sti
tm1?=tm? and &hf0
tm2?=tm? and &h0f
rotate right tm1?,4
tm%=tm1?*10+tm2?
tm$=str$(tm%)
tm$=right$("0"+right$(tm$,len(tm$)-1),2)
'!cli
'out &h70,4
'th?=inp(&h71)
'!sti
th1?=th? and &hf0
th2?=th? and &h0f
rotate right th1?,4
th%=th1?*10+th2?
th24%=th%
if th%=0 then th%=12
if th% > 12 then decr th%,12
th$=str$(th%)
th$=right$("  "+right$(th$,len(th$)-1),2)
'out &h70,6
'td=inp(&h71)
'out &h70,8
'tmo=inp(&h71)

skiprtf:
realtime$= th$+":"+tm$+":"+ts$
end function

fixtime:
t$= realtim$
th24%=th24%*256+tm%
reg 3,th24%
reg 4,ts%*256
reg 1,&h2d00
call interrupt &h21

return



resetirq:
'-------- reset the clock speed ------
out &h43,&h3c
out &h40,&hff
out &h40,&hff
'----------- reset timer interrupt vector back to old vector -----
!push ax
!push dx
!push ds
!mov ax,ip1 ; segment
!mov dx,ip2 ; pointer
!mov ds,ax
!mov ax,&h2508
!int &h21
!pop ds
!pop dx
!pop ax
return




setirq:
ipseg = codeseg(testsub)
ipptr = codeptr(testsub)

!push dx
!push ds
!push di
!mov ax,&h3508   ;get pic timer interrupt 0 or dos int8 vector and save it
!int &h21
!mov ax,es
!mov ip1,ax
'!mov rtc4,ax	;save so can be ran from new timer service routine
!mov ip2,bx
'!mov rtc3,bx	;save for new timer routine - run every 55 msec
!mov ax,ipptr
!mov dx,ax
!mov ax,ipseg           ;set the new vector
!mov ds,ax
!mov ax,&h2508
!int &h21
!pop di
!pop ds
!pop dx
return

'------------------ Handle ISR, inc/dec's hdat%
handsub:

!pushf
!push ax
!push bx
!push cx
!push dx
!push bp
!push sp
!push ss
!push di
!push ds
!push es

!mov ax,word hp
!mov ds,ax

              	'read lptx status port
!mov dx,statusport2% 	;lptx
!in al,dx
!xor al,&h80	;corrects inverted bit 7
!mov ah,al
!and al,&h88
!jz nd
!mov al,ah
!and al,&h30	;'encoder bits are 4 & 5, so mask out others

!mov ah,al	;perform task to align output in binary order
!shr ah,1
!xor al,ah
!and al,&h30

!mov bl, byte hy		;get last value read
!cmp al,bl		;test if input has changed and skips if not changed
!jz nd                  ;nothing changed so skip everything
!mov byte hy,al		;save new value to last value
!jc minus               ;check direction

!cmp al,&h30		;check for center transition
!jnz up			;for count down
!cmp bl,&h00		;otherwise count up
!jz dwn
!jmp up

minus:
!cmp al,&h00		;check for center transition
!jnz dwn		;for count up
!cmp bl,&h30           	;otherwise count down
!jz up

dwn:
incr hdat%                'incr the handle pulse counter up
!jmp nd

up:
decr hdat%                'dec the handle pulse counter down
nd:
		'clear the interrupt on the pic
!mov al,&h20
!out &h20,al

!pop es
!pop ds
!pop di
!pop ss
!pop sp
!pop bp
!pop dx
!pop cx
!pop bx
!pop ax
!popf
!iret
hp:
!dw 0
!dw 0
hy:
!db 0
!dw 0

'---------------- keyboard FILTER ISR! ------------
nonsub:
!push ax
!in al,&h60
!cmp al,58
!je trash_it
!cmp al,69
!je trash_it
!pop ax
jmpfix:
!jmp far nonsub
trash_it:
!in al,&h61
!mov ah,al
!or al,&h80
!out &h61,al
!xchg ah,al
!out &h61,al
!mov al,&h20
!out &h20,al
!pop ax
!iret

'--------------- keyboard interrupt ISR! --------
keysub:
'!cli
!pushf
!push ax
!push bx
!push cx
!push dx
!push bp
!push sp
!push ss
!push di
!push ds
!push es
!mov ax,word kak
!mov ds,ax
!in al,&h60
!xor ah,ah
!mov kt0%,ax
!in al,&h61
!mov ah,al
!or al,&h80
!out &h61,al
!xchg ah,al
!out &h61,al
!mov al,&h20
!out &h20,al
!mov cx,kt1??
!mov es,cx
!mov di,kt2??

!mov ax,kt0%		;check for shift key
!cmp al,42		;and process as usual if shift
!jz shiftkey
!cmp al,170
!jz shiftkey

!mov ax,joglock%	;skip saving the bit if joglock is on
!cmp al,0		;might want to skip only if one of the jog keys
!jnz skey		;71 to 83 instead of passing only shift key 42

!mov ax,kt0%
shiftkey:
!xor ah,ah
!cmp al,102
!jnc hkey
!mov bp,ax
!mov al,1
!jmp lkey
hkey:
!sub al,128
!cmp al,102
!jnc skey
!mov bp,ax
!xor al,al
lkey:
!dec bp
!mov es:[di+bp],al
skey:
'!sti
!pop es
!pop ds
!pop di
!pop ss
!pop sp
!pop bp
!pop dx
!pop cx
!pop bx
!pop ax
!popf
!iret
kak:
!dw 0
!dw 0
!dw 0

'------------------------- rtc timmer interrupt sub --------------
'---------timer variables ----
rtc:
!dw 0
!dw 0
rtc2:
!dd 0
!dd 0
rtc3:
!dd 0
rtc4:
!dd 0
!dd 0
rtc5:
!dd 0

rt_int:
!pushf
!push ax
!push bx
!push cx
!push dx
!push bp
!push sp
!push ss
!push di
!push ds
!push es
!mov ax,word rtc
!mov ds,ax
'----------------------- if you call the old dos interrupt then ----
'-------------------- you don't need to reset the pic or the rtc ---
'!mov al,rtc5
'!dec al
'!jnz skp_dos
'!mov al,205
'!mov rtc5,al
'!mov ax,ip1
'!mov rtc4,ax
'!mov ax,ip2
'!mov rtc3,ax
'!pushf
'!call dword rtc3
'------------ jmp to dos clock update ISR else the dos clock will stop!---
'------------ the old dos ISR vector is copied to rtc2: ---------
skp_dos:
!mov al,&hc
!out &h70,al  ;test status register c for source of interrupt
!in al,&h71	;must read in twice or rtc interrupt chip may not be reset!
!in al,&h71
'----- divide by 8 counter for the 1msc timers -----
!mov al,lrtc2?  ;read the loop counter
!inc al       ;inc counter
!mov lrtc2?,al  ;save counter back
!cmp al,8     ;test for 8 times thru
!jc skp_tmr  ;inc timers only after 8 times thru otherwise skip
!xor al,al
!mov lrtc2?,al  ;reset the loop counter rtc2 back to zero
incr tmr1??
incr tmr2??
incr tmr5??
incr tmr6??
incr comtmr1??

'--- WATCHDOG timer output at 4khz cycle ------------
skp_tmr:
!mov al,watchdog?    ; check if watchdog is turned on
!or al,al
!jz nodog            ; skip if not on
!mov dx,controlport2%  ;toggle watchdog bit at 8khz = 4khz out
!in al,dx
!xor al,&h8
!out dx,al
nodog:

chkrpm:
'-------RPM -------------------------------
!add tmr3??,1     ;inc tmr3??
!jnc notlimit	  ;check if over counted back to zero and reset back if did
!dec tmr3??       ;set back to max count of 65535 not to exceed 65536
notlimit:

!mov dx,statusport% ;look for spindle rotation on limitw input
!in al,dx
!xor al,&h80
!and al,limitw?
!jnz readhigh       ;jump if limit input is high and save count
!mov tmr3low??,1     ;remember that a logic low did occur
!jmp rpm_end

'------------- when high mark found do this ---------
readhigh:
!mov ax,tmr3low??	;test tmr3low?? if there was first a low level on limitw input
!cmp ax,0
!jz rpm_end             ;and skip if there was not a low on limitw yet
!inc revcnt??
!mov ax,rpmcntr??      	;get last count and subtract new count
!sub ax,tmr3??          ;else move the new count in tmr3?? to rpmcntr??
!jnc t3lower		;if carry then do neg to get the abs value
!neg ax

t3lower:
!cmp ax,3		;compare to 3 to see if the change was bigger than 2
!mov ax,tmr3??		;mov tmr3?? back into ax
!jc ave_cnt		;if carry then change is small so jump and average
!mov rpmcntr??,ax  	;other wise save the value as the count time
!mov tmr4??,ax
!jmp clrtmr

ave_cnt:
!add ax,rpmcntr??	;average the count with previous count
!rcr ax,1		;divide in half
!add ax,tmr4??		;add again
!rcr ax,1		;divide again
!mov rpmcntr??,ax	;save the count
!mov tmr4??,ax
clrtmr:
!mov tmr3??,0         ;then clear tmr3??
!mov tmr3low??,0       ;and clear tmr3low??

rpm_end:

'--------------- PWM output 5hz signal ------------
!mov al,pwm?    ;PWM counter base frequency  8192 / 1638 = 5hz
!or al,al       ; If PWM is ON (not zero) then run pwm update else skip it
!jz pwm_end
!mov dx,controlport%  ;toggle PWM bit at 8khz = 4khz out
!mov bx,pwmcount%
!mov cx,pwmmax%
!inc bx
!cmp cx,bx
!jnc keepcount
!mov bx,0
keepcount:
!in al,dx
!mov pwmcount%,bx
!mov cx,pwmpos%
!mov ah,pwmphase?	;get pwmphase? to test for zero or one logic
!cmp cx,bx
!jc onebit	;use jc for positive pulse width or jnc for negitive
!or ah,ah               ;test pwm phase for one or zero
!jz pwm_end             ;skip if zero already
!and al,&hf7            ;otherwise set output bit low
!mov ah,0
!mov pwmphase?,ah        ;set phase to low
!jmp setout    		;go set output to low

onebit:
!or ah,ah               ;test pwm phase for one or zero
!jnz pwm_end             ;skip if one already
!or al,&h8            ;otherwise set output bit one
!mov ah,1
!mov pwmphase?,ah        ;set phase to one
setout:
!out dx,al
pwm_end:


skp_inc:
!mov al,&h20	;clear interrupts on PIC 1 and PIC 2
!out &ha0,al
!out &h20,al
skip_all:
!pop es
!pop ds
!pop di
!pop ss
!pop sp
!pop bp
!pop dx
!pop cx
!pop bx
!pop ax
!popf
!iret


'--------------------------- main move engine timmer interrupt sub -----
p:
!dw 0
!dw 0
!dw 0
!dw 0

testsub:
!pushf
!push bp
!push ax
!push bx
!push cx
!push dx
!push ds
!push di
!push si
!push es
!push ss
!mov ax,word p
!mov ds,ax

!mov ax,&h20
!out &h20,ax

xdon:
if wafast% then goto tofast	'Test if already in routine and skip if is!
wafast%=1                       'set bit for in routine

if tog? = 0 then goto calc2
tog?=0

if softlim% then
statin2?=0
if limxhi& < machx& or limxlo& > machx& then if zerox% then statin2? = statin2? or limitx?
if limyhi& < machy& or limylo& > machy& then if zeroy% then statin2? = statin2? or limity?
if limzhi& < machz& or limzlo& > machz& then if zeroz% then statin2? = statin2? or limitz?
if limwhi& < machw& or limwlo& > machw& then if zerow% then statin2? = statin2? or limitw?
end if

!cli
statin1? = inp(statusport%):bit toggle statin1?,7
!sti
if emstop% then
if (statin1? and servready?) then extstop%=1 else extstop%=0
end if


'--- sync move wait for limitw to go high after first being low ----
if syncdelay% then
if (statin1? and limitw?) then
if synclow% then syncdelay%=0:synclow%=0
else
synclow%=1
end if
end if

'---- ends movement if limitw goes high while in skipcut mode ---
if skipcut% then
if (statin1? and limitw?) then
requpd%=1:fin?=1
end if
end if


if homebit%=0 and (statin2? and softlimchk?) then
statin1? = (statin2? or statin1?)
if jogbit% = 0 and handbit%=0 then estop%=1
onlimit%=1

if (statin1? and limitx?) then limithit%=1:if stopx%=0 then stopx%=1:limitxdir?=dirmask?
if (statin1? and limity?) then limithit%=1:if stopy%=0 then stopy%=1:limitydir?=dirmask?
if (statin1? and limitz?) then limithit%=1:if stopz%=0 then stopz%=1:limitzdir?=dirmask?
if (statin1? and limitw?) then limithit%=1:if stopw%=0 then stopw%=1:limitwdir?=dirmask?



elseif (statin1? and limchk?) then

if jogbit%=0 and handbit%=0 then estop%=1
onlimit%=1

if xlim% and (statin1? and limitx?) then limithit%=1:if stopx%=0 then stopx%=1:limitxdir?=dirmask?
if ylim% and (statin1? and limity?) then limithit%=1:if stopy%=0 then stopy%=1:limitydir?=dirmask?
if zlim% and (statin1? and limitz?) then limithit%=1:if stopz%=0 then stopz%=1:limitzdir?=dirmask?
if wlim% and (statin1? and limitw?) then limithit%=1:if stopw%=0 then stopw%=1:limitwdir?=dirmask?

else
statin1? = 0
onlimit%=0
end if


dout2? = (dout? xor steplevmask?) or (dirmask? xor chdirmask?)


if bcklsh%=1 then out dataport%,dout2? : goto cend2

if (dout? and mxstep?) then
	if (dout2? and mxdir?) then
		incr machx&,xdup%
                incr dvdrx&,dvspix&
                if dvdrx& >= dvspi& then
                	decr dvdrx&,dvspi&
                else
        	        dout2?=dout2? xor mxstep?
                end if
	else
		decr machx&,xdup%
        	decr dvdrx&,dvspix&
                if dvdrx& < 0 then
	                incr dvdrx&,dvspi&
                else
			dout2?=dout2? xor mxstep?
                end if

	end if
end if

if (dout? and mystep?) then
	if (dout2? and mydir?) then
		incr machy&,ydup%
                incr dvdry&,dvspiy&
                if dvdry& >= dvspi& then
                	decr dvdry&,dvspi&
                else
        	        dout2?=dout2? xor mystep?
                end if
	else
		decr machy&,ydup%
        	decr dvdry&,dvspiy&
                if dvdry& < 0 then
	                incr dvdry&,dvspi&
                else
			dout2?=dout2? xor mystep?
                end if

	end if
end if

if (dout? and mzstep?) then
	if (dout2? and mzdir?) then
		incr machz&,zdup%
                incr dvdrz&,dvspiz&
                if dvdrz& >= dvspi& then
                	decr dvdrz&,dvspi&
                else
        	        dout2?=dout2? xor mzstep?
                end if
	else
		decr machz&,zdup%
        	decr dvdrz&,dvspiz&
                if dvdrz& < 0 then
	                incr dvdrz&,dvspi&
                else
			dout2?=dout2? xor mzstep?
                end if

	end if
end if

if (dout? and mwstep?) then
	if (dout2? and mwdir?) then
		incr machw&,wdup%
                incr dvdrw&,dvspiw&
                if dvdrw& >= dvspi& then
                	decr dvdrw&,dvspi&
                else
        	        dout2?=dout2? xor mwstep?
                end if
	else
		decr machw&,wdup%
        	decr dvdrw&,dvspiw&
                if dvdrw& < 0 then
	                incr dvdrw&,dvspi&
                else
			dout2?=dout2? xor mwstep?
                end if

	end if
end if


if (dout? and mxstep?) then if lastdirx? <> (dout2? and mxdir?) then bcklsh%=1:bcklx%=bcklshx%:lastdirx?=(dout2? and mxdir?)
if (dout? and mystep?) then if lastdiry? <> (dout2? and mydir?) then bcklsh%=1:bckly%=bcklshy%:lastdiry?=(dout2? and mydir?)
if (dout? and mzstep?) then if lastdirz? <> (dout2? and mzdir?) then bcklsh%=1:bcklz%=bcklshz%:lastdirz?=(dout2? and mzdir?)
if (dout? and mwstep?) then if lastdirw? <> (dout2? and mwdir?) then bcklsh%=1:bcklw%=bcklshw%:lastdirw?=(dout2? and mwdir?)

if rotarya% then
if machw& >= rolldspa& then decr machw&,rolldspa&
if machw& < 0 then incr machw&,rolldspa&
end if
!cli
out dataport%,dout2?
!sti

if fin?=1 and bcklsh%=0 then
calcfin%=0
fin?=0
acl%=dcl%
tmr2??=0
skipcut%=0
if next_op%=0 then in_op%=0
end if

if updfeed% then gosub newfeed


'-----------------------------------
goto cend2

calc2:
tog?=1
if extfeed% and handbit% = 0 then
	extpercent?=inp(dataport2%)
        extfin?=(extpercent? and &h0f)
        extrin?=(extpercent? and &h30)
        rotate right extrin?,4
        if extrin?=3 then extrin?=4
	if extfin? <> lastfin? then lastfin? = extfin?:gosub newfeed
	if extrin? <> lastrin? then lastrin? = extrin?:gosub newfeed
end if

dout1? = (dirmask? xor chdirmask?) or steplevmask?
!cli
out dataport%,dout1?
!sti
dout?=0

if bcklx%=0 and bckly%=0 and bcklz%=0 and bcklw%=0 then bcklsh%=0
calc:


if homebit% then goto homeadd
if handbit% then goto handadd
if jogbit% then goto jogadd

if estop%=1 and es3& = acelpoint& then ehold%=1 else ehold%=0

'if syncdelay% then tog?=1:goto cend2
if syncdelay% then goto cend2

if Gdelay% then
if tmr5?? > p_out% then Gdelay%=0
'tog?=1
goto cend2
end if

if ehold% then goto cend2
if in_op% = 0 and next_op%=0 then mov%=0 : goto cend2
if extfin? = 0 and makefeed%=0 then goto cend2

'---------- one pass calculations ------------------
if calcfin% = 1 then goto cloop
calcfin%=1
mov%=0
next_op%=0

m_ready%=1
m0_out% = mov_mg0%
m1_out% = mov_mg1%
m2_out% = mov_mg2%
m3_out% = mov_mg3%
m4_out% = mov_mg4%
g0_out% = mov_g0%
g1_out% = mov_g1%
g2_out% = mov_g2%
g3_out% = mov_g3%
g6_out% = mov_g6%
g7_out% = mov_g7%
g8_out% = mov_g8%
g9_out% = mov_g9%
g10_out% = mov_g10%
g5_out% = mov_g5%
g13_out% = mov_g13%
g14_out% = mov_g14%
x92_out& = mov_x92&
y92_out& = mov_y92&
z92_out& = mov_z92&
w92_out& = mov_w92&
hx_out& = mov_hx&
hy_out& = mov_hy&
hz_out& = mov_hz&
rfeed%= mov_rpd%
p_out% = mov_P%
feed1! = mov_F!
feed2! = mov_Ftmp!
h_out% = mov_H%
d_out% = mov_D%
l_out% = mov_L%
t_out% = mov_T%
sub_out% = mov_sub%
multi_out% = mov_multi%
cdly_out% = mov_cdly%
cur_lnum& = mov_lnum&
nextdist&=next_dist& '----- length of next move !!

if mov_mg0%= 50 then syncdelay%=1:synclow%=0
if mov_g0% = 31 then skipcut%=1 'else skipcut%=0
if mov_g0% = 33 then syncdelay%=1:synclow%=0

if g13_out%=97 then
s_out! = mov_S!
else
sf_out! = mov_S!
end if

if g5_out% = 95 then
if feed_ipr%=0 then feed1!=feed1! / sout!
feed_ipr%=1
end if
if g5_out% = 94 then
if feed_ipr%=1 then feed1!=feed1! * sout!
feed_ipr%=0
end if

!cli
if mov_g0% = 4 then tmr5?? = 0 : Gdelay%=1
!sti

dx&=mov_xdi&: dy&=mov_ydi&:dw&=mov_wdi&
dz&=mov_zdi&

di&=mov_idi&:dj&=mov_jdi&:drad&=mov_rdi&
dcircum&=mov_cir&
dc1&=mov_cd&

if dx&=0 and dy&=0 and dz&=0 and dw&=0 and dcircum&=0 then goto cend2 'fin?=1:goto cend2

x1dir?=mov_xdr?:y1dir?=mov_ydr?:z1dir?=mov_zdr?:w1dir?=mov_wdr?
x1step?=mov_xst?:y1step?=mov_yst?:z1step?=mov_zst?:w1step?=mov_wst?

cda%=mov_cdr%:cdx%=mov_cdx%:cdy%=mov_cdy%:cdz%=mov_cdz%
cdw%=mov_cdw%
gtype%=mov_gtp%

declv& = mov_vec&
'acelmod!=mov_acel!
declmod!=mov_decl!
'----------------------

'------- set dirmask -------------
if dx& < 0 then dirmask? = dirmask? or x1dir? else dirmask? = dirmask? and not x1dir?
if dy& < 0 then dirmask? = dirmask? or y1dir? else dirmask? = dirmask? and not y1dir?
if dz& < 0 then dirmask? = dirmask? or z1dir? else dirmask? = dirmask? and not z1dir?
if dw& < 0 then dirmask? = dirmask? or w1dir? else dirmask? = dirmask? and not w1dir?
if gtype% > 1 then
if cdy%=1 then dirmask? = dirmask? or y1dir? else dirmask? = dirmask? and not y1dir?
if cdx%=1 then dirmask? = dirmask? or x1dir? else dirmask? = dirmask? and not x1dir?
end if

'------ set general values used ----------

txv&=abs(dx&):tyv&=abs(dy&):tzv&=abs(dz&):twv&=abs(dw&):dc&=drad&
tx&=0:ty&=0:tz&=0:tw&=0



'-------- stops deceleration between moves, except for rapid moves ----
'if gtype% > 0 and acelg0% and auto%=1 then se1&=adecl&:decl&=adecl&

!cli
s3&=dc1&
if gtype% > 1 then s3&=dcircum&+1
!sti

gosub newfeed

if gtype% > 1 then
tx&=adecl&:ty&=adecl&:tz&=adecl&:tw&=adecl&
dc&=drad&:tw1&=adecl&:tz1&=adecl&
tx&=dj&/2:ty&=di&/2:if dj&=0 or di&=0 then tx&=dc&/2:ty&=tx&
end if


mov%=1

'goto cend
goto cloop


'---------- set feed rate  -------
newfeed:

updfeed%=0

if lastrin? = 0 then rpd1!=.05 else rpd1!=(lastrin?/4)
if no_feed_overide% then fpd1!=1 else fpd1! = (lastfin? / 10)

if handbit% then rfeed%=1:rpd1!=1

if dohome% then
ovr_feed!= feed1!
ipm! = ovr_feed!
else
ovr_feed!= (feed1! * fpd1!)

if feed_ipr% then   '----  feed in IPR then, else feed is in IPM
if g0_out%=33 then ovr_feed! = feed2!
ipm! = ovr_feed! * sout!
else
if g0_out%=33 then ovr_feed! = feed2! * sout!
ipm! = ovr_feed!
end if
end if

if ipm! > maxfeed! then ipm!=maxfeed!


'rpd1!=.5	'---- temp set rapid to 50% ---------


if rfeed% then
pps& = spi?? * (maxfeed!/60)
ips! = maxfeed! / 60

else

ips!=ipm! / 60
pps& = spi?? * ips!  '--- calculation for inches per min feed
end if

if pps& < 1 then pps&=1

'-- calculate accel & decl distance --

dcdist& = spi?? * ips! * ips! *.5 /accl!
if dcdist& < 1 then dcdist&=1
adecl& = 2 * dcdist&
acelpoint& = adecl&

acel_point& = adecl& 	'--- linear feed limit point percent ratio

'---- if move distancd s3& is shorter than deceleration distance then adjust max acceleration to fit
'if dcdist& > declv& then acel_point& = declv& * 2

if mov%=0 then if dcdist& > s3& then acel_point&=s3& * 2

'---------------------
if rfeed% then
acelpoint& = adecl& * rpd1!
end if

'------- update the estop value if not set, so it stops corectly if set! ---
'if estop%=1 then
'es3& = acelpoint&
'else
'es3&=0
'end if

if estop%=0 then es3&=0 ':se2&=0 's3&


'-------------------- ADJUST DECLERATION BETWEEN MOVES ---

if auto% and gtype% > 0  then
'decl&=adecl& * declmod!
decl&=acel_point& * declmod!	'--- adjust deceleration by decelmod!

'--- if next move is short then decl so it can stop if no more moves
if decl& > nextdist& then decl& = nextdist&
'if dcdist& > nextdist& then decl& = (dcdist&-nextdist&) *2
else
'---- use full deceleration when G0 move
decl&=1 	'-- 1 sets full decl value, no decl when set equal to adecl
se1&=1  	'-- 1 sets full accel value, no decl when set equal to adecl
end if

'se1& = acel_point& * last_declmod!  '--- set next acceleration to last deceleration ----
'last_declmod! =  declmod!
'se1& = last_decl&  '--- set next acceleration to last deceleration ----
'last_decl& =  decl&

'-------- forces full deceleration between all moves, ----
if acelg0%=0 then
decl&=1
se1&=1
end if

if pps& = last_pps& then return
last_pps&=pps&

timer1 = 1193180 / pps&
if timer1 > 131070 then timer1 = 131070
if timer1 < clklimit& then timer1=clklimit&
rxt1& = timer1 / 2
f1? = bits?(rxt1&)
f2?= fix(rxt1&/256)
!cli
out &h43,&h3c
out &h40,f1?
out &h40,f2?
!sti



return


'------- new home routine , this is working really well !---
homeadd:
'tog?=1
mov%=1
if bcklsh% = 1 goto stback

if x2home% then
xfromlim%=0
if (statin1? and limitx?) then
xlimclear%=limclear%
x2home%=0:xfromlim%=1
else
dout?=dout? or mxstep?
if dirhomex% then dirmask?=dirmask? and not mxdir? else dirmask?=dirmask? or mxdir?
end if
end if

if xfromlim% then
if ratiox% > retratio% then
ratiox%=0
if dirhomex% then dirmask?=dirmask? or mxdir? else dirmask?=dirmask? and not mxdir?
if (statin1? and limitx?) then
dout?=dout? or mxstep?
else
if xlimclear% then
decr xlimclear%
dout?=dout? or mxstep?
else
machx&=0:xfromlim%=0:zerox%=1:stopx%=0:dout?=dout? and not mxstep?
end if
end if
else
incr ratiox%
end if
end if


if y2home% then
yfromlim%=0
if (statin1? and limity?) then
ylimclear%=limclear%
y2home%=0:yfromlim%=1
else
dout?=dout? or mystep?
if dirhomey% then dirmask?=dirmask? and not mydir? else dirmask?=dirmask? or mydir?
end if
end if

if yfromlim% then
if ratioy% > retratio% then
ratioy%=0
if dirhomey% then dirmask?=dirmask? or mydir? else dirmask?=dirmask? and not mydir?
if (statin1? and limity?) then
dout?=dout? or mystep?
else
if ylimclear% then
decr ylimclear%
dout?=dout? or mystep?
else
machy&=0:yfromlim%=0:zeroy%=1:stopy%=0:dout?=dout? and not mystep?
end if
end if
else
incr ratioy%
end if
end if


if z2home% then
zfromlim%=0
if (statin1? and limitz?) then
zlimclear%=limclear%
z2home%=0:zfromlim%=1
else
dout?=dout? or mzstep?
if dirhomez% then dirmask?=dirmask? and not mzdir? else dirmask?=dirmask? or mzdir?
end if
end if

if zfromlim% then
if ratioz% > retratio% then
ratioz%=0
if dirhomez% then dirmask?=dirmask? or mzdir? else dirmask?=dirmask? and not mzdir?
if (statin1? and limitz?) then
dout?=dout? or mzstep?
else
if zlimclear% then
decr zlimclear%
dout?=dout? or mzstep?
else
machz&=0:zfromlim%=0:zeroz%=1:stopz%=0:dout?=dout? and not mzstep?
end if
end if
else
incr ratioz%
end if
end if



if w2home% then
wfromlim%=0
if (statin1? and limitw?) then
wlimclear%=limclear%
w2home%=0:wfromlim%=1
else
dout?=dout? or mwstep?
if dirhomew% then dirmask?=dirmask? and not mwdir? else dirmask?=dirmask? or mwdir?
end if
end if

if wfromlim% then
if ratiow% > retratio% then
ratiow%=0
if dirhomew% then dirmask?=dirmask? or mwdir? else dirmask?=dirmask? and not mwdir?
if (statin1? and limitw?) then
dout?=dout? or mwstep?
else
if wlimclear% then
decr wlimclear%
dout?=dout? or mwstep?
else
machw&=0:wfromlim%=0:zerow%=1:stopw%=0:dout?=dout? and not mwstep?
end if
end if
else
incr ratiow%
end if
end if


goto cend


'------------------- manual HANDLE routine -----------------
handadd:
'tog?=1
mov%=1
if bcklsh% = 1 goto stback

if extstop% then
handx&=0
handy&=0
handz&=0
handw&=0
end if

if stopx% or stopy% or stopz% or stopw% then
if (statin1? and limitx?) then
	if (limitxdir? and mxdir?) then
        if handx& < 0 then handx&=0
        else
        if handx& > 0 then handx& =0
        end if
else
	stopx%=0
end if

if (statin1? and limity?) then
	if (limitydir? and mydir?) then
        if handy& < 0 then handy&=0
        else
        if handy& > 0 then handy& =0
        end if
else
	stopy%=0
end if

if (statin1? and limitz?) then
	if (limitzdir? and mzdir?) then
        if handz& < 0 then handz&=0
        else
        if handz& > 0 then handz& =0
        end if
else
	stopz%=0
end if

if (statin1? and limitw?) then
	if (limitwdir? and mwdir?) then
        if handw& < 0 then handw&=0
        else
        if handw& > 0 then handw& =0
        end if
else
	stopw%=0
end if
end if

'------------ X axis handle acel/decl routine -----------------
if handx& = 0 then
hndx&=1:hndx1&=0
else
if abs(handx&) < handsoft% * hndx&*((hndx&/adecl&)) then
if hndx& > 1 then decr hndx&
else
if hndx& < acelpoint& then incr hndx&
if hndx& > acelpoint& then decr hndx&
end if
incr hndx1&,hndx&
if hndx1& > adecl& then
decr hndx1&,adecl&
if handx& < 0 then incr handx&:dout?=dout? or mxstep?:dirmask?=dirmask? or mxdir?
if handx& > 0 then decr handx&:dout?=dout? or mxstep?:dirmask?=dirmask? and not mxdir?
end if
end if

'------------ Y axis handle acel/decl routine -----------------
if handy& = 0 then
hndy&=1:hndy1&=0
else
if abs(handy&) < handsoft% * hndy&*((hndy&/adecl&)) then
if hndy& > 1 then decr hndy&
else
if hndy& < acelpoint& then incr hndy&
if hndy& > acelpoint& then decr hndy&
end if
incr hndy1&,hndy&
if hndy1& > adecl& then
decr hndy1&,adecl&
if handy& < 0 then incr handy&:dout?=dout? or mystep?:dirmask?=dirmask? or mydir?
if handy& > 0 then decr handy&:dout?=dout? or mystep?:dirmask?=dirmask? and not mydir?
end if
end if

'------------ Z axis handle acel/decl routine -----------------
if handz& = 0 then
hndz&=1:hndz1&=0
else
if abs(handz&) < handsoft% * hndz&*((hndz&/adecl&)) then
if hndz& > 1 then decr hndz&
else
if hndz& < acelpoint& then incr hndz&
if hndz& > acelpoint& then decr hndz&
end if
incr hndz1&,hndz&
if hndz1& > adecl& then
decr hndz1&,adecl&
if handz& < 0 then incr handz&:dout?=dout? or mzstep?:dirmask?=dirmask? or mzdir?
if handz& > 0 then decr handz&:dout?=dout? or mzstep?:dirmask?=dirmask? and not mzdir?
end if
end if
'------------ W axis handle acel/decl routine -----------------
if handw& = 0 then
hndw&=1:hndw1&=0
else
if abs(handw&) < handsoft% * hndw&*((hndw&/adecl&)) then
if hndw& > 1 then decr hndw&
else
if hndw& < acelpoint& then incr hndw&
if hndw& > acelpoint& then decr hndw&
end if
incr hndw1&,hndw&
if hndw1& > adecl& then
decr hndw1&,adecl&
if handw& < 0 then incr handw&:dout?=dout? or mwstep?:dirmask?=dirmask? or mwdir?
if handw& > 0 then decr handw&:dout?=dout? or mwstep?:dirmask?=dirmask? and not mwdir?
end if
end if

goto cend

'---------------------- manual JOG routine -------------------
jogadd:

mov%=1
if bcklsh% = 1 goto stback

if extstop% then	'clear all move keys while emg-stop is on
ktx?(jx1?)=0:ktx?(jx2?)=0
ktx?(jy1?)=0:ktx?(jy2?)=0
ktx?(jz1?)=0:ktx?(jz2?)=0
ktx?(jw1?)=0:ktx?(jw2?)=0
end if

if stopx% or stopy% or stopz% or stopw% then
if (statin1? and limitx?) then
	if (limitxdir? and mxdir?) then ktx?(jx1?)=0 else ktx?(jx2?)=0
else
	stopx%=0
end if

if (statin1? and limity?) then
	if( limitydir? and mydir?) then ktx?(jy1?)=0 else ktx?(jy2?)=0
else
	stopy%=0
end if

if (statin1? and limitz?) then
	if( limitzdir? and mzdir?) then ktx?(jz1?)=0 else ktx?(jz2?)=0
else
	stopz%=0
end if

if (statin1? and limitw?) then
	if( limitwdir? and mwdir?) then ktx?(jw1?)=0 else ktx?(jw2?)=0
else
	stopw%=0
end if
end if


normjog:

'----------------------------- X accel/decl --------------
if ktx?(jx1?) and jx1&=0 and jx2&=0 then jx2&=1
if ktx?(jx1?) and jx2& then
if jx2& < acelpoint& then incr jx2&
if jx2& > acelpoint& then decr jx2&
incr jxo&,jx2&
if jxo& > adecl& then
decr jxo&,adecl&
dout?=dout? or mxstep?:dirmask?=dirmask? or mxdir?
end if
end if

if ktx?(jx1?)=0 and jx2& then
decr jx2&
incr jxo&,jx2&
if jxo& > adecl& then
decr jxo&,adecl&
dout?=dout? or mxstep?:dirmask?=dirmask? or mxdir?
end if
end if

'------- other x dir ----------
if ktx?(jx2?) and jx1&=0 and jx2&=0 then jx1&=1
if ktx?(jx2?) and jx1& then
if jx1& < acelpoint& then incr jx1&
if jx1& > acelpoint& then decr jx1&
incr jxo&,jx1&
if jxo& > adecl& then
decr jxo&,adecl&
dout?=dout? or mxstep?:dirmask?=dirmask? and not mxdir?
end if
end if

if ktx?(jx2?)=0 and jx1& then
decr jx1&
incr jxo&,jx1&
if jxo& > adecl& then
decr jxo&,adecl&
dout?=dout? or mxstep?:dirmask?=dirmask? and not mxdir?
end if
end if

'-------------------------- Y accel/decl -------------------
if ktx?(jy1?) and jy1&=0 and jy2&=0 then jy2&=1
if ktx?(jy1?) and jy2& then
if jy2& < acelpoint& then incr jy2&
if jy2& > acelpoint& then decr jy2&
incr jyo&,jy2&
if jyo& > adecl& then
decr jyo&,adecl&
dout?=dout? or mystep?:dirmask?=dirmask? or mydir?
end if
end if

if ktx?(jy1?)=0 and jy2& then
decr jy2&
incr jyo&,jy2&
if jyo& > adecl& then
decr jyo&,adecl&
dout?=dout? or mystep?:dirmask?=dirmask? or mydir?
end if
end if

'------- other y dir ----------------

if ktx?(jy2?) and jy1&=0 and jy2&=0 then jy1&=1
if ktx?(jy2?) and jy1& then
if jy1& < acelpoint& then incr jy1&
if jy1& > acelpoint& then decr jy1&
incr jyo&,jy1&
if jyo& > adecl& then
decr jyo&,adecl&
dout?=dout? or mystep?:dirmask?=dirmask? and not mydir?
end if
end if

if ktx?(jy2?)=0 and jy1& then
decr jy1&
incr jyo&,jy1&
if jyo& > adecl& then
decr jyo&,adecl&
dout?=dout? or mystep?:dirmask?=dirmask? and not mydir?
end if
end if

'-------------------------- Z accel/decl -------------------
if ktx?(jz1?) and jz1&=0 and jz2&=0 then jz2&=1
if ktx?(jz1?) and jz2& then
if jz2& < acelpoint& then incr jz2&
if jz2& > acelpoint& then decr jz2&
incr jzo&,jz2&
if jzo& > adecl& then
decr jzo&,adecl&
dout?=dout? or mzstep?:dirmask?=dirmask? or mzdir?
end if
end if

if ktx?(jz1?)=0 and jz2& then
decr jz2&
incr jzo&,jz2&
if jzo& > adecl& then
decr jzo&,adecl&
dout?=dout? or mzstep?:dirmask?=dirmask? or mzdir?
end if
end if

'------- other z dir ----------
if ktx?(jz2?) and jz1&=0 and jz2&=0 then jz1&=1
if ktx?(jz2?) and jz1& then
if jz1& < acelpoint& then incr jz1&
if jz1& > acelpoint& then decr jz1&
incr jzo&,jz1&
if jzo& > adecl& then
decr jzo&,adecl&
dout?=dout? or mzstep?:dirmask?=dirmask? and not mzdir?
end if
end if

if ktx?(jz2?)=0 and jz1& then
decr jz1&
incr jzo&,jz1&
if jzo& > adecl& then
decr jzo&,adecl&
dout?=dout? or mzstep?:dirmask?=dirmask? and not mzdir?
end if
end if


'-------------------------- W accel/decl -------------------
if ktx?(jw1?) and jw1&=0 and jw2&=0 then jw2&=1
if ktx?(jw1?) and jw2& then
if jw2& < acelpoint& then incr jw2&
if jw2& > acelpoint& then decr jw2&
incr jwo&,jw2&
if jwo& > adecl& then
decr jwo&,adecl&
dout?=dout? or mwstep?:dirmask?=dirmask? or mwdir?
end if
end if

if ktx?(jw1?)=0 and jw2& then
decr jw2&
incr jwo&,jw2&
if jwo& > adecl& then
decr jwo&,adecl&
dout?=dout? or mwstep?:dirmask?=dirmask? or mwdir?
end if
end if

'------- other w dir ----------
if ktx?(jw2?) and jw1&=0 and jw2&=0 then jw1&=1
if ktx?(jw2?) and jw1& then
if jw1& < acelpoint& then incr jw1&
if jw1& > acelpoint& then decr jw1&
incr jwo&,jw1&
if jwo& > adecl& then
decr jwo&,adecl&
dout?=dout? or mwstep?:dirmask?=dirmask? and not mwdir?
end if
end if

if ktx?(jw2?)=0 and jw1& then
decr jw1&
incr jwo&,jw1&
if jwo& > adecl& then
decr jwo&,adecl&
dout?=dout? or mwstep?:dirmask?=dirmask? and not mwdir?
end if
end if

goto cend
'------------------------END OF JOG ------------


cloop:

'tog?=1
mov%=1
if bcklsh% = 1 goto stback

'-----------------------------

'----------------------- ESTOP IS  SET ! ------------------


if estop%=1 then
if es3& < acelpoint& then incr es3&
incr es4&,es3&
if es4& > acelpoint& then
decr es4&,acelpoint&
goto cend2
end if
end if



'---------------- restart estop -----

if estop%=0 and es3& then
if es3& > 0 then decr es3&
incr es4&,es3&
if es4& > acelpoint& then
decr es4&,acelpoint&
goto cend2
end if
end if


if gtype% = 0 then goto rapid


'------ linear and circular acel & decl loop ------------


if s3& < se1&*((se1&/adecl&)/2) then
if se1& > decl& then if se1& > 1 then decr se1&
else
if se1& < acel_point& then incr se1&
if se1& > acel_point& then if se1& > 1 then decr se1&
'if se1& < adecl& then incr se1&
'if se1& > adecl& then if se1& > 1 then decr se1&
end if
incr se2&,se1&
if se2& > adecl& then
decr se2&,adecl&
else
goto cend2
end if


decr s3& '--- making a move so decrement distance counter s3&


if gtype% = 3 goto circ1
if gtype% = 2 goto circ1
if gtype% = 1 goto linear
if gtype% = 0 goto rapid
goto cend2

rapid:

if dx& <> 0 then
if dx& < 0 then dirmask? = dirmask? or x1dir? else dirmask? = dirmask? and not x1dir?
if abs(dx&) < tx&*((tx&/adecl&)/2) then
if tx& > 1 then decr tx&
else
if tx& < acelpoint& then incr tx&
if tx& > acelpoint& then decr tx&
end if
incr tx1&,tx&
if tx1& > adecl& then decr tx1&,adecl&:incr dx&,cdx%:dout?=dout? or x1step?
end if

if dy& <> 0 then
if dy& < 0 then dirmask? = dirmask? or y1dir? else dirmask? = dirmask? and not y1dir?
if abs(dy&) < ty&*((ty&/adecl&)/2) then
if ty& > 1 then decr ty&
else
if ty& < acelpoint& then incr ty&
if ty& > acelpoint& then decr ty&
end if
incr ty1&,ty&
if ty1& > adecl& then decr ty1&,adecl&:incr dy&,cdy%:dout?=dout? or y1step?
end if

if dz& <> 0 then
if dz& < 0 then dirmask? = dirmask? or z1dir? else dirmask? = dirmask? and not z1dir?
if abs(dz&) < tz&*((tz&/adecl&)/2) then
if tz& > 1 then decr tz&
else
if tz& < acelpoint& then incr tz&
if tz& > acelpoint& then decr tz&
end if
incr tz1&,tz&
if tz1& > adecl& then decr tz1&,adecl&:incr dz&,cdz%:dout?=dout? or z1step?
end if

if dw& <> 0 then
if dw& < 0 then dirmask? = dirmask? or w1dir? else dirmask? = dirmask? and not w1dir?
if abs(dw&) < tw&*((tw&/adecl&)/2) then
if tw& > 1 then decr tw&
else
if tw& < acelpoint& then incr tw&
if tw& > acelpoint& then decr tw&
end if
incr tw1&,tw&
if tw1& > adecl& then decr tw1&,adecl&:incr dw&,cdw%:dout?=dout? or w1step?
end if
!cli
if dx&=0 and dy&=0 and dz&=0 and dw&=0 then fin?=1
!sti
goto cend

linear:
incr tx&,txv&:incr ty&,tyv&:incr tz&,tzv&:incr tw&,twv&

if dx& <>0 then
if dx& < 0 then dirmask? = dirmask? or x1dir? else dirmask? = dirmask? and not x1dir?
if tx& > dc1& then decr tx&,dc1&:incr dx&,cdx%:dout?=dout? or x1step?
end if

if dy& <>0 then
if dy& < 0 then dirmask? = dirmask? or y1dir? else dirmask? = dirmask? and not y1dir?
if ty& > dc1& then decr ty&,dc1&:incr dy&,cdy%:dout?=dout? or y1step?
end if

if dz& <>0 then
if dz& < 0 then dirmask? = dirmask? or z1dir? else dirmask? = dirmask? and not z1dir?
if tz& > dc1& then decr tz&,dc1&:incr dz&,cdz%:dout?=dout? or z1step?
end if

if dw& <>0 then
if dw& < 0 then dirmask? = dirmask? or w1dir? else dirmask? = dirmask? and not w1dir?
if tw& > dc1& then decr tw&,dc1&:incr dw&,cdw%:dout?=dout? or w1step?
end if
!cli
if dx&=0 and dy&=0 and dz&=0 and dw&=0 then fin?=1
!sti
goto cend

circ1:
if s3& <= 0 goto cerr

incr tx&,di&
if s3& > 2 or dy& <> 0 then
if tx& > dc& then
decr tx&,dc&:incr dy&,cdy%:incr dj&,cda%:dout?=dout? or y1step?
if cdx% =1 then dirmask?=dirmask? or x1dir? else dirmask?=dirmask? and not x1dir?
if dj&=< 0 then cda% =-cda%:cdx%= -cdx%
end if
end if

incr ty&,dj&
if s3& > 2 or dx& <> 0 then
if ty& > dc& then
decr ty&,dc&:incr dx&,cdx%:decr di&,cda%:dout?=dout? or x1step?
if cdy% =1 then dirmask? = dirmask? or y1dir? else dirmask?=dirmask? and not y1dir?
if di& =< 0 then cda% =-cda%:cdy%= -cdy%
end if
end if

incr tz&,tzv&
if dz& <> 0 then if tz& > dcircum& then decr tz&,dcircum&:incr dz&,cdz%:dout?=dout? or z1step?
incr tw&,twv&
if dw& <> 0 then if tw& > dcircum& then decr tw&,dcircum&:incr dw&,cdw%:dout?=dout? or w1step?
!cli
if s3&<1 and dx&=0 and dy&=0 and dz&=0 and dw&=0 then fin?=1
!sti
goto cend

cerr:
if dx& <> 0 then
dout?=dout? or x1step?
if dx& > 0 then
decr dx&:dirmask?=dirmask? and not x1dir?
else
incr dx&: dirmask?=dirmask? or x1dir?
end if
end if

if dy& <> 0 then
dout?=dout? or y1step?
if dy& > 0 then
decr dy&::dirmask?=dirmask? and not y1dir?
else
incr dy&: dirmask?=dirmask? or y1dir?
end if
end if

if dx&=0 and dy&=0 and dz&=0 and dw&=0 then fin?=1
goto cend

stback:
'if bcklx%=0 and bckly%=0 and bcklz%=0 and bcklw%=0 then bcklsh%=0:goto cend
if bcklx% > 0 then decr bcklx%:dout?=dout? or mxstep?
if bckly% > 0 then decr bckly%:dout?=dout? or mystep?
if bcklz% > 0 then decr bcklz%:dout?=dout? or mzstep?
if bcklw% > 0 then decr bcklw%:dout?=dout? or mwstep?

cend:

dout1? = (dirmask? xor chdirmask?) or steplevmask?
!cli
out dataport%,dout1?
!sti

cend2:

if s4% < 1 then s4%=1
wafast%=0
tofast:

		'restore registers back to original before interrupt
!pop ss
!pop es
!pop si
!pop di
!pop ds
!pop dx
!pop cx
!pop bx
!pop ax
!pop bp
!popf
!iret
end
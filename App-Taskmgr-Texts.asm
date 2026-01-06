;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                 S y m b O S   -   T a s k - M a n a g e r                  @
;@                   (default application texts [english])                    @
;@                                                                            @
;@             (c) 2004-2025 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;### POINTER ##################################################################

;general

prgmsginf1   db 1:dw prgmsginf1_eng
prgmsginf3   db 1:dw prgmsginf3_eng

przpritxt1   db 1:dw przpritxt1_eng
przpritxt2   db 1:dw przpritxt2_eng
przpritxt3   db 1:dw przpritxt3_eng

tskdattit   db 1:dw tskdattit_eng

prgdatmentx1   db 1:dw prgdatmentx1_eng
prgdatmen1tx1   db 1:dw prgdatmen1tx1_eng
prgdatmen1tx2   db 1:dw prgdatmen1tx2_eng

prgdatmentx2   db 1:dw prgdatmentx2_eng
prgdatmen2tx1   db 1:dw prgdatmen2tx1_eng
prgdatmen2tx2   db 1:dw prgdatmen2tx2_eng
prgdatmen4tx1   db 1:dw prgdatmen4tx1_eng
prgdatmen4tx2   db 1:dw prgdatmen4tx2_eng
prgdatmen4tx3   db 1:dw prgdatmen4tx3_eng
prgdatmen4tx4   db 1:dw prgdatmen4tx4_eng
prgdatmen2tx3   db 1:dw prgdatmen2tx3_eng

prgdatmentx3   db 1:dw prgdatmentx3_eng
prgdatmen3tx1   db 1:dw prgdatmen3tx1_eng
prgdatmen3tx2   db 1:dw prgdatmen3tx2_eng

;process context menu

przdatmentx1   db 1:dw przdatmentx1_eng
przdatmentx2   db 1:dw przdatmentx2_eng
przdatmentx3   db 1:dw przdatmentx3_eng
przdatmen1tx1   db 1:dw przdatmen1tx1_eng
przdatmen1tx2   db 1:dw przdatmen1tx2_eng
przdatmen1tx3   db 1:dw przdatmen1tx3_eng
przdatmen1tx4   db 1:dw przdatmen1tx4_eng
przdatmen1tx5   db 1:dw przdatmen1tx5_eng
przdatmen1tx6   db 1:dw przdatmen1tx6_eng
przdatmen1tx7   db 1:dw przdatmen1tx7_eng

;performance

tsktabtxt1   db 1:dw tsktabtxt1_eng
tsktabtxt2   db 1:dw tsktabtxt2_eng
tsktabtxt3   db 1:dw tsktabtxt3_eng

tskobjtxt1   db 1:dw tskobjtxt1_eng
tskobjtxt2   db 1:dw tskobjtxt2_eng
tskobjtxt3   db 1:dw tskobjtxt3_eng
tskobjtxt4   db 1:dw tskobjtxt4_eng
tskobjtxt5   db 1:dw tskobjtxt5_eng
tskobjtxt6   db 1:dw tskobjtxt6_eng
tskobjtxt7   db 1:dw tskobjtxt7_eng
tskobjtxt8   db 1:dw tskobjtxt8_eng
tskobjtxt10   db 1:dw tskobjtxt10_eng
tskobjtxt11   db 1:dw tskobjtxt11_eng

;processes

prgprzrow1   db 1:dw prgprzrow1_eng
prgprzrow2   db 1:dw prgprzrow2_eng
prgprzrow3   db 1:dw prgprzrow3_eng
prgprzrow4   db 1:dw prgprzrow4_eng

tskapprow3   db 1:dw tskapprow3_eng
tskapprow4   db 1:dw tskapprow4_eng

prgprzsta0   db 1:dw prgprzsta0_eng
prgprzsta1   db 1:dw prgprzsta1_eng
prgprzsta2   db 1:dw prgprzsta2_eng
prgprzsta3   db 1:dw prgprzsta3_eng

App_PrcIDam4   db 1:dw App_PrcIDam4_eng

prgprzbut1   db 1:dw prgprzbut1_eng
prgprzbut2   db 1:dw prgprzbut2_eng
prgprzbut3   db 1:dw prgprzbut3_eng
tskappbut2   db 1:dw tskappbut2_eng
tskappbut3   db 1:dw tskappbut3_eng


;### TEXTS #####################################################################

;general

prgmsginf1_eng db "SymbOS TASK MANAGER",0
prgmsginf3_eng db " Copyright <c> 2025 SymbiosiS",0

przpritxt1_eng db "Do you want to change the priority?",0
przpritxt2_eng db " Changing the proirity could",0
przpritxt3_eng db " cause system instability.",0

tskdattit_eng db "Task Manager",0

prgdatmentx1_eng  db "File",0
prgdatmen1tx1_eng db "New Task (Run...)",0
prgdatmen1tx2_eng db "Exit Task Manager",0

prgdatmentx2_eng  db "View",0
prgdatmen2tx1_eng db "Refresh Now",0
prgdatmen2tx2_eng db "Update Speed",0
prgdatmen4tx1_eng db "High",0
prgdatmen4tx2_eng db "Normal",0
prgdatmen4tx3_eng db "Low",0
prgdatmen4tx4_eng db "Paused",0
prgdatmen2tx3_eng db "Only when focus",0

prgdatmentx3_eng  db "?",0
prgdatmen3tx1_eng db "Help Topics",0
prgdatmen3tx2_eng db "About",0

;process context menu

przdatmentx1_eng    db "Kill process",0
przdatmentx2_eng    db "Send to sleep",0
przdatmentx3_eng    db "Set priority",0
przdatmen1tx1_eng   db "Realtime",0
przdatmen1tx2_eng   db "High",0
przdatmen1tx3_eng   db "Above normal",0
przdatmen1tx4_eng   db "Normal",0
przdatmen1tx5_eng   db "Below normal",0
przdatmen1tx6_eng   db "Low",0
przdatmen1tx7_eng   db "Background",0

;performance

tsktabtxt1_eng db "Apps.",0
tsktabtxt2_eng db "Proces.",0
tsktabtxt3_eng db "Performance",0

tskobjtxt1_eng  db "Totals",0
tskobjtxt2_eng  db "Memory (KB)",0
tskobjtxt3_eng  db "Applications",0
tskobjtxt4_eng  db "Processes",0
tskobjtxt5_eng  db "Timers",0
tskobjtxt6_eng  db "Total",0
tskobjtxt7_eng  db "Used",0
tskobjtxt8_eng  db "Available",0
tskobjtxt10_eng db "CPU",0
tskobjtxt11_eng db "Memory",0

;processes

prgprzrow1_eng db "Name",0
prgprzrow2_eng db "ID",0
prgprzrow3_eng db "Pri.",0
prgprzrow4_eng db "Status",0

tskapprow3_eng db "Prc.",0
tskapprow4_eng db "Mem.",0

prgprzsta0_eng db "Timer",0
prgprzsta1_eng db "Sleep",0
prgprzsta2_eng db "Idle",0
prgprzsta3_eng db "Work",0

App_PrcIDam4_eng db "Application",0

prgprzbut1_eng db "Refresh",0
prgprzbut2_eng db "Sleep",0
prgprzbut3_eng db "Kill",0
tskappbut2_eng db "Switch",0
tskappbut3_eng db "End App.",0


;### RESERVE
ds 0

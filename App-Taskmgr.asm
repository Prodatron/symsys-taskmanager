;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                 S y m b O S   -   T a s k - M a n a g e r                  @
;@                                                                            @
;@             (c) 2004-2022 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;TODO
;- clean up/rewrite
;  - use dskextvars instead of sysinf(1)


;==============================================================================
;### CODE-TEIL ################################################################
;==============================================================================

krnprzn     equ 1
dskprzn     equ 2
sysprzn     equ 3

;### PRGPRZ -> Taskmanager-Prozess
prgwin      db 0            ;Nummer des Taskmanager-Fensters

windatprz   equ 3           ;Prozeßnummer
tskupddel   dw 2            ;Update verzögerung
tskupdtim   dw 1
tskupdmil   db 1
tskupdsec   db 0
tskacttab   db 2            ;aktueller Tab
tsklstupd   db 0

przdatstk   equ 0           ;Stackpointer
przdatbnk   equ 2           ;Bank-Konfiguration, Bit7=0 -> Hard-Unterbrochen (Status = "Work")
przdatzgr   equ 3           ;Zeiger auf naechsten Prozess
przdatpri   equ 5           ;Prioritaet

przdatlen   equ 6           ;Laenge
przdatmax   equ 32          ;maximal 32 Prozesse
timdatmax   equ 32          ;maximal 32 Timer
przdatmem   equ 0
timdatmem   equ przdatmax*przdatlen


prgprz  call tskini
        call SySystem_HLPINI

        ld a,(App_BnkNum)
        ld de,tskdatwin
        call SyDesktop_WINOPN
        jp c,prgend             ;memory full -> quit process
        ld (prgwin),a           ;window has been opened -> store ID

prgprz0 call prgmsg
        jr nc,prgprz4
        cp MSR_DSK_WCLICK       ;*** Fenster-Aktion wurde geklickt
        jr nz,prgprz4
        ld a,(App_MsgBuf+2)
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,prgend
        cp DSK_ACT_MENU         ;*** Menü wurde geklickt
        jr z,prgprz1
        cp DSK_ACT_CONTENT      ;*** Inhalt wurde geklickt
        jr nz,prgprz4
prgprz1 ld hl,(App_MsgBuf+8)       ;content clicked
        ld a,l
        or h
        jr z,prgprz0
        jp (hl)
prgprz4 ld hl,tskupdmil         ;Screen-Update-Timer runterzählen
        dec (hl)
        jr nz,prgprz0
        ld (hl),50
        jr nz,prgprz0
        rst #20:dw jmp_timget
        ld hl,tskupdsec
        cp (hl)
        jr z,prgprz0
        ld (hl),a
        ld hl,tskupdtim
        dec (hl)
        jr nz,prgprz0
        ld a,(tskupddel)
        ld (hl),a
        call tskupd0            ;Update durchführen
        jr prgprz0

;### PRGEND -> Taskmanager-Prozess beenden
prgend  ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend1 rst #30                     ;wait for death
        jr prgend1

;### PRGHLP -> show help
prghlp  call SySystem_HLPOPN
        jp prgprz0

;### PRGINF -> show info-box
prginf  ld hl,prgmsginf         ;*** info box
        ld b,1+128+64
prginf1 call prginf0
        jp prgprz0
prginf0 ld a,(App_BnkNum)
        ld de,tskdatwin
        jp SySystem_SYSWRN

;### PRGMSG -> Message für Taskmanager-Prozess abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (App_MsgBuf)=Message, A=(App_MsgBuf+0)
;### Veraendert 
prgmsg  rst #30
        ld a,(App_PrcID)
        db #dd:ld l,a           ;IXL=Taskmanager-Prozeß-Nummer
        db #dd:ld h,-1
        ld iy,App_MsgBuf        ;IY=Messagebuffer
        rst #18                 ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        or a
        db #dd:dec l
        ret nz
        ld a,(App_MsgBuf+0)
        or a
        jr z,prgend
        scf
        ret


;==============================================================================
;### GENERAL TASKMANAGER ROUTINES #############################################
;==============================================================================

;### Tab click
tsktab  ld a,(tsktabdat0)
        ld hl,(tskacttab)
        cp l
        jp z,prgprz0            ;gleicher wie aktueller -> nichts machen
        ld (tskacttab),a
        cp 1
        jr z,tsktab3
        ld hl,tskdatgrp3
        jr nc,tsktab1
        xor a
        call tskupdb
        ld hl,tskdatgrp1
        jr tsktab1
tsktab3 xor a
        call tskupda
        ld hl,tskdatgrp2
tsktab1 ld (tskdatwin0),hl      ;Tab wechseln
        ld e,-1
        ld a,(prgwin)
        call SyDesktop_WINDIN
        jp prgprz0

;### Refresh click
tskref  ld a,1
        ld (tsklstupd),a
        scf
        call tskupd
        jp prgprz0


;### TSKUPD -> Updated Taskmanager-Fenster
;### Eingabe    CF=0 -> nur CPU-Messung, kein Screen-Update, CF=1 -> volles Update
tskupdler   dw 0
tskupdsys   dw 0
tskupdcpu   db 0
tskupdmem   db 0
tskupdstp   db -1       ;Flag (=0), ob keine Anzeige erwünscht

tskupd0 ld a,(tskupdstp)
        cp 1
        ccf
tskupd  push af
        ld hl,jmp_mtgcnt        ;*** CPU-Last Infos holen
        rst #28                 ;IY,IX=Systemzähler, DE=Leerlaufprozess-Counter
        ld hl,(tskupdler)
        ex de,hl
        ld (tskupdler),hl
        or a
        sbc hl,de               ;HL=Differenz Leerlauf-Counter
        push hl  
        push ix
        pop hl
        ld de,(tskupdsys)
        ld (tskupdsys),hl
        or a
        sbc hl,de               ;HL=Differenz System-Counter
        ex de,hl
        ld a,156
        call clcm16             ;HL=Sys*156
        pop de
        push hl
        ld bc,100
        call clcmul             ;A,HL=Leer*100
        pop de
        ld c,l
        ld b,h
        call clcdiv             ;HL = (Leer * 100) / (Sys * 156) = freie CPU in %
        ex de,hl
        ld hl,100
        or a
        sbc hl,de
        jr nc,tskupd1
        ld hl,0                 ;HL = 100-freie CPU = Auslastung
tskupd1 ld a,l
        cp 101
        jr c,tskupd2
        ld a,100
tskupd2 ld l,a
        ld h,0
        push hl
        call tskadj             ;A=A*48/100
        ld (tskupdcpu),a        ;merken
        pop ix                  ;IX=Wert
        ld iy,tskobjtxt12       ;(IY)=Ascii-String
        ld e,3
        ld hl,jmp_clcnum
        rst #28                 ;Wert umwandeln
        ld (iy+1),"%"
        ld (iy+2),0
        ld a,(tskupdcpu)        ;*** Kurven updaten
        ld e,a
        xor a 
        call tskcur             ;CPU-Kurve updaten
        ld a,(tskupdmem)
        ld e,a
        ld a,1
        call tskcur             ;Memory-Kurve updaten
        pop af
        ret nc

        ld a,(tskacttab)
        cp 2
        jp z,tskupd4
        call tskchg
        ret z
        jp c,tskupd5

        call tskupda            ;*** Processes-Infos holen
tskupdc ld a,2
        jp tskdsp1
tskupda call appnam
        ld hl,jmp_sysinf
        ld e,2
        ld iy,prgprzdat
        rst #28                 ;(prgprzdat)=przdatmem + timdatmem = (32+32)*6 = 384 Bytes
        ld iy,prgprzdat         ;IY=Quelldata
        ld b,64                 ;B=Zähler/ID
        ld c,0                  ;C=Anzahl
        ld ix,tsklstrow         ;IX=Zieltab
tskupd6 xor a
        cp (iy+przdatbnk)
        jp z,tskupd9            ;Slot nicht belegt
        ld a,b
        cp 32
        jr z,tskupd9
        inc c
        ld de,prgprzsta0
        ld a,0
        jr c,tskupd7            ;Timer   -> Prio=0, Status=Timer
        ld a,(iy+przdatpri)     ;A=Prio
        ld de,prgprzsta1
        bit 7,(iy+przdatstk+1)
        jr z,tskupd7            ;SleepBit=0 -> Status=Sleep
        bit 7,(iy+przdatbnk)
        ld de,prgprzsta3
        jr z,tskupd7            ;WorkBit=0  -> Status=Work
        ld de,prgprzsta2
;DE=Status, A=Prio, B=Zähler, C=Anzahl
tskupd7 ld (ix+8),e
        ld (ix+9),d             ;Status
        ld (ix+6),a             ;Priorität
        ld (ix+7),0
        ld a,64
        sub b
        ld (ix+4),a             ;ID
        ld (ix+5),0
        ld (ix+0),a             ;ID
        ld (ix+1),0
        cp 1
        ld de,App_PrcIDam0
        jr c,tskupd8
        ld de,App_PrcIDam1
        jr z,tskupd8
        cp 33
        jr z,tskupd8
        cp 3
        ld de,App_PrcIDam2
        jr c,tskupd8
        ld de,App_PrcIDam3
        jr z,tskupd8
        push hl
        push bc
        push ix
        push iy
        call appfnd
        ld de,App_PrcIDam4
        jr c,tskupdj
        call appnam0
        ex de,hl
tskupdj pop iy
        pop ix
        pop bc
        pop hl
tskupd8 ld (ix+2),e
        ld (ix+3),d
        ld de,10
        add ix,de
tskupd9 ld de,przdatlen
        add iy,de
        inc hl
        dec b
        jp nz,tskupd6
        ld a,c
        ld (prgprzlis),a
        ld a,(prgprzlis+9)
        set 6,a
        ld (prgprzlis+9),a
        ld a,(prgprzlis+2)
        cp c
        ret c
        xor a
        ld (prgprzlis+2),a
        ret

tskupd5 call tskupdb            ;*** Applications-Infos holen
        jp tskupdc
tskupdb call appnam
        ld de,256+1             ;E=1, D=prognum
        ld ix,tsklstrow         ;24*8
        ld hl,appdatmem         ;24*4
tskupdd push de
        push hl
        push ix
        ld iy,tskapphed
        rst #20:dw jmp_sysinf   ;(prgprzdat)=przdatmem + timdatmem (384 Bytes)
        pop ix                  ;IX=aktuelle TabZeile
        ld a,e
        or a
        jp z,tskupdg
        call appnam0
        ld (ix+2),l
        ld (ix+3),h
        ld hl,(tskapphed+prgpstnum)
        xor a
        srl h:rra
        srl h:rra
        srl h:rra
        or l
        ld (ix+0),a             ;store application[bit0-4] and process[bit5-9] ID
        ld (ix+1),h
        ld hl,tskapphed+prgpstspz
        xor a
        ld bc,4*256+1           ;C=Anzahl Prozesse
tskupde cp (hl)
        jr z,tskupdf
        inc c
tskupdf inc hl
        djnz tskupde
        ld (ix+4),c
        ld (ix+5),0
        pop hl                  ;HL=MemAdr
        ld (ix+6),l
        ld (ix+7),h
        xor a
        push hl
        xor a
        ld hl,(tskapphed+prgdatcod)
        ld de,(tskapphed+prgdatdat):add hl,de:adc 0
        ld de,(tskapphed+prgdattra):add hl,de:adc 0
        ld b,8
        ld iy,tskapphed+prgpstmem
tskupdi ld e,(iy+3)
        ld d,(iy+4)
        add hl,de
        adc 0
        ld de,5
        add iy,de
        djnz tskupdi
        ex de,hl
        pop hl
        ld (hl),e
        inc hl
        ld (hl),d
        inc hl
        ld (hl),a
        inc hl
        ld (hl),0
        inc hl
        ld bc,8
        add ix,bc
        pop de
        inc d
        ld a,24
        cp d
        jp nc,tskupdd
        jr tskupdh
tskupdg pop de
        pop de
tskupdh dec d                   ;D=Anzahl Programme (eines läuft immer -> Taskmanager)
        ld a,d
        ld (tskapplis),a
        ld a,(tskapplis+9)
        set 6,a
        ld (tskapplis+9),a
        ld a,(tskapplis+2)
        cp d
        ret c
        xor a
        ld (tskapplis+2),a
        ret

tskupd4 ld hl,jmp_memsum        ;*** Speicher-Infos holen
        rst #28                 ;E,IX=freier Speicher insgesamt, D=Anzahl verfügbarer Bänke
        ld l,d                  ;Gesamter Speicher
        ld h,0
        add hl,hl:add hl,hl:add hl,hl:add hl,hl:add hl,hl:add hl,hl
        ld bc,64
        add hl,bc               ;HL=Gesamter Speicher
        push hl
        ld a,3
        push hl
        push ix
        push de
        call tskdsp
        pop de                  ;Freier Speicher
        pop ix
        ld h,e
        db #dd:ld a,h
        ld l,a
        srl h:rr l
        srl h:rr l
        ld a,5
        push hl
        call tskdsp
        pop de                  ;Belegter Speicher (=Gesamt-Frei)
        pop hl
        or a
        sbc hl,de
        push hl
        ld a,4
        call tskdsp
        pop de                  ;DE=Belegt
        ld a,50
        call clcm16             ;HL=Belegt*50
        ld c,l
        ld b,h
        pop de
        srl d:rr e
        call clcd16             ;HL=Belegt*50/(Gesamt/2)=MemNutzung in %
        ld a,l
        cp 101
        jr c,tskupd3
        ld a,100
tskupd3 ld l,a
        ld h,0
        push hl
        call tskadj             ;A=A*48/100
        ld (tskupdmem),a        ;merken
        pop ix                  ;IX=Wert
        ld iy,tskobjtxt13       ;(IY)=Ascii-String
        ld e,3
        ld hl,jmp_clcnum
        rst #28                 ;Wert umwandeln
        ld (iy+1),"%"
        ld (iy+2),0

        ld hl,jmp_sysinf        ;*** Programm-Infos holen
        ld e,0
        rst #28                 ;E=Anzahl Prozesse, D=Anzahl Timer, IXL=Anzahl Programme, IXH=Anzahl Filehandlers
        ld l,d                  ;Timer
        ld h,0
        push ix
        push de
        ld a,2
        call tskdsp
        pop hl                  ;Prozesse
        ld h,0
        ld a,1
        call tskdsp
        pop hl                  ;Programme
        ld h,0
        xor a
        call tskdsp
        ld d,12
        ld a,-4
        jr tskdsp1              ;Kurven anzeigen

;### TSKADJ -> Prozent in Kurven-Position umrechnen
;### Eingabe    A=Prozent (0-100)
;### Eingabe    A=Kurvenposition (A=A*48/100)
;### Veraendert F,BC,DE,HL
tskadj  srl a
        sub 1
        jr nc,tskadj1
        xor a
tskadj1 cp 48
        ret c
        ld a,47
        ret

;### TSKDSP -> Wert-Anzeige updaten, falls geändert
;### Eingabe    HL=neuer Wert, A=Typ (0=Apps, 1=Tasks, 2=Timer, 3=MemGesamt, 4=MemUsed, 5=MemFre)
;### Veraendert AF,BC,DE,HL,IX,IY
tskdspb dw 0,tskobjtxt3n+4,0,tskobjtxt4n+4,0,tskobjtxt5n+4,0,tskobjtxt6n+4,0,tskobjtxt7n+4,0,tskobjtxt8n+4
tskdspt db 0
tskdsp  ld (tskdspt),a
        add a
        add a
        ex de,hl                ;DE=neuer Wert
        ld l,a
        ld h,0
        ld bc,tskdspb
        add hl,bc               ;(HL)=alter Wert
        push hl
        ld a,(hl)               ;*** Wert vergleichen
        inc hl
        ld h,(hl)
        ld l,a
        or a
        sbc hl,de
        pop hl
        ret z
        ld (hl),e               ;*** Wert speichern
        inc hl
        ld (hl),d
        inc hl
        ld c,(hl)               ;*** Wert in String umwandeln
        inc hl
        ld b,(hl)               ;BC=Anzeige
        push de
        pop ix                  ;IX=Wert
        push bc
        pop iy                  ;(IY)=Ascii-String
        ld e,4
        ld hl,jmp_clcnum
        rst #28                 ;Wert umwandeln
        ld a,(tskdspt)
        add 16
;A=Upzudatendes Objekt
tskdsp1 ld e,a
        ld a,(prgwin)
tskdsp2 jp SyDesktop_WINDIN


;### TSKCUR -> Updated eine Auslastungskurve
;### Eingabe    A=Kurvennummer, E=neuer Wert (0-100)
;### Veraendert AF,BC,DE,HL,IX
tskcurbeg   equ 0
tskcurzgr   equ 2
tskcurpos   equ 4
tskcurmax   equ 39
tskcurdat   dw tsksprcpu+3,tskdatobj1,0,0
            dw tsksprmem+3,tskdatobj2,0,0
tskcurval   db 0,0
tskcurold   ds 4
tskcurpix   dw 0

tskzgrlin   dw tskcurlin0
tskzgrbox   dw tskcurbox0
tskzgrdot   dw tskcurdot0

tskcurlin0  db #FF,#FF  ;Matrix Linie       CPC
tskcurbox0  db #F8,#F0  ;Matrix Kästchen
tskcurdot0  db #77,#BB,#DD,#EE
tskcurlin1  db #FF,#FF  ;Matrix Linie       MSX
tskcurbox1  db #d5,#55  ;Matrix Kästchen
tskcurdot1  db #3f,#cf,#f3,#fc

tskcur  add a
        push af
        ld ix,tskcurold
        ld c,a
        ld b,0
        add ix,bc
        ld a,(ix+0)
        ld (ix+1),a             ;A=alter (sollte kleiner sein), E=neuer Wert
        ld (ix+0),e
        cp e
        jr c,tskcur7
        jr z,tskcur8
        ld d,a                  ;alter größer -> A=neuer, E=alter-1
        ld a,e
        ld e,d
        dec e
        jr tskcur8
tskcur7 inc a                   ;neuer größer -> A=alter+1, E=neuer
tskcur8 ld (tskcurval),a
        ld a,e
        inc a
        ld (tskcurval+1),a
        pop af
        add a:add a
        db #dd:ld h,0
        db #dd:ld l,a
        ld bc,tskcurdat
        add ix,bc
        ld a,(ix+tskcurpos)
        sub 1                   ;Rückwärtsscrollen, damit neue Pixel oben erscheinen
        jr nc,tskcur1
        ld a,tskcurmax
tskcur1 ld (ix+tskcurpos),a
        push af
        add a
        add a
        ld l,a
        ld h,0
        ld c,l
        ld b,h
        add hl,bc
        add hl,bc               ;HL=pos*12
        ld c,(ix+tskcurbeg)
        ld b,(ix+tskcurbeg+1)
        add hl,bc
        pop af
        and 7
        ld de,(tskzgrlin)
        jr z,tskcur2
        ld de,(tskzgrbox)
tskcur2 push hl
        call tskcur3
        ld bc,tskcurmax+1*12
        add hl,bc
        call tskcur3
        pop hl
        dec hl
        ld (hl),tskcurmax+1
        dec hl
        ld (hl),48
        dec hl
tskcur9 ld (hl),12
        ex de,hl
        ld l,(ix+tskcurzgr)
        ld h,(ix+tskcurzgr+1)
        ld (hl),e
        inc hl
        ld (hl),d
        ret

;HL=Ziel, DE=Quelle
tskcur3 push hl
        ld b,6
        ex de,hl
tskcur4 ldi
        ldi
        dec hl
        dec hl
        djnz tskcur4
        ex de,hl                ;DE=Quelle
        ld bc,(tskcurval)       ;C=erster, B=letzter+1
        pop hl                  ;HL=Ziel
        push hl
        push de
tskcur6 push bc
        push hl
        ld a,c
        ld b,a                  ;B=Start
        srl a
        srl a                   ;A=Start/4
        ld c,a
        ld a,b                  ;A=Start
        ld b,0                  ;BC=Start/4
        add hl,bc
        ex de,hl                ;DE=Ziel
        and 3
        ld c,a
        ld hl,(tskzgrdot)
        add hl,bc               ;HL=Quelle
        ld a,(de)
        and (hl)
        ld (de),a
        pop hl
        pop bc
        ld a,c
        inc a
        ld c,a
        cp b
        jr nz,tskcur6
        pop de
        pop hl
        ret

;### TSKINI -> Taskmanager initialisieren
tskini  ld hl,jmp_sysinf        ;*** Computer-Typ holen
        ld de,256*1+5
        ld ix,cfgcpctyp
        ld iy,66+2+6+8
        rst #28
        ld a,(cfgcpctyp)
        bit 7,a
        db #dd:ld l,12
        ld hl,tskcurlin0
        ld de,tskcurbox0
        ld bc,tskcurdot0
        jr z,tskini1
        ld hl,tsksprcpu
        call sprcnv
        ld hl,tsksprmem
        call sprcnv
        db #dd:ld l,12+128
        ld hl,tskcurlin1
        ld de,tskcurbox1
        ld bc,tskcurdot1
tskini1 db #dd:ld a,l
        ld (tskcur9+1),a
        ld (tskzgrlin),hl
        ld (tskzgrbox),de
        ld (tskzgrdot),bc
        ret

;### TSKMEN -> Führt Menü-Befehle aus
tskspd1 ld hl,1                 ;*** Geschwindigkeiten setzen
        ld a,1
        jr tskspd0
tskspd2 ld hl,2
        ld a,2
        jr tskspd0
tskspd3 ld hl,4
        ld a,3
        jr tskspd0
tskspd4 ld hl,4
        ld a,4
tskspd0 ld (tskupddel),hl
        ld (tskupdtim),hl
        ld b,4
        ld hl,prgdatmen4+2
        ld de,8
tskspd5 dec a
        ld c,1+2
        jr z,tskspd6
        ld c,1
tskspd6 ld (hl),c
        add hl,de
        djnz tskspd5
        ld (tskupdstp),a
        jp prgprz0

tskupf  ld hl,prgdatmen2+18     ;*** Focus-Update-Option
        ld a,(hl)
        xor 2
        ld (hl),a
        cp 1
        ld hl,SyDesktop_WINDIN
        jr z,tskupf1
        ld hl,SyDesktop_WININH
tskupf1 ld (tskdsp2+1),hl
        jp prgprz0

tskrun  ld c,MSC_SYS_PRGSTA     ;*** Neuen Task ausführen
        call SySystem_SendMessage
        jp prgprz0

;### TSKCHG -> Prüft, ob Änderung in Prozess/Programm-Anzahl
;### Ausgabe    ZF=1 -> keine Änderung -> ZF=0 -> Änderung, CF=1 AppTab, CF=0 PrzTab
tskchgv ds 4

tskchg  ld hl,jmp_sysinf
        ld e,0
        rst #28                 ;E=Anzahl Prozesse, D=Anzahl Timer, IXL=Anzahl Programme, IXH=Anzahl Filehandlers
        ld hl,(tskchgv)
        or a
        sbc hl,de
        jr z,tskchg0
        ld (tskchgv),de
        ld (tskchgv+2),ix
        jr tskchg2
tskchg0 push ix
        pop de
        ld hl,(tskchgv+2)
        or a
        sbc hl,de
        jr z,tskchg1
        ld (tskchgv+2),de
        jr tskchg2
tskchg1 ld a,(tsklstupd)
        or a
        ret z
tskchg2 xor a
        ld (tsklstupd),a
        ld a,(tskacttab)
        cp 1
        ret c
        or a
        ret


;==============================================================================
;### APPLIACTION ROUTINES #####################################################
;==============================================================================

symextwgm   equ 11 
wingrpmax   equ 32
windatsta   equ 0

;### Application switch
appswt  ld de,8
        ld a,(tskapplis)
        call getlid
        jp c,prgprz0
        ld a,(hl)
        inc hl
        ld l,(hl)
        add a:rl l
        add a:rl l
        add a:rl l
        ld a,l
        and 31
        ld (appswt2+1),a
        ld e,7
        ld hl,jmp_sysinf
        rst #28                 ;IYL=Databank
        ld a,iyl
        push af
        ld e,8
        ld hl,jmp_sysinf
        rst #28
        pop af
        push iy:pop hl
        ld de,32
        add hl,de               ;hl=pointer to desktop extended vars
        rst #20:dw jmp_bnkrwd
        ld hl,symextwgm
        add hl,bc
        xor a
        rst #20:dw jmp_bnkrwd
        ld l,c
        ld h,b
        ld b,wingrpmax
appswt1 push bc
        xor a
        rst #20:dw jmp_bnkrwd   
        push bc
        rst #20:dw jmp_bnkrwd   ;bc=adr
        pop de                  ;e=status, d=bank
        push hl
        inc e:dec e
        jr z,appswt3
        ld hl,windatprz
        add hl,bc
        ld a,d
        rst #20:dw jmp_bnkrbt   ;b=prozessID of window
        ld a,b
appswt2 cp 0
        jr z,appswt4
appswt3 pop hl
        pop bc
        djnz appswt1
        jp prgprz0
appswt4 ld bc,windatsta-windatprz-1
        add hl,bc
        ld a,d
        rst #20:dw jmp_bnkrbt   ;b=status
        pop hl
        pop de
        ld a,wingrpmax
        sub d                   ;a=window ID
        dec b:dec b:dec b
        jr nz,appswt5
        call SyDesktop_WINMID   ;window was minimized -> restore
        jp prgprz0
appswt5 call SyDesktop_WINTOP   ;window was open -> set to top
        jp prgprz0

;### Application end
append  ld de,8
        ld a,(tskapplis)
        call getlid
        jp c,prgprz0
        and 31
append1 ld l,a
        call SySystem_PRGEND
        jp prgprz0

;### APPFND -> find application via process ID
;### Input      A=process ID
;### Output     CF=0 -> A=application ID
;###            CF=1 -> not found
;### Destroyed  F,BC,DE,HL,IX,IY
appfnd  ld (appfnd2+1),a
        ld de,256+1
appfnd1 push de
        ld iy,tskapphed
        rst #20:dw jmp_sysinf           ;get 90byte header
        ld a,e
        sub 1
        pop de
        ret c                           ;cf=1 here, not found
appfnd2 ld a,0
        ld hl,tskapphed+prgpstprz
        cp (hl)
        jr z,appfnd4
        ld hl,tskapphed+prgpstspz
        ld b,4
appfnd3 cp (hl)
        jr z,appfnd4
        inc hl
        djnz appfnd3
        inc d
        ld a,24
        cp d
        jr nc,appfnd1
        ret                             ;cf=1 here, not found
appfnd4 ld a,(tskapphed+prgpstnum)      ;cf=0 here, A=appID
        ret

;### APPNAM -> get names from all applications
;### Output     (tskappnam)=filled
appnam  ld de,256+1
appnam1 push de
        ld iy,tskapphed
        rst #20:dw jmp_sysinf           ;get 90byte header
        ld a,e
        or a
        jp z,appnam2
        call appnam0
        ex de,hl
        ld hl,tskapphed+prgdatnam
        ld c,19
        ldir                            ;copy name
        xor a
        ld (de),a
        pop de
        inc d
        ld a,24
        cp d
        jr nc,appnam1
        ret
appnam2 pop de
        ret
appnam0 ld a,(tskapphed+prgpstnum)      ;a=appID -> get name address in list
        ld c,a
        add a:add a:add c
        add a:add a                     ;a=a*20
        ld c,a
        ld b,0
        ld hl,tskappnam
        add hl,bc
        ret


;==============================================================================
;### PROCESS ROUTINES #########################################################
;==============================================================================

;### Process context menu
przent  ld a,(App_MsgBuf+3)
        dec a
        jp nz,prgprz0
        ld de,10
        ld a,(prgprzlis)
        call getlid
        jp c,prgprz0
        ld (przent3+1),a
        ld de,6
        add hl,de
        ld a,(hl)               ;a=priority
        or a
        jp z,prgprz0            ;don't change idle priority
        cp 8
        jp z,prgprz0            ;don't change kernel priority
        ld hl,przdatmen1+2
        ld b,7
        ld de,8
przent1 res 1,(hl)              ;reset all prio checks
        dec a
        jr nz,przent2
        set 1,(hl)              ;check only actual prio
przent2 add hl,de
        djnz przent1
        ld a,(App_BnkNum)
        ld de,przdatmen
        ld hl,-1
        call SyDesktop_MENCTX   ;open context menu
        jp c,prgprz0
        dec c
        jp z,prgprz0
        ld a,h
        or a
        jr z,przent3
        jp (hl)                 ;call process routine
przent3 ld d,0
        ld e,l
        push de
        ld hl,przpridat
        ld b,2+24+64+128
        call prginf0
        pop de
        cp 3
        jp nz,prgprz0
        ld a,d
        call SyKernel_MTPRIO    ;change process prio
        jr przslp1

;### Process sleep
przslp  ld de,10
        ld a,(prgprzlis)
        call getlid
        call nc,SyKernel_MTSLPP
przslp1 rst #30
        jp tskref

;### Process kill
przkil  ld de,10
        ld a,(prgprzlis)
        call getlid
        jp c,prgprz0
        push af
        call appfnd
        pop bc
        jp nc,append1
        ld a,b
        call SyKernel_MTDELP
        jp prgprz0


;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

;### GETLID -> get ID from selected list entry
;### Input      DE=list width, A=number of entries
;### Output     CF=0 -> A=ID, HL=1st row, CF=1 -> nothing selected
getlid  ld b,a
        ld hl,tsklstrow+1
getlid1 bit 7,(hl)
        jr nz,getlid2
        add hl,de
        djnz getlid1
        scf
        ret
getlid2 dec hl
        ld a,(hl)
        or a
        ret

;### SPRCNV -> Konvertiert Sprite vom CPC ins MSX Format
;### Eingabe    IX=Sprite (inklusive Header)
;### Veraendert BC
sprcnv  set 7,(hl)
        ld de,12*80
        inc hl:inc hl
sprcnv1 inc hl
        ld c,(hl)
        xor a:rl c:rla
        add a:rl c:rla
        add a:rl c:rla
        add a:rl c:rla
        ld b,a
        rl c:rla:add a
        rl c:rla:add a
        rl c:rla:add a
        rl c:rla:add a
        or b
        ld (hl),a
        dec de
        ld a,e
        or d
        jr nz,sprcnv1
        ret

;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### Eingabe    A=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1*Wert2 (16bit)
;### Veraendert AF,DE
clcm16  ld hl,0
        or a
clcm161 rra
        jr nc,clcm162
        add hl,de
clcm162 sla e
        rl d
        or a
        jr nz,clcm161
        ret

;### CLCD16 -> Dividiert zwei Werte (16bit)
;### Eingabe    BC=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1/Wert2 (16bit)
;### Veraendert AF,BC,DE
clcd16  ld a,e
        or d
        ld hl,0
        ret z
        ld a,b
        ld b,16
clcd161 rl c
        rla
        rl l
        rl h
        sbc hl,de
        jr nc,clcd162
        add hl,de
clcd162 ccf
        djnz clcd161
        rl c
        rla
        ld h,a
        ld l,c
        ret

;### CLCMUL -> Multipliziert zwei Werte (24bit)
;### Eingabe    BC=Wert1, DE=Wert2
;### Ausgabe    A,HL=Wert1*Wert2 (24bit)
;### Veraendert F,BC,DE,IX
clcmul  ld ix,0
        ld hl,0
clcmul1 ld a,c
        or b
        jr z,clcmul3
        srl b
        rr c
        jr nc,clcmul2
        add ix,de
        ld a,h
        adc l
        ld h,a
clcmul2 sla e
        rl d
        rl l
        jr clcmul1
clcmul3 ld a,h
        db #dd:ld e,l
        db #dd:ld d,h
        ex de,hl
        ret

;### CLCDIV -> Dividiert zwei Werte (24bit)
;### Eingabe    A,BC=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1/Wert2
;### Veraendert AF,BC,DE,IX,IY
clcdiv  db #dd:ld l,e
        db #dd:ld h,d   ;IX=Wert2(Nenner)
        ld e,a          ;E,BC=Wert1(Zaehler)
        ld hl,0
        db #dd:ld a,l
        db #dd:or h
        ret z
        ld d,l          ;D,HL=RechenVar
        db #fd:ld l,24  ;IYL=Counter
clcdiv1 rl c
        rl b
        rl e
        rl l
        rl h
        rl d
        ld a,l
        db #dd:sub l
        ld l,a
        ld a,h
        db #dd:sbc h
        ld h,a
        ld a,d
        sbc 0
        ld d,a          ;D,HL=D,HL-IX
        jr nc,clcdiv2
        ld a,l
        db #dd:add l
        ld l,a
        ld a,h
        db #dd:adc h
        ld h,a
        ld a,d
        adc 0
        ld d,a
        scf
clcdiv2 ccf
        db #fd:dec l
        jr nz,clcdiv1
        rl c
        rl b
        ld l,c
        ld h,b
        ret


;==============================================================================
;### DATEN-TEIL ###############################################################
;==============================================================================

App_BegData

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#81,#11,#11,#11,#11,#11,#11,#11,#11,#88,#88,#88,#1D,#DD,#DD,#DD,#DD,#DD,#DD,#DD,#61,#38,#88,#81,#DD,#DD,#DD,#DD,#DD,#DD,#DD,#D6,#11,#38
db #88,#16,#66,#66,#66,#66,#66,#66,#66,#61,#11,#38,#88,#16,#D1,#55,#55,#55,#55,#55,#51,#D1,#11,#38,#88,#16,#D5,#55,#55,#55,#55,#55,#55,#D1,#11,#38,#88,#16,#D5,#55,#55,#55,#55,#55,#55,#D1,#11,#38
db #88,#16,#D5,#55,#5A,#55,#55,#55,#55,#D1,#11,#38,#88,#16,#D5,#55,#A5,#A5,#55,#55,#55,#D1,#11,#38,#88,#16,#D1,#AA,#55,#5A,#55,#5A,#A1,#D1,#11,#38,#88,#16,#D5,#55,#55,#55,#A5,#A5,#55,#D1,#11,#38
db #88,#16,#D5,#55,#55,#55,#5A,#55,#55,#D1,#11,#38,#88,#16,#D5,#55,#55,#55,#55,#55,#55,#D1,#11,#38,#88,#16,#D1,#55,#55,#55,#55,#55,#51,#D1,#11,#38,#88,#16,#DD,#DD,#DD,#DD,#DD,#DD,#DD,#D1,#13,#18
db #88,#11,#11,#11,#11,#11,#11,#11,#11,#11,#31,#13,#83,#33,#33,#33,#33,#33,#33,#33,#33,#33,#11,#13,#1D,#DD,#DD,#DD,#DD,#DD,#DD,#DD,#DD,#DD,#11,#13,#1D,#66,#66,#66,#66,#66,#66,#66,#66,#66,#11,#13
db #1D,#61,#16,#6F,#FF,#FF,#66,#FF,#FF,#F6,#11,#13,#1D,#66,#66,#66,#66,#16,#66,#66,#61,#66,#11,#38,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#13,#88,#83,#33,#33,#33,#33,#33,#33,#33,#33,#33,#38,#88

;### GENERAL ##################################################################

prgmsginf1 db "SymbOS TASK MANAGER",0
prgmsginf2 db " Version 2.0 (Build 220115pdt)",0
prgmsginf3 db " Copyright <c> 2022 SymbiosiS",0

przpritxt1 db "Do you want to change the priority?",0
przpritxt2 db " Changing the proirity could",0
przpritxt3 db " cause system instability.",0

tskdattit db "Task Manager",0

prgdatmentx1  db "File",0
    prgdatmen1tx1 db "New Task (Run...)",0
    prgdatmen1tx2 db "Exit Task Manager",0

prgdatmentx2  db "View",0
    prgdatmen2tx1 db "Refresh Now",0
    prgdatmen2tx2 db "Update Speed",0
        prgdatmen4tx1 db "High",0
        prgdatmen4tx2 db "Normal",0
        prgdatmen4tx3 db "Low",0
        prgdatmen4tx4 db "Paused",0
    prgdatmen2tx3 db "Only when focus",0

prgdatmentx3  db "?",0
    prgdatmen3tx1 db "Help Topics",0
    prgdatmen3tx2 db "About",0

;### PROCESS CONTEXT MENU #####################################################

przdatmentx1    db "Kill process",0
przdatmentx2    db "Send to sleep",0
przdatmentx3    db "Set priority",0
    przdatmen1tx1   db "Realtime",0
    przdatmen1tx2   db "High",0
    przdatmen1tx3   db "Above normal",0
    przdatmen1tx4   db "Normal",0
    przdatmen1tx5   db "Below normal",0
    przdatmen1tx6   db "Low",0
    przdatmen1tx7   db "Background",0

;### PERFORMANCE ##############################################################

tsktabtxt1 db "Apps.",0
tsktabtxt2 db "Proces.",0
tsktabtxt3 db "Performance",0

tskobjtxt1  db "Totals",0
tskobjtxt2  db "Memory (KB)",0
tskobjtxt3  db "Applications",0
tskobjtxt4  db "Processes",0
tskobjtxt5  db "Timers",0
tskobjtxt6  db "Total",0
tskobjtxt7  db "Used",0
tskobjtxt8  db "Available",0
tskobjtxt10 db "CPU",0
tskobjtxt11 db "Memory",0

tsksprcpu   db 12,48,40
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0

tsksprmem   db 12,48,40
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0
db #F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0,#F8,#F0

;### PROCESSES ################################################################

prgprzrow1 db "Name",0
prgprzrow2 db "ID",0
prgprzrow3 db "Pri.",0
prgprzrow4 db "Status",0

tskapprow3 db "Prc.",0
tskapprow4 db "Mem.",0

prgprzsta0 db "Timer",0
prgprzsta1 db "Sleep",0
prgprzsta2 db "Idle",0
prgprzsta3 db "Work",0

App_PrcIDam0 db "IDLE",0
App_PrcIDam1 db "KERNEL",0
App_PrcIDam2 db "DESKTOP",0
App_PrcIDam3 db "SYSTEM",0
App_PrcIDam4 db "Application",0

prgprzbut1 db "Refresh",0
prgprzbut2 db "Sleep",0
prgprzbut3 db "Kill",0
tskappbut2 db "Switch",0
tskappbut3 db "End App.",0


;==============================================================================
;### TRANSFER-TEIL ############################################################
;==============================================================================

App_BegTrns
;### Stack für Taskmanager-Prozess
            ds 128
prgstk      ds 6*2
            dw prgprz
App_PrcID   db 0            ;Nummer des Prozesses
App_MsgBuf  ds 14           ;Message-Buffer

;### FENSTER ##################################################################

tskdatwin   dw #3501,#0000,187,8,128,150,0,0,128,150,128,150,128,150,tskicnsml,tskdattit,0,prgdatmen
tskdatwin0  dw tskdatgrp3,0,0:ds 136+14

prgdatmen   dw 3, 1+4,prgdatmentx1,prgdatmen1,0, 1+4,prgdatmentx2,prgdatmen2,0, 1+4,prgdatmentx3,prgdatmen3,0
prgdatmen1  dw 3, 1,prgdatmen1tx1,tskrun,0,  1+8,#0000,prgprz0,0,            1,prgdatmen1tx2,prgend,0   ;new task/-/exit
prgdatmen2  dw 3, 1,prgdatmen2tx1,tskref,0,  1+4,prgdatmen2tx2,prgdatmen4,0, 1,prgdatmen2tx3,tskupf,0   ;update/update speed/only when focus
prgdatmen3  dw 3, 1,prgdatmen3tx1,prghlp,0,  1+8,#0000,prgprz0,0,            1,prgdatmen3tx2,prginf,0   ;helptopics/-/info
prgdatmen4  dw 4, 1,prgdatmen4tx1,tskspd1,0, 1+2,prgdatmen4tx2,tskspd2,0,    1,prgdatmen4tx3,tskspd3,0, 1,prgdatmen4tx4,tskspd4,0 ;high/normal/low/stop

przdatmen   dw 4, 1,przdatmentx1,przkil,0,  1,przdatmentx2,przslp,0, 1+8,#0000,prgprz0,0, 1+4,przdatmentx3,przdatmen1,0
przdatmen1  dw 7, 1,przdatmen1tx1,1,0, 1,przdatmen1tx2,2,0, 1,przdatmen1tx3,3,0, 1,przdatmen1tx4,4,0, 1,przdatmen1tx5,5,0, 1,przdatmen1tx6,6,0, 1,przdatmen1tx7,7,0

tskdatgrp1  db  6,0:dw tskdatobja,0,0,0,0,0,0
tskdatgrp2  db  6,0:dw tskdatobjb,0,0,0,0,0,0
tskdatgrp3  db 22,0:dw tskdatobjc,0,0,0,0,0,0

;Shared
tskappnam   ds 20*24            ;application names
tskapphed   ds 90               ;temp application header

tsklstrow   ds 5*2*64               ;640 = 5*2*64       process (640)/application (192) list rows
prgprzdat   equ tsklstrow+640-384   ;384 = 2*32*6+32    przdatmem+timdatmem
appdatmem   equ prgprzdat           ; 96                app length table (24*4)

;Applications
tskdatobja
dw      0,255*256+0,2,           0,0,1000,1000,0    ;00=Hintergrund
dw tsktab,255*256+20,tsktabdat,   0,  1,128,11,0    ;01=Tab-Leiste
dw      0,255*256+43,tskapplis,   3,16,122,116,0    ;02=Liste Applicationen
dw tskref,255*256+16,prgprzbut1,   3,135,39,12,0    ;35="Refresh"-Button
dw appswt,255*256+16,tskappbut2,  45,135,38,12,0    ;36="Switch To"-Button
dw append,255*256+16,tskappbut3,  86,135,39,12,0    ;37="End"-Button

tskapplis  dw 1,0,tsklstrow,0,256*0+3,tskapprow,0,1
tskapprow
dw 0+0,63,prgprzrow1,0
dw 8+2,19,tskapprow3,0
dw 12+1,32,tskapprow4,0

;Processes
tskdatobjb
dw      0,255*256+0,2,           0,0,1000,1000,0    ;00=Hintergrund
dw tsktab,255*256+20,tsktabdat,   0,  1,128,11,0    ;01=Tab-Leiste
dw przent,255*256+43,prgprzlis,   3,16,122,116,0    ;02=Liste Prozesse
dw tskref,255*256+16,prgprzbut1,   3,135,39,12,0    ;32="Refresh"-Button
dw przslp,255*256+16,prgprzbut2,  45,135,38,12,0    ;33="Sleep"-Button
dw przkil,255*256+16,prgprzbut3,  86,135,39,12,0    ;34="Kill"-Button

prgprzlis   dw 1,0,tsklstrow,0,256*1+4,prgprzrow,0,1
prgprzrow
dw 0+0,54,prgprzrow1,0
dw 8+2,16,prgprzrow2,0
dw 8+2,16,prgprzrow3,0
dw 0+2,28,prgprzrow4,0

;Performance
tskdatobjc
dw      0,255*256+0,2,           0,0,1000,1000,0    ;00=Hintergrund
dw tsktab,255*256+20,tsktabdat,   0,  1,128,11,0    ;01=Tab-Leiste

dw      0,255*256+3,tskobjdat1,   0, 70,128,40,0    ;02=Rahmen Prozesse
dw      0,255*256+3,tskobjdat2,   0,110,128,40,0    ;03=Rahmen Speicher

dw      0,255*256+1,tskobjdat3,  10, 79,108,8,0     ;04=Anwendungen
dw      0,255*256+1,tskobjdat4,  10, 87,108,8,0     ;05=Prozesse
dw      0,255*256+1,tskobjdat5,  10, 95,108,8,0     ;06=Filehandler
dw      0,255*256+1,tskobjdat6,  10,119,108,8,0     ;07=Total
dw      0,255*256+1,tskobjdat7,  10,127,108,8,0     ;08=Used
dw      0,255*256+1,tskobjdat8,  10,135,108,8,0     ;09=Free
dw      0,255*256+3,tskobjdat10,  0, 14,64,56,0     ;10=Kurve CPU
dw      0,255*256+3,tskobjdat11, 64, 14,64,56,0     ;11=Kurve Speicher

dw      0,255*256+8:tskdatobj1 dw tsksprcpu,   8,22,48,40,0   ;12=Grafik CPU
dw      0,255*256+1,              tskobjdat12, 8,54,48, 8,0   ;13=Anzeige CPU
dw      0,255*256+8:tskdatobj2 dw tsksprmem,  72,22,48,40,0   ;14=Grafik Speicher
dw      0,255*256+1,              tskobjdat13,72,54,48, 8,0   ;15=Anzeige Speicher

dw      0,255*256+1,tskobjdat3n, 93, 79,25,8,0      ;16=Anwendungen
dw      0,255*256+1,tskobjdat4n, 93, 87,25,8,0      ;17=Prozesse
dw      0,255*256+1,tskobjdat5n, 93, 95,25,8,0      ;18=Timer
dw      0,255*256+1,tskobjdat6n, 93,119,25,8,0      ;19=Total
dw      0,255*256+1,tskobjdat7n, 93,127,25,8,0      ;20=Used
dw      0,255*256+1,tskobjdat8n, 93,135,25,8,0      ;21=Free

tsktabdat  db 3,2+4+48+64
tsktabdat0 db 2:dw tsktabtxt1:db -1:dw tsktabtxt2:db -1:dw tsktabtxt3:db -1

tskobjdat1 dw tskobjtxt1,2+4
tskobjdat2 dw tskobjtxt2,2+4

tskobjdat10 dw tskobjtxt10,2+4
tskobjdat11 dw tskobjtxt11,2+4
tskobjdat12 dw tskobjtxt12,1+256 ;+128
tskobjdat13 dw tskobjtxt13,1+256 ;+128

tskobjdat3  dw tskobjtxt3 :dw 2+4
tskobjdat3n dw tskobjtxt3n:dw 2+4+256
tskobjdat4  dw tskobjtxt4 :dw 2+4
tskobjdat4n dw tskobjtxt4n:dw 2+4+256
tskobjdat5  dw tskobjtxt5 :dw 2+4
tskobjdat5n dw tskobjtxt5n:dw 2+4+256
tskobjdat6  dw tskobjtxt6 :dw 2+4
tskobjdat6n dw tskobjtxt6n:dw 2+4+256
tskobjdat7  dw tskobjtxt7 :dw 2+4
tskobjdat7n dw tskobjtxt7n:dw 2+4+256
tskobjdat8  dw tskobjtxt8 :dw 2+4
tskobjdat8n dw tskobjtxt8n:dw 2+4+256

;place in transfer area because of os-clcnum-call
tskobjtxt3n db "      11",0
tskobjtxt4n db "      99",0
tskobjtxt5n db "       9",0
tskobjtxt6n db "     000",0
tskobjtxt7n db "     000",0
tskobjtxt8n db "     000",0
tskobjtxt12 db "100%",0
tskobjtxt13 db "100%",0

;Info/Warning boxes
prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,tskicnbig
przpridat  dw przpritxt1,4*1+2,przpritxt2,4*1+2,przpritxt3,4*1+2,tskicnbig

cfgcpctyp   db 0

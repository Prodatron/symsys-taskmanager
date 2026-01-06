nolist

org #1000

WRITE "f:\symbos\taskmgr.exe"
READ "..\..\..\SRC-Main\SymbOS-Constants.asm"

relocate_start

App_BegCode

;### APPLICATION HEADER #######################################################

;header structure
prgdatcod       equ 0           ;Length of the code area (OS will place this area everywhere)
prgdatdat       equ 2           ;Length of the data area (screen manager data; OS will place this area inside a 16k block of one 64K bank)
prgdattra       equ 4           ;Length of the transfer area (stack, message buffer, desktop manager data; placed between #c000 and #ffff of a 64K bank)
prgdatorg       equ 6           ;Original origin of the assembler code
prgdatrel       equ 8           ;Number of entries in the relocator table
prgdatstk       equ 10          ;Length of the stack in bytes
prgdatrs1       equ 12          ;*reserved* (3 bytes)
prgdatnam       equ 15          ;program name (24+1[0] chars)
prgdatflg       equ 40          ;flags (+1=16colour icon available)
prgdat16i       equ 41          ;file offset of 16colour icon
prgdatrs2       equ 43          ;*reserved* (5 bytes)
prgdatidn       equ 48          ;"SymExe10" SymbOS executable file identification
prgdatcex       equ 56          ;additional memory for code area (will be reserved directly behind the loaded code area)
prgdatdex       equ 58          ;additional memory for data area (see above)
prgdattex       equ 60          ;additional memory for transfer area (see above)
prgdatres       equ 62          ;*reserviert* (26 bytes)
prgdatver       equ 88          ;required OS version (1.0)
prgdatism       equ 90          ;Application icon (small version), 8x8 pixel, SymbOS graphic format
prgdatibg       equ 109         ;Application icon (big version), 24x24 pixel, SymbOS graphic format
prgdatlen       equ 256         ;length of header

prgpstdat       equ 6           ;start address of the data area
prgpsttra       equ 8           ;start address of the transfer area
prgpstspz       equ 10          ;additional sub process or timer IDs (4*1)
prgpstbnk       equ 14          ;64K ram bank (1-8), where the application is located
prgpstmem       equ 48          ;additional memory areas; 8 memory areas can be registered here, each entry consists of 5 bytes
                                ;00  1B  Ram bank number (1-8; if 0, the entry will be ignored)
                                ;01  1W  Address
                                ;03  1W  Length
prgpstnum       equ 88          ;Application ID
prgpstprz       equ 89          ;Main process ID

            dw App_BegData-App_BegCode  ;length of code area
            dw App_BegTrns-App_BegData  ;length of data area
            dw App_EndTrns-App_BegTrns  ;length of transfer area
prgdatadr   dw #1000                ;original origin                    POST address data area
prgtrnadr   dw relocate_count       ;number of relocator table entries  POST address transfer area
prgprztab   dw prgstk-App_BegTrns   ;stack length                       POST table processes
            dw 0                    ;*reserved*
App_BnkNum  db 0                    ;*reserved*                         POST bank number
            db "Task Manager":ds 12:db 0 ;Name
            db 1                    ;flags (+1=16c icon)
            dw prgicn16c-App_BegCode ;16 colour icon offset
            ds 5                    ;*reserved*
prgmemtab   db "SymExe10"           ;SymbOS-EXE-identifier              POST table reserved memory areas
            dw 0                    ;additional code memory
            dw 0                    ;additional data memory (temp text + actual font)
            dw 0                    ;additional transfer memory
            ds 26                   ;*reserviert*
            db 1,4                  ;required OS version (4.0)

tskicnsml   db 2,8,8,#77,#EE,#F8,#F1,#E9,#F1,#DA,#79,#BC,#5B,#F8,#B5,#F8,#F1,#77,#EE
tskicnbig   db 6,24,24
            db #3f,#ff,#ff,#ff,#ff,#fe,#6f,#0f,#0f,#0f,#0f,#3a,#cc,#00,#00,#00,#00,#fe,#8b,#0f,#0f,#0f,#0f,#fe,#9b,#ff,#ff,#ff,#cd,#fe,#9a,#f0,#f0,#f0,#c5,#fe,#9a,#f0,#e0,#f0,#c1,#fe,#9a,#f0,#60,#f0,#c1,#fe
            db #9a,#e0,#50,#70,#c1,#fe,#8a,#00,#50,#00,#01,#fe,#9a,#f0,#90,#f0,#c1,#fe,#9a,#f0,#b0,#f0,#c1,#fe,#9b,#f0,#f0,#f0,#c1,#fe,#8a,#00,#00,#00,#01,#fd,#8b,#0f,#0f,#0f,#0f,#fa,#ff,#ff,#ff,#ff,#ff,#d6
            db #f0,#f0,#f0,#f0,#f0,#be,#ff,#ff,#ff,#ff,#ff,#7e,#88,#00,#00,#00,#00,#fe,#8b,#0f,#0f,#0f,#0f,#fe,#8a,#07,#3c,#f0,#c3,#fe,#9b,#8f,#0f,#6f,#0f,#fe,#8b,#0f,#0f,#0f,#0f,#fc,#f0,#f0,#f0,#f0,#f0,#e1


;*** KERNEL LIBRARY USAGE
use_SyKernel_MTADDP     equ 0   ;Adds a new process and starts it
use_SyKernel_MTDELP     equ 1   ;Stops an existing process and deletes it
use_SyKernel_MTADDT     equ 0   ;Adds a new timer and starts it
use_SyKernel_MTDELT     equ 0   ;Stops an existing timer and deletes it
use_SyKernel_MTSLPP     equ 1   ;Puts an existing process into sleep mode
use_SyKernel_MTWAKP     equ 0   ;Wakes up a process, which was sleeping
use_SyKernel_TMADDT     equ 0   ;Adds a counter for a process
use_SyKernel_TMDELT     equ 0   ;Stops a counter of a process
use_SyKernel_TMDELP     equ 0   ;Stops all counters of one process
use_SyKernel_MTPRIO     equ 1   ;Changes the priority of a process

;*** SYSTEM MANAGER LIBRARY USAGE
use_SySystem_PRGRUN     equ 1   ;Starts an application or opens a document
use_SySystem_PRGEND     equ 1   ;Stops an application and frees its resources
use_SySystem_PRGSRV     equ 0   ;Manages shared services or finds applications
use_SySystem_SYSWRN     equ 1   ;Opens an info, warning or confirm box
use_SySystem_SELOPN     equ 1   ;Opens the file selection dialogue
use_SySystem_HLPOPN	equ 1   ;HLP file handling
use_SySystem_LNGLOD     equ 1   ;Loads a text pack from a language file

;*** DESKTOP MANAGER LIBRARY USAGE
use_SyDesktop_WINOPN    equ 1   ;Opens a new window
use_SyDesktop_WINMEN    equ 0   ;Redraws the menu bar of a window
use_SyDesktop_WININH    equ 1   ;Redraws the content of a window
use_SyDesktop_WINTOL    equ 0   ;Redraws the content of the window toolbar
use_SyDesktop_WINTIT    equ 0   ;Redraws the title bar of a window
use_SyDesktop_WINSTA    equ 0   ;Redraws the status bar of a window
use_SyDesktop_WINMVX    equ 0   ;Sets the X offset of a window content
use_SyDesktop_WINMVY    equ 0   ;Sets the Y offset of a window content
use_SyDesktop_WINTOP    equ 1   ;Takes a window to the front position
use_SyDesktop_WINMAX    equ 0   ;Maximizes a window
use_SyDesktop_WINMIN    equ 0   ;Minimizes a window
use_SyDesktop_WINMID    equ 1   ;Restores a window or the size of a window
use_SyDesktop_WINMOV    equ 0   ;Moves a window to another position
use_SyDesktop_WINSIZ    equ 0   ;Resizes a window
use_SyDesktop_WINCLS    equ 0   ;Closes a window
use_SyDesktop_WINDIN    equ 1   ;Redraws the content of a window (always)
use_SyDesktop_WINSLD    equ 0   ;Redraws the two slider of a window
use_SyDesktop_WINPIN    equ 0   ;Redraws the content of a window (clipped)
use_SyDesktop_WINSIN    equ 0   ;Redraws the content of a control collection
use_SyDesktop_MENCTX    equ 1   ;Opens a context menu
use_SyDesktop_STIADD    equ 0   ;Adds an icon to the systray
use_SyDesktop_STIREM    equ 0   ;Removes an icon from the systray
use_SyDesktop_CONPOS    equ 0   ;Moves a virtual control
use_SyDesktop_CONSIZ    equ 0   ;Resizes a virtual control
use_SyDesktop_Service   equ 0   ;[REQUIRED FOR THE FOLLOWING FUNCTIONS]
use_SyDesktop_MODGET    equ 0   ;Returns the current screen mode
use_SyDesktop_MODSET    equ 0   ;Sets the current screen 
use_SyDesktop_COLGET    equ 0   ;Returns the definition of a colours
use_SyDesktop_COLSET    equ 0   ;Defines one colours
use_SyDesktop_DSKSTP    equ 0   ;Stops the Desktop Manager
use_SyDesktop_DSKCNT    equ 0   ;Continues the Desktop Manager
use_SyDesktop_DSKPNT    equ 0   ;Fills the screen
use_SyDesktop_DSKBGR    equ 0   ;Redraws the desktop background
use_SyDesktop_DSKPLT    equ 0   ;Redraws the complete screen

READ "..\..\..\SRC-Main\Docs-Developer\symbos_lib-Kernel.asm"
READ "..\..\..\SRC-Main\Docs-Developer\symbos_lib-SystemManager.asm"
READ "..\..\..\SRC-Main\Docs-Developer\symbos_lib-DesktopManager.asm"
READ "App-Taskmgr.asm"

App_EndTrns

relocate_table
relocate_end

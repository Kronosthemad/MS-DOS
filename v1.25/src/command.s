# COMMAND version 1.17
#
# This version of COMMAND is divided into three distinct parts. First
# is the resident portion, which includes handlers for interrupts
# 22H (terminate), 23H (Cntrl-C), 24H (fatal error), and 27H (stay
# resident); it also has code to test and, if necessary, reload the
# transient portion. Following the resident is the init code, which is
# overwritten after use. Then comes the transient portion, which
# includes all command processing (whether internal or external).
# The transient portion loads at the end of physical memory, and it may
# be overlayed by programs that need as much memory as possible. When
# the resident portion of command regains control from a user program,
# a checksum is performed on the transient portion to see if it must be
# reloaded. Thus programs which do not need maximum memory will save
# the time required to reload COMMAND when they terminate.

#Use the following booleans to set assembly flags
.equ    FALSE,  0
.equ    TRUE,   NOT FALSE

.equ    IBMVER, FALSE    #Switch to build IBM version of Command
.equ    MSVER,  TRUE     #Switch to build MS-DOS version of Command

.equ    HIGHMEM, TRUE     #Run resident part above transient (high memory)

.equ            LINPERPAG, 23
.equ            NORMPERLIN, 1
.equ            WIDEPERLIN, 5

MISMATCH: "        IF      IBMVER"
.equ    SYM,    ">"
.equ    COMDRV, 1
MISMATCH: "        ENDIF"

MISMATCH: "        IF      MSVER"
.equ    SYM,    ":"
.equ    COMDRV, 0
MISMATCH: "        ENDIF"

.equ    FCB,    5CH
.equ     DSKRESET, 13
.equ    SETBASE, 38
.equ     SRCHFRST, 17
.equ    SRCHNXT, 18
.equ    RENAM,  23
.equ    INCHAR, 1
.equ    GETFAT, 27
.equ    OPEN,   15
.equ    CLOSE,  16
.equ    MAKE,   22
.equ    DELETE, 19
.equ    RDBLK,  39
.equ    WRBLK,  40
.equ    SETDMA, 26
.equ    SELDRV, 14
.equ    GETDRV, 25
.equ     PRINTBUF, 9
.equ    OUTCH,  2
.equ    INBUF,  10
.equ    GETDATE, 2AH
.equ    SETDATE, 2BH
.equ    GETTIME, 2CH
.equ    SETTIME, 2DH
.equ    RR,     33
.equ    RECLEN, 14
.equ    FILLEN, 16
.equ    OFFDATE, 20


#The following are all of the segments used in the load order

MISMATCH: "CODERES SEGMENT"
MISMATCH: "CODERES ENDS"

MISMATCH: "DATARES SEGMENT BYTE"
MISMATCH: "DATARES ENDS"

MISMATCH: "INIT    SEGMENT BYTE"
MISMATCH: "INIT    ENDS"

MISMATCH: "TAIL    SEGMENT PARA"
MISMATCH: "TAIL    ENDS"

MISMATCH: "TRANCODE        SEGMENT PARA"
MISMATCH: "TRANCODE        ENDS"

MISMATCH: "TRANDATA        SEGMENT BYTE"
MISMATCH: "TRANDATA        ENDS"

MISMATCH: "TRANSPACE       SEGMENT BYTE"
MISMATCH: "TRANSPACE       ENDS"

MISMATCH: "RESGROUP        GROUP   CODERES,DATARES,INIT,TAIL"
MISMATCH: "TRANGROUP       GROUP   TRANCODE,TRANDATA,TRANSPACE"

#Data for resident portion

MISMATCH: "DATARES SEGMENT BYTE"
MISMATCH: "        ORG     0"
MISMATCH: "ZERO    =       $"
MISMATCH: "MESBAS  DW      OFFSET RESGROUP:ERR0"
MISMATCH: "        DW      OFFSET RESGROUP:ERR2"
MISMATCH: "        DW      OFFSET RESGROUP:ERR4"
MISMATCH: "        DW      OFFSET RESGROUP:ERR6"
MISMATCH: "        DW      OFFSET RESGROUP:ERR8"
MISMATCH: "        DW      OFFSET RESGROUP:ERR10"
MISMATCH: "        DW      OFFSET RESGROUP:ERR12"
ERR0:   .string "Write protect$"
ERR2:   .string "Not ready$"
ERR4:   .string "Data$"
ERR6:   .string "Seek$"
ERR8:   .string "Sector not found$"
ERR10:  .string "Write fault$"
ERR12:  .string "Disk$"
READ:   .string "read$"
WRITE:  .string "writ$"
ERRMES: .string " error "
IOTYP:  .string "writing"
DRVNUM: .string " drive "
DRVLET: .string "A"
NEWLIN: .byte   13,10,"$"
REQUEST: .string "Abort, Retry, Ignore? $"
MISMATCH: "BADFAT  DB      13,10,"File allocation table bad,$""
MISMATCH: "COMBAD  DB      13,10,"Invalid COMMAND.COM""
MISMATCH: "NEEDCOM DB      13,10,"Insert DOS disk in ""
MISMATCH: "        IF      IBMVER"
MISMATCH: "        DB      "drive A""
MISMATCH: "        ELSE"
MISMATCH: "        DB      "default drive""
MISMATCH: "        ENDIF"
MISMATCH: "PROMPT  DB      13,10,"and strike any key when ready",13,10,"$""
MISMATCH: "NEEDBAT DB      13,10,"Insert disk with batch file$""
MISMATCH: "ENDBATMES DB    13,10,"Terminate batch job (Y/N)? $""
LOADING: .byte  0
BATFCB: .byte   1,"AUTOEXECBAT"
MISMATCH: "        DB      21 DUP(?)"
        .word   0
        .word   0               #Initialize RR field to zero
PARMTAB: .space 2*10, -1        #No parameters initially
BATCH:  .byte   1               #Assume batch mode initially
MISMATCH: "COMFCB  DB      COMDRV,"COMMAND COM""
MISMATCH: "        DB      25 DUP(?)"
MISMATCH: "TRANS   DW      OFFSET TRANGROUP:COMMAND"
TRNSEG: .space  2
BATBYT: .space  1
MEMSIZ: .space  2
SUM:    .space  2
MISMATCH: "INITADD DB      4 DUP(?)"
.equ            RESDATASIZE, $-ZERO
MISMATCH: "DATARES ENDS"

#Data for transient portion

MISMATCH: "TRANDATA        SEGMENT BYTE"
MISMATCH: "        ORG     0"
.equ    ZERO,   $
MISMATCH: "BADNAM  DB      "Bad command or file name",13,10,"$""
MISNAM: .string "Missing file name$"
RENERR: .string "Duplicate file name or "
NOTFND: .string "File not found$"
EXEBAD: .string "Error in EXE file$"
MISMATCH: "NOSPACE DB      "Insufficient disk space",13,10,"$""
MISMATCH: "FULDIR  DB      "File creation error",13,10,"$""
MISMATCH: "OVERWR  DB      "File cannot be copied onto itself",13,10,"$""
MISMATCH: "LOSTERR DB      "Content of destination lost before copy",13,10,"$""
COPIED: .string " File(s) copied$"
DIRMES: .string " File(s)$"
TOOBIG: .string "Program too big to fit in memory$"
BADDRV: .string "Invalid drive specification$"
PAUSMES: .string "Strike a key when ready . . . $"
MISMATCH: "BADSWT  DB      "Illegal switch",13,10,"$""
WEEKTAB: .string "SunMonTueWedThuFriSat"
MISMATCH: "BADDAT  DB      13,10,"Invalid date$""
CURDAT: .string "Current date is $"
MISMATCH: "NEWDAT  DB      13,10,"Enter new date: $""
MISMATCH: "BADTIM  DB      13,10,"Invalid time$""
CURTIM: .string "Current time is $"
MISMATCH: "NEWTIM  DB      13,10,"Enter new time: $""
SUREMES: .string "Are you sure (Y/N)? $"

COMTAB: .byte   4,"DIR",1
MISMATCH: "        DW      OFFSET TRANGROUP:CATALOG"
MISMATCH: "        DB      7,"RENAME",1"
MISMATCH: "        DW      OFFSET TRANGROUP:RENAME"
MISMATCH: "        DB      4,"REN",1"
MISMATCH: "        DW      OFFSET TRANGROUP:RENAME"
MISMATCH: "        DB      6,"ERASE",1"
MISMATCH: "        DW      OFFSET TRANGROUP:ERASE"
MISMATCH: "        DB      4,"DEL",1"
MISMATCH: "        DW      OFFSET TRANGROUP:ERASE"
MISMATCH: "        DB      5,"TYPE",1"
MISMATCH: "        DW      OFFSET TRANGROUP:TYPEFIL"
MISMATCH: "        DB      4,"REM",1"
MISMATCH: "        DW      OFFSET TRANGROUP:COMMAND"
MISMATCH: "        DB      5,"COPY",1"
MISMATCH: "        DW      OFFSET TRANGROUP:COPY"
MISMATCH: "        DB      6,"PAUSE",1"
MISMATCH: "        DW      OFFSET TRANGROUP:PAUSE"
MISMATCH: "        DB      5,"DATE",0"
MISMATCH: "        DW      OFFSET TRANGROUP:DATE"
MISMATCH: "        DB      5,"TIME",0"
MISMATCH: "        DW      OFFSET TRANGROUP:TIME"
        .byte   0               #Terminate command table

COMBUF: .byte   128,1,13

.equ            TRANDATASIZE, $-ZERO
MISMATCH: "TRANDATA        ENDS"

#Uninitialized transient data
MISMATCH: "TRANSPACE       SEGMENT BYTE"
MISMATCH: "        ORG     0"
MISMATCH: "ZERO    =       $"
MISMATCH: "        DB      128 DUP(?)"
MISMATCH: "TPA     DW      1 DUP(?)"
MISMATCH: "RESSEG  DW      1 DUP(?)"
MISMATCH: "CHKDRV  DB      1 DUP(?)"
MISMATCH: "FILTYP  DB      1 DUP(?)"
MISMATCH: "CURDRV  DB      1 DUP(?)"
MISMATCH: "PARM1   DB      1 DUP(?)"
MISMATCH: "PARM2   DB      1 DUP(?)"
MISMATCH: "COMSW   DW      1 DUP(?)"
MISMATCH: "ARG1S   DW      1 DUP(?)"
MISMATCH: "ARG2S   DW      1 DUP(?)"
MISMATCH: "FLAGER  DB      1 DUP(?)"
MISMATCH: "CFLAG   DB      1 DUP(?)"
MISMATCH: "SPECDRV DB      1 DUP(?)"
MISMATCH: "BYTCNT  DW      1 DUP(?)"
MISMATCH: "NXTADD  DW      1 DUP(?)"
MISMATCH: "LINCNT  DB      1 DUP(?)"
MISMATCH: "LINLEN  DB      1 DUP(?)"
MISMATCH: "FILECNT DW      1 DUP(?)"
MISMATCH: "EXEFCB  LABEL WORD"
MISMATCH: "IDLEN   DB      1 DUP(?)"
MISMATCH: "ID      DB      8 DUP(?)"
MISMATCH: "COM     DB      3 DUP(?)"
MISMATCH: "DEST    DB      37 DUP(?)"
MISMATCH: "DESTNAME DB     11 DUP(?)"
MISMATCH: "DIRBUF  DB    
MOV R0, #0xFFFFFFFF
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0x0
MOV R1, #0xa
MOV R2, #0x14
MOV R3, #0x1e
MOV R4, #0x28
MOV R5, #0x32
MOV R6, #0x3c
MOV R7, #0x46
MOV R8, #0x50
MOV R9, #0x5a
MOV R10, #0x64
MOV R11, #0x6e
MOV R12, #0x78
MOV R13, #0x82
MOV R14, #0x8c



MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
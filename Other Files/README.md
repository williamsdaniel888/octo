# Tasks to Complete

- [x] Write MDS of code
- [x] Write implementation and tests for subset of instructions
- [x] Test against VisUAL
- [ ] Randomize VisUAL tests
- [ ] Document bugs in VisUAL and discuss whether they will be followed
- [x] Write MD personal statement
- [ ] Submit code with tests, screenshots of test results, MDS of code, personal statement *_in zip file_*

# Schedule of Instructions
Key parameters:
- **instruction**
- **dest** destination
- **op1** first operand
- **op2** second operand

Here \{_\} stands for an optional parameter:
- **S** write status bits if set
- **cond** condition
- **SHIFT_OP \#expression** extension to implement flexible op2

- **N** negative
- **Z** zero
- **C** carry
- **V** signed overflow

## ADD
op1 + flexible op2, places result in dest register
`ADD{S}{cond} dest, op1, op2 {,SHIFT_OP #expression}`

## ADC
op1 + flexible op2 + C, places result in dest register
`ADC{S}{cond} dest, op1, op2 {,SHIFT_OP #expression}`

## SUB
op1 - flexible op2, places result in dest register
`SUB{S}{cond} dest, op1, op2 {,SHIFT_OP #expression}`

## SBC
op1 - flexible op2 + C - 1, places result in dest register
`SBC{S}{cond} dest, op1, op2 {,SHIFT_OP #expression}`

## RSB
flexible op2 - op1, places result in dest register
`RSB{S}{cond} dest, op1, op2 {,SHIFT_OP #expression}`

## RSC
flexible op2 - op1 + C - 1, places result in dest register
`RSC{S}{cond} dest, op1, op2 {,SHIFT_OP #expression}`

## CMP
Set NZCV on op1 - flexible op2
`CMP{cond} op1, op2 {,SHIFT_OP #expression}`

## CMN
Set NZCV on op1 + flexible op2
`CMN{cond} op1, op2 {,SHIFT_OP #expression}`

# Discussion of Individual Contribution
In the individual phase I attempted to implement the functions belonging to the 'ARITH' set of instructions within the DP.fs module. All types defined in CommonData and CommonLex are used in DP.fs and a minimal set of new types have been introduced to aid with parsing. The module requires the machine's state (DataPath<'INS>) and input from the top-level parser. As output the module determines the new state if the parsed instruction has been successfully executed.

Due to my work pace being unexpectedly slow, the code implements a subset of the basic functionality of VisUAL. Key omissions include:
- conversion of non-immediates to valid literals and adjustment of instructions before evaluation 
- adding an imm12 D.U. type to the flexible operand 2 type
- permitting extra constant types mentioned in the ARM Cortex M3 specification `0x00XY00XY, 0xXY00XY00, 0xXYXYXYXY`. I have noted that VisUAL does not support such instructions from manual testing.

As a consequence of poor time management, I was unable to complete randomized property-based testing against VisUAL. I will press on with completing the code's functionality and begin testing as soon as possible.
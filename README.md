# Tasks to Complete

- [ ] Write MDS of code
- [ ] Write implementation and tests for subset of instructions
- [ ] Test against VisUAL
- [ ] Randomize VisUAL tests
- [ ] Document bugs in VisUAL and discuss whether they will be followed
- [ ] Write MD personal statement
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

# Discussion of Bugs found in VisUAL

Watch this space.
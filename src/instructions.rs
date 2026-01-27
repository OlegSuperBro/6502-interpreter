#![allow(clippy::upper_case_acronyms)] // idk, uppercase instructions feels better for me

#[derive(Debug, PartialEq)]
pub enum LoadOp {
    LDA,
    LDX,
    LDY,
    STA,
    STX,
    STY,
}

#[derive(Debug, PartialEq)]
pub enum RegTransOp {
    TAX,
    TAY,
    TXA,
    TYA,
}

#[derive(Debug, PartialEq)]
pub enum StackOp {
    TSX,
    TXS,
    PHA,
    PHP,
    PLA,
    PLP,
}

#[derive(Debug, PartialEq)]
pub enum LogicOp {
    AND,
    EOR,
    ORA,
    BIT,
}

#[derive(Debug, PartialEq)]
pub enum ArithmeticOp {
    ADC,
    SBC,
    CMP,
    CPX,
    CPY,
}

#[derive(Debug, PartialEq)]
pub enum IncDecOp {
    INC,
    INX,
    INY,
    DEC,
    DEX,
    DEY,
}

#[derive(Debug, PartialEq)]
pub enum ShiftOp {
    ASL,
    LSR,
    ROL,
    ROR,
}

#[derive(Debug, PartialEq)]
pub enum JumpCallOp {
    JMP,
    JSR,
    RTS,
}

#[derive(Debug, PartialEq)]
pub enum BranchOp {
    BCC,
    BCS,
    BEQ,
    BMI,
    BNE,
    BPL,
    BVC,
    BVS,
}

#[derive(Debug, PartialEq)]
pub enum StatusFlagOp {
    CLC,
    CLD,
    CLI,
    CLV,
    SEC,
    SED,
    SEI,
}

#[derive(Debug, PartialEq)]
pub enum SystemFuncOp {
    BRK,
    NOP,
    RTI,
}


#[derive(Debug, PartialEq)]
pub enum OpCode {
    Load(LoadOp),
    RegTrans(RegTransOp),
    Stack(StackOp),
    Logical(LogicOp),
    Arithmetic(ArithmeticOp),
    IncDec(IncDecOp),
    Shift(ShiftOp),
    JumpCall(JumpCallOp),
    Branch(BranchOp),
    StatusFlag(StatusFlagOp),
    SystemFunc(SystemFuncOp)
}